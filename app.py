#!/usr/bin/env python3
"""
朵拉的热带探险 - Web前端
通过SWI-Prolog运行真正的Prolog游戏 (dora_adventure.pl)
"""

from flask import Flask, render_template, jsonify, request, session
import subprocess
import os
import re
import uuid
import threading
import time
import select

# 导入Swiper AI (优先使用Fast-Downward PDDL规划器)
from swiper_planner import SwiperAI

# 获取当前目录
BASE_DIR = os.path.dirname(os.path.abspath(__file__))

app = Flask(__name__,
            template_folder=os.path.join(BASE_DIR, 'templates'),
            static_folder=os.path.join(BASE_DIR, 'static'))
app.secret_key = 'dora_adventure_prolog_2024'

# 存储每个会话的Prolog进程
prolog_sessions = {}

# Swiper AI实例 (自动选择Fast-Downward或启发式AI)
swiper_ai = SwiperAI()

# 回合制相关配置
TURN_BASED_ENABLED = True  # 是否启用回合制
PLAYER_ACTION_COMMANDS = {'go', 'take', 'drop', 'use', 'boots_search'}  # 触发Swiper回合的命令

# 允许的游戏命令白名单
ALLOWED_COMMANDS = {
    # 基础命令（无参数）
    'start', 'look', 'inventory', 'status', 'help', 'quit',
    'boots_search', 'stop_swiper',
    # 带参数的命令前缀
    'go', 'take', 'drop', 'use', 'drop_confirm'
}

# 允许的方向
ALLOWED_DIRECTIONS = {'north', 'south', 'east', 'west', 'up', 'down'}

# 危险命令黑名单 - 只检查命令名，不检查参数
DANGEROUS_COMMANDS = {
    'halt', 'shell', 'system', 'open', 'read_term', 'write_canonical',
    'consult', 'load_files', 'ensure_loaded', 'use_module',
    'assert', 'retract', 'abolish', 'compile',
    'read', 'write', 'put', 'get', 'see', 'seen', 'tell', 'told',
    'current_input', 'current_output', 'set_prolog_flag', 'prolog_flag',
    'call', 'apply', 'findall', 'bagof', 'setof',
    'atom_codes', 'atom_chars', 'number_codes',
    'process_create', 'exec', 'fork', 'popen',
    'listing', 'trace', 'spy', 'debug', 'nodebug',
    'asserta', 'assertz', 'retractall'
}

# 危险字符模式
DANGEROUS_CHARS = [':-', '?-', '->', ';', '!', '[', ']', '|', '\\']


def validate_command(command):
    """
    验证用户输入的命令是否合法
    返回: (is_valid, error_message)
    """
    if not command:
        return False, "请输入命令！"

    # 移除末尾的点号进行验证
    cmd = command.strip()
    if cmd.endswith('.'):
        cmd = cmd[:-1].strip()

    # 检查命令长度
    if len(cmd) > 100:
        return False, "命令太长了！"

    # 检查危险字符
    for char in DANGEROUS_CHARS:
        if char in cmd:
            return False, f"不允许使用该字符！请使用游戏命令如 go(north), take(item) 等。"

    # 解析命令名和参数
    if '(' in cmd:
        # 带参数的命令，如 go(north)
        match = re.match(r'^(\w+)\s*\(\s*(\w+)\s*\)$', cmd)
        if not match:
            return False, f"命令格式不正确！正确格式如: go(north), take(item_name)"

        cmd_name = match.group(1).lower()
        cmd_arg = match.group(2)  # 保持原始大小写

        # 检查是否是危险命令
        if cmd_name in DANGEROUS_COMMANDS:
            return False, f"不允许使用该命令！请使用游戏命令如 go(north), take(item) 等。"

        # 检查是否是允许的命令
        if cmd_name not in ALLOWED_COMMANDS:
            return False, f"未知命令 '{cmd_name}'！可用命令: go, take, drop, use, look, inventory 等"

        # 验证 go 命令的方向
        if cmd_name == 'go' and cmd_arg.lower() not in ALLOWED_DIRECTIONS:
            return False, f"无效方向 '{cmd_arg}'！可用方向: north, south, east, west, up, down"

        # 验证参数只包含字母、数字和下划线
        if not re.match(r'^\w+$', cmd_arg):
            return False, "参数只能包含字母、数字和下划线！"

    else:
        # 无参数命令，如 look, inventory
        cmd_name = cmd.lower()

        # 检查是否是危险命令
        if cmd_name in DANGEROUS_COMMANDS:
            return False, f"不允许使用该命令！请使用游戏命令如 go(north), take(item) 等。"

        if cmd_name not in ALLOWED_COMMANDS:
            return False, f"未知命令 '{cmd_name}'！可用命令: start, look, inventory, status, help, boots_search, stop_swiper"

    return True, None


class PrologSession:
    """管理单个Prolog会话"""

    def __init__(self, session_id):
        self.session_id = session_id
        self.process = None
        self.lock = threading.Lock()
        self.last_activity = time.time()
        # 缓存游戏状态（从输出解析）
        self.cached_state = {
            'player_location': None,
            'swiper_location': None,
            'backpack': [],
            'score': 0,
            'game_time': 100,
            'hidden_items': []
        }

    def start(self):
        """启动Prolog进程"""
        prolog_file = os.path.join(BASE_DIR, 'dora_adventure.pl')

        # 启动SWI-Prolog进程（使用管道，非交互模式）
        self.process = subprocess.Popen(
            ['swipl', '-q', '-s', prolog_file],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=0  # 无缓冲
        )

        # 等待加载完成，并清空初始输出
        time.sleep(1.0)
        # 循环读取直到没有更多输出
        while True:
            output = self._read_output(timeout=1.0)
            if not output:
                break
            time.sleep(0.1)
        return "Prolog游戏已加载"

    def _read_output(self, timeout=2.0):
        """读取Prolog输出"""
        output_lines = []
        start_time = time.time()
        last_read_time = start_time
        idle_timeout = 0.6  # 没有新输出后等待0.6秒

        try:
            while time.time() - start_time < timeout:
                if self.process.poll() is not None:
                    break

                readable, _, _ = select.select([self.process.stdout], [], [], 0.05)
                if readable:
                    line = self.process.stdout.readline()
                    if line:
                        output_lines.append(line.rstrip('\n'))
                        last_read_time = time.time()
                    else:
                        break
                else:
                    # 如果已有输出且空闲超过idle_timeout，认为输出完成
                    if output_lines and (time.time() - last_read_time) > idle_timeout:
                        break

        except Exception as e:
            output_lines.append(f"[读取错误: {e}]")

        return '\n'.join(output_lines)

    def send_command(self, command, silent=False):
        """
        发送命令到Prolog
        silent: 如果为True，执行命令但不更新last_activity（用于管理员查询）
        """
        if not silent:
            self.last_activity = time.time()

        with self.lock:
            if self.process is None or self.process.poll() is not None:
                return "Prolog会话已结束，请刷新页面重新开始。"

            try:
                # 先读取残留的输出（不丢弃，合并到结果中）
                pending_output = self._read_output(timeout=0.1)

                # 确保命令以点号结尾
                command = command.strip()
                if not command.endswith('.'):
                    command += '.'

                # 使用标记法确保读取完整输出
                # 发送命令后，再发送一个标记命令，当看到标记时说明原命令输出完成
                marker = f'__END_MARKER_{time.time()}__'

                # 发送用户命令
                self.process.stdin.write(command + '\n')
                self.process.stdin.flush()

                # 发送标记命令
                self.process.stdin.write(f"write('{marker}'), nl.\n")
                self.process.stdin.flush()

                # 读取输出直到看到标记
                output_parts = []
                start_time = time.time()
                max_wait = 15.0  # 最长等待15秒

                while time.time() - start_time < max_wait:
                    part = self._read_output(timeout=2.0)
                    if part:
                        # 检查是否包含标记
                        if marker in part:
                            # 移除标记及其后面的内容
                            idx = part.find(marker)
                            before_marker = part[:idx].rstrip()
                            if before_marker:
                                output_parts.append(before_marker)
                            break
                        output_parts.append(part)
                    else:
                        # 没有输出，短暂等待后继续
                        time.sleep(0.1)

                output = '\n'.join(output_parts) if output_parts else ''

                # 合并残留输出和新输出
                if pending_output:
                    output = pending_output + '\n' + output if output else pending_output

                # 清理输出
                output = self._clean_output(output)

                # 从输出解析状态信息更新缓存（仅非静默模式）
                if not silent:
                    self._parse_state_from_output(output)

                return output if output else "(命令已执行)"

            except Exception as e:
                return f"命令执行错误: {e}"

    def _parse_state_from_output(self, output):
        """从游戏输出中解析状态信息"""
        # 解析位置（从 "=== xxx (Xxx) ===" 格式）
        loc_match = re.search(r'=== .+ \((\w+)\) ===', output)
        if loc_match:
            # 从房间名提取位置ID
            pass

        # 解析移动信息
        move_match = re.search(r'你从\w+移动到(\w+)', output)
        if move_match:
            self.cached_state['player_location'] = move_match.group(1)

        # 解析得分
        score_match = re.search(r'得分:\s*(\d+)', output)
        if score_match:
            self.cached_state['score'] = int(score_match.group(1))

        # 解析时间
        time_match = re.search(r'剩余时间:\s*(\d+)', output)
        if time_match:
            self.cached_state['game_time'] = int(time_match.group(1))

        # 解析当前位置（从输出中）
        current_loc_match = re.search(r'当前位置:\s*(\w+)', output)
        if current_loc_match:
            self.cached_state['player_location'] = current_loc_match.group(1)

    def _clean_output(self, output):
        """清理Prolog输出"""
        lines = output.split('\n')
        cleaned = []

        for line in lines:
            # 移除Prolog提示符
            line = re.sub(r'^\?-\s*', '', line)
            line = re.sub(r'^\|\s*', '', line)
            # 移除单独的true/false
            stripped = line.strip()
            if stripped in ['true.', 'false.', 'true', 'false']:
                continue
            if stripped:
                cleaned.append(line)

        return '\n'.join(cleaned)

    def get_game_state_for_ai(self):
        """
        从Prolog查询完整游戏状态,供Swiper AI使用
        """
        state = {
            'player_location': None,
            'swiper_location': None,
            'player_backpack': [],
            'swiper_carrying': [],
            'room_items': {},
            'hidden_items': {},
            'swiper_blocked': False
        }

        if self.process is None or self.process.poll() is not None:
            return state

        try:
            # 查询玩家位置
            output = self.send_command('player_location(X), write(X), nl', silent=True)
            match = re.search(r'(\w+)', output)
            if match and match.group(1) not in ['true', 'false', 'X', 'write', 'nl']:
                state['player_location'] = match.group(1)

            # 查询Swiper位置
            output = self.send_command('swiper_location(X), write(X), nl', silent=True)
            match = re.search(r'(\w+)', output)
            if match and match.group(1) not in ['true', 'false', 'X', 'write', 'nl']:
                state['swiper_location'] = match.group(1)

            # 查询玩家背包
            output = self.send_command('player_backpack(L), write(L), nl', silent=True)
            list_match = re.search(r'\[([^\]]*)\]', output)
            if list_match:
                items_str = list_match.group(1)
                if items_str.strip():
                    state['player_backpack'] = [i.strip() for i in items_str.split(',') if i.strip()]

            # 查询房间物品
            output = self.send_command('findall((R,I), room_has_item(R,I), L), write(L), nl', silent=True)
            # 解析 [(room1,item1),(room2,item2),...]
            pairs = re.findall(r'\((\w+),\s*(\w+)\)', output)
            for room, item in pairs:
                if room not in state['room_items']:
                    state['room_items'][room] = []
                state['room_items'][room].append(item)

            # 查询藏匿物品
            output = self.send_command('findall((L,I), swiper_hidden_item(L,I), R), write(R), nl', silent=True)
            pairs = re.findall(r'\((\w+),\s*(\w+)\)', output)
            for loc, item in pairs:
                if loc not in state['hidden_items']:
                    state['hidden_items'][loc] = []
                state['hidden_items'][loc].append(item)

            # 查询Swiper是否被阻止(检查swiper_confused)
            output = self.send_command('(swiper_confused(_) -> write(blocked) ; write(ok)), nl', silent=True)
            if 'blocked' in output:
                state['swiper_blocked'] = True

        except Exception as e:
            pass

        return state

    def execute_swiper_action(self, action: dict) -> str:
        """
        在Prolog中执行Swiper的行动
        返回描述消息
        """
        action_type = action.get('type', 'wait')
        message = action.get('message', '')

        try:
            if action_type == 'move':
                # 更新Swiper位置
                new_loc = action.get('to')
                if new_loc:
                    self.send_command(f"retract(swiper_location(_)), assert(swiper_location({new_loc}))", silent=True)

            elif action_type == 'steal':
                # 从玩家背包偷窃
                item = action.get('item')
                if item:
                    # 从玩家背包移除
                    self.send_command(f"player_backpack(B), delete(B, {item}, NB), retract(player_backpack(B)), assert(player_backpack(NB))", silent=True)
                    # 注意: 简化处理,偷窃后Swiper立即藏匿或逃跑

            elif action_type == 'take':
                # Swiper从地上捡物品
                item = action.get('item')
                loc = action.get('location')
                if item and loc:
                    self.send_command(f"retract(room_has_item({loc}, {item}))", silent=True)

            elif action_type == 'hide':
                # Swiper藏匿物品
                item = action.get('item')
                loc = action.get('location')
                if item and loc:
                    self.send_command(f"assert(swiper_hidden_item({loc}, {item}))", silent=True)

        except Exception as e:
            message += f" (执行错误: {e})"

        return message

    def close(self):
        """关闭Prolog进程"""
        if self.process:
            try:
                self.process.stdin.write('halt.\n')
                self.process.stdin.flush()
                self.process.wait(timeout=2)
            except:
                self.process.kill()
            finally:
                self.process = None


def get_or_create_session():
    """获取或创建Prolog会话"""
    if 'session_id' not in session:
        session['session_id'] = str(uuid.uuid4())

    session_id = session['session_id']

    if session_id not in prolog_sessions:
        prolog_session = PrologSession(session_id)
        prolog_sessions[session_id] = prolog_session

    return prolog_sessions[session_id]


def cleanup_old_sessions():
    """清理长时间不活跃的会话"""
    current_time = time.time()
    timeout = 1800  # 30分钟

    to_remove = []
    for sid, ps in prolog_sessions.items():
        if current_time - ps.last_activity > timeout:
            to_remove.append(sid)

    for sid in to_remove:
        prolog_sessions[sid].close()
        del prolog_sessions[sid]


# ============================================================
# 玩家界面路由
# ============================================================

@app.route('/')
def index():
    return render_template('index.html')


@app.route('/api/start', methods=['POST'])
def start_game():
    """启动游戏"""
    cleanup_old_sessions()

    # 关闭旧会话
    if 'session_id' in session and session['session_id'] in prolog_sessions:
        prolog_sessions[session['session_id']].close()
        del prolog_sessions[session['session_id']]

    # 创建新会话
    session['session_id'] = str(uuid.uuid4())
    prolog_session = PrologSession(session['session_id'])
    prolog_sessions[session['session_id']] = prolog_session

    try:
        prolog_session.start()
        output = prolog_session.send_command('start')
        return jsonify({
            'success': True,
            'output': output
        })
    except Exception as e:
        return jsonify({
            'success': False,
            'error': str(e)
        })


@app.route('/api/command', methods=['POST'])
def send_command():
    """发送游戏命令"""
    data = request.get_json()
    command = data.get('command', '').strip()

    if not command:
        return jsonify({'output': '请输入命令！'})

    # 验证命令
    is_valid, error_msg = validate_command(command)
    if not is_valid:
        return jsonify({'output': f'警告: {error_msg}'})

    if 'session_id' not in session or session['session_id'] not in prolog_sessions:
        return jsonify({'output': '会话已过期，请点击"开始新游戏"。'})

    prolog_session = prolog_sessions[session['session_id']]

    # 执行玩家命令
    output = prolog_session.send_command(command)

    # 回合制: 检查是否需要执行Swiper回合
    swiper_message = ''
    if TURN_BASED_ENABLED:
        # 解析命令名
        cmd = command.strip()
        if cmd.endswith('.'):
            cmd = cmd[:-1]
        cmd_name = cmd.split('(')[0].lower() if '(' in cmd else cmd.lower()

        # 如果是玩家行动命令,触发Swiper回合
        if cmd_name in PLAYER_ACTION_COMMANDS:
            swiper_message = execute_swiper_turn(prolog_session)

    # 合并输出
    if swiper_message:
        output = output + '\n\n--- Swiper的回合 ---\n' + swiper_message

    return jsonify({'output': output})


def execute_swiper_turn(prolog_session) -> str:
    """
    执行Swiper的回合
    使用PDDL AI规划Swiper的行动并执行
    """
    try:
        # 获取当前游戏状态
        game_state = prolog_session.get_game_state_for_ai()

        # 如果无法获取状态,跳过Swiper回合
        if not game_state.get('player_location'):
            return ''

        # AI决策
        action = swiper_ai.get_action(game_state)

        # 执行行动
        message = prolog_session.execute_swiper_action(action)

        return message

    except Exception as e:
        return f'(Swiper AI错误: {e})'


# ============================================================
# 管理员界面路由
# ============================================================

@app.route('/admin')
def admin():
    return render_template('admin.html')


def query_prolog_state(prolog_session, use_cache=True):
    """
    获取Prolog会话的游戏状态
    use_cache: 如果为True，只返回缓存状态；如果为False，直接查询Prolog（静默模式）
    """
    if use_cache:
        # 直接返回缓存的状态，避免干扰游戏
        return {
            'player_location': prolog_session.cached_state.get('player_location'),
            'swiper_location': prolog_session.cached_state.get('swiper_location'),
            'backpack': prolog_session.cached_state.get('backpack', []),
            'swiper_carrying': [],
            'hidden_items': prolog_session.cached_state.get('hidden_items', []),
            'score': prolog_session.cached_state.get('score', 0),
            'game_time': prolog_session.cached_state.get('game_time', 100)
        }

    # 直接查询Prolog（静默模式 - 不更新缓存，不影响游戏输出）
    state = {
        'player_location': None,
        'swiper_location': None,
        'backpack': [],
        'swiper_carrying': [],
        'hidden_items': [],
        'score': 0,
        'game_time': 100
    }

    if prolog_session.process is None or prolog_session.process.poll() is not None:
        return state

    try:
        # 查询玩家位置
        output = prolog_session.send_command('player_at(X), write(X), nl', silent=True)
        match = re.search(r'(\w+)', output)
        if match and match.group(1) not in ['true', 'false', 'X']:
            state['player_location'] = match.group(1)

        # 查询Swiper位置
        output = prolog_session.send_command('swiper_at(X), write(X), nl', silent=True)
        match = re.search(r'(\w+)', output)
        if match and match.group(1) not in ['true', 'false', 'X']:
            state['swiper_location'] = match.group(1)

        # 查询背包
        output = prolog_session.send_command('findall(I, player_has(I), L), write(L), nl', silent=True)
        list_match = re.search(r'\[([^\]]*)\]', output)
        if list_match:
            items_str = list_match.group(1)
            if items_str.strip():
                state['backpack'] = [i.strip() for i in items_str.split(',') if i.strip()]

    except Exception as e:
        pass

    return state


@app.route('/admin/api/sessions')
def admin_sessions():
    """获取所有会话列表"""
    sessions_data = []
    total_items = 0
    total_hidden = 0

    for session_id, prolog_session in prolog_sessions.items():
        state = query_prolog_state(prolog_session, use_cache=True)

        # 计算最后活动时间
        elapsed = time.time() - prolog_session.last_activity
        if elapsed < 60:
            last_activity = f"{int(elapsed)}秒前"
        elif elapsed < 3600:
            last_activity = f"{int(elapsed/60)}分钟前"
        else:
            last_activity = f"{int(elapsed/3600)}小时前"

        sessions_data.append({
            'session_id': session_id,
            'process_alive': prolog_session.process is not None and prolog_session.process.poll() is None,
            'player_location': state['player_location'],
            'swiper_location': state['swiper_location'],
            'backpack': state['backpack'],
            'swiper_carrying': state.get('swiper_carrying', []),
            'hidden_items': state['hidden_items'],
            'score': state['score'],
            'game_time': state['game_time'],
            'last_activity': last_activity
        })

        total_items += len(state['backpack'])
        total_hidden += len(state['hidden_items'])

    return jsonify({
        'total_sessions': len(sessions_data),
        'total_items_collected': total_items,
        'total_hidden_items': total_hidden,
        'sessions': sessions_data
    })


@app.route('/admin/api/shell', methods=['POST'])
def admin_shell():
    """管理员直接执行Prolog命令（无过滤）"""
    data = request.get_json()
    session_id = data.get('session_id')
    command = data.get('command', '').strip()

    if not session_id or session_id not in prolog_sessions:
        return jsonify({'error': '会话不存在'})

    if not command:
        return jsonify({'error': '请输入命令'})

    prolog_session = prolog_sessions[session_id]

    # 管理员命令不经过验证，直接发送到Prolog
    output = prolog_session.send_command(command, silent=True)

    return jsonify({'output': output})


@app.route('/admin/api/new_session', methods=['POST'])
def admin_new_session():
    """管理员创建新的Prolog会话"""
    session_id = str(uuid.uuid4())
    prolog_session = PrologSession(session_id)
    prolog_sessions[session_id] = prolog_session

    try:
        prolog_session.start()
        # 自动启动游戏
        output = prolog_session.send_command('start')
        return jsonify({
            'session_id': session_id,
            'output': output
        })
    except Exception as e:
        return jsonify({
            'error': str(e)
        })


@app.route('/admin/api/kill', methods=['POST'])
def admin_kill_session():
    """管理员终止会话"""
    data = request.get_json()
    session_id = data.get('session_id')

    if not session_id or session_id not in prolog_sessions:
        return jsonify({'error': '会话不存在'})

    prolog_sessions[session_id].close()
    del prolog_sessions[session_id]

    return jsonify({'message': '会话已终止'})


if __name__ == '__main__':
    print("=" * 60)
    print("朵拉的热带探险 - Prolog Web版")
    print("=" * 60)
    print()
    print("  这是真正的Prolog游戏！")
    print("  运行 dora_adventure.pl (SWI-Prolog)")
    print()
    print("  请访问: http://localhost:5002")
    print("  管理员: http://localhost:5002/admin")
    print()
    print("  确保已安装 SWI-Prolog:")
    print("     brew install swi-prolog")
    print()
    print("=" * 60)
    print()

    app.run(debug=True, host='0.0.0.0', port=5002, threaded=True)
