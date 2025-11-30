"""
Swiper AI Planner - 使用PDDL规划Swiper的行动
回合制系统: 玩家行动后，Swiper使用PDDL规划最优行动

支持两种规划器:
1. FastDownwardPlanner - 使用fast-downward (功能更完整)
2. SimpleSwiperAI - 启发式AI (后备方案)
"""

import subprocess
import tempfile
import os
import re
import shutil
from typing import Optional, Dict, List, Tuple

# Fast-Downward 路径
FAST_DOWNWARD_PATH = '/tmp/fast-downward/fast-downward.py'

# 完整PDDL域定义 - 基于对手领域.pddl，移除派生谓词以兼容更多规划器
SWIPER_DOMAIN = '''
(define (domain swiper_behavior)
    (:requirements :strips :typing :negative-preconditions)

    (:types
        location item
    )

    (:predicates
        ; 位置相关
        (swiper_at ?l - location)
        (player_at ?l - location)
        (connected ?from - location ?to - location)

        ; 物品相关
        (item_at ?i - item ?l - location)
        (player_has ?i - item)
        (swiper_has ?i - item)
        (hidden_item ?i - item ?l - location)
        (valuable ?i - item)

        ; 地形特性
        (climbable ?l - location)
        (swimmable ?l - location)
        (has_escape_route ?l - location)
        (safe_zone ?l - location)

        ; Swiper状态
        (swiper_blocked)
        (swiper_confused)
    )

    ; 动作1: Swiper普通移动
    (:action swiper_move
        :parameters (?from - location ?to - location)
        :precondition (and
            (swiper_at ?from)
            (connected ?from ?to)
            (not (swiper_confused))
            (not (swiper_blocked))
        )
        :effect (and
            (not (swiper_at ?from))
            (swiper_at ?to)
        )
    )

    ; 动作2: Swiper潜行 (选择有逃跑路线的位置)
    (:action swiper_sneak
        :parameters (?from - location ?to - location)
        :precondition (and
            (swiper_at ?from)
            (connected ?from ?to)
            (not (swiper_confused))
            (not (swiper_blocked))
            (has_escape_route ?to)
        )
        :effect (and
            (not (swiper_at ?from))
            (swiper_at ?to)
        )
    )

    ; 动作3: Swiper从地上捡物品
    (:action swiper_take
        :parameters (?i - item ?l - location)
        :precondition (and
            (swiper_at ?l)
            (item_at ?i ?l)
            (valuable ?i)
            (not (swiper_confused))
            (not (swiper_blocked))
        )
        :effect (and
            (not (item_at ?i ?l))
            (swiper_has ?i)
        )
    )

    ; 动作4: Swiper从玩家偷窃
    (:action swiper_steal
        :parameters (?i - item ?l - location)
        :precondition (and
            (swiper_at ?l)
            (player_at ?l)
            (player_has ?i)
            (valuable ?i)
            (not (swiper_confused))
            (not (swiper_blocked))
        )
        :effect (and
            (not (player_has ?i))
            (swiper_has ?i)
        )
    )

    ; 动作5: Swiper藏匿物品 (在可攀爬位置)
    (:action swiper_hide_climb
        :parameters (?i - item ?l - location)
        :precondition (and
            (swiper_at ?l)
            (swiper_has ?i)
            (climbable ?l)
        )
        :effect (and
            (not (swiper_has ?i))
            (hidden_item ?i ?l)
        )
    )

    ; 动作6: Swiper藏匿物品 (在可游泳位置)
    (:action swiper_hide_swim
        :parameters (?i - item ?l - location)
        :precondition (and
            (swiper_at ?l)
            (swiper_has ?i)
            (swimmable ?l)
        )
        :effect (and
            (not (swiper_has ?i))
            (hidden_item ?i ?l)
        )
    )

    ; 动作7: Swiper逃跑
    (:action swiper_flee
        :parameters (?from - location ?to - location)
        :precondition (and
            (swiper_at ?from)
            (player_at ?from)
            (connected ?from ?to)
            (has_escape_route ?to)
            (not (swiper_blocked))
        )
        :effect (and
            (not (swiper_at ?from))
            (swiper_at ?to)
        )
    )
)
'''

# 地图连接定义
MAP_CONNECTIONS = [
    # 区域1: 茂密丛林
    ('jungle_entrance', 'jungle_path'),
    ('jungle_path', 'jungle_entrance'),
    ('jungle_path', 'deep_forest'),
    ('deep_forest', 'jungle_path'),
    ('deep_forest', 'ancient_grove'),
    ('ancient_grove', 'deep_forest'),
    ('ancient_grove', 'mystic_pond'),
    ('mystic_pond', 'ancient_grove'),
    # 区域1到区域2
    ('jungle_entrance', 'beach_entrance'),
    ('beach_entrance', 'jungle_entrance'),
    # 区域2: 金色海滩
    ('beach_entrance', 'sandy_shore'),
    ('sandy_shore', 'beach_entrance'),
    ('beach_entrance', 'coral_reef'),
    ('coral_reef', 'beach_entrance'),
    ('sandy_shore', 'palm_grove'),
    ('palm_grove', 'sandy_shore'),
    # 区域2到区域3
    ('coral_reef', 'valley_entrance'),
    ('valley_entrance', 'coral_reef'),
    # 区域3: 瀑布河谷
    ('valley_entrance', 'waterfall_base'),
    ('waterfall_base', 'valley_entrance'),
    ('waterfall_base', 'river_bend'),
    ('river_bend', 'waterfall_base'),
    ('waterfall_base', 'crystal_cave'),
    ('crystal_cave', 'waterfall_base'),
    # 区域3到区域4
    ('crystal_cave', 'ruins_entrance'),
    ('ruins_entrance', 'crystal_cave'),
    # 区域4: 古老遗迹
    ('ruins_entrance', 'stone_circle'),
    ('stone_circle', 'ruins_entrance'),
    ('stone_circle', 'secret_chamber'),
    ('secret_chamber', 'stone_circle'),
    ('stone_circle', 'ancestral_hall'),
    ('ancestral_hall', 'stone_circle'),
    ('ancestral_hall', 'final_gate'),
    ('final_gate', 'ancestral_hall'),
]

# 可攀爬位置
CLIMBABLE_LOCATIONS = [
    'ancient_grove', 'mystic_pond', 'deep_forest',
    'waterfall_base', 'crystal_cave',
    'stone_circle', 'ruins_entrance'
]

# 可游泳位置
SWIMMABLE_LOCATIONS = [
    'coral_reef', 'sandy_shore', 'palm_grove',
    'river_bend', 'waterfall_base'
]

# 有逃跑路线的位置
ESCAPE_ROUTE_LOCATIONS = [
    'jungle_path', 'beach_entrance', 'valley_entrance',
    'ruins_entrance', 'coral_reef', 'stone_circle'
]

# 安全区域
SAFE_ZONES = ['jungle_entrance', 'final_gate']

# 有价值的物品
VALUABLE_ITEMS = [
    'crystal_key', 'water_amulet', 'ancestral_map',
    'ancient_totem', 'rainbow_scale', 'river_pearl'
]

# 所有位置
ALL_LOCATIONS = [
    'jungle_entrance', 'jungle_path', 'deep_forest', 'ancient_grove', 'mystic_pond',
    'beach_entrance', 'sandy_shore', 'coral_reef', 'palm_grove',
    'valley_entrance', 'waterfall_base', 'river_bend', 'crystal_cave',
    'ruins_entrance', 'stone_circle', 'secret_chamber', 'ancestral_hall', 'final_gate'
]

# 所有物品
ALL_ITEMS = [
    'herbal_medicine', 'sharp_machete', 'ancient_totem',
    'glowing_seashell', 'fishing_net', 'coconut_water',
    'water_amulet', 'rainbow_scale', 'river_pearl',
    'sun_dial', 'crystal_key', 'ancestral_map'
]


class FastDownwardPlanner:
    """使用Fast-Downward的Swiper AI规划器"""

    def __init__(self):
        self.temp_dir = tempfile.mkdtemp()
        self.domain_file = os.path.join(self.temp_dir, 'domain.pddl')
        self.problem_file = os.path.join(self.temp_dir, 'problem.pddl')
        self.plan_file = os.path.join(self.temp_dir, 'sas_plan')

        # 写入域文件
        with open(self.domain_file, 'w') as f:
            f.write(SWIPER_DOMAIN)

        # 检查fast-downward是否可用
        self.fd_available = os.path.exists(FAST_DOWNWARD_PATH)

    def generate_problem(self, game_state: Dict, goal_type: str = 'steal') -> str:
        """
        根据游戏状态生成PDDL问题文件
        goal_type: 'steal' - 偷窃目标, 'hide' - 藏匿目标, 'approach' - 接近玩家
        """
        swiper_loc = game_state.get('swiper_location', 'jungle_path')
        player_loc = game_state.get('player_location', 'jungle_entrance')
        player_items = game_state.get('player_backpack', [])
        swiper_items = game_state.get('swiper_carrying', [])
        room_items = game_state.get('room_items', {})
        swiper_blocked = game_state.get('swiper_blocked', False)
        swiper_confused = game_state.get('swiper_confused', False)

        # 构建init部分
        init_parts = [
            f'(swiper_at {swiper_loc})',
            f'(player_at {player_loc})',
        ]

        # 地图连接
        for f, t in MAP_CONNECTIONS:
            init_parts.append(f'(connected {f} {t})')

        # 地形特性
        for loc in CLIMBABLE_LOCATIONS:
            init_parts.append(f'(climbable {loc})')
        for loc in SWIMMABLE_LOCATIONS:
            init_parts.append(f'(swimmable {loc})')
        for loc in ESCAPE_ROUTE_LOCATIONS:
            init_parts.append(f'(has_escape_route {loc})')
        for loc in SAFE_ZONES:
            init_parts.append(f'(safe_zone {loc})')

        # 物品价值
        for item in VALUABLE_ITEMS:
            init_parts.append(f'(valuable {item})')

        # 玩家物品
        for item in player_items:
            init_parts.append(f'(player_has {item})')

        # Swiper物品
        for item in swiper_items:
            init_parts.append(f'(swiper_has {item})')

        # 地上物品
        for loc, items in room_items.items():
            for item in items:
                init_parts.append(f'(item_at {item} {loc})')

        # Swiper状态
        if swiper_blocked:
            init_parts.append('(swiper_blocked)')
        if swiper_confused:
            init_parts.append('(swiper_confused)')

        init_str = '\n        '.join(init_parts)

        # 根据目标类型设置goal
        if goal_type == 'hide' and swiper_items:
            # 藏匿目标: 藏匿手中物品
            item = swiper_items[0]
            goal_str = f'(hidden_item {item} {swiper_loc})' if (swiper_loc in CLIMBABLE_LOCATIONS or swiper_loc in SWIMMABLE_LOCATIONS) else f'(exists (?l - location) (hidden_item {item} ?l))'
            # Fast-downward不支持exists在goal中，简化
            hide_locs = [l for l in CLIMBABLE_LOCATIONS + SWIMMABLE_LOCATIONS]
            if hide_locs:
                goal_str = f'(hidden_item {item} {hide_locs[0]})'
        elif goal_type == 'steal' and player_items:
            # 偷窃目标
            valuable_items = [i for i in player_items if i in VALUABLE_ITEMS]
            if valuable_items:
                goal_str = f'(swiper_has {valuable_items[0]})'
            else:
                goal_str = f'(swiper_at {player_loc})'
        else:
            # 接近玩家
            goal_str = f'(swiper_at {player_loc})'

        problem = f'''
(define (problem swiper_turn)
    (:domain swiper_behavior)

    (:objects
        {' '.join(ALL_LOCATIONS)} - location
        {' '.join(ALL_ITEMS)} - item
    )

    (:init
        {init_str}
    )

    (:goal
        {goal_str}
    )
)
'''
        return problem

    def plan(self, game_state: Dict) -> Optional[Tuple[str, List[str]]]:
        """
        使用Fast-Downward规划Swiper的行动
        返回: (action_name, [params]) 或 None
        """
        if not self.fd_available:
            return None

        # 根据状态选择目标
        swiper_items = game_state.get('swiper_carrying', [])
        player_items = game_state.get('player_backpack', [])

        if swiper_items:
            goal_type = 'hide'
        elif player_items:
            goal_type = 'steal'
        else:
            goal_type = 'approach'

        # 生成问题文件
        problem = self.generate_problem(game_state, goal_type)
        with open(self.problem_file, 'w') as f:
            f.write(problem)

        # 清理旧的计划文件
        if os.path.exists(self.plan_file):
            os.remove(self.plan_file)

        try:
            # 调用fast-downward
            result = subprocess.run(
                [
                    'python3', FAST_DOWNWARD_PATH,
                    '--plan-file', self.plan_file,
                    self.domain_file,
                    self.problem_file,
                    '--search', 'astar(lmcut())'
                ],
                capture_output=True,
                text=True,
                timeout=3,
                cwd=self.temp_dir
            )

            # 读取计划文件
            if os.path.exists(self.plan_file):
                with open(self.plan_file, 'r') as f:
                    lines = f.readlines()
                    for line in lines:
                        line = line.strip()
                        if line.startswith('(') and line.endswith(')'):
                            # 解析 (action param1 param2)
                            content = line[1:-1]
                            parts = content.split()
                            if parts:
                                return (parts[0], parts[1:])

            return None

        except subprocess.TimeoutExpired:
            return None
        except Exception as e:
            print(f"Fast-Downward规划错误: {e}")
            return None

    def get_action(self, game_state: Dict) -> Dict:
        """获取Swiper的下一步行动"""
        result = self.plan(game_state)

        if result is None:
            # 规划失败，使用后备策略
            return self._fallback_action(game_state)

        action, params = result

        # 解析动作
        if action == 'swiper_move' or action == 'swiper_sneak':
            return {
                'type': 'move',
                'from': params[0] if len(params) > 0 else None,
                'to': params[1] if len(params) > 1 else None,
                'message': f'Swiper悄悄移动到了{params[1] if len(params) > 1 else "某处"}...'
            }
        elif action == 'swiper_take':
            return {
                'type': 'take',
                'item': params[0] if len(params) > 0 else None,
                'location': params[1] if len(params) > 1 else None,
                'message': f'Swiper捡起了地上的{params[0] if params else "物品"}!'
            }
        elif action == 'swiper_steal':
            return {
                'type': 'steal',
                'item': params[0] if len(params) > 0 else None,
                'message': f'Swiper从你的背包偷走了{params[0] if params else "东西"}!'
            }
        elif action in ('swiper_hide_climb', 'swiper_hide_swim'):
            hide_type = "树上" if action == 'swiper_hide_climb' else "水里"
            return {
                'type': 'hide',
                'item': params[0] if len(params) > 0 else None,
                'location': params[1] if len(params) > 1 else None,
                'message': f'Swiper把{params[0] if params else "物品"}藏到了{hide_type}!'
            }
        elif action == 'swiper_flee':
            return {
                'type': 'move',
                'from': params[0] if len(params) > 0 else None,
                'to': params[1] if len(params) > 1 else None,
                'message': f'Swiper慌张地逃向{params[1] if len(params) > 1 else "某处"}!'
            }
        else:
            return self._fallback_action(game_state)

    def _fallback_action(self, game_state: Dict) -> Dict:
        """后备策略"""
        import random

        swiper_loc = game_state.get('swiper_location', 'jungle_path')
        neighbors = [to for (fr, to) in MAP_CONNECTIONS if fr == swiper_loc]

        if neighbors:
            target = random.choice(neighbors)
            return {
                'type': 'move',
                'from': swiper_loc,
                'to': target,
                'message': 'Swiper在某处游荡...'
            }

        return {
            'type': 'wait',
            'message': 'Swiper在观察你的行动...'
        }

    def cleanup(self):
        """清理临时文件"""
        try:
            shutil.rmtree(self.temp_dir)
        except:
            pass


# 简化版Swiper AI (后备方案)
class SimpleSwiperAI:
    """
    简化的Swiper AI - 使用启发式规则
    当PDDL规划器不可用时使用
    """

    def __init__(self):
        self.adjacency = {}
        for f, t in MAP_CONNECTIONS:
            if f not in self.adjacency:
                self.adjacency[f] = []
            self.adjacency[f].append(t)

    def _find_path(self, start: str, goal: str) -> List[str]:
        """BFS找最短路径"""
        if start == goal:
            return [start]

        visited = {start}
        queue = [(start, [start])]

        while queue:
            current, path = queue.pop(0)
            for neighbor in self.adjacency.get(current, []):
                if neighbor == goal:
                    return path + [neighbor]
                if neighbor not in visited:
                    visited.add(neighbor)
                    queue.append((neighbor, path + [neighbor]))

        return []

    def get_action(self, game_state: Dict) -> Dict:
        """决定Swiper的行动"""
        import random

        swiper_loc = game_state.get('swiper_location', 'jungle_path')
        player_loc = game_state.get('player_location', 'jungle_entrance')
        player_items = game_state.get('player_backpack', [])
        swiper_items = game_state.get('swiper_carrying', [])
        room_items = game_state.get('room_items', {})
        swiper_blocked = game_state.get('swiper_blocked', False)

        if swiper_blocked:
            return {
                'type': 'wait',
                'message': 'Swiper: "Oh man!" (被阻止了)'
            }

        # 优先藏匿
        if swiper_items:
            if swiper_loc in CLIMBABLE_LOCATIONS or swiper_loc in SWIMMABLE_LOCATIONS:
                item = swiper_items[0]
                hide_type = "树上" if swiper_loc in CLIMBABLE_LOCATIONS else "水里"
                return {
                    'type': 'hide',
                    'item': item,
                    'location': swiper_loc,
                    'message': f'Swiper把{item}藏到了{hide_type}!'
                }
            else:
                hide_locations = CLIMBABLE_LOCATIONS + SWIMMABLE_LOCATIONS
                best_path = None
                for loc in hide_locations:
                    path = self._find_path(swiper_loc, loc)
                    if path and (best_path is None or len(path) < len(best_path)):
                        best_path = path

                if best_path and len(best_path) > 1:
                    return {
                        'type': 'move',
                        'from': swiper_loc,
                        'to': best_path[1],
                        'message': f'Swiper带着偷来的东西逃向{best_path[1]}...'
                    }

        # 偷窃
        if swiper_loc == player_loc and player_items:
            valuable_player_items = [i for i in player_items if i in VALUABLE_ITEMS]
            if valuable_player_items:
                item = random.choice(valuable_player_items)
                return {
                    'type': 'steal',
                    'item': item,
                    'message': f'Swiper突然出现，从你的背包里偷走了{item}!'
                }

        # 捡起地上物品
        current_items = room_items.get(swiper_loc, [])
        valuable_here = [i for i in current_items if i in VALUABLE_ITEMS]
        if valuable_here:
            item = valuable_here[0]
            return {
                'type': 'take',
                'item': item,
                'location': swiper_loc,
                'message': f'Swiper捡起了{item}!'
            }

        # 朝玩家移动
        path = self._find_path(swiper_loc, player_loc)
        if path and len(path) > 1:
            if random.random() < 0.8:
                return {
                    'type': 'move',
                    'from': swiper_loc,
                    'to': path[1],
                    'message': f'你听到Swiper在附近移动的声音...'
                }

        # 随机移动
        neighbors = self.adjacency.get(swiper_loc, [])
        if neighbors:
            target = random.choice(neighbors)
            return {
                'type': 'move',
                'from': swiper_loc,
                'to': target,
                'message': 'Swiper在某处游荡...'
            }

        return {
            'type': 'wait',
            'message': 'Swiper在观察你的行动...'
        }


# 智能选择器: 优先使用Fast-Downward，失败时回退到启发式AI
class SwiperAI:
    """智能Swiper AI - 自动选择最佳规划器"""

    def __init__(self):
        self.fd_planner = FastDownwardPlanner()
        self.simple_ai = SimpleSwiperAI()
        self.use_pddl = self.fd_planner.fd_available

        if self.use_pddl:
            print("[Swiper AI] 使用 Fast-Downward PDDL规划器")
        else:
            print("[Swiper AI] Fast-Downward不可用，使用启发式AI")

    def get_action(self, game_state: Dict) -> Dict:
        """获取Swiper的行动"""
        if self.use_pddl:
            try:
                action = self.fd_planner.get_action(game_state)
                if action.get('type') != 'wait' or not game_state.get('swiper_blocked'):
                    return action
            except Exception as e:
                print(f"[Swiper AI] PDDL规划失败: {e}")

        # 回退到启发式AI
        return self.simple_ai.get_action(game_state)


# 测试代码
if __name__ == '__main__':
    print("=" * 50)
    print("测试Swiper AI")
    print("=" * 50)

    ai = SwiperAI()

    test_state = {
        'swiper_location': 'jungle_path',
        'player_location': 'deep_forest',
        'player_backpack': ['crystal_key', 'herbal_medicine'],
        'swiper_carrying': [],
        'room_items': {
            'deep_forest': ['herbal_medicine'],
            'ancient_grove': ['sharp_machete']
        },
        'swiper_blocked': False
    }

    print(f"\n测试1: 基础状态")
    print(f"Swiper在: {test_state['swiper_location']}")
    print(f"玩家在: {test_state['player_location']}")
    action = ai.get_action(test_state)
    print(f"Swiper行动: {action}")

    print(f"\n测试2: Swiper有物品需要藏匿")
    test_state['swiper_carrying'] = ['water_amulet']
    test_state['swiper_location'] = 'beach_entrance'
    action = ai.get_action(test_state)
    print(f"Swiper在: {test_state['swiper_location']}, 携带: {test_state['swiper_carrying']}")
    print(f"Swiper行动: {action}")

    print(f"\n测试3: Swiper和玩家同位置")
    test_state['swiper_carrying'] = []
    test_state['swiper_location'] = 'deep_forest'
    test_state['player_location'] = 'deep_forest'
    action = ai.get_action(test_state)
    print(f"两者都在: {test_state['swiper_location']}")
    print(f"Swiper行动: {action}")
