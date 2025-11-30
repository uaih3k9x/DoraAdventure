// 朵拉的热带探险 - 管理员界面脚本

let currentSessionId = null;
let commandHistory = [];
let historyIndex = -1;
let currentMode = 'shell'; // 'shell' 或 'gui'

// ============================================================
// 模式切换
// ============================================================

function switchMode(mode) {
    currentMode = mode;

    // 更新按钮状态
    document.getElementById('shellModeBtn').classList.toggle('active', mode === 'shell');
    document.getElementById('guiModeBtn').classList.toggle('active', mode === 'gui');

    // 切换显示区域
    if (currentSessionId) {
        document.getElementById('shellArea').style.display = mode === 'shell' ? 'flex' : 'none';
        document.getElementById('guiArea').style.display = mode === 'gui' ? 'flex' : 'none';
        document.getElementById('noSessionArea').style.display = 'none';

        // GUI模式下刷新状态
        if (mode === 'gui') {
            guiRefresh();
        }
    }
}

// ============================================================
// 会话管理
// ============================================================

async function loadSessions() {
    try {
        const response = await fetch('/admin/api/sessions');
        const data = await response.json();
        renderSessionList(data.sessions);
    } catch (error) {
        console.error('加载会话失败:', error);
    }
}

function renderSessionList(sessions) {
    const container = document.getElementById('sessionList');
    if (sessions.length === 0) {
        container.innerHTML = '<div style="padding: 20px; color: #8b949e; text-align: center;">暂无活跃会话</div>';
        return;
    }

    container.innerHTML = sessions.map(s => `
        <div class="session-item ${s.session_id === currentSessionId ? 'active' : ''}"
             onclick="selectSession('${s.session_id}')">
            <span class="status ${s.process_alive ? 'status-alive' : 'status-dead'}"></span>
            <span class="sid">${s.session_id.substring(0, 12)}...</span>
            <div style="font-size: 11px; color: #8b949e; margin-top: 4px;">
                ${s.process_alive ? '运行中' : '已结束'} | ${s.last_activity}
            </div>
        </div>
    `).join('');
}

async function selectSession(sessionId) {
    currentSessionId = sessionId;
    const shortId = sessionId.substring(0, 12) + '...';

    document.getElementById('noSessionArea').style.display = 'none';

    if (currentMode === 'shell') {
        document.getElementById('shellArea').style.display = 'flex';
        document.getElementById('guiArea').style.display = 'none';
        document.getElementById('currentSessionInfo').textContent = 'Session: ' + shortId;
        document.getElementById('shellOutput').innerHTML = '<span class="system">已连接到会话 ' + sessionId.substring(0, 8) + '...</span>\n';
        document.getElementById('shellInput').focus();
    } else {
        document.getElementById('shellArea').style.display = 'none';
        document.getElementById('guiArea').style.display = 'flex';
        document.getElementById('guiSessionInfo').textContent = 'Session: ' + shortId;
        document.getElementById('guiOutput').innerHTML = '<span class="system">已连接到会话 ' + sessionId.substring(0, 8) + '...</span>\n';
        guiRefresh();
    }

    loadSessions();
}

async function createNewSession() {
    try {
        const response = await fetch('/admin/api/new_session', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' }
        });
        const data = await response.json();
        if (data.session_id) {
            selectSession(data.session_id);
            if (currentMode === 'shell') {
                appendOutput('新会话已创建: ' + data.session_id.substring(0, 8) + '...', 'system');
                if (data.output) {
                    appendOutput(data.output, 'output');
                }
            } else {
                appendGuiOutput('新会话已创建并已启动游戏', 'system');
                if (data.output) {
                    appendGuiOutput(data.output, 'output');
                }
                guiRefresh();
            }
        }
        loadSessions();
    } catch (error) {
        alert('创建会话失败: ' + error.message);
    }
}

async function killSession() {
    if (!currentSessionId) return;
    if (!confirm('确定要终止这个会话吗？')) return;

    try {
        const response = await fetch('/admin/api/kill', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ session_id: currentSessionId })
        });
        const data = await response.json();

        if (currentMode === 'shell') {
            appendOutput(data.message || '会话已终止', 'system');
        } else {
            appendGuiOutput(data.message || '会话已终止', 'system');
        }
        loadSessions();
    } catch (error) {
        if (currentMode === 'shell') {
            appendOutput('错误: ' + error.message, 'error');
        } else {
            appendGuiOutput('错误: ' + error.message, 'error');
        }
    }
}

// ============================================================
// Shell模式功能
// ============================================================

async function sendShellCommand() {
    if (!currentSessionId) return;

    const input = document.getElementById('shellInput');
    const command = input.value.trim();
    if (!command) return;

    // 添加到历史
    commandHistory.push(command);
    historyIndex = commandHistory.length;

    // 显示命令
    appendOutput('?- ' + command, 'command');
    input.value = '';

    try {
        const response = await fetch('/admin/api/shell', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                session_id: currentSessionId,
                command: command
            })
        });
        const data = await response.json();
        if (data.output) {
            appendOutput(data.output, 'output');
        }
        if (data.error) {
            appendOutput(data.error, 'error');
        }
    } catch (error) {
        appendOutput('错误: ' + error.message, 'error');
    }
}

function quickQuery(cmd) {
    document.getElementById('shellInput').value = cmd;
    sendShellCommand();
}

function appendOutput(text, className) {
    const output = document.getElementById('shellOutput');
    output.innerHTML += '<span class="' + className + '">' + escapeHtml(text) + '</span>\n';
    // 自动滚动到底部
    setTimeout(() => {
        output.scrollTop = output.scrollHeight;
    }, 10);
}

function clearOutput() {
    document.getElementById('shellOutput').innerHTML = '<span class="system">输出已清空</span>\n';
}

async function refreshOutput() {
    appendOutput('[刷新]', 'system');
}

function handleShellKeyDown(event) {
    if (event.key === 'Enter') {
        sendShellCommand();
    } else if (event.key === 'ArrowUp') {
        event.preventDefault();
        if (historyIndex > 0) {
            historyIndex--;
            document.getElementById('shellInput').value = commandHistory[historyIndex];
        }
    } else if (event.key === 'ArrowDown') {
        event.preventDefault();
        if (historyIndex < commandHistory.length - 1) {
            historyIndex++;
            document.getElementById('shellInput').value = commandHistory[historyIndex];
        } else {
            historyIndex = commandHistory.length;
            document.getElementById('shellInput').value = '';
        }
    }
}

// ============================================================
// GUI模式功能
// ============================================================

async function guiCommand(cmd) {
    if (!currentSessionId) return;

    appendGuiOutput('> ' + cmd, 'command');

    try {
        const response = await fetch('/admin/api/shell', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                session_id: currentSessionId,
                command: cmd + '.'
            })
        });
        const data = await response.json();
        if (data.output) {
            appendGuiOutput(data.output, 'output');
        }
        if (data.error) {
            appendGuiOutput(data.error, 'error');
        }

        // 执行命令后刷新状态
        setTimeout(guiRefresh, 500);

    } catch (error) {
        appendGuiOutput('错误: ' + error.message, 'error');
    }
}

function guiTakeItem() {
    const item = document.getElementById('guiItemInput').value.trim();
    if (item) {
        guiCommand('take(' + item + ')');
    }
}

function guiDropItem() {
    const item = document.getElementById('guiItemInput').value.trim();
    if (item) {
        guiCommand('drop(' + item + ')');
    }
}

function guiUseItem() {
    const item = document.getElementById('guiItemInput').value.trim();
    if (item) {
        guiCommand('use(' + item + ')');
    }
}

function sendGuiCommand() {
    const input = document.getElementById('guiCustomInput');
    const cmd = input.value.trim();
    if (cmd) {
        guiCommand(cmd);
        input.value = '';
    }
}

function handleGuiKeyDown(event) {
    if (event.key === 'Enter') {
        sendGuiCommand();
    }
}

function appendGuiOutput(text, className) {
    const output = document.getElementById('guiOutput');

    // 检查是否包含Swiper回合
    if (text.includes('--- Swiper')) {
        const parts = text.split('--- Swiper');
        output.innerHTML += '<span class="' + className + '">' + escapeHtml(parts[0]) + '</span>\n';
        if (parts[1]) {
            output.innerHTML += '<span class="swiper-turn">--- Swiper' + escapeHtml(parts[1]) + '</span>\n';
        }
    } else {
        output.innerHTML += '<span class="' + className + '">' + escapeHtml(text) + '</span>\n';
    }

    setTimeout(() => {
        output.scrollTop = output.scrollHeight;
    }, 10);
}

function clearGuiOutput() {
    document.getElementById('guiOutput').innerHTML = '<span class="system">输出已清空</span>\n';
}

async function guiRefresh() {
    if (!currentSessionId) return;

    try {
        // 查询玩家位置
        let response = await fetch('/admin/api/shell', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                session_id: currentSessionId,
                command: 'player_location(X), write(X), nl.'
            })
        });
        let data = await response.json();
        let match = data.output ? data.output.match(/(\w+)/) : null;
        document.getElementById('guiPlayerLocation').textContent = '位置: ' + (match ? match[1] : '-');

        // 查询Swiper位置
        response = await fetch('/admin/api/shell', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                session_id: currentSessionId,
                command: 'swiper_location(X), write(X), nl.'
            })
        });
        data = await response.json();
        match = data.output ? data.output.match(/(\w+)/) : null;
        document.getElementById('guiSwiperLocation').textContent = '位置: ' + (match ? match[1] : '-');

        // 查询得分
        response = await fetch('/admin/api/shell', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                session_id: currentSessionId,
                command: 'player_score(S), write(S), nl.'
            })
        });
        data = await response.json();
        match = data.output ? data.output.match(/(\d+)/) : null;
        document.getElementById('guiPlayerScore').textContent = '得分: ' + (match ? match[1] : '0');

        // 查询时间
        response = await fetch('/admin/api/shell', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                session_id: currentSessionId,
                command: 'game_time(T), write(T), nl.'
            })
        });
        data = await response.json();
        match = data.output ? data.output.match(/(\d+)/) : null;
        document.getElementById('guiGameTime').textContent = '时间: ' + (match ? match[1] : '-');

        // 查询背包
        response = await fetch('/admin/api/shell', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                session_id: currentSessionId,
                command: 'player_backpack(L), write(L), nl.'
            })
        });
        data = await response.json();
        const backpackMatch = data.output ? data.output.match(/\[([^\]]*)\]/) : null;
        if (backpackMatch && backpackMatch[1].trim()) {
            document.getElementById('guiBackpack').textContent = backpackMatch[1];
        } else {
            document.getElementById('guiBackpack').textContent = '空';
        }

        // 查询藏匿物品
        response = await fetch('/admin/api/shell', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                session_id: currentSessionId,
                command: 'findall((L,I), swiper_hidden_item(L,I), R), write(R), nl.'
            })
        });
        data = await response.json();
        const hiddenMatch = data.output ? data.output.match(/\[([^\]]*)\]/) : null;
        if (hiddenMatch && hiddenMatch[1].trim()) {
            document.getElementById('guiHiddenItems').textContent = hiddenMatch[1];
        } else {
            document.getElementById('guiHiddenItems').textContent = '无';
        }

    } catch (error) {
        console.error('刷新状态失败:', error);
    }
}

// ============================================================
// 工具函数
// ============================================================

function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// ============================================================
// 初始化
// ============================================================

// 定期刷新会话列表
setInterval(loadSessions, 5000);
loadSessions();
