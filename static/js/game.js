// 朵拉的热带探险 - 游戏前端脚本

const outputDiv = document.getElementById('output');
const commandInput = document.getElementById('command');
const loadingDiv = document.getElementById('loading');

function appendOutput(text, isCommand = false) {
    if (isCommand) {
        outputDiv.innerHTML += '\n<span class="command">&gt; ' + escapeHtml(text) + '</span>';
    } else {
        // 检查是否包含Swiper回合分隔符
        if (text.includes('--- Swiper')) {
            const parts = text.split('--- Swiper');
            // 玩家行动输出
            outputDiv.innerHTML += '\n' + escapeHtml(parts[0]);
            // Swiper回合输出(高亮显示)
            if (parts[1]) {
                outputDiv.innerHTML += '\n<span class="swiper-turn">--- Swiper' + escapeHtml(parts[1]) + '</span>';
            }
        } else {
            outputDiv.innerHTML += '\n' + escapeHtml(text);
        }
    }
    outputDiv.scrollTop = outputDiv.scrollHeight;
}

function setOutput(text) {
    outputDiv.innerHTML = escapeHtml(text);
    outputDiv.scrollTop = outputDiv.scrollHeight;
}

function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

function showLoading(show) {
    loadingDiv.classList.toggle('show', show);
}

async function startGame() {
    showLoading(true);
    try {
        const response = await fetch('/api/start', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' }
        });
        const data = await response.json();
        if (data.success) {
            setOutput(data.output || '游戏已启动！输入 start. 开始游戏');
        } else {
            appendOutput('启动失败: ' + data.error);
        }
    } catch (error) {
        appendOutput('错误: ' + error.message);
    }
    showLoading(false);
    commandInput.focus();
}

async function sendCommand() {
    const command = commandInput.value.trim();
    if (!command) return;

    appendOutput(command, true);
    commandInput.value = '';
    showLoading(true);

    try {
        const response = await fetch('/api/command', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ command: command })
        });
        const data = await response.json();
        if (data.output) {
            appendOutput(data.output);
        }
    } catch (error) {
        appendOutput('错误: ' + error.message);
    }
    showLoading(false);
    commandInput.focus();
}

function quickCommand(cmd) {
    commandInput.value = cmd;
    sendCommand();
}

function handleKeyPress(event) {
    if (event.key === 'Enter') {
        sendCommand();
    }
}

window.onload = function() {
    commandInput.focus();
};
