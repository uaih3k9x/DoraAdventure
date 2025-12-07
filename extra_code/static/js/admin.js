// Dora's Tropical Adventure - Admin Interface Script

let currentSessionId = null;
let commandHistory = [];
let historyIndex = -1;
let currentMode = 'shell'; // 'shell' or 'gui'

// ============================================================
// Mode Switching
// ============================================================

function switchMode(mode) {
    currentMode = mode;

    // Update button states
    document.getElementById('shellModeBtn').classList.toggle('active', mode === 'shell');
    document.getElementById('guiModeBtn').classList.toggle('active', mode === 'gui');

    // Switch display areas
    if (currentSessionId) {
        document.getElementById('shellArea').style.display = mode === 'shell' ? 'flex' : 'none';
        document.getElementById('guiArea').style.display = mode === 'gui' ? 'flex' : 'none';
        document.getElementById('noSessionArea').style.display = 'none';

        // Refresh status in GUI mode
        if (mode === 'gui') {
            guiRefresh();
        }
    }
}

// ============================================================
// Session Management
// ============================================================

async function loadSessions() {
    try {
        const response = await fetch('/admin/api/sessions');
        const data = await response.json();
        renderSessionList(data.sessions);
    } catch (error) {
        console.error('Failed to load sessions:', error);
    }
}

function renderSessionList(sessions) {
    const container = document.getElementById('sessionList');
    if (sessions.length === 0) {
        container.innerHTML = '<div style="padding: 20px; color: #8b949e; text-align: center;">No active sessions</div>';
        return;
    }

    container.innerHTML = sessions.map(s => `
        <div class="session-item ${s.session_id === currentSessionId ? 'active' : ''}"
             onclick="selectSession('${s.session_id}')">
            <span class="status ${s.process_alive ? 'status-alive' : 'status-dead'}"></span>
            <span class="sid">${s.session_id.substring(0, 12)}...</span>
            <div style="font-size: 11px; color: #8b949e; margin-top: 4px;">
                ${s.process_alive ? 'Running' : 'Ended'} | ${s.last_activity}
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
        document.getElementById('shellOutput').innerHTML = '<span class="system">Connected to session ' + sessionId.substring(0, 8) + '...</span>\n';
        document.getElementById('shellInput').focus();
    } else {
        document.getElementById('shellArea').style.display = 'none';
        document.getElementById('guiArea').style.display = 'flex';
        document.getElementById('guiSessionInfo').textContent = 'Session: ' + shortId;
        document.getElementById('guiOutput').innerHTML = '<span class="system">Connected to session ' + sessionId.substring(0, 8) + '...</span>\n';
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
                appendOutput('New session created: ' + data.session_id.substring(0, 8) + '...', 'system');
                if (data.output) {
                    appendOutput(data.output, 'output');
                }
            } else {
                appendGuiOutput('New session created and game started', 'system');
                if (data.output) {
                    appendGuiOutput(data.output, 'output');
                }
                guiRefresh();
            }
        }
        loadSessions();
    } catch (error) {
        alert('Failed to create session: ' + error.message);
    }
}

async function killSession() {
    if (!currentSessionId) return;
    if (!confirm('Are you sure you want to terminate this session?')) return;

    try {
        const response = await fetch('/admin/api/kill', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ session_id: currentSessionId })
        });
        const data = await response.json();

        if (currentMode === 'shell') {
            appendOutput(data.message || 'Session terminated', 'system');
        } else {
            appendGuiOutput(data.message || 'Session terminated', 'system');
        }
        loadSessions();
    } catch (error) {
        if (currentMode === 'shell') {
            appendOutput('Error: ' + error.message, 'error');
        } else {
            appendGuiOutput('Error: ' + error.message, 'error');
        }
    }
}

// ============================================================
// Shell Mode Functions
// ============================================================

async function sendShellCommand() {
    if (!currentSessionId) return;

    const input = document.getElementById('shellInput');
    const command = input.value.trim();
    if (!command) return;

    // Add to history
    commandHistory.push(command);
    historyIndex = commandHistory.length;

    // Display command
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
        appendOutput('Error: ' + error.message, 'error');
    }
}

function quickQuery(cmd) {
    document.getElementById('shellInput').value = cmd;
    sendShellCommand();
}

function appendOutput(text, className) {
    const output = document.getElementById('shellOutput');
    output.innerHTML += '<span class="' + className + '">' + escapeHtml(text) + '</span>\n';
    // Auto-scroll to bottom
    setTimeout(() => {
        output.scrollTop = output.scrollHeight;
    }, 10);
}

function clearOutput() {
    document.getElementById('shellOutput').innerHTML = '<span class="system">Output cleared</span>\n';
}

async function refreshOutput() {
    appendOutput('[Refresh]', 'system');
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
// GUI Mode Functions
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

        // Refresh status after executing command
        setTimeout(guiRefresh, 500);

    } catch (error) {
        appendGuiOutput('Error: ' + error.message, 'error');
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

    // Check if contains Swiper turn
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
    document.getElementById('guiOutput').innerHTML = '<span class="system">Output cleared</span>\n';
}

async function guiRefresh() {
    if (!currentSessionId) return;

    try {
        // Query player location
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
        document.getElementById('guiPlayerLocation').textContent = 'Location: ' + (match ? match[1] : '-');

        // Query Swiper location
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
        document.getElementById('guiSwiperLocation').textContent = 'Location: ' + (match ? match[1] : '-');

        // Query score
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
        document.getElementById('guiPlayerScore').textContent = 'Score: ' + (match ? match[1] : '0');

        // Query time
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
        document.getElementById('guiGameTime').textContent = 'Time: ' + (match ? match[1] : '-');

        // Query backpack
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
            document.getElementById('guiBackpack').textContent = 'Empty';
        }

        // Query hidden items
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
            document.getElementById('guiHiddenItems').textContent = 'None';
        }

    } catch (error) {
        console.error('Failed to refresh status:', error);
    }
}

// ============================================================
// Utility Functions
// ============================================================

function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// ============================================================
// Initialization
// ============================================================

// Periodically refresh session list
setInterval(loadSessions, 5000);
loadSessions();
