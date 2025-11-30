#!/usr/bin/env python3
"""
Dora's Tropical Adventure - Web Frontend
Runs the actual Prolog game (dora_adventure.pl) via SWI-Prolog
"""

from flask import Flask, render_template, jsonify, request, session
import subprocess
import os
import re
import uuid
import threading
import time
import select

# Import Swiper AI (prefers Fast-Downward PDDL planner)
from swiper_planner import SwiperAI

# Get current directory
BASE_DIR = os.path.dirname(os.path.abspath(__file__))

app = Flask(__name__,
            template_folder=os.path.join(BASE_DIR, 'templates'),
            static_folder=os.path.join(BASE_DIR, 'static'))
app.secret_key = 'dora_adventure_prolog_2024'

# Store Prolog processes for each session
prolog_sessions = {}

# Swiper AI instance (auto-selects Fast-Downward or heuristic AI)
swiper_ai = SwiperAI()

# Turn-based configuration
TURN_BASED_ENABLED = True  # Enable turn-based mode
PLAYER_ACTION_COMMANDS = {'go', 'take', 'drop', 'use', 'boots_search'}  # Commands that trigger Swiper's turn

# Allowed game commands whitelist
ALLOWED_COMMANDS = {
    # Basic commands (no parameters)
    'start', 'look', 'inventory', 'status', 'help', 'quit',
    'boots_search', 'stop_swiper',
    # Commands with parameters
    'go', 'take', 'drop', 'use', 'drop_confirm'
}

# Allowed directions
ALLOWED_DIRECTIONS = {'north', 'south', 'east', 'west', 'up', 'down'}

# Dangerous commands blacklist - only check command name, not arguments
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

# Dangerous character patterns
DANGEROUS_CHARS = [':-', '?-', '->', ';', '!', '[', ']', '|', '\\']


def validate_command(command):
    """
    Validate user input command
    Returns: (is_valid, error_message)
    """
    if not command:
        return False, "Please enter a command!"

    # Remove trailing period for validation
    cmd = command.strip()
    if cmd.endswith('.'):
        cmd = cmd[:-1].strip()

    # Check command length
    if len(cmd) > 100:
        return False, "Command too long!"

    # Check dangerous characters
    for char in DANGEROUS_CHARS:
        if char in cmd:
            return False, f"Character not allowed! Please use game commands like go(north), take(item), etc."

    # Parse command name and arguments
    if '(' in cmd:
        # Command with parameters, e.g., go(north)
        match = re.match(r'^(\w+)\s*\(\s*(\w+)\s*\)$', cmd)
        if not match:
            return False, f"Invalid command format! Correct format: go(north), take(item_name)"

        cmd_name = match.group(1).lower()
        cmd_arg = match.group(2)  # Keep original case

        # Check if dangerous command
        if cmd_name in DANGEROUS_COMMANDS:
            return False, f"Command not allowed! Please use game commands like go(north), take(item), etc."

        # Check if allowed command
        if cmd_name not in ALLOWED_COMMANDS:
            return False, f"Unknown command '{cmd_name}'! Available: go, take, drop, use, look, inventory, etc."

        # Validate go command direction
        if cmd_name == 'go' and cmd_arg.lower() not in ALLOWED_DIRECTIONS:
            return False, f"Invalid direction '{cmd_arg}'! Available: north, south, east, west, up, down"

        # Validate argument contains only letters, numbers, and underscores
        if not re.match(r'^\w+$', cmd_arg):
            return False, "Arguments can only contain letters, numbers, and underscores!"

    else:
        # Command without parameters, e.g., look, inventory
        cmd_name = cmd.lower()

        # Check if dangerous command
        if cmd_name in DANGEROUS_COMMANDS:
            return False, f"Command not allowed! Please use game commands like go(north), take(item), etc."

        if cmd_name not in ALLOWED_COMMANDS:
            return False, f"Unknown command '{cmd_name}'! Available: start, look, inventory, status, help, boots_search, stop_swiper"

    return True, None


class PrologSession:
    """Manage a single Prolog session"""

    def __init__(self, session_id):
        self.session_id = session_id
        self.process = None
        self.lock = threading.Lock()
        self.last_activity = time.time()
        # Cache game state (parsed from output)
        self.cached_state = {
            'player_location': None,
            'swiper_location': None,
            'backpack': [],
            'score': 0,
            'game_time': 100,
            'hidden_items': []
        }

    def start(self):
        """Start Prolog process"""
        prolog_file = os.path.join(BASE_DIR, 'dora_adventure.pl')

        # Start SWI-Prolog process (using pipes, non-interactive mode)
        self.process = subprocess.Popen(
            ['swipl', '-q', '-s', prolog_file],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=0  # Unbuffered
        )

        # Wait for loading and clear initial output
        time.sleep(1.0)
        # Read until no more output
        while True:
            output = self._read_output(timeout=1.0)
            if not output:
                break
            time.sleep(0.1)
        return "Prolog game loaded"

    def _read_output(self, timeout=2.0):
        """Read Prolog output"""
        output_lines = []
        start_time = time.time()
        last_read_time = start_time
        idle_timeout = 0.6  # Wait 0.6 seconds after no new output

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
                    # If we have output and idle for more than idle_timeout, consider complete
                    if output_lines and (time.time() - last_read_time) > idle_timeout:
                        break

        except Exception as e:
            output_lines.append(f"[Read error: {e}]")

        return '\n'.join(output_lines)

    def send_command(self, command, silent=False):
        """
        Send command to Prolog
        silent: If True, execute command but don't update last_activity (for admin queries)
        """
        if not silent:
            self.last_activity = time.time()

        with self.lock:
            if self.process is None or self.process.poll() is not None:
                return "Prolog session ended. Please refresh the page to restart."

            try:
                # Read any pending output first (don't discard, merge into result)
                pending_output = self._read_output(timeout=0.1)

                # Ensure command ends with period
                command = command.strip()
                if not command.endswith('.'):
                    command += '.'

                # Use marker method to ensure complete output reading
                # Send command, then send a marker command; when we see the marker, original command output is complete
                marker = f'__END_MARKER_{time.time()}__'

                # Send user command
                self.process.stdin.write(command + '\n')
                self.process.stdin.flush()

                # Send marker command
                self.process.stdin.write(f"write('{marker}'), nl.\n")
                self.process.stdin.flush()

                # Read output until we see the marker
                output_parts = []
                start_time = time.time()
                max_wait = 15.0  # Maximum wait 15 seconds

                while time.time() - start_time < max_wait:
                    part = self._read_output(timeout=2.0)
                    if part:
                        # Check if contains marker
                        if marker in part:
                            # Remove marker and everything after it
                            idx = part.find(marker)
                            before_marker = part[:idx].rstrip()
                            if before_marker:
                                output_parts.append(before_marker)
                            break
                        output_parts.append(part)
                    else:
                        # No output, wait briefly then continue
                        time.sleep(0.1)

                output = '\n'.join(output_parts) if output_parts else ''

                # Merge pending output and new output
                if pending_output:
                    output = pending_output + '\n' + output if output else pending_output

                # Clean output
                output = self._clean_output(output)

                # Parse state info from output to update cache (only in non-silent mode)
                if not silent:
                    self._parse_state_from_output(output)

                return output if output else "(Command executed)"

            except Exception as e:
                return f"Command execution error: {e}"

    def _parse_state_from_output(self, output):
        """Parse state information from game output"""
        # Parse location (from "=== xxx ===" format)
        loc_match = re.search(r'=== .+ ===', output)
        if loc_match:
            # Extract location ID from room name
            pass

        # Parse movement info
        move_match = re.search(r'You moved from \w+ to (\w+)', output)
        if move_match:
            self.cached_state['player_location'] = move_match.group(1)

        # Parse score
        score_match = re.search(r'Score:\s*(\d+)', output)
        if score_match:
            self.cached_state['score'] = int(score_match.group(1))

        # Parse time
        time_match = re.search(r'Remaining [Tt]ime:\s*(\d+)', output)
        if time_match:
            self.cached_state['game_time'] = int(time_match.group(1))

        # Parse current location (from output)
        current_loc_match = re.search(r'Current location:\s*(\w+)', output)
        if current_loc_match:
            self.cached_state['player_location'] = current_loc_match.group(1)

    def _clean_output(self, output):
        """Clean Prolog output"""
        lines = output.split('\n')
        cleaned = []

        for line in lines:
            # Remove Prolog prompts
            line = re.sub(r'^\?-\s*', '', line)
            line = re.sub(r'^\|\s*', '', line)
            # Remove standalone true/false
            stripped = line.strip()
            if stripped in ['true.', 'false.', 'true', 'false']:
                continue
            if stripped:
                cleaned.append(line)

        return '\n'.join(cleaned)

    def get_game_state_for_ai(self):
        """
        Query complete game state from Prolog for Swiper AI
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
            # Query player location
            output = self.send_command('player_location(X), write(X), nl', silent=True)
            match = re.search(r'(\w+)', output)
            if match and match.group(1) not in ['true', 'false', 'X', 'write', 'nl']:
                state['player_location'] = match.group(1)

            # Query Swiper location
            output = self.send_command('swiper_location(X), write(X), nl', silent=True)
            match = re.search(r'(\w+)', output)
            if match and match.group(1) not in ['true', 'false', 'X', 'write', 'nl']:
                state['swiper_location'] = match.group(1)

            # Query player backpack
            output = self.send_command('player_backpack(L), write(L), nl', silent=True)
            list_match = re.search(r'\[([^\]]*)\]', output)
            if list_match:
                items_str = list_match.group(1)
                if items_str.strip():
                    state['player_backpack'] = [i.strip() for i in items_str.split(',') if i.strip()]

            # Query room items
            output = self.send_command('findall((R,I), room_has_item(R,I), L), write(L), nl', silent=True)
            # Parse [(room1,item1),(room2,item2),...]
            pairs = re.findall(r'\((\w+),\s*(\w+)\)', output)
            for room, item in pairs:
                if room not in state['room_items']:
                    state['room_items'][room] = []
                state['room_items'][room].append(item)

            # Query hidden items
            output = self.send_command('findall((L,I), swiper_hidden_item(L,I), R), write(R), nl', silent=True)
            pairs = re.findall(r'\((\w+),\s*(\w+)\)', output)
            for loc, item in pairs:
                if loc not in state['hidden_items']:
                    state['hidden_items'][loc] = []
                state['hidden_items'][loc].append(item)

            # Query if Swiper is blocked (check swiper_confused)
            output = self.send_command('(swiper_confused(_) -> write(blocked) ; write(ok)), nl', silent=True)
            if 'blocked' in output:
                state['swiper_blocked'] = True

        except Exception as e:
            pass

        return state

    def execute_swiper_action(self, action: dict) -> str:
        """
        Execute Swiper's action in Prolog
        Returns description message
        """
        action_type = action.get('type', 'wait')
        message = action.get('message', '')

        try:
            if action_type == 'move':
                # Update Swiper location
                new_loc = action.get('to')
                if new_loc:
                    self.send_command(f"retract(swiper_location(_)), assert(swiper_location({new_loc}))", silent=True)

            elif action_type == 'steal':
                # Steal from player backpack
                item = action.get('item')
                if item:
                    # Remove from player backpack
                    self.send_command(f"player_backpack(B), delete(B, {item}, NB), retract(player_backpack(B)), assert(player_backpack(NB))", silent=True)
                    # Note: Simplified handling, Swiper immediately hides or escapes after stealing

            elif action_type == 'take':
                # Swiper picks up item from ground
                item = action.get('item')
                loc = action.get('location')
                if item and loc:
                    self.send_command(f"retract(room_has_item({loc}, {item}))", silent=True)

            elif action_type == 'hide':
                # Swiper hides item
                item = action.get('item')
                loc = action.get('location')
                if item and loc:
                    self.send_command(f"assert(swiper_hidden_item({loc}, {item}))", silent=True)

        except Exception as e:
            message += f" (Execution error: {e})"

        return message

    def close(self):
        """Close Prolog process"""
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
    """Get or create Prolog session"""
    if 'session_id' not in session:
        session['session_id'] = str(uuid.uuid4())

    session_id = session['session_id']

    if session_id not in prolog_sessions:
        prolog_session = PrologSession(session_id)
        prolog_sessions[session_id] = prolog_session

    return prolog_sessions[session_id]


def cleanup_old_sessions():
    """Clean up inactive sessions"""
    current_time = time.time()
    timeout = 1800  # 30 minutes

    to_remove = []
    for sid, ps in prolog_sessions.items():
        if current_time - ps.last_activity > timeout:
            to_remove.append(sid)

    for sid in to_remove:
        prolog_sessions[sid].close()
        del prolog_sessions[sid]


# ============================================================
# Player Interface Routes
# ============================================================

@app.route('/')
def index():
    return render_template('index.html')


@app.route('/api/start', methods=['POST'])
def start_game():
    """Start game"""
    cleanup_old_sessions()

    # Close old session
    if 'session_id' in session and session['session_id'] in prolog_sessions:
        prolog_sessions[session['session_id']].close()
        del prolog_sessions[session['session_id']]

    # Create new session
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
    """Send game command"""
    data = request.get_json()
    command = data.get('command', '').strip()

    if not command:
        return jsonify({'output': 'Please enter a command!'})

    # Validate command
    is_valid, error_msg = validate_command(command)
    if not is_valid:
        return jsonify({'output': f'Warning: {error_msg}'})

    if 'session_id' not in session or session['session_id'] not in prolog_sessions:
        return jsonify({'output': 'Session expired. Please click "Start New Game".'})

    prolog_session = prolog_sessions[session['session_id']]

    # Execute player command
    output = prolog_session.send_command(command)

    # Turn-based: Check if Swiper's turn needs to be executed
    swiper_message = ''
    if TURN_BASED_ENABLED:
        # Parse command name
        cmd = command.strip()
        if cmd.endswith('.'):
            cmd = cmd[:-1]
        cmd_name = cmd.split('(')[0].lower() if '(' in cmd else cmd.lower()

        # If player action command, trigger Swiper's turn
        if cmd_name in PLAYER_ACTION_COMMANDS:
            swiper_message = execute_swiper_turn(prolog_session)

    # Merge output
    if swiper_message:
        output = output + "\n\n--- Swiper's Turn ---\n" + swiper_message

    return jsonify({'output': output})


def execute_swiper_turn(prolog_session) -> str:
    """
    Execute Swiper's turn
    Uses PDDL AI to plan Swiper's action and execute it
    """
    try:
        # Get current game state
        game_state = prolog_session.get_game_state_for_ai()

        # If unable to get state, skip Swiper's turn
        if not game_state.get('player_location'):
            return ''

        # AI decision
        action = swiper_ai.get_action(game_state)

        # Execute action
        message = prolog_session.execute_swiper_action(action)

        return message

    except Exception as e:
        return f'(Swiper AI error: {e})'


# ============================================================
# Admin Interface Routes
# ============================================================

@app.route('/admin')
def admin():
    return render_template('admin.html')


def query_prolog_state(prolog_session, use_cache=True):
    """
    Get game state from Prolog session
    use_cache: If True, return cached state only; if False, query Prolog directly (silent mode)
    """
    if use_cache:
        # Return cached state directly to avoid interfering with game
        return {
            'player_location': prolog_session.cached_state.get('player_location'),
            'swiper_location': prolog_session.cached_state.get('swiper_location'),
            'backpack': prolog_session.cached_state.get('backpack', []),
            'swiper_carrying': [],
            'hidden_items': prolog_session.cached_state.get('hidden_items', []),
            'score': prolog_session.cached_state.get('score', 0),
            'game_time': prolog_session.cached_state.get('game_time', 100)
        }

    # Query Prolog directly (silent mode - don't update cache, don't affect game output)
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
        # Query player location
        output = prolog_session.send_command('player_at(X), write(X), nl', silent=True)
        match = re.search(r'(\w+)', output)
        if match and match.group(1) not in ['true', 'false', 'X']:
            state['player_location'] = match.group(1)

        # Query Swiper location
        output = prolog_session.send_command('swiper_at(X), write(X), nl', silent=True)
        match = re.search(r'(\w+)', output)
        if match and match.group(1) not in ['true', 'false', 'X']:
            state['swiper_location'] = match.group(1)

        # Query backpack
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
    """Get all sessions list"""
    sessions_data = []
    total_items = 0
    total_hidden = 0

    for session_id, prolog_session in prolog_sessions.items():
        state = query_prolog_state(prolog_session, use_cache=True)

        # Calculate last activity time
        elapsed = time.time() - prolog_session.last_activity
        if elapsed < 60:
            last_activity = f"{int(elapsed)}s ago"
        elif elapsed < 3600:
            last_activity = f"{int(elapsed/60)}m ago"
        else:
            last_activity = f"{int(elapsed/3600)}h ago"

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
    """Admin direct Prolog command execution (no filtering)"""
    data = request.get_json()
    session_id = data.get('session_id')
    command = data.get('command', '').strip()

    if not session_id or session_id not in prolog_sessions:
        return jsonify({'error': 'Session not found'})

    if not command:
        return jsonify({'error': 'Please enter a command'})

    prolog_session = prolog_sessions[session_id]

    # Admin commands bypass validation, send directly to Prolog
    output = prolog_session.send_command(command, silent=True)

    return jsonify({'output': output})


@app.route('/admin/api/new_session', methods=['POST'])
def admin_new_session():
    """Admin creates new Prolog session"""
    session_id = str(uuid.uuid4())
    prolog_session = PrologSession(session_id)
    prolog_sessions[session_id] = prolog_session

    try:
        prolog_session.start()
        # Auto-start game
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
    """Admin terminates session"""
    data = request.get_json()
    session_id = data.get('session_id')

    if not session_id or session_id not in prolog_sessions:
        return jsonify({'error': 'Session not found'})

    prolog_sessions[session_id].close()
    del prolog_sessions[session_id]

    return jsonify({'message': 'Session terminated'})


if __name__ == '__main__':
    print("=" * 60)
    print("Dora's Tropical Adventure - Prolog Web Version")
    print("=" * 60)
    print()
    print("  This is a real Prolog game!")
    print("  Running dora_adventure.pl (SWI-Prolog)")
    print()
    print("  Visit: http://localhost:5002")
    print("  Admin: http://localhost:5002/admin")
    print()
    print("  Make sure SWI-Prolog is installed:")
    print("     brew install swi-prolog")
    print()
    print("=" * 60)
    print()

    app.run(debug=True, host='0.0.0.0', port=5002, threaded=True)
