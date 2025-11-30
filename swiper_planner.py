"""
Swiper AI Planner - Uses PDDL to plan Swiper's actions
Turn-based system: After player action, Swiper uses PDDL to plan optimal action

Supports two planners:
1. FastDownwardPlanner - Uses fast-downward (more complete features)
2. SimpleSwiperAI - Heuristic AI (fallback)
"""

import subprocess
import tempfile
import os
import re
import shutil
from typing import Optional, Dict, List, Tuple

# Fast-Downward path
FAST_DOWNWARD_PATH = '/tmp/fast-downward/fast-downward.py'

# Complete PDDL domain definition - Based on opponent domain, removed derived predicates for planner compatibility
SWIPER_DOMAIN = '''
(define (domain swiper_behavior)
    (:requirements :strips :typing :negative-preconditions)

    (:types
        location item
    )

    (:predicates
        ; Location related
        (swiper_at ?l - location)
        (player_at ?l - location)
        (connected ?from - location ?to - location)

        ; Item related
        (item_at ?i - item ?l - location)
        (player_has ?i - item)
        (swiper_has ?i - item)
        (hidden_item ?i - item ?l - location)
        (valuable ?i - item)

        ; Terrain features
        (climbable ?l - location)
        (swimmable ?l - location)
        (has_escape_route ?l - location)
        (safe_zone ?l - location)

        ; Swiper status
        (swiper_blocked)
        (swiper_confused)
    )

    ; Action 1: Swiper normal move
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

    ; Action 2: Swiper sneak (choose location with escape route)
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

    ; Action 3: Swiper picks up item from ground
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

    ; Action 4: Swiper steals from player
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

    ; Action 5: Swiper hides item (at climbable location)
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

    ; Action 6: Swiper hides item (at swimmable location)
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

    ; Action 7: Swiper flees
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

# Map connections definition
MAP_CONNECTIONS = [
    # Area 1: Dense Jungle
    ('jungle_entrance', 'jungle_path'),
    ('jungle_path', 'jungle_entrance'),
    ('jungle_path', 'deep_forest'),
    ('deep_forest', 'jungle_path'),
    ('deep_forest', 'ancient_grove'),
    ('ancient_grove', 'deep_forest'),
    ('ancient_grove', 'mystic_pond'),
    ('mystic_pond', 'ancient_grove'),
    # Area 1 to Area 2
    ('jungle_entrance', 'beach_entrance'),
    ('beach_entrance', 'jungle_entrance'),
    # Area 2: Golden Beach
    ('beach_entrance', 'sandy_shore'),
    ('sandy_shore', 'beach_entrance'),
    ('beach_entrance', 'coral_reef'),
    ('coral_reef', 'beach_entrance'),
    ('sandy_shore', 'palm_grove'),
    ('palm_grove', 'sandy_shore'),
    # Area 2 to Area 3
    ('coral_reef', 'valley_entrance'),
    ('valley_entrance', 'coral_reef'),
    # Area 3: Waterfall Valley
    ('valley_entrance', 'waterfall_base'),
    ('waterfall_base', 'valley_entrance'),
    ('waterfall_base', 'river_bend'),
    ('river_bend', 'waterfall_base'),
    ('waterfall_base', 'crystal_cave'),
    ('crystal_cave', 'waterfall_base'),
    # Area 3 to Area 4
    ('crystal_cave', 'ruins_entrance'),
    ('ruins_entrance', 'crystal_cave'),
    # Area 4: Ancient Ruins
    ('ruins_entrance', 'stone_circle'),
    ('stone_circle', 'ruins_entrance'),
    ('stone_circle', 'secret_chamber'),
    ('secret_chamber', 'stone_circle'),
    ('stone_circle', 'ancestral_hall'),
    ('ancestral_hall', 'stone_circle'),
    ('ancestral_hall', 'final_gate'),
    ('final_gate', 'ancestral_hall'),
]

# Climbable locations
CLIMBABLE_LOCATIONS = [
    'ancient_grove', 'mystic_pond', 'deep_forest',
    'waterfall_base', 'crystal_cave',
    'stone_circle', 'ruins_entrance'
]

# Swimmable locations
SWIMMABLE_LOCATIONS = [
    'coral_reef', 'sandy_shore', 'palm_grove',
    'river_bend', 'waterfall_base'
]

# Locations with escape routes
ESCAPE_ROUTE_LOCATIONS = [
    'jungle_path', 'beach_entrance', 'valley_entrance',
    'ruins_entrance', 'coral_reef', 'stone_circle'
]

# Safe zones
SAFE_ZONES = ['jungle_entrance', 'final_gate']

# Valuable items
VALUABLE_ITEMS = [
    'crystal_key', 'water_amulet', 'ancestral_map',
    'ancient_totem', 'rainbow_scale', 'river_pearl'
]

# All locations
ALL_LOCATIONS = [
    'jungle_entrance', 'jungle_path', 'deep_forest', 'ancient_grove', 'mystic_pond',
    'beach_entrance', 'sandy_shore', 'coral_reef', 'palm_grove',
    'valley_entrance', 'waterfall_base', 'river_bend', 'crystal_cave',
    'ruins_entrance', 'stone_circle', 'secret_chamber', 'ancestral_hall', 'final_gate'
]

# All items
ALL_ITEMS = [
    'herbal_medicine', 'sharp_machete', 'ancient_totem',
    'glowing_seashell', 'fishing_net', 'coconut_water',
    'water_amulet', 'rainbow_scale', 'river_pearl',
    'sun_dial', 'crystal_key', 'ancestral_map'
]


class FastDownwardPlanner:
    """Swiper AI Planner using Fast-Downward"""

    def __init__(self):
        self.temp_dir = tempfile.mkdtemp()
        self.domain_file = os.path.join(self.temp_dir, 'domain.pddl')
        self.problem_file = os.path.join(self.temp_dir, 'problem.pddl')
        self.plan_file = os.path.join(self.temp_dir, 'sas_plan')

        # Write domain file
        with open(self.domain_file, 'w') as f:
            f.write(SWIPER_DOMAIN)

        # Check if fast-downward is available
        self.fd_available = os.path.exists(FAST_DOWNWARD_PATH)

    def generate_problem(self, game_state: Dict, goal_type: str = 'steal') -> str:
        """
        Generate PDDL problem file based on game state
        goal_type: 'steal' - steal target, 'hide' - hide target, 'approach' - approach player
        """
        swiper_loc = game_state.get('swiper_location', 'jungle_path')
        player_loc = game_state.get('player_location', 'jungle_entrance')
        player_items = game_state.get('player_backpack', [])
        swiper_items = game_state.get('swiper_carrying', [])
        room_items = game_state.get('room_items', {})
        swiper_blocked = game_state.get('swiper_blocked', False)
        swiper_confused = game_state.get('swiper_confused', False)

        # Build init section
        init_parts = [
            f'(swiper_at {swiper_loc})',
            f'(player_at {player_loc})',
        ]

        # Map connections
        for f, t in MAP_CONNECTIONS:
            init_parts.append(f'(connected {f} {t})')

        # Terrain features
        for loc in CLIMBABLE_LOCATIONS:
            init_parts.append(f'(climbable {loc})')
        for loc in SWIMMABLE_LOCATIONS:
            init_parts.append(f'(swimmable {loc})')
        for loc in ESCAPE_ROUTE_LOCATIONS:
            init_parts.append(f'(has_escape_route {loc})')
        for loc in SAFE_ZONES:
            init_parts.append(f'(safe_zone {loc})')

        # Item values
        for item in VALUABLE_ITEMS:
            init_parts.append(f'(valuable {item})')

        # Player items
        for item in player_items:
            init_parts.append(f'(player_has {item})')

        # Swiper items
        for item in swiper_items:
            init_parts.append(f'(swiper_has {item})')

        # Ground items
        for loc, items in room_items.items():
            for item in items:
                init_parts.append(f'(item_at {item} {loc})')

        # Swiper status
        if swiper_blocked:
            init_parts.append('(swiper_blocked)')
        if swiper_confused:
            init_parts.append('(swiper_confused)')

        init_str = '\n        '.join(init_parts)

        # Set goal based on goal type
        if goal_type == 'hide' and swiper_items:
            # Hide goal: hide item in hand
            item = swiper_items[0]
            goal_str = f'(hidden_item {item} {swiper_loc})' if (swiper_loc in CLIMBABLE_LOCATIONS or swiper_loc in SWIMMABLE_LOCATIONS) else f'(exists (?l - location) (hidden_item {item} ?l))'
            # Fast-downward doesn't support exists in goal, simplify
            hide_locs = [l for l in CLIMBABLE_LOCATIONS + SWIMMABLE_LOCATIONS]
            if hide_locs:
                goal_str = f'(hidden_item {item} {hide_locs[0]})'
        elif goal_type == 'steal' and player_items:
            # Steal goal
            valuable_items = [i for i in player_items if i in VALUABLE_ITEMS]
            if valuable_items:
                goal_str = f'(swiper_has {valuable_items[0]})'
            else:
                goal_str = f'(swiper_at {player_loc})'
        else:
            # Approach player
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
        Use Fast-Downward to plan Swiper's action
        Returns: (action_name, [params]) or None
        """
        if not self.fd_available:
            return None

        # Choose goal based on state
        swiper_items = game_state.get('swiper_carrying', [])
        player_items = game_state.get('player_backpack', [])

        if swiper_items:
            goal_type = 'hide'
        elif player_items:
            goal_type = 'steal'
        else:
            goal_type = 'approach'

        # Generate problem file
        problem = self.generate_problem(game_state, goal_type)
        with open(self.problem_file, 'w') as f:
            f.write(problem)

        # Clean old plan file
        if os.path.exists(self.plan_file):
            os.remove(self.plan_file)

        try:
            # Call fast-downward
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

            # Read plan file
            if os.path.exists(self.plan_file):
                with open(self.plan_file, 'r') as f:
                    lines = f.readlines()
                    for line in lines:
                        line = line.strip()
                        if line.startswith('(') and line.endswith(')'):
                            # Parse (action param1 param2)
                            content = line[1:-1]
                            parts = content.split()
                            if parts:
                                return (parts[0], parts[1:])

            return None

        except subprocess.TimeoutExpired:
            return None
        except Exception as e:
            print(f"Fast-Downward planning error: {e}")
            return None

    def get_action(self, game_state: Dict) -> Dict:
        """Get Swiper's next action"""
        result = self.plan(game_state)

        if result is None:
            # Planning failed, use fallback strategy
            return self._fallback_action(game_state)

        action, params = result

        # Parse action
        if action == 'swiper_move' or action == 'swiper_sneak':
            return {
                'type': 'move',
                'from': params[0] if len(params) > 0 else None,
                'to': params[1] if len(params) > 1 else None,
                'message': f'Swiper sneaks to {params[1] if len(params) > 1 else "somewhere"}...'
            }
        elif action == 'swiper_take':
            return {
                'type': 'take',
                'item': params[0] if len(params) > 0 else None,
                'location': params[1] if len(params) > 1 else None,
                'message': f'Swiper picks up {params[0] if params else "an item"} from the ground!'
            }
        elif action == 'swiper_steal':
            return {
                'type': 'steal',
                'item': params[0] if len(params) > 0 else None,
                'message': f'Swiper steals {params[0] if params else "something"} from your backpack!'
            }
        elif action in ('swiper_hide_climb', 'swiper_hide_swim'):
            hide_type = "in the trees" if action == 'swiper_hide_climb' else "in the water"
            return {
                'type': 'hide',
                'item': params[0] if len(params) > 0 else None,
                'location': params[1] if len(params) > 1 else None,
                'message': f'Swiper hides {params[0] if params else "an item"} {hide_type}!'
            }
        elif action == 'swiper_flee':
            return {
                'type': 'move',
                'from': params[0] if len(params) > 0 else None,
                'to': params[1] if len(params) > 1 else None,
                'message': f'Swiper frantically flees to {params[1] if len(params) > 1 else "somewhere"}!'
            }
        else:
            return self._fallback_action(game_state)

    def _fallback_action(self, game_state: Dict) -> Dict:
        """Fallback strategy"""
        import random

        swiper_loc = game_state.get('swiper_location', 'jungle_path')
        neighbors = [to for (fr, to) in MAP_CONNECTIONS if fr == swiper_loc]

        if neighbors:
            target = random.choice(neighbors)
            return {
                'type': 'move',
                'from': swiper_loc,
                'to': target,
                'message': 'Swiper wanders somewhere...'
            }

        return {
            'type': 'wait',
            'message': 'Swiper watches your movements...'
        }

    def cleanup(self):
        """Clean up temporary files"""
        try:
            shutil.rmtree(self.temp_dir)
        except:
            pass


# Simplified Swiper AI (fallback)
class SimpleSwiperAI:
    """
    Simplified Swiper AI - Uses heuristic rules
    Used when PDDL planner is unavailable
    """

    def __init__(self):
        self.adjacency = {}
        for f, t in MAP_CONNECTIONS:
            if f not in self.adjacency:
                self.adjacency[f] = []
            self.adjacency[f].append(t)

    def _find_path(self, start: str, goal: str) -> List[str]:
        """BFS to find shortest path"""
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
        """Decide Swiper's action"""
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
                'message': 'Swiper: "Oh man!" (blocked)'
            }

        # Priority: hide items
        if swiper_items:
            if swiper_loc in CLIMBABLE_LOCATIONS or swiper_loc in SWIMMABLE_LOCATIONS:
                item = swiper_items[0]
                hide_type = "in the trees" if swiper_loc in CLIMBABLE_LOCATIONS else "in the water"
                return {
                    'type': 'hide',
                    'item': item,
                    'location': swiper_loc,
                    'message': f'Swiper hides {item} {hide_type}!'
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
                        'message': f'Swiper escapes to {best_path[1]} with stolen goods...'
                    }

        # Steal
        if swiper_loc == player_loc and player_items:
            valuable_player_items = [i for i in player_items if i in VALUABLE_ITEMS]
            if valuable_player_items:
                item = random.choice(valuable_player_items)
                return {
                    'type': 'steal',
                    'item': item,
                    'message': f'Swiper suddenly appears and steals {item} from your backpack!'
                }

        # Pick up items from ground
        current_items = room_items.get(swiper_loc, [])
        valuable_here = [i for i in current_items if i in VALUABLE_ITEMS]
        if valuable_here:
            item = valuable_here[0]
            return {
                'type': 'take',
                'item': item,
                'location': swiper_loc,
                'message': f'Swiper picks up {item}!'
            }

        # Move toward player
        path = self._find_path(swiper_loc, player_loc)
        if path and len(path) > 1:
            if random.random() < 0.8:
                return {
                    'type': 'move',
                    'from': swiper_loc,
                    'to': path[1],
                    'message': 'You hear Swiper moving nearby...'
                }

        # Random move
        neighbors = self.adjacency.get(swiper_loc, [])
        if neighbors:
            target = random.choice(neighbors)
            return {
                'type': 'move',
                'from': swiper_loc,
                'to': target,
                'message': 'Swiper wanders somewhere...'
            }

        return {
            'type': 'wait',
            'message': 'Swiper watches your movements...'
        }


# Smart selector: Prefer Fast-Downward, fallback to heuristic AI
class SwiperAI:
    """Smart Swiper AI - Auto-selects best planner"""

    def __init__(self):
        self.fd_planner = FastDownwardPlanner()
        self.simple_ai = SimpleSwiperAI()
        self.use_pddl = self.fd_planner.fd_available

        if self.use_pddl:
            print("[Swiper AI] Using Fast-Downward PDDL planner")
        else:
            print("[Swiper AI] Fast-Downward unavailable, using heuristic AI")

    def get_action(self, game_state: Dict) -> Dict:
        """Get Swiper's action"""
        if self.use_pddl:
            try:
                action = self.fd_planner.get_action(game_state)
                if action.get('type') != 'wait' or not game_state.get('swiper_blocked'):
                    return action
            except Exception as e:
                print(f"[Swiper AI] PDDL planning failed: {e}")

        # Fallback to heuristic AI
        return self.simple_ai.get_action(game_state)


# Test code
if __name__ == '__main__':
    print("=" * 50)
    print("Testing Swiper AI")
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

    print(f"\nTest 1: Basic state")
    print(f"Swiper at: {test_state['swiper_location']}")
    print(f"Player at: {test_state['player_location']}")
    action = ai.get_action(test_state)
    print(f"Swiper action: {action}")

    print(f"\nTest 2: Swiper has items to hide")
    test_state['swiper_carrying'] = ['water_amulet']
    test_state['swiper_location'] = 'beach_entrance'
    action = ai.get_action(test_state)
    print(f"Swiper at: {test_state['swiper_location']}, carrying: {test_state['swiper_carrying']}")
    print(f"Swiper action: {action}")

    print(f"\nTest 3: Swiper and player at same location")
    test_state['swiper_carrying'] = []
    test_state['swiper_location'] = 'deep_forest'
    test_state['player_location'] = 'deep_forest'
    action = ai.get_action(test_state)
    print(f"Both at: {test_state['swiper_location']}")
    print(f"Swiper action: {action}")
