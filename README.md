# Dora's Tropical Adventure

A Prolog-based text adventure game with a PDDL-powered AI opponent (Swiper).

## Project Structure

```
dora_game/
├── app.py                 # Flask main app, session management, turn-based logic
├── dora_adventure.pl      # Prolog game core logic
├── swiper_planner.py      # PDDL AI planner (Fast-Downward / heuristic fallback)
├── templates/
│   ├── index.html         # Player game interface
│   └── admin.html         # Admin console
└── static/
    ├── css/
    │   ├── game.css       # Game interface styles
    │   └── admin.css      # Admin interface styles
    └── js/
        ├── game.js        # Game frontend logic
        └── admin.js       # Admin frontend logic
```

## Installation

### 1. Install SWI-Prolog

```bash
# macOS
brew install swi-prolog

# Ubuntu/Debian
sudo apt install swi-prolog
```

### 2. Install Python Dependencies

```bash
pip install flask
```

### 3. (Optional) Install Fast-Downward PDDL Planner

If not installed, the system will automatically use heuristic AI as fallback.

```bash
cd /tmp
git clone https://github.com/aibasel/downward.git fast-downward
cd fast-downward
python build.py
```

## Running the Game

```bash
cd dora_game
python app.py
```

Access:
- Game Interface: http://localhost:5002
- Admin Console: http://localhost:5002/admin

## Game Commands

| Command | Description |
|---------|-------------|
| `start` | Start the game |
| `look` | Look around current location |
| `go(direction)` | Move (north/south/east/west) |
| `take(item)` | Pick up an item |
| `drop(item)` | Drop an item |
| `inventory` | Check backpack contents |
| `status` | View game status |
| `boots_search` | Let Boots search for hidden items |
| `stop_swiper` | Stop Swiper from stealing |

## Admin Features

- **Shell Mode**: Execute any Prolog command directly
- **GUI Mode**: Visual game status, one-click operations

## Tech Stack

- **Backend**: Flask + SWI-Prolog (subprocess)
- **Frontend**: Vanilla HTML/CSS/JS
- **AI**: PDDL planning (Fast-Downward) / Heuristic fallback
