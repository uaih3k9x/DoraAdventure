# Dora's Tropical Adventure 🦊

基于 Prolog 的文字冒险游戏，带有 PDDL 驱动的 AI 对手 Swiper。

## 项目结构

```
dora_game/
├── app.py                 # Flask 主应用，会话管理，回合制逻辑
├── dora_adventure.pl      # Prolog 游戏核心逻辑
├── swiper_planner.py      # PDDL AI 规划器 (Fast-Downward / 启发式)
├── templates/
│   ├── index.html         # 玩家游戏界面
│   └── admin.html         # 管理员控制台
└── static/
    ├── css/
    │   ├── game.css       # 游戏界面样式
    │   └── admin.css      # 管理员界面样式
    └── js/
        ├── game.js        # 游戏前端逻辑
        └── admin.js       # 管理员前端逻辑
```

## 安装

### 1. 安装 SWI-Prolog

```bash
# macOS
brew install swi-prolog

# Ubuntu/Debian
sudo apt install swi-prolog
```

### 2. 安装 Python 依赖

```bash
pip install flask
```

### 3. (可选) 安装 Fast-Downward PDDL 规划器

如果不安装，会自动使用启发式 AI 作为备选。

```bash
cd /tmp
git clone https://github.com/aibasel/downward.git fast-downward
cd fast-downward
python build.py
```

## 运行

```bash
cd dora_game
python app.py
```

访问:
- 游戏界面: http://localhost:5002
- 管理员: http://localhost:5002/admin

## 游戏命令

| 命令 | 说明 |
|------|------|
| `start` | 开始游戏 |
| `look` | 查看当前位置 |
| `go(方向)` | 移动 (north/south/east/west) |
| `take(物品)` | 拾取物品 |
| `drop(物品)` | 丢弃物品 |
| `inventory` | 查看背包 |
| `status` | 查看状态 |
| `boots_search` | 让 Boots 搜索藏匿物品 |
| `stop_swiper` | 阻止 Swiper |

## 管理员功能

- **Shell 模式**: 直接执行任意 Prolog 命令
- **GUI 模式**: 可视化游戏状态，一键操作

## 技术栈

- **后端**: Flask + SWI-Prolog (subprocess)
- **前端**: 原生 HTML/CSS/JS
- **AI**: PDDL 规划 (Fast-Downward) / 启发式备选
