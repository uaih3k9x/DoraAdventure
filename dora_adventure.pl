% ============================================================
% 朵拉的热带探险 (Dora's Tropical Adventure)
% 知识表示大作业 - Prolog文本冒险游戏
% 版本: v5 - 新增Boots猴子同伴系统、Swiper藏匿机制
% ============================================================

:- use_module(library(random)).

% ============================================================
% 动态谓词声明
% ============================================================
:- dynamic player_location/1.
:- dynamic player_backpack/1.
:- dynamic player_score/1.
:- dynamic game_time/1.
:- dynamic room_has_item/2.
:- dynamic item_effect/2.
:- dynamic special_item_used/1.
:- dynamic trap_effect_active/1.
:- dynamic swiper_confused/1.
:- dynamic area_dark/1.
:- dynamic swiper_illusion_active/0.
:- dynamic fatigued/1.
:- dynamic movement_bonus/1.
:- dynamic theft_protection/1.
:- dynamic hidden_secret/1.
:- dynamic final_gate_locked/0.
:- dynamic final_gate_unlocked/0.
:- dynamic blocked_path/2.
:- dynamic swiper_location/1.

% === v5新增: 猴子Boots系统相关动态谓词 ===
:- dynamic monkey_skill_active/1.          % 猴子技能激活状态
:- dynamic monkey_found_gift/0.            % 猴子找到上天礼物
:- dynamic swiper_hidden_item/2.           % Swiper藏匿的物品(位置,物品)
:- dynamic gift_location/1.                % 上天礼物位置
:- dynamic monkey_search_cooldown/1.       % 猴子搜索冷却时间

% ============================================================
% 猴子Boots基础属性定义 (v5新增)
% ============================================================
monkey_name(boots).
monkey_skill(climb).      % 攀爬技能
monkey_skill(swim).       % 游泳技能

% === 可攀爬地点定义 ===
% 茂密丛林 - 可攀爬
climbable_location(ancient_grove).
climbable_location(mystic_pond).
climbable_location(deep_forest).

% 瀑布河谷 - 可攀爬
climbable_location(waterfall_base).
climbable_location(crystal_cave).

% 古老遗迹 - 可攀爬
climbable_location(stone_circle).
climbable_location(ruins_entrance).

% === 可游泳地点定义 ===
% 金色海滩 - 可游泳
swimmable_location(coral_reef).
swimmable_location(sandy_shore).
swimmable_location(palm_grove).

% 瀑布河谷 - 可游泳
swimmable_location(river_bend).
swimmable_location(waterfall_base).

% ============================================================
% 游戏启动谓词 (start/0 - 作业要求必须包含)
% ============================================================
start :-
    writeln('==============================================='),
    writeln('  欢迎来到《朵拉的热带探险》!'),
    writeln('==============================================='),
    nl,
    writeln('故事背景:'),
    writeln('朵拉在热带雨林中探险时迷路了,她需要穿越四个神秘区域,'),
    writeln('收集关键物品,躲避狐狸Swiper的偷窃,找到回家的路!'),
    nl,
    writeln('游戏目标:'),
    writeln('- 探索四个区域:茂密丛林、金色海滩、瀑布河谷、古老遗迹'),
    writeln('- 收集12种专属道具'),
    writeln('- 阻止Swiper偷窃(说"Swiper, no swiping!")'),
    writeln('- 找到水晶钥匙并打开最终大门'),
    writeln('- [隐藏彩蛋] 让Boots搜索可能找到上天礼物直接胜利!'),
    nl,
    writeln('可用指令:'),
    writeln('1. go(方向)          - 移动(north/south/east/west/up/down)'),
    writeln('2. take(物品名)      - 拾取当前房间的物品'),
    writeln('3. drop(物品名)      - 丢弃背包中的物品'),
    writeln('4. use(物品名)       - 使用背包中的物品'),
    writeln('5. look.             - 查看当前房间环境'),
    writeln('6. inventory.        - 查看背包物品'),
    writeln('7. status.           - 查看游戏状态'),
    writeln('8. help.             - 重新查看可用指令'),
    writeln('9. stop_swiper.      - 尝试阻止Swiper偷窃'),
    writeln('10. boots_search.    - [新!] 让Boots搜索当前区域'),
    writeln('11. quit.            - 退出游戏'),
    nl,
    writeln('注意:直接输入指令即可,如: go(north). 别忘了结尾的点号!'),
    nl,
    initial_state,
    writeln('游戏开始!你现在在起点...'),
    writeln('Boots: "我会帮你找到Swiper藏起来的东西!"'),
    nl,
    look.

% ============================================================
% 初始状态设置
% ============================================================
initial_state :-
    % 清除所有动态状态
    retractall(player_location(_)),
    retractall(player_backpack(_)),
    retractall(player_score(_)),
    retractall(game_time(_)),
    retractall(room_has_item(_, _)),
    retractall(special_item_used(_)),
    retractall(trap_effect_active(_)),
    retractall(swiper_confused(_)),
    retractall(area_dark(_)),
    retractall(swiper_illusion_active),
    retractall(fatigued(_)),
    retractall(movement_bonus(_)),
    retractall(theft_protection(_)),
    retractall(hidden_secret(_)),
    retractall(final_gate_locked),
    retractall(final_gate_unlocked),
    retractall(blocked_path(_, _)),
    retractall(swiper_location(_)),
    % v5新增清除
    retractall(monkey_skill_active(_)),
    retractall(monkey_found_gift),
    retractall(swiper_hidden_item(_, _)),
    retractall(gift_location(_)),
    retractall(monkey_search_cooldown(_)),

    % 设置初始状态
    assert(player_location(jungle_entrance)),
    assert(player_backpack([])),
    assert(player_score(0)),
    assert(game_time(100)),
    assert(final_gate_locked),
    assert(swiper_location(jungle_path)),

    % v5新增: 初始化猴子系统
    assert(monkey_skill_active(true)),

    % 初始化所有物品位置
    initialize_all_items,

    % v5新增: 初始化上天礼物
    initialize_heavens_gift.

% ============================================================
% 物品位置初始化
% ============================================================
initialize_all_items :-
    % 茂密丛林区域物品
    assert(room_has_item(deep_forest, herbal_medicine)),
    assert(room_has_item(ancient_grove, sharp_machete)),
    assert(room_has_item(mystic_pond, ancient_totem)),

    % 金色海滩区域物品
    assert(room_has_item(sandy_shore, glowing_seashell)),
    assert(room_has_item(coral_reef, fishing_net)),
    assert(room_has_item(palm_grove, coconut_water)),

    % 瀑布河谷区域物品
    assert(room_has_item(waterfall_base, water_amulet)),
    assert(room_has_item(river_bend, rainbow_scale)),
    assert(room_has_item(crystal_cave, river_pearl)),

    % 古老遗迹区域物品
    assert(room_has_item(stone_circle, sun_dial)),
    assert(room_has_item(secret_chamber, crystal_key)),
    assert(room_has_item(ancestral_hall, ancestral_map)),

    % 设置一些隐藏秘密
    assert(hidden_secret('丛林深处有一条通往海滩的捷径')),
    assert(hidden_secret('水晶洞穴中藏有回家的线索')),
    assert(hidden_secret('让Boots在可攀爬的地方搜索可能有惊喜')),

    % 设置一些阻塞路径
    assert(blocked_path(ancient_grove, 'dense_vines')).

% ============================================================
% v5新增: 上天礼物系统
% ============================================================
initialize_heavens_gift :-
    % 随机选择一个可攀爬位置放置上天礼物
    findall(Loc, climbable_location(Loc), ClimbableLocs),
    random_member(GiftLocation, ClimbableLocs),
    assert(gift_location(GiftLocation)).

% ============================================================
% 房间描述 (v5增强: 显示可攀爬/游泳信息)
% ============================================================

% 区域1: 茂密丛林 (Dense Jungle)
room_description(jungle_entrance, '丛林入口 (Jungle Entrance)',
    '你站在热带雨林的入口处。高大的树木遮天蔽日,藤蔓交织。\n你可以听到远处传来的动物叫声。\n前方有一条通往深林的小路(north),东边似乎通向海滩(east)。').

room_description(jungle_path, '丛林小径 (Jungle Path)',
    '一条蜿蜒的小径穿过密林。地上有新鲜的脚印,可能是Swiper留下的!\n北边是深林(north),南边可以回到入口(south)。').

room_description(deep_forest, '深林 (Deep Forest)',
    '这里是丛林的深处,光线昏暗。你看到一些药草在角落生长。\n巨大的树木可以攀爬。\n南边是小径(south),西边有一片古老的树林(west)。').

room_description(ancient_grove, '古老树林 (Ancient Grove)',
    '巨大的古树矗立在这里,树龄可能有数百年。地上散落着一些工具。\n枝叶茂密,Boots可以在这里攀爬搜索。\n东边回到深林(east),北边有个神秘的池塘(north)。').

room_description(mystic_pond, '神秘池塘 (Mystic Pond)',
    '一个清澈的池塘,水面泛着微光。池边有一个古老的图腾。\n池边的岩石可以攀爬。\n南边是古老树林(south)。').

% 区域2: 金色海滩 (Golden Beach)
room_description(beach_entrance, '海滩入口 (Beach Entrance)',
    '从丛林走出,你来到一片金色的沙滩。海风清新,浪花拍打着海岸。\n西边回到丛林入口(west),东边是沙滩(east),南边有珊瑚礁(south)。').

room_description(sandy_shore, '沙滩 (Sandy Shore)',
    '柔软的沙滩上散落着贝壳,其中一个在发光!\n海水清澈,Boots可以在这里游泳搜索。\n西边回到海滩入口(west),北边有椰林(north)。').

room_description(coral_reef, '珊瑚礁 (Coral Reef)',
    '退潮后露出的珊瑚礁,五颜六色。你看到一张渔网挂在礁石上。\n水域清澈见底,Boots可以在这里游泳搜索。\n北边回到海滩入口(north),东边通向河谷(east)。').

room_description(palm_grove, '椰林 (Palm Grove)',
    '高大的椰树成林,地上有新鲜的椰子。\n海边的浅水区可以游泳。\n南边回到沙滩(south)。').

% 区域3: 瀑布河谷 (Waterfall Valley)
room_description(valley_entrance, '河谷入口 (Valley Entrance)',
    '你来到一个河谷的入口,可以听到远处的瀑布声。\n西边回到珊瑚礁(west),北边是瀑布底(north)。').

room_description(waterfall_base, '瀑布底 (Waterfall Base)',
    '壮观的瀑布从高处倾泻而下,水雾弥漫。在水边你看到一个闪光的护身符。\n这里既有可攀爬的岩壁也有可游泳的深潭!\n南边是河谷入口(south),东边沿河而行(east),西边有个水晶洞穴(west)。').

room_description(river_bend, '河湾 (River Bend)',
    '河流在这里转弯,形成一个平静的水湾。水面上漂浮着彩虹色的鳞片。\n水深足够游泳。\n西边回到瀑布底(west)。').

room_description(crystal_cave, '水晶洞穴 (Crystal Cave)',
    '洞穴内布满了闪亮的水晶,美丽极了。你发现一颗珍珠。\n洞穴岩壁可以攀爬。\n东边回到瀑布底(east),北边有条通往遗迹的路(north)。').

% 区域4: 古老遗迹 (Ancient Ruins)
room_description(ruins_entrance, '遗迹入口 (Ruins Entrance)',
    '古老的石质建筑遗迹出现在眼前,充满神秘感。\n石墙可以攀爬。\n南边回到水晶洞穴(south),东边是石圈(east)。').

room_description(stone_circle, '石圈 (Stone Circle)',
    '一圈巨大的石柱围成一个圆圈,中央有个日晷。\n石柱可以攀爬。\n西边回到遗迹入口(west),北边是密室(north),东边是祖先大厅(east)。').

room_description(secret_chamber, '密室 (Secret Chamber)',
    '一个隐藏的密室,墙上刻满了古老的符文。地上有一把水晶钥匙!\n南边回到石圈(south)。').

room_description(ancestral_hall, '祖先大厅 (Ancestral Hall)',
    '宏伟的大厅,墙上挂着古老的地图和壁画。\n西边回到石圈(west),北边是最终大门(north)。').

room_description(final_gate, '最终大门 (Final Gate)',
    '一扇巨大的石门,门上有个钥匙孔。这就是回家的出口!\n南边回到祖先大厅(south)。').

room_description(home, '家 (Home)',
    '你成功回家了!恭喜通关!').

% ============================================================
% 房间连接
% ============================================================

% 区域1: 茂密丛林连接
connected(jungle_entrance, north, jungle_path).
connected(jungle_path, south, jungle_entrance).
connected(jungle_path, north, deep_forest).
connected(deep_forest, south, jungle_path).
connected(deep_forest, west, ancient_grove).
connected(ancient_grove, east, deep_forest).
connected(ancient_grove, north, mystic_pond).
connected(mystic_pond, south, ancient_grove).

% 区域1到区域2连接
connected(jungle_entrance, east, beach_entrance).
connected(beach_entrance, west, jungle_entrance).

% 区域2: 金色海滩连接
connected(beach_entrance, east, sandy_shore).
connected(sandy_shore, west, beach_entrance).
connected(beach_entrance, south, coral_reef).
connected(coral_reef, north, beach_entrance).
connected(sandy_shore, north, palm_grove).
connected(palm_grove, south, sandy_shore).

% 区域2到区域3连接
connected(coral_reef, east, valley_entrance).
connected(valley_entrance, west, coral_reef).

% 区域3: 瀑布河谷连接
connected(valley_entrance, north, waterfall_base).
connected(waterfall_base, south, valley_entrance).
connected(waterfall_base, east, river_bend).
connected(river_bend, west, waterfall_base).
connected(waterfall_base, west, crystal_cave).
connected(crystal_cave, east, waterfall_base).

% 区域3到区域4连接
connected(crystal_cave, north, ruins_entrance).
connected(ruins_entrance, south, crystal_cave).

% 区域4: 古老遗迹连接
connected(ruins_entrance, east, stone_circle).
connected(stone_circle, west, ruins_entrance).
connected(stone_circle, north, secret_chamber).
connected(secret_chamber, south, stone_circle).
connected(stone_circle, east, ancestral_hall).
connected(ancestral_hall, west, stone_circle).
connected(ancestral_hall, north, final_gate).
connected(final_gate, south, ancestral_hall).

% 最终大门(需要水晶钥匙)
connected(final_gate, north, home) :- final_gate_unlocked.

% ============================================================
% 移动命令
% ============================================================
go(Direction) :-
    player_location(CurrentRoom),
    (   connected(CurrentRoom, Direction, NextRoom)
    ->  move_to_room(NextRoom)
    ;   format('你不能往~w走,那边没有路!~n', [Direction])
    ).

move_to_room(NextRoom) :-
    player_location(CurrentRoom),
    retract(player_location(CurrentRoom)),
    assert(player_location(NextRoom)),
    format('你从~w移动到~w~n', [CurrentRoom, NextRoom]),
    nl,

    % 检查时间
    game_time(Time),
    NewTime is Time - 1,
    retract(game_time(Time)),
    assert(game_time(NewTime)),

    % v5修改: 处理猴子搜索冷却
    process_monkey_cooldown,

    % v5增强: 随机事件(包含Swiper藏匿)
    random(R),
    (   R < 0.3
    ->  trigger_random_event
    ;   true
    ),

    % 显示新房间
    look,

    % 检查是否到家
    (   NextRoom = home
    ->  win_game
    ;   true
    ).

% ============================================================
% 查看命令 (v5增强: 显示攀爬/游泳信息和藏匿物品)
% ============================================================
look :-
    player_location(Room),
    room_description(Room, Name, Description),
    format('~n=== ~w ===~n', [Name]),
    writeln(Description),
    nl,

    % v5新增: 显示可攀爬/游泳信息
    (   climbable_location(Room)
    ->  writeln('[Boots可以在这里攀爬搜索]')
    ;   true
    ),
    (   swimmable_location(Room)
    ->  writeln('[Boots可以在这里游泳搜索]')
    ;   true
    ),

    % 显示房间内的物品
    findall(Item, room_has_item(Room, Item), Items),
    (   Items = []
    ->  true
    ;   writeln('这里有:'),
        forall(member(Item, Items), format('  - ~w~n', [Item]))
    ),

    % v5新增: 显示Swiper藏匿的物品提示
    (   swiper_hidden_item(Room, HiddenItem)
    ->  format('你注意到~w似乎被藏在了某个难以到达的地方...~n', [HiddenItem]),
        writeln('(提示: 让Boots搜索可能找到它!)')
    ;   true
    ),

    % 检查Swiper是否在附近
    swiper_location(SwiperRoom),
    (   SwiperRoom = Room
    ->  writeln('警告! Swiper就在这里,快说"stop_swiper."阻止他!')
    ;   true
    ).

% ============================================================
% 物品重量定义
% ============================================================
item_weight(herbal_medicine, 2).
item_weight(sharp_machete, 4).
item_weight(ancient_totem, 3).
item_weight(glowing_seashell, 1).
item_weight(fishing_net, 3).
item_weight(coconut_water, 2).
item_weight(water_amulet, 2).
item_weight(rainbow_scale, 1).
item_weight(river_pearl, 1).
item_weight(sun_dial, 3).
item_weight(crystal_key, 1).
item_weight(ancestral_map, 2).
% v5新增: 上天礼物
item_weight(heavens_gift, 0).

% 消耗品标记
consumable_item(coconut_water).
consumable_item(ancestral_map).
consumable_item(ancient_totem).

% 关键道具标记
critical_item(crystal_key).
critical_item(water_amulet).

% ============================================================
% 背包系统
% ============================================================

% 计算背包重量
backpack_weight([], 0).
backpack_weight([Item|Rest], TotalWeight) :-
    (   item_weight(Item, Weight)
    ->  true
    ;   Weight = 1  % 默认重量
    ),
    backpack_weight(Rest, RestWeight),
    TotalWeight is Weight + RestWeight.

% 拾取物品
take(Item) :-
    player_location(CurrentRoom),
    (   room_has_item(CurrentRoom, Item)
    ->  player_backpack(Backpack),
        item_weight(Item, Weight),
        backpack_weight(Backpack, CurrentWeight),
        MaxWeight = 15,
        NewWeight is CurrentWeight + Weight,
        (   NewWeight =< MaxWeight
        ->  retract(room_has_item(CurrentRoom, Item)),
            retract(player_backpack(Backpack)),
            append(Backpack, [Item], NewBackpack),
            assert(player_backpack(NewBackpack)),
            format('你拾取了 ~w!~n', [Item]),
            % 更新分数
            player_score(Score),
            NewScore is Score + 10,
            retract(player_score(Score)),
            assert(player_score(NewScore))
        ;   format('背包太重了!当前重量:~w/15,无法拿起~w(重量:~w)~n',
                   [CurrentWeight, Item, Weight])
        )
    ;   format('这里没有~w!~n', [Item])
    ).

% 丢弃物品
drop(Item) :-
    player_backpack(Backpack),
    (   member(Item, Backpack)
    ->  (   critical_item(Item)
        ->  format('警告:丢弃~w可能导致游戏无法完成!确定要丢弃吗?~n', [Item]),
            writeln('(输入 drop_confirm(Item). 确认丢弃)')
        ;   remove_item(Item, Backpack, NewBackpack),
            retract(player_backpack(Backpack)),
            assert(player_backpack(NewBackpack)),
            player_location(CurrentRoom),
            assert(room_has_item(CurrentRoom, Item)),
            format('你丢弃了~w!~n', [Item])
        )
    ;   format('你的背包里没有~w!~n', [Item])
    ).

% 确认丢弃关键物品
drop_confirm(Item) :-
    player_backpack(Backpack),
    remove_item(Item, Backpack, NewBackpack),
    retract(player_backpack(Backpack)),
    assert(player_backpack(NewBackpack)),
    player_location(CurrentRoom),
    assert(room_has_item(CurrentRoom, Item)),
    format('你丢弃了~w!~n', [Item]).

% 移除物品辅助函数
remove_item(_, [], []).
remove_item(Item, [Item|Tail], Tail) :- !.
remove_item(Item, [Head|Tail], [Head|Result]) :-
    remove_item(Item, Tail, Result).

% 查看背包
inventory :-
    player_backpack(Backpack),
    (   Backpack = []
    ->  writeln('你的背包是空的。')
    ;   writeln('=== 背包内容 ==='),
        backpack_weight(Backpack, Weight),
        format('总重量:~w/15~n', [Weight]),
        list_backpack_items(Backpack, 1)
    ).

list_backpack_items([], _).
list_backpack_items([Item|Rest], N) :-
    (   item_weight(Item, Weight)
    ->  true
    ;   Weight = 1
    ),
    format('~w. ~w (重量:~w)~n', [N, Item, Weight]),
    N1 is N + 1,
    list_backpack_items(Rest, N1).

% ============================================================
% 物品使用系统
% ============================================================

use(Item) :-
    player_backpack(Backpack),
    (   member(Item, Backpack)
    ->  (   apply_item_effect(Item)
        ->  (   consumable_item(Item)
            ->  remove_item(Item, Backpack, NewBackpack),
                retract(player_backpack(Backpack)),
                assert(player_backpack(NewBackpack)),
                format('你使用了~w(物品已消耗)~n', [Item])
            ;   format('你使用了~w~n', [Item])
            )
        ;   format('~w无法在这里使用!~n', [Item])
        )
    ;   format('你没有~w!~n', [Item])
    ).

% 物品效果应用
apply_item_effect(crystal_key) :-
    player_location(final_gate),
    final_gate_locked,
    retract(final_gate_locked),
    assert(final_gate_unlocked),
    writeln('水晶钥匙发出耀眼的光芒,最终大门开启了!'),
    writeln('现在你可以往北走回家了! go(north).').

apply_item_effect(water_amulet) :-
    writeln('水之护身符发出柔和的蓝光,保护着你。'),
    assert(theft_protection(3)),
    writeln('Swiper暂时无法偷窃你的物品!').

apply_item_effect(sharp_machete) :-
    player_location(Location),
    (   blocked_path(Location, Obstacle)
    ->  retract(blocked_path(Location, Obstacle)),
        format('你用砍刀清除了~w!~n', [Obstacle])
    ;   writeln('砍刀挥动,但这里没有障碍物。')
    ).

apply_item_effect(coconut_water) :-
    writeln('椰子水让你精力充沛!恢复了一些时间。'),
    game_time(Time),
    NewTime is Time + 5,
    retract(game_time(Time)),
    assert(game_time(NewTime)),
    format('剩余时间: ~w~n', [NewTime]).

apply_item_effect(ancestral_map) :-
    writeln('祖先地图显示了你的位置和目标:'),
    writeln('你需要找到水晶钥匙(在密室),然后前往最终大门!'),
    writeln('路线提示: 密室 -> 石圈 -> 祖先大厅 -> 最终大门').

apply_item_effect(ancient_totem) :-
    writeln('古老图腾发出神秘光芒!'),
    assert(swiper_confused(3)),
    writeln('Swiper被迷惑了3回合,无法偷窃!').

apply_item_effect(river_pearl) :-
    writeln('河流珍珠形成保护结界!'),
    assert(theft_protection(5)),
    writeln('Swiper无法偷窃你的物品!').

apply_item_effect(rainbow_scale) :-
    writeln('彩虹鳞片揭示了一些秘密:'),
    findall(Secret, hidden_secret(Secret), Secrets),
    (   Secrets = []
    ->  writeln('没有更多秘密了。')
    ;   forall(member(Secret, Secrets), (write('  - '), writeln(Secret)))
    ).

apply_item_effect(Item) :-
    format('你使用了~w,但似乎没有特别的效果。~n', [Item]).

% ============================================================
% 游戏状态
% ============================================================
status :-
    writeln('=== 游戏状态 ==='),
    player_location(Room),
    format('当前位置: ~w~n', [Room]),
    player_score(Score),
    format('得分: ~w~n', [Score]),
    game_time(Time),
    format('剩余时间: ~w~n', [Time]),
    player_backpack(Backpack),
    length(Backpack, ItemCount),
    backpack_weight(Backpack, Weight),
    format('背包: ~w件物品, 重量~w/15~n', [ItemCount, Weight]),
    % v5新增: 显示猴子状态
    (   monkey_search_cooldown(CD)
    ->  format('Boots搜索冷却: ~w回合~n', [CD])
    ;   writeln('Boots搜索: 可用')
    ).

% ============================================================
% v5新增: 猴子Boots搜索系统
% ============================================================

boots_search :-
    % 检查冷却时间
    (   monkey_search_cooldown(_)
    ->  writeln('Boots: "我有点累了,需要休息一下再搜索。"')
    ;   monkey_search_implementation
    ).

monkey_search_implementation :-
    monkey_name(Boots),
    player_location(Location),
    format('~w: "我可以帮你搜索这个区域!"~n', [Boots]),
    nl,

    % 检查是否是可攀爬或可游泳的位置
    (   climbable_location(Location)
    ->  format('~w爬上去寻找...~n', [Boots]),
        SearchType = climb
    ;   swimmable_location(Location)
    ->  format('~w跳进水里寻找...~n', [Boots]),
        SearchType = swim
    ;   writeln('Boots: "这里没有我可以搜索的地方..."'),
        SearchType = none
    ),

    (   SearchType \= none
    ->  % 搜索Swiper藏匿的物品
        (   swiper_hidden_item(Location, HiddenItem)
        ->  random(Chance),
            (   Chance < 0.8  % 80%成功率
            ->  retract(swiper_hidden_item(Location, HiddenItem)),
                player_backpack(Backpack),
                append(Backpack, [HiddenItem], NewBackpack),
                retract(player_backpack(Backpack)),
                assert(player_backpack(NewBackpack)),
                format('找到了! Swiper藏起来的~w!~n', [HiddenItem]),
                format('~w把~w放进了你的背包。~n', [Boots, HiddenItem]),
                player_score(Score),
                NewScore is Score + 20,
                retract(player_score(Score)),
                assert(player_score(NewScore))
            ;   format('~w: "太难够到了,我再试试..."~n', [Boots])
            )
        ;   format('~w: "这里没有Swiper藏的东西。"~n', [Boots])
        ),

        % 检查上天礼物 (10%概率)
        gift_location(GiftLoc),
        (   Location = GiftLoc
        ->  random(GiftChance),
            (   GiftChance < 0.1  % 10%概率找到
            ->  find_heavens_gift
            ;   true
            )
        ;   true
        ),

        % 设置冷却时间
        assert(monkey_search_cooldown(3))
    ;   true
    ).

% 处理猴子搜索冷却
process_monkey_cooldown :-
    (   retract(monkey_search_cooldown(Turns))
    ->  NewTurns is Turns - 1,
        (   NewTurns > 0
        ->  assert(monkey_search_cooldown(NewTurns))
        ;   true
        )
    ;   true
    ).

% v5新增: 找到上天礼物 - 直接胜利!
find_heavens_gift :-
    assert(monkey_found_gift),
    nl,
    writeln('============================================='),
    writeln('    *** 奇迹发生了! ***'),
    writeln('============================================='),
    writeln('Boots: "看!我找到了上天的礼物!"'),
    nl,
    writeln('一道金光从天而降,照亮了整个区域!'),
    writeln('神秘的力量将你和Boots瞬间传送回家!'),
    nl,
    writeln('============================================='),
    writeln('    恭喜!你触发了隐藏结局!'),
    writeln('    通过上天礼物直接获得胜利!'),
    writeln('============================================='),
    player_score(Score),
    BonusScore is Score + 100,
    format('最终得分: ~w (含隐藏奖励+100)~n', [BonusScore]),
    writeln('感谢游玩《朵拉的热带探险》!'),
    nl.

% ============================================================
% Swiper相关 (v5增强: 藏匿物品机制)
% ============================================================

stop_swiper :-
    player_location(PlayerRoom),
    swiper_location(SwiperRoom),
    (   PlayerRoom = SwiperRoom
    ->  writeln('你大声喊道:"Swiper, no swiping!"'),
        writeln('Swiper:"Oh, man!"'),
        writeln('Swiper溜走了!'),
        % Swiper移动到随机位置
        move_swiper_randomly,
        % 增加分数
        player_score(Score),
        NewScore is Score + 15,
        retract(player_score(Score)),
        assert(player_score(NewScore))
    ;   writeln('Swiper不在这里,你对着空气喊话...')
    ).

move_swiper_randomly :-
    retract(swiper_location(_)),
    random_member(NewRoom, [jungle_path, beach_entrance, valley_entrance, ruins_entrance]),
    assert(swiper_location(NewRoom)).

% v5新增: Swiper藏匿物品
swiper_hide_item(Item, Location) :-
    (   climbable_location(Location)
    ->  format('Swiper狡猾地笑着,把~w藏到了高高的树上!~n', [Item])
    ;   swimmable_location(Location)
    ->  format('Swiper大笑着,把~w扔进了深水里!~n', [Item])
    ;   format('Swiper鬼鬼祟祟地把~w藏在了某个角落!~n', [Item])
    ),
    assert(swiper_hidden_item(Location, Item)).

% v5增强: Swiper偷窃并藏匿
enhanced_steal_action :-
    player_backpack(Backpack),
    Backpack \= [],
    % 检查是否有偷窃保护
    (   theft_protection(_)
    ->  writeln('Swiper想偷东西,但被神秘力量阻挡了!'),
        writeln('Swiper: "Oh, man! 下次再来!"')
    ;   swiper_confused(_)
    ->  writeln('Swiper迷迷糊糊的,偷窃失败了!'),
        writeln('Swiper: "我...我在哪?"')
    ;   % 随机偷一件物品
        random_member(StolenItem, Backpack),
        \+ critical_item(StolenItem),  % 不偷关键物品
        retract(player_backpack(Backpack)),
        delete(Backpack, StolenItem, NewBackpack),
        assert(player_backpack(NewBackpack)),
        format('Swiper突然出现,抢走了你的~w!~n', [StolenItem]),

        % 50%概率藏匿物品
        random(HideChance),
        player_location(Location),
        (   HideChance < 0.5,
            (climbable_location(Location) ; swimmable_location(Location))
        ->  swiper_hide_item(StolenItem, Location),
            writeln('Swiper: "你永远也找不到它!哈哈哈哈!"')
        ;   writeln('Swiper偷走了物品并迅速逃跑了!'),
            writeln('Swiper: "You are too late!"')
        )
    ).

% v5增强: 随机事件
trigger_random_event :-
    random(R),
    (   R < 0.15
    ->  % 15%概率Swiper尝试偷窃
        writeln('Swiper出现了!'),
        enhanced_steal_action,
        move_swiper_randomly
    ;   R < 0.25
    ->  % 10%概率Boots提示
        monkey_name(Boots),
        format('~w: "我发现了一些可疑的地方,要我去搜索吗?"~n', [Boots]),
        writeln('(输入 boots_search. 让Boots搜索)')
    ;   R < 0.5
    ->  writeln('你听到了Swiper的笑声,他可能在附近!')
    ;   R < 0.7
    ->  writeln('你发现了一些闪光的物品碎片,但没什么用。')
    ;   writeln('一阵微风吹过,树叶沙沙作响。')
    ).

% ============================================================
% 帮助 (v5更新)
% ============================================================
help :-
    writeln('=== 可用指令 ==='),
    writeln('1. go(方向)          - 移动 (north/south/east/west/up/down)'),
    writeln('2. take(物品名)      - 拾取物品'),
    writeln('3. drop(物品名)      - 丢弃物品'),
    writeln('4. use(物品名)       - 使用物品'),
    writeln('5. look.             - 查看房间'),
    writeln('6. inventory.        - 查看背包'),
    writeln('7. status.           - 查看状态'),
    writeln('8. help.             - 显示帮助'),
    writeln('9. stop_swiper.      - 阻止Swiper'),
    writeln('10. boots_search.    - [新!] 让Boots搜索当前区域'),
    writeln('11. quit.            - 退出游戏'),
    nl,
    writeln('=== Boots搜索提示 ==='),
    writeln('- Boots可以在可攀爬或可游泳的地点搜索'),
    writeln('- 搜索可以找回被Swiper藏起来的物品'),
    writeln('- 搜索后需要等待3回合冷却'),
    writeln('- [隐藏彩蛋] 在某些地方搜索可能找到上天礼物!').

% ============================================================
% 游戏结束
% ============================================================
win_game :-
    nl,
    writeln('==============================================='),
    writeln('    恭喜!你成功回家了!'),
    writeln('==============================================='),
    player_score(Score),
    format('最终得分: ~w~n', [Score]),
    game_time(Time),
    format('剩余时间: ~w~n', [Time]),
    writeln('感谢游玩《朵拉的热带探险》!'),
    nl.

quit :-
    writeln('你选择离开游戏。再见!'),
    halt.

% ============================================================
% 游戏初始化提示
% ============================================================
:- writeln('《朵拉的热带探险》v5 已加载!').
:- writeln('新增功能: Boots猴子搜索系统、Swiper藏匿机制、上天礼物彩蛋').
:- writeln('输入 start. 开始游戏!').
