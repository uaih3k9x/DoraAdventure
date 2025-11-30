% ============================================================
% Dora's Tropical Adventure
% Knowledge Representation Project - Prolog Text Adventure Game
% Version: v5 - Boots Monkey Companion System, Swiper Hiding Mechanism
% ============================================================

:- use_module(library(random)).

% ============================================================
% Dynamic Predicate Declarations
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

% === v5 New: Monkey Boots System Dynamic Predicates ===
:- dynamic monkey_skill_active/1.          % Monkey skill activation status
:- dynamic monkey_found_gift/0.            % Monkey found heaven's gift
:- dynamic swiper_hidden_item/2.           % Items hidden by Swiper (location, item)
:- dynamic gift_location/1.                % Heaven's gift location
:- dynamic monkey_search_cooldown/1.       % Monkey search cooldown time

% ============================================================
% Monkey Boots Basic Attributes (v5 New)
% ============================================================
monkey_name(boots).
monkey_skill(climb).      % Climbing skill
monkey_skill(swim).       % Swimming skill

% === Climbable Locations ===
% Dense Jungle - Climbable
climbable_location(ancient_grove).
climbable_location(mystic_pond).
climbable_location(deep_forest).

% Waterfall Valley - Climbable
climbable_location(waterfall_base).
climbable_location(crystal_cave).

% Ancient Ruins - Climbable
climbable_location(stone_circle).
climbable_location(ruins_entrance).

% === Swimmable Locations ===
% Golden Beach - Swimmable
swimmable_location(coral_reef).
swimmable_location(sandy_shore).
swimmable_location(palm_grove).

% Waterfall Valley - Swimmable
swimmable_location(river_bend).
swimmable_location(waterfall_base).

% ============================================================
% Game Start Predicate (start/0 - Required for Assignment)
% ============================================================
start :-
    writeln('==============================================='),
    writeln('  Welcome to Dora\'s Tropical Adventure!'),
    writeln('==============================================='),
    nl,
    writeln('Story Background:'),
    writeln('Dora got lost while exploring the tropical rainforest.'),
    writeln('She needs to traverse four mysterious areas, collect key items,'),
    writeln('avoid Swiper the fox\'s theft, and find her way home!'),
    nl,
    writeln('Game Objectives:'),
    writeln('- Explore four areas: Dense Jungle, Golden Beach, Waterfall Valley, Ancient Ruins'),
    writeln('- Collect 12 unique items'),
    writeln('- Stop Swiper from stealing (say "Swiper, no swiping!")'),
    writeln('- Find the Crystal Key and open the Final Gate'),
    writeln('- [Hidden Easter Egg] Let Boots search to possibly find Heaven\'s Gift for instant victory!'),
    nl,
    writeln('Available Commands:'),
    writeln('1. go(Direction)     - Move (north/south/east/west/up/down)'),
    writeln('2. take(ItemName)    - Pick up item in current room'),
    writeln('3. drop(ItemName)    - Drop item from backpack'),
    writeln('4. use(ItemName)     - Use item from backpack'),
    writeln('5. look.             - View current room'),
    writeln('6. inventory.        - View backpack contents'),
    writeln('7. status.           - View game status'),
    writeln('8. help.             - Show available commands'),
    writeln('9. stop_swiper.      - Try to stop Swiper'),
    writeln('10. boots_search.    - [New!] Let Boots search current area'),
    writeln('11. quit.            - Exit game'),
    nl,
    writeln('Note: Enter commands directly, e.g.: go(north). Don\'t forget the period!'),
    nl,
    initial_state,
    writeln('Game starts! You are now at the starting point...'),
    writeln('Boots: "I\'ll help you find what Swiper hid!"'),
    nl,
    look.

% ============================================================
% Initial State Setup
% ============================================================
initial_state :-
    % Clear all dynamic states
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
    % v5 new clear
    retractall(monkey_skill_active(_)),
    retractall(monkey_found_gift),
    retractall(swiper_hidden_item(_, _)),
    retractall(gift_location(_)),
    retractall(monkey_search_cooldown(_)),

    % Set initial state
    assert(player_location(jungle_entrance)),
    assert(player_backpack([])),
    assert(player_score(0)),
    assert(game_time(100)),
    assert(final_gate_locked),
    assert(swiper_location(jungle_path)),

    % v5 new: Initialize monkey system
    assert(monkey_skill_active(true)),

    % Initialize all item locations
    initialize_all_items,

    % v5 new: Initialize heaven's gift
    initialize_heavens_gift.

% ============================================================
% Item Location Initialization
% ============================================================
initialize_all_items :-
    % Dense Jungle area items
    assert(room_has_item(deep_forest, herbal_medicine)),
    assert(room_has_item(ancient_grove, sharp_machete)),
    assert(room_has_item(mystic_pond, ancient_totem)),

    % Golden Beach area items
    assert(room_has_item(sandy_shore, glowing_seashell)),
    assert(room_has_item(coral_reef, fishing_net)),
    assert(room_has_item(palm_grove, coconut_water)),

    % Waterfall Valley area items
    assert(room_has_item(waterfall_base, water_amulet)),
    assert(room_has_item(river_bend, rainbow_scale)),
    assert(room_has_item(crystal_cave, river_pearl)),

    % Ancient Ruins area items
    assert(room_has_item(stone_circle, sun_dial)),
    assert(room_has_item(secret_chamber, crystal_key)),
    assert(room_has_item(ancestral_hall, ancestral_map)),

    % Set some hidden secrets
    assert(hidden_secret('There is a shortcut from deep forest to the beach')),
    assert(hidden_secret('The crystal cave contains clues to finding home')),
    assert(hidden_secret('Let Boots search in climbable places for surprises')),

    % Set some blocked paths
    assert(blocked_path(ancient_grove, 'dense_vines')).

% ============================================================
% v5 New: Heaven's Gift System
% ============================================================
initialize_heavens_gift :-
    % Randomly select a climbable location for heaven's gift
    findall(Loc, climbable_location(Loc), ClimbableLocs),
    random_member(GiftLocation, ClimbableLocs),
    assert(gift_location(GiftLocation)).

% ============================================================
% Room Descriptions (v5 Enhanced: Shows climbable/swimmable info)
% ============================================================

% Area 1: Dense Jungle
room_description(jungle_entrance, 'Jungle Entrance',
    'You stand at the entrance of the tropical rainforest. Tall trees block the sky,\nvines intertwine everywhere. You can hear animal calls in the distance.\nA path leads north into the forest, and east seems to lead to a beach.').

room_description(jungle_path, 'Jungle Path',
    'A winding path through the dense forest. Fresh footprints on the ground - \npossibly left by Swiper!\nNorth leads to deep forest, south returns to the entrance.').

room_description(deep_forest, 'Deep Forest',
    'This is the heart of the jungle, dimly lit. You see some herbs growing in the corner.\nGiant trees here can be climbed.\nSouth is the path, west leads to an ancient grove.').

room_description(ancient_grove, 'Ancient Grove',
    'Massive ancient trees stand here, possibly hundreds of years old. \nSome tools are scattered on the ground.\nDense branches - Boots can climb and search here.\nEast returns to deep forest, north has a mystic pond.').

room_description(mystic_pond, 'Mystic Pond',
    'A clear pond with a shimmering surface. An ancient totem stands by the water.\nRocks by the pond can be climbed.\nSouth leads to the ancient grove.').

% Area 2: Golden Beach
room_description(beach_entrance, 'Beach Entrance',
    'Emerging from the jungle, you arrive at a golden sandy beach. \nThe sea breeze is refreshing, waves lap at the shore.\nWest returns to jungle entrance, east to sandy shore, south to coral reef.').

room_description(sandy_shore, 'Sandy Shore',
    'Soft sand scattered with shells, one of which is glowing!\nClear waters - Boots can swim and search here.\nWest returns to beach entrance, north leads to palm grove.').

room_description(coral_reef, 'Coral Reef',
    'Colorful coral reef exposed by low tide. You see a fishing net caught on the rocks.\nCrystal clear waters - Boots can swim and search here.\nNorth returns to beach entrance, east leads to the valley.').

room_description(palm_grove, 'Palm Grove',
    'Tall palm trees form a grove, fresh coconuts lie on the ground.\nShallow waters by the beach for swimming.\nSouth returns to sandy shore.').

% Area 3: Waterfall Valley
room_description(valley_entrance, 'Valley Entrance',
    'You arrive at the entrance of a river valley. \nYou can hear a distant waterfall.\nWest returns to coral reef, north leads to the waterfall base.').

room_description(waterfall_base, 'Waterfall Base',
    'A magnificent waterfall cascades from above, mist fills the air. \nYou spot a gleaming amulet by the water.\nBoth climbable rock walls and swimmable deep pools here!\nSouth is valley entrance, east follows the river, west has a crystal cave.').

room_description(river_bend, 'River Bend',
    'The river bends here, forming a calm pool. \nRainbow-colored scales float on the surface.\nDeep enough for swimming.\nWest returns to waterfall base.').

room_description(crystal_cave, 'Crystal Cave',
    'The cave is filled with sparkling crystals, beautiful! You find a pearl.\nCave walls can be climbed.\nEast returns to waterfall base, north leads to the ruins.').

% Area 4: Ancient Ruins
room_description(ruins_entrance, 'Ruins Entrance',
    'Ancient stone ruins appear before you, full of mystery.\nStone walls can be climbed.\nSouth returns to crystal cave, east leads to stone circle.').

room_description(stone_circle, 'Stone Circle',
    'A circle of massive stone pillars with a sundial in the center.\nPillars can be climbed.\nWest returns to ruins entrance, north to secret chamber, east to ancestral hall.').

room_description(secret_chamber, 'Secret Chamber',
    'A hidden chamber with ancient runes carved on the walls. \nA crystal key lies on the ground!\nSouth returns to stone circle.').

room_description(ancestral_hall, 'Ancestral Hall',
    'A grand hall with ancient maps and murals on the walls.\nWest returns to stone circle, north leads to the final gate.').

room_description(final_gate, 'Final Gate',
    'A massive stone door with a keyhole. This is the exit home!\nSouth returns to ancestral hall.').

room_description(home, 'Home',
    'You made it home! Congratulations on completing the game!').

% ============================================================
% Room Connections
% ============================================================

% Area 1: Dense Jungle Connections
connected(jungle_entrance, north, jungle_path).
connected(jungle_path, south, jungle_entrance).
connected(jungle_path, north, deep_forest).
connected(deep_forest, south, jungle_path).
connected(deep_forest, west, ancient_grove).
connected(ancient_grove, east, deep_forest).
connected(ancient_grove, north, mystic_pond).
connected(mystic_pond, south, ancient_grove).

% Area 1 to Area 2 Connection
connected(jungle_entrance, east, beach_entrance).
connected(beach_entrance, west, jungle_entrance).

% Area 2: Golden Beach Connections
connected(beach_entrance, east, sandy_shore).
connected(sandy_shore, west, beach_entrance).
connected(beach_entrance, south, coral_reef).
connected(coral_reef, north, beach_entrance).
connected(sandy_shore, north, palm_grove).
connected(palm_grove, south, sandy_shore).

% Area 2 to Area 3 Connection
connected(coral_reef, east, valley_entrance).
connected(valley_entrance, west, coral_reef).

% Area 3: Waterfall Valley Connections
connected(valley_entrance, north, waterfall_base).
connected(waterfall_base, south, valley_entrance).
connected(waterfall_base, east, river_bend).
connected(river_bend, west, waterfall_base).
connected(waterfall_base, west, crystal_cave).
connected(crystal_cave, east, waterfall_base).

% Area 3 to Area 4 Connection
connected(crystal_cave, north, ruins_entrance).
connected(ruins_entrance, south, crystal_cave).

% Area 4: Ancient Ruins Connections
connected(ruins_entrance, east, stone_circle).
connected(stone_circle, west, ruins_entrance).
connected(stone_circle, north, secret_chamber).
connected(secret_chamber, south, stone_circle).
connected(stone_circle, east, ancestral_hall).
connected(ancestral_hall, west, stone_circle).
connected(ancestral_hall, north, final_gate).
connected(final_gate, south, ancestral_hall).

% Final Gate (requires Crystal Key)
connected(final_gate, north, home) :- final_gate_unlocked.

% ============================================================
% Movement Commands
% ============================================================
go(Direction) :-
    player_location(CurrentRoom),
    (   connected(CurrentRoom, Direction, NextRoom)
    ->  move_to_room(NextRoom)
    ;   format('You cannot go ~w, there is no path that way!~n', [Direction])
    ).

move_to_room(NextRoom) :-
    player_location(CurrentRoom),
    retract(player_location(CurrentRoom)),
    assert(player_location(NextRoom)),
    format('You moved from ~w to ~w~n', [CurrentRoom, NextRoom]),
    nl,

    % Check time
    game_time(Time),
    NewTime is Time - 1,
    retract(game_time(Time)),
    assert(game_time(NewTime)),

    % v5 modified: Process monkey search cooldown
    process_monkey_cooldown,

    % v5 enhanced: Random events (including Swiper hiding)
    random(R),
    (   R < 0.3
    ->  trigger_random_event
    ;   true
    ),

    % Display new room
    look,

    % Check if arrived home
    (   NextRoom = home
    ->  win_game
    ;   true
    ).

% ============================================================
% Look Command (v5 Enhanced: Shows climbable/swimmable info and hidden items)
% ============================================================
look :-
    player_location(Room),
    room_description(Room, Name, Description),
    format('~n=== ~w ===~n', [Name]),
    writeln(Description),
    nl,

    % v5 new: Show climbable/swimmable info
    (   climbable_location(Room)
    ->  writeln('[Boots can climb and search here]')
    ;   true
    ),
    (   swimmable_location(Room)
    ->  writeln('[Boots can swim and search here]')
    ;   true
    ),

    % Show items in room
    findall(Item, room_has_item(Room, Item), Items),
    (   Items = []
    ->  true
    ;   writeln('Items here:'),
        forall(member(Item, Items), format('  - ~w~n', [Item]))
    ),

    % v5 new: Show hint about Swiper's hidden items
    (   swiper_hidden_item(Room, HiddenItem)
    ->  format('You notice ~w seems to be hidden somewhere hard to reach...~n', [HiddenItem]),
        writeln('(Hint: Let Boots search to find it!)')
    ;   true
    ),

    % Check if Swiper is nearby
    swiper_location(SwiperRoom),
    (   SwiperRoom = Room
    ->  writeln('WARNING! Swiper is here! Quick, say "stop_swiper." to stop him!')
    ;   true
    ).

% ============================================================
% Item Weight Definitions
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
% v5 new: Heaven's gift
item_weight(heavens_gift, 0).

% Consumable items
consumable_item(coconut_water).
consumable_item(ancestral_map).
consumable_item(ancient_totem).

% Critical items
critical_item(crystal_key).
critical_item(water_amulet).

% ============================================================
% Backpack System
% ============================================================

% Calculate backpack weight
backpack_weight([], 0).
backpack_weight([Item|Rest], TotalWeight) :-
    (   item_weight(Item, Weight)
    ->  true
    ;   Weight = 1  % Default weight
    ),
    backpack_weight(Rest, RestWeight),
    TotalWeight is Weight + RestWeight.

% Pick up item
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
            format('You picked up ~w!~n', [Item]),
            % Update score
            player_score(Score),
            NewScore is Score + 10,
            retract(player_score(Score)),
            assert(player_score(NewScore))
        ;   format('Backpack too heavy! Current weight: ~w/15, cannot pick up ~w (weight: ~w)~n',
                   [CurrentWeight, Item, Weight])
        )
    ;   format('There is no ~w here!~n', [Item])
    ).

% Drop item
drop(Item) :-
    player_backpack(Backpack),
    (   member(Item, Backpack)
    ->  (   critical_item(Item)
        ->  format('WARNING: Dropping ~w may make the game impossible to complete!~n', [Item]),
            writeln('(Enter drop_confirm(Item). to confirm dropping)')
        ;   remove_item(Item, Backpack, NewBackpack),
            retract(player_backpack(Backpack)),
            assert(player_backpack(NewBackpack)),
            player_location(CurrentRoom),
            assert(room_has_item(CurrentRoom, Item)),
            format('You dropped ~w!~n', [Item])
        )
    ;   format('You don\'t have ~w in your backpack!~n', [Item])
    ).

% Confirm dropping critical item
drop_confirm(Item) :-
    player_backpack(Backpack),
    remove_item(Item, Backpack, NewBackpack),
    retract(player_backpack(Backpack)),
    assert(player_backpack(NewBackpack)),
    player_location(CurrentRoom),
    assert(room_has_item(CurrentRoom, Item)),
    format('You dropped ~w!~n', [Item]).

% Remove item helper function
remove_item(_, [], []).
remove_item(Item, [Item|Tail], Tail) :- !.
remove_item(Item, [Head|Tail], [Head|Result]) :-
    remove_item(Item, Tail, Result).

% View inventory
inventory :-
    player_backpack(Backpack),
    (   Backpack = []
    ->  writeln('Your backpack is empty.')
    ;   writeln('=== Backpack Contents ==='),
        backpack_weight(Backpack, Weight),
        format('Total weight: ~w/15~n', [Weight]),
        list_backpack_items(Backpack, 1)
    ).

list_backpack_items([], _).
list_backpack_items([Item|Rest], N) :-
    (   item_weight(Item, Weight)
    ->  true
    ;   Weight = 1
    ),
    format('~w. ~w (weight: ~w)~n', [N, Item, Weight]),
    N1 is N + 1,
    list_backpack_items(Rest, N1).

% ============================================================
% Item Usage System
% ============================================================

use(Item) :-
    player_backpack(Backpack),
    (   member(Item, Backpack)
    ->  (   apply_item_effect(Item)
        ->  (   consumable_item(Item)
            ->  remove_item(Item, Backpack, NewBackpack),
                retract(player_backpack(Backpack)),
                assert(player_backpack(NewBackpack)),
                format('You used ~w (item consumed)~n', [Item])
            ;   format('You used ~w~n', [Item])
            )
        ;   format('~w cannot be used here!~n', [Item])
        )
    ;   format('You don\'t have ~w!~n', [Item])
    ).

% Item effect applications
apply_item_effect(crystal_key) :-
    player_location(final_gate),
    final_gate_locked,
    retract(final_gate_locked),
    assert(final_gate_unlocked),
    writeln('The Crystal Key emits a dazzling light, the Final Gate opens!'),
    writeln('Now you can go north to return home! go(north).').

apply_item_effect(water_amulet) :-
    writeln('The Water Amulet emits a soft blue glow, protecting you.'),
    assert(theft_protection(3)),
    writeln('Swiper cannot steal your items temporarily!').

apply_item_effect(sharp_machete) :-
    player_location(Location),
    (   blocked_path(Location, Obstacle)
    ->  retract(blocked_path(Location, Obstacle)),
        format('You cleared ~w with the machete!~n', [Obstacle])
    ;   writeln('You swing the machete, but there are no obstacles here.')
    ).

apply_item_effect(coconut_water) :-
    writeln('The coconut water refreshes you! Some time recovered.'),
    game_time(Time),
    NewTime is Time + 5,
    retract(game_time(Time)),
    assert(game_time(NewTime)),
    format('Remaining time: ~w~n', [NewTime]).

apply_item_effect(ancestral_map) :-
    writeln('The ancestral map shows your location and destination:'),
    writeln('You need to find the Crystal Key (in Secret Chamber), then go to the Final Gate!'),
    writeln('Route hint: Secret Chamber -> Stone Circle -> Ancestral Hall -> Final Gate').

apply_item_effect(ancient_totem) :-
    writeln('The Ancient Totem emits a mysterious glow!'),
    assert(swiper_confused(3)),
    writeln('Swiper is confused for 3 turns and cannot steal!').

apply_item_effect(river_pearl) :-
    writeln('The River Pearl creates a protective barrier!'),
    assert(theft_protection(5)),
    writeln('Swiper cannot steal your items!').

apply_item_effect(rainbow_scale) :-
    writeln('The Rainbow Scale reveals some secrets:'),
    findall(Secret, hidden_secret(Secret), Secrets),
    (   Secrets = []
    ->  writeln('No more secrets.')
    ;   forall(member(Secret, Secrets), (write('  - '), writeln(Secret)))
    ).

apply_item_effect(Item) :-
    format('You used ~w, but nothing special happened.~n', [Item]).

% ============================================================
% Game Status
% ============================================================
status :-
    writeln('=== Game Status ==='),
    player_location(Room),
    format('Current location: ~w~n', [Room]),
    player_score(Score),
    format('Score: ~w~n', [Score]),
    game_time(Time),
    format('Remaining time: ~w~n', [Time]),
    player_backpack(Backpack),
    length(Backpack, ItemCount),
    backpack_weight(Backpack, Weight),
    format('Backpack: ~w items, weight ~w/15~n', [ItemCount, Weight]),
    % v5 new: Show monkey status
    (   monkey_search_cooldown(CD)
    ->  format('Boots search cooldown: ~w turns~n', [CD])
    ;   writeln('Boots search: Available')
    ).

% ============================================================
% v5 New: Monkey Boots Search System
% ============================================================

boots_search :-
    % Check cooldown time
    (   monkey_search_cooldown(_)
    ->  writeln('Boots: "I\'m a bit tired, need to rest before searching again."')
    ;   monkey_search_implementation
    ).

monkey_search_implementation :-
    monkey_name(Boots),
    player_location(Location),
    format('~w: "I can help you search this area!"~n', [Boots]),
    nl,

    % Check if location is climbable or swimmable
    (   climbable_location(Location)
    ->  format('~w climbs up to search...~n', [Boots]),
        SearchType = climb
    ;   swimmable_location(Location)
    ->  format('~w dives into the water to search...~n', [Boots]),
        SearchType = swim
    ;   writeln('Boots: "There\'s nowhere for me to search here..."'),
        SearchType = none
    ),

    (   SearchType \= none
    ->  % Search for Swiper's hidden items
        (   swiper_hidden_item(Location, HiddenItem)
        ->  random(Chance),
            (   Chance < 0.8  % 80% success rate
            ->  retract(swiper_hidden_item(Location, HiddenItem)),
                player_backpack(Backpack),
                append(Backpack, [HiddenItem], NewBackpack),
                retract(player_backpack(Backpack)),
                assert(player_backpack(NewBackpack)),
                format('Found it! ~w that Swiper hid!~n', [HiddenItem]),
                format('~w put ~w in your backpack.~n', [Boots, HiddenItem]),
                player_score(Score),
                NewScore is Score + 20,
                retract(player_score(Score)),
                assert(player_score(NewScore))
            ;   format('~w: "It\'s too hard to reach, let me try again..."~n', [Boots])
            )
        ;   format('~w: "Nothing hidden by Swiper here."~n', [Boots])
        ),

        % Check for heaven's gift (10% chance)
        gift_location(GiftLoc),
        (   Location = GiftLoc
        ->  random(GiftChance),
            (   GiftChance < 0.1  % 10% chance to find
            ->  find_heavens_gift
            ;   true
            )
        ;   true
        ),

        % Set cooldown time
        assert(monkey_search_cooldown(3))
    ;   true
    ).

% Process monkey search cooldown
process_monkey_cooldown :-
    (   retract(monkey_search_cooldown(Turns))
    ->  NewTurns is Turns - 1,
        (   NewTurns > 0
        ->  assert(monkey_search_cooldown(NewTurns))
        ;   true
        )
    ;   true
    ).

% v5 New: Find Heaven's Gift - Instant Victory!
find_heavens_gift :-
    assert(monkey_found_gift),
    nl,
    writeln('============================================='),
    writeln('    *** A Miracle Happened! ***'),
    writeln('============================================='),
    writeln('Boots: "Look! I found Heaven\'s Gift!"'),
    nl,
    writeln('A golden light descends from above, illuminating the entire area!'),
    writeln('Mysterious power instantly teleports you and Boots home!'),
    nl,
    writeln('============================================='),
    writeln('    Congratulations! You triggered the hidden ending!'),
    writeln('    Victory through Heaven\'s Gift!'),
    writeln('============================================='),
    player_score(Score),
    BonusScore is Score + 100,
    format('Final Score: ~w (includes hidden bonus +100)~n', [BonusScore]),
    writeln('Thank you for playing Dora\'s Tropical Adventure!'),
    nl.

% ============================================================
% Swiper Related (v5 Enhanced: Item Hiding Mechanism)
% ============================================================

stop_swiper :-
    player_location(PlayerRoom),
    swiper_location(SwiperRoom),
    (   PlayerRoom = SwiperRoom
    ->  writeln('You shout: "Swiper, no swiping!"'),
        writeln('Swiper: "Oh, man!"'),
        writeln('Swiper runs away!'),
        % Move Swiper to random location
        move_swiper_randomly,
        % Add score
        player_score(Score),
        NewScore is Score + 15,
        retract(player_score(Score)),
        assert(player_score(NewScore))
    ;   writeln('Swiper is not here, you\'re shouting at nothing...')
    ).

move_swiper_randomly :-
    retract(swiper_location(_)),
    random_member(NewRoom, [jungle_path, beach_entrance, valley_entrance, ruins_entrance]),
    assert(swiper_location(NewRoom)).

% v5 New: Swiper Hides Item
swiper_hide_item(Item, Location) :-
    (   climbable_location(Location)
    ->  format('Swiper laughs slyly and hides ~w high up in the trees!~n', [Item])
    ;   swimmable_location(Location)
    ->  format('Swiper laughs and throws ~w into the deep water!~n', [Item])
    ;   format('Swiper sneakily hides ~w in a corner!~n', [Item])
    ),
    assert(swiper_hidden_item(Location, Item)).

% v5 Enhanced: Swiper Steals and Hides
enhanced_steal_action :-
    player_backpack(Backpack),
    Backpack \= [],
    % Check for theft protection
    (   theft_protection(_)
    ->  writeln('Swiper tries to steal, but is blocked by a mysterious force!'),
        writeln('Swiper: "Oh, man! I\'ll be back!"')
    ;   swiper_confused(_)
    ->  writeln('Swiper is confused and fails to steal!'),
        writeln('Swiper: "I... where am I?"')
    ;   % Randomly steal an item
        random_member(StolenItem, Backpack),
        \+ critical_item(StolenItem),  % Don't steal critical items
        retract(player_backpack(Backpack)),
        delete(Backpack, StolenItem, NewBackpack),
        assert(player_backpack(NewBackpack)),
        format('Swiper appears suddenly and snatches your ~w!~n', [StolenItem]),

        % 50% chance to hide item
        random(HideChance),
        player_location(Location),
        (   HideChance < 0.5,
            (climbable_location(Location) ; swimmable_location(Location))
        ->  swiper_hide_item(StolenItem, Location),
            writeln('Swiper: "You\'ll never find it! Hahahaha!"')
        ;   writeln('Swiper steals the item and quickly escapes!'),
            writeln('Swiper: "You\'re too slow!"')
        )
    ).

% v5 Enhanced: Random Events
trigger_random_event :-
    random(R),
    (   R < 0.15
    ->  % 15% chance Swiper attempts theft
        writeln('Swiper appears!'),
        enhanced_steal_action,
        move_swiper_randomly
    ;   R < 0.25
    ->  % 10% chance Boots hints
        monkey_name(Boots),
        format('~w: "I noticed something suspicious, should I search?"~n', [Boots]),
        writeln('(Enter boots_search. to let Boots search)')
    ;   R < 0.5
    ->  writeln('You hear Swiper\'s laughter, he might be nearby!')
    ;   R < 0.7
    ->  writeln('You find some shiny item fragments, but they\'re useless.')
    ;   writeln('A gentle breeze blows, leaves rustle softly.')
    ).

% ============================================================
% Help (v5 Updated)
% ============================================================
help :-
    writeln('=== Available Commands ==='),
    writeln('1. go(Direction)     - Move (north/south/east/west/up/down)'),
    writeln('2. take(ItemName)    - Pick up item'),
    writeln('3. drop(ItemName)    - Drop item'),
    writeln('4. use(ItemName)     - Use item'),
    writeln('5. look.             - View room'),
    writeln('6. inventory.        - View backpack'),
    writeln('7. status.           - View status'),
    writeln('8. help.             - Show help'),
    writeln('9. stop_swiper.      - Stop Swiper'),
    writeln('10. boots_search.    - [New!] Let Boots search current area'),
    writeln('11. quit.            - Exit game'),
    nl,
    writeln('=== Boots Search Tips ==='),
    writeln('- Boots can search in climbable or swimmable locations'),
    writeln('- Searching can recover items hidden by Swiper'),
    writeln('- After searching, wait 3 turns for cooldown'),
    writeln('- [Hidden Easter Egg] Searching in certain places may find Heaven\'s Gift!').

% ============================================================
% Game End
% ============================================================
win_game :-
    nl,
    writeln('==============================================='),
    writeln('    Congratulations! You made it home!'),
    writeln('==============================================='),
    player_score(Score),
    format('Final Score: ~w~n', [Score]),
    game_time(Time),
    format('Remaining Time: ~w~n', [Time]),
    writeln('Thank you for playing Dora\'s Tropical Adventure!'),
    nl.

quit :-
    writeln('You chose to leave the game. Goodbye!'),
    halt.

% ============================================================
% Game Initialization Message
% ============================================================
:- writeln('Dora\'s Tropical Adventure v5 loaded!').
:- writeln('New features: Boots monkey search system, Swiper hiding mechanism, Heaven\'s Gift easter egg').
:- writeln('Enter start. to begin the game!').
