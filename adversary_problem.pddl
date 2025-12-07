;; ============================================================
;; Dora's Tropical Adventure - Adversary (Swiper) Problem
;; ============================================================
;; This PDDL problem defines a specific game state for Swiper
;; to plan his next action. The problem is dynamically generated
;; by the game based on current game state.
;;
;; This example shows a typical mid-game scenario where:
;; - Player is exploring the deep forest with valuable items
;; - Swiper is nearby planning to steal
;; ============================================================

(define (problem swiper_turn)
    (:domain swiper_behavior)

    ;; ============================================================
    ;; Objects: All locations and items in the game world
    ;; ============================================================
    (:objects
        ;; Area 1: Dense Jungle
        jungle_entrance jungle_path deep_forest ancient_grove mystic_pond - location
        ;; Area 2: Golden Beach
        beach_entrance sandy_shore coral_reef palm_grove - location
        ;; Area 3: Waterfall Valley
        valley_entrance waterfall_base river_bend crystal_cave - location
        ;; Area 4: Ancient Ruins
        ruins_entrance stone_circle secret_chamber ancestral_hall final_gate - location

        ;; Items
        herbal_medicine sharp_machete ancient_totem - item
        glowing_seashell fishing_net coconut_water - item
        water_amulet rainbow_scale river_pearl - item
        sun_dial crystal_key ancestral_map - item
    )

    ;; ============================================================
    ;; Initial State: Current game situation
    ;; ============================================================
    (:init
        ;; ----- Character Positions -----
        (swiper_at jungle_path)
        (player_at deep_forest)

        ;; ----- Map Connections -----
        ;; Area 1: Dense Jungle
        (connected jungle_entrance jungle_path)
        (connected jungle_path jungle_entrance)
        (connected jungle_path deep_forest)
        (connected deep_forest jungle_path)
        (connected deep_forest ancient_grove)
        (connected ancient_grove deep_forest)
        (connected ancient_grove mystic_pond)
        (connected mystic_pond ancient_grove)

        ;; Area 1 to Area 2 connection
        (connected jungle_entrance beach_entrance)
        (connected beach_entrance jungle_entrance)

        ;; Area 2: Golden Beach
        (connected beach_entrance sandy_shore)
        (connected sandy_shore beach_entrance)
        (connected beach_entrance coral_reef)
        (connected coral_reef beach_entrance)
        (connected sandy_shore palm_grove)
        (connected palm_grove sandy_shore)

        ;; Area 2 to Area 3 connection
        (connected coral_reef valley_entrance)
        (connected valley_entrance coral_reef)

        ;; Area 3: Waterfall Valley
        (connected valley_entrance waterfall_base)
        (connected waterfall_base valley_entrance)
        (connected waterfall_base river_bend)
        (connected river_bend waterfall_base)
        (connected waterfall_base crystal_cave)
        (connected crystal_cave waterfall_base)

        ;; Area 3 to Area 4 connection
        (connected crystal_cave ruins_entrance)
        (connected ruins_entrance crystal_cave)

        ;; Area 4: Ancient Ruins
        (connected ruins_entrance stone_circle)
        (connected stone_circle ruins_entrance)
        (connected stone_circle secret_chamber)
        (connected secret_chamber stone_circle)
        (connected stone_circle ancestral_hall)
        (connected ancestral_hall stone_circle)
        (connected ancestral_hall final_gate)
        (connected final_gate ancestral_hall)

        ;; ----- Terrain Features -----
        ;; Climbable locations (trees, structures)
        (climbable ancient_grove)
        (climbable mystic_pond)
        (climbable deep_forest)
        (climbable waterfall_base)
        (climbable crystal_cave)
        (climbable stone_circle)
        (climbable ruins_entrance)

        ;; Swimmable locations (water areas)
        (swimmable coral_reef)
        (swimmable sandy_shore)
        (swimmable palm_grove)
        (swimmable river_bend)
        (swimmable waterfall_base)

        ;; Locations with escape routes
        (has_escape_route jungle_path)
        (has_escape_route beach_entrance)
        (has_escape_route valley_entrance)
        (has_escape_route ruins_entrance)
        (has_escape_route coral_reef)
        (has_escape_route stone_circle)

        ;; Safe zones (Swiper avoids)
        (safe_zone jungle_entrance)
        (safe_zone final_gate)

        ;; ----- Valuable Items -----
        (valuable crystal_key)
        (valuable water_amulet)
        (valuable ancestral_map)
        (valuable ancient_totem)
        (valuable rainbow_scale)
        (valuable river_pearl)

        ;; ----- Current Item Locations -----
        ;; Player's backpack
        (player_has crystal_key)
        (player_has herbal_medicine)

        ;; Items on the ground
        (item_at sharp_machete ancient_grove)
        (item_at water_amulet waterfall_base)
        (item_at rainbow_scale coral_reef)

        ;; Swiper's status (not blocked or confused in this example)
        ;; (swiper_blocked)  ; Uncomment if player blocked Swiper
        ;; (swiper_confused) ; Uncomment if player said "Swiper no swiping!"
    )

    ;; ============================================================
    ;; Goal: What Swiper wants to achieve
    ;; ============================================================
    ;; In this example, Swiper wants to steal the crystal_key from player
    ;; The goal is dynamically set based on game situation:
    ;; - If player has valuable items -> steal goal
    ;; - If Swiper has items -> hide goal
    ;; - Otherwise -> approach player goal
    (:goal
        (swiper_has crystal_key)
    )
)
