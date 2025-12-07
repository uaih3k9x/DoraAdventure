;; ============================================================
;; Dora's Tropical Adventure - Adversary (Swiper) Domain
;; ============================================================
;; This PDDL domain defines Swiper the Fox's behavior and capabilities
;; as an intelligent adversary in the game world.
;;
;; Swiper can:
;; - Move between connected locations
;; - Sneak to locations with escape routes
;; - Pick up valuable items from the ground
;; - Steal items from the player's backpack
;; - Hide stolen items in climbable/swimmable locations
;; - Flee when confronted by the player
;; ============================================================

(define (domain swiper_behavior)
    (:requirements :strips :typing :negative-preconditions)

    (:types
        location item
    )

    (:predicates
        ;; Location-related predicates
        (swiper_at ?l - location)           ; Swiper's current location
        (player_at ?l - location)           ; Player's current location
        (connected ?from - location ?to - location)  ; Two locations are connected

        ;; Item-related predicates
        (item_at ?i - item ?l - location)   ; Item is on the ground at location
        (player_has ?i - item)              ; Player has item in backpack
        (swiper_has ?i - item)              ; Swiper is carrying item
        (hidden_item ?i - item ?l - location)  ; Item is hidden at location
        (valuable ?i - item)                ; Item is valuable (worth stealing)

        ;; Terrain feature predicates
        (climbable ?l - location)           ; Location has trees/structures to climb
        (swimmable ?l - location)           ; Location has water to swim in
        (has_escape_route ?l - location)    ; Location has multiple exits
        (safe_zone ?l - location)           ; Location where Swiper won't go

        ;; Swiper status predicates
        (swiper_blocked)                    ; Swiper is blocked by player
        (swiper_confused)                   ; Swiper is confused (player said "Swiper no swiping!")
    )

    ;; ============================================================
    ;; Action 1: Normal Movement
    ;; Swiper moves from one location to an adjacent location
    ;; ============================================================
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

    ;; ============================================================
    ;; Action 2: Sneaky Movement
    ;; Swiper sneaks to a location with an escape route (safer)
    ;; ============================================================
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

    ;; ============================================================
    ;; Action 3: Take Item from Ground
    ;; Swiper picks up a valuable item from the current location
    ;; ============================================================
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

    ;; ============================================================
    ;; Action 4: Steal from Player
    ;; Swiper steals a valuable item from player's backpack
    ;; Requires Swiper and player to be at the same location
    ;; ============================================================
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

    ;; ============================================================
    ;; Action 5: Hide Item by Climbing
    ;; Swiper hides an item in the trees at a climbable location
    ;; ============================================================
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

    ;; ============================================================
    ;; Action 6: Hide Item by Swimming
    ;; Swiper hides an item underwater at a swimmable location
    ;; ============================================================
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

    ;; ============================================================
    ;; Action 7: Flee from Player
    ;; Swiper escapes to a location with an escape route
    ;; Triggered when player is at the same location
    ;; ============================================================
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
