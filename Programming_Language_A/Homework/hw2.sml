(*Your solutions must use pattern-matching. You may not use the functions null, hd, tl, isSome, or valOf,
nor may you use anything containing a # character or features not used in class (such as mutation). Note
that list order does not matter unless specifically stated in the problem.*)

(*1*)
(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1. This problem involves using first-name substitutions to come up with alternate names. For example,
Fredrick William Smith could also be Fred William Smith or Freddie William Smith. Only part (d) is
specifically about this, but the other problems are helpful.*)

(*(a)*)

fun all_except_option (x, xs) = 
    let 
        fun remove_i_from_list (x_to_remove, ys) =
            case ys of
                [] => []
                | x::ys => if same_string(x, x_to_remove)
                            then ys
                            else x::remove_i_from_list(x_to_remove, ys)
        val removed_list = remove_i_from_list(x ,xs)
    in
        if removed_list = xs
        then NONE
        else SOME removed_list
    end

(* (b) *)

fun get_substitutions1 (list_substitutions, name) =
    case list_substitutions of
        [] => []
        |x::ys => case all_except_option(name, x) of 
                     NONE => get_substitutions1(ys, name)
                    |SOME y => y @ get_substitutions1(ys, name)

(* (c) Write a function get_substitutions2, which is like 
get_substitutions1 except it uses a tail-recursive
local helper function. *)

fun get_substitutions2 (list_substitutions, name) =
    let 
        fun helper (list_substitutions, acc) = 
            case list_substitutions of 
                 [] => acc
                |y::ys => case all_except_option(name, y) of
                                NONE => helper(ys,acc)
                                |SOME y => helper(ys, y @ acc)
    in 
        helper(list_substitutions, [])
    end

(* (d) *)

(* names_subst ->  string list list 
   full_name -> record => {first:string,middle:string,last:string} *)
fun similar_names(names_subst, full_names) = 
    let
        val {first=first_name, middle=middle_name, last=last_name} = full_names
        val same_first_name = get_substitutions2 (names_subst, first_name)

        fun helper(first_names) =
            case first_names of
            [] => []
            |y::ys => {first=y, middle=middle_name, last=last_name} :: helper(ys)
    in
        full_names :: helper(same_first_name)
    end

(*2*)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 2. This problem involves a solitaire card game invented just for this question. You will write a program that
tracks the progress of a game.
A game is played with a card-list and a goal.*)

(*(a)*)

fun card_color(suit, rank) =
    case suit of
        Spades => Black
        |Clubs => Black
        |Diamonds => Red
        |Hearts => Red

(*(b)*)

 fun card_value(suit, rank) =
    case rank of
        Jack => 10
        |Queen => 10
        |King  => 10
        |Ace   => 11
        |Num n => n

(*(c)*)

fun remove_card(list_cards, card, e) =
    case list_cards of 
        [] => raise e
        |c::cs => if card = c
                  then cs
                  else c::remove_card(cs, card, e)
        
(*(d)*)

fun all_same_color(list_cards) =
    case list_cards of
        [] => true
        |_::[]=> true
        |c1::c2::tl => card_color(c1) = card_color(c2) andalso all_same_color(c2::tl)

(*(e)*)

fun sum_cards(list_cards) = 
    let
        fun helper(list_cards, acc) =
            case list_cards of
                [] => acc
                |c::cs => helper(cs, card_value(c) + acc)
    in
        helper(list_cards, 0)
    end

(*(f)*)

fun score(held_cards, goal) =
    let
        val sum = sum_cards(held_cards)
        fun prel_score(sum, goal) =
            if sum > goal
            then 3 * (sum - goal)
            else (goal - sum)
        val pr_score = prel_score(sum, goal)
        val same_color = all_same_color(held_cards)
    in
        if same_color
        then pr_score div 2
        else pr_score
    end

(*(g) *)

fun cards_game(card_list, move_list, goal) = 
    let
        fun helper(card_list, moves_list, held_cards) =
            case moves_list of
                [] => score(held_cards, goal)
                |(Discard c)::tl => helper(card_list, tl, remove_card(card_list, c, IllegalMove)) 
                |Draw::tl => case card_list of
                                [] => score(held_cards, goal) 
                                |c::cs => if sum_cards(c::held_cards) > goal
                                          then score(c::held_cards, goal)
                                          else helper(cs, tl, c::held_cards)
    in
        helper(card_list, move_list, [])
    end

(* 3. Challenge Problem *)
(*a*)

fun replace_aces(list_card) =
    case list_card of
    [] => []
    |(suit, Ace)::cs => (suit, Num 1)::replace_aces(cs)
    |card::cs => card::replace_aces(cs)

fun score_challenge(held_cards, goal) =
    let
        val replace_ace_cards = replace_aces(held_cards)
        val original_score = score(held_cards, goal)
        val other_score = score(replace_ace_cards, goal)
    in
        Int.min(original_score, other_score)
    end

fun officiate_challenge(card_list, move_list, goal) = 
    let
        val replace_ace_cards = replace_aces(card_list)
        val original_game = cards_game(card_list, move_list, goal)
        val other_game = cards_game(replace_ace_cards, move_list, goal)
    in
        Int.min(original_game, other_game)
    end

(*b*)

fun careful_player(card_list, goal) = 
    let
        fun helper(card, held_cards, visited_cards) =
            case held_cards of
            [] => NONE
            |c::cs => if score(visited_cards @ (card::cs), goal) = 0
                      then SOME card
                      else helper(c, cs, c::visited_cards)

        fun make_moves(card_list, held_cards) =
            if score(held_cards, goal) = 0
            then []
            else
                let
                    val sum = sum_cards(held_cards)
                in  
                    case card_list of
                        [] => []
                        |c::cs => case helper(c, held_cards, []) of
                                    SOME(c) => Discard(c)::Draw::[]
                                    |NONE => if sum < (goal - 10)
                                            then Draw::make_moves(cs,c::held_cards)
                                            else []
                end
    in
        make_moves(card_list, [])
    end




    
    




        

