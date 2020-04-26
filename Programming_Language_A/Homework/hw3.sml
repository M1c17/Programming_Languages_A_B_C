(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1. *)

fun is_Upper s = Char.isUpper(String.sub(s, 0))
(*only_capitals = fn : string list -> string list*)
fun only_capitals(xs: string list) = 
    List.filter(is_Upper) xs

(* 2. *)

fun is_longest(x1, x2) =
    if String.size x1 > String.size x2
    then x1
    else x2
(*longest_string1 = fn : string list -> string*)
fun longest_string1(xs: string list) =
    foldl(is_longest) "" xs

(*OR*)

fun longest_string1 (xs)= 
    List.foldl(fn (x1, x2) => if String.size x1 > String.size x2
                          then x1
                          else x2) "" xs

(* 3. *)

fun is_longest(x1, x2) =
    if String.size x1 >= String.size x2
    then x1
    else x2

(*longest_string2 = fn : string list -> string*)
fun longest_string2(xs: string list) =
    List.foldl(is_longest) "" xs

(*OR*)
fun longest_string2 (xs)= 
    List.foldl(fn (x1, x2) => if String.size x1 >= String.size x2
                          then x1
                          else x2) "" xs

(* 4. *)

(*(int * int -> bool) -> string list -> string*)
fun longest_string_helper (f)=
    List.foldl(fn (x1, x2) => if f (String.size x1, String.size x2)
                          then x1
                          else x2) ""
(*string list -> string*)
val longest_string3 = 
    longest_string_helper (fn (x, y) => x > y)

(*string list -> string*)
val longest_string4 =
    longest_string_helper (fn (x, y) => x >= y)

(* 5. *)
(*string list -> string*)
val longest_capitalized = longest_string1 o only_capitals

(* 6. *)
(*string -> string*)
fun rev_string(s) = String.implode o rev o String.explode

(* OR *)

fun helper [] = []
| helper [x] = [x]
| helper (l::ls) = (helper ls) @ [l];

fun rev_string1(s) = implode (helper (explode s))

(* 7. *)

(* (’a -> ’b option) -> ’a list -> ’b *)

(*(’a -> ’b option) -> ’a list -> ’b*)
fun first_answer f list = 
    case list of 
        [] => raise NoAnswer
        |x::xs => case f x of
                    NONE =>  first_answer f xs
                    |SOME v => v

(* 8. *)

(*(’a -> ’b list option) -> ’a list -> ’b list option*)
fun all_answers f list = 
    let
        fun helper list acc =
            case list of
                [] => SOME acc
                |x::xs => case f x of
                            NONE => NONE
                            |SOME i => helper xs (i @ acc)
    in
        helper list []
    end

(********************************************)

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

(*(unit -> int) -> (string -> int) -> pattern -> int*)
fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(*9.(a)*)
(*pattern -> int*)
fun count_wildcards(p) = 
    g(fn () => 1)(fn x => 0)p

(*9.(b)*)
(*pattern -> int*)
fun count_wild_and_variable_lengths (p) =
    g(fn () => 1) String.size p

(*9.(c)*)
(*string * pattern -> int*)
fun count_some_var(s, p) =
    g(fn () => 0)(fn x => if x = s
                          then 1
                          else 0)p

(*10.*)
(*pattern -> bool*)
fun check_pat(p) =
    let
        fun all_variables(p, acc) =
            case p of
                Wildcard => acc
                | Variable s => s::acc
                | TupleP ps => List.foldl all_variables acc ps
                | ConstructorP(_,p) => all_variables(p, acc)
                | _ => acc

        fun is_repeat xs =
            case xs of
                [] => false
                |s::xs => (List.exists(fn x => x = s)) xs orelse is_repeat xs
    in
        not(is_repeat(all_variables(p, [])))
    end

(*11.*)
(*valu * pattern -> (string * valu) list option*)

(*rules => what patterns match what values, and what bindings they produce *)

fun match(v, p) =
    case (p, v) of
        (Wildcard, _) => SOME []
        | (Variable s, v) => SOME [(s, v)]
        | (UnitP, Unit) => SOME []
        | (ConstP(p), Const(v)) => if p = v then (SOME []) else NONE
        | (TupleP(ps), Tuple(vs)) => if List.length(ps) = List.length(vs) 
                                     then all_answers match(ListPair.zip(vs, ps))
                                     else NONE 
        | (ConstructorP(s1, p), Constructor(s2, v)) => if s1 = s2
                                                       then match(v, p)
                                                       else NONE
        | (_, _) => NONE

(*12.*)
(*(string * valu) list option*)
fun first_match(v, lst_patterns) =
    SOME (first_answer(fn p => match (v, p)) lst_patterns)
    handle NoAnswer => NONE












