(********Extra_Practice_Problems************)


(*1*)
(*intlist->int*)

fun alternate(numbers_list) =
    if null numbers_list
    then 0
    else if null (tl numbers_list)
    then hd(numbers_list)
    else hd(numbers_list) - hd(tl numbers_list) + alternate(tl(tl numbers_list))

(*2*)
(*intlist->int*int*)
fun max(list_numbers) =
    if null list_numbers
    then 0
    else if null list_numbers
    then hd list_numbers
    else
        let
            val max_num = max(tl list_numbers)
        in
            if hd list_numbers > max_num
            then hd list_numbers
            else max_num
        end

fun min(list_numbers) =
    if null list_numbers
    then 0
    else if null (tl list_numbers)
    then hd list_numbers
    else
        let
            val min_num = min(tl list_numbers) 
        in
            if hd list_numbers < min_num
            then hd list_numbers
            else min_num
        end

fun min_max(list_numbers) =
    ((min(list_numbers)), (max(list_numbers)))

(*OR*)

fun max1 xs =
        foldl(fn (x, y) => if x > y then x else y) 0 xs
fun min1 xs =
        foldl(fn (x, y) => if x < y then x else y) 0 xs

fun min_max1(xs) =
    ((min1(xs)), (max1(xs)))

(*3.*)
(*intlist->intlist*)
fun cumsum(list_nums) =
    let
        fun helper(xs, sum) =
            if null xs
            then []
            else
            sum + hd(xs)::helper((tl xs), sum + hd(xs))
    in
        helper(list_nums, 0)
    end

(*4.*)
(*stringoption->string*)
fun greeting(name) =
    if isSome name
    then "Hello there, "^valOf name^"!"
    else "Hello there, you!"


(*5*)
(*intlist*intlist->intlist*)

fun repeat(xs, ys) =
    let 
        fun helper (x, y) =
        if y = 0
        then []
        else x::helper(x, y - 1)
    in
        if null xs
        then []
        else helper(hd(xs), hd(ys)) @ repeat(tl(xs), tl(ys))
    end

(*6*)
(*intoption*intoption->intoption*)

fun addOpt(x, y) =
    if isSome x andalso isSome y 
    then SOME(valOf x + valOf y)
    else NONE

(*7*)
(*intoptionlist->intoption*)

fun addAllOpt(xs) =
    if null xs
    then NONE
    else
        let
            val tl_addAllOpt = addAllOpt(tl xs)
        in  
            if isSome(hd xs) andalso isSome(tl_addAllOpt)
            then addOpt(hd xs, tl_addAllOpt)
            else if isSome (hd xs)
            then hd xs
            else if isSome (tl_addAllOpt)
            then tl_addAllOpt
            else NONE
        end

(*8*)
(*boollist->bool*)

fun any(booleans) =
    if null booleans
    then false
    else
        hd(booleans) orelse any(tl booleans)

(*9*)
(*boollist->bool*)

fun all(booleans) =
    if null booleans
    then true 
    else hd(booleans) andalso all(tl booleans)
    
(*10*)
(*intlist*intlist->int*int list*)

fun zip(xs, ys) =
    if null xs orelse null ys
    then []
    else
        (hd xs, hd ys)::zip(tl xs, tl ys)
     
(*11*)

fun zipRecycle(xs, ys) =
    let
        fun helper(xs, ys, other_xs, other_ys) =
            if null xs andalso not (null other_xs)
            then helper(other_xs, ys, other_xs, [])
            else if null ys andalso not (null other_ys)
            then helper(xs, other_ys, [], other_ys)
            else if null xs orelse null ys
            then []
            else (hd xs, hd ys)::helper(tl xs, tl ys, other_xs, other_ys)
    in 
        helper(xs, ys, xs, ys)
    end

(*12*)
(*(int*int)listoption*)

fun zipOpt(xs, ys) =
    let
        fun lenght(lst) =
            if null lst
            then 0
            else 1 + lenght(tl lst)
    in
        if lenght(xs) = lenght(ys)
        then SOME(zip(xs, ys))
        else NONE
    end

(*13*)
(*(string*int)list*string->intoption*)

fun lookup(lst_pairs: (int * string) list, s: string) =  
    if null lst_pairs
    then NONE
    else if #2(hd lst_pairs) = s
    then SOME (#1(hd lst_pairs))
    else lookup(tl(lst_pairs), s)

(*14*)
(*intlist->intlist*intlist*)

fun splitup(lst) =
    let
        fun filter(lst, n) =
            if null lst
            then []
            else 
                if (hd lst < 0 andalso n) orelse (hd lst >= 0 andalso not n)
                then hd lst::filter(tl lst, n)
            else filter(tl lst, n)
    in
        (filter(lst, false), filter(lst, true))     
    end

(*15*)
(*intlist*int->intlist*intlist*)

fun splitAt(lst, threshold) =
    let 
        fun filter(lst, n) =
            if null lst
            then []
            else
                if (hd lst < threshold andalso n) orelse (hd lst >= threshold andalso not n)
                then hd lst::filter(tl lst, n)
            else filter(tl lst, n)
    in
        (filter(lst, false), filter(lst, true))
    end
        
(*16*)
(*intlist->boolean*)

fun isSorted(lst) =
    if null lst orelse null (tl lst)
    then true
    else
        if hd(lst) > hd(tl lst)
        then false
    else isSorted(tl lst)

(*OR*)

fun isSorted1(lst) =
    if null lst orelse null (tl lst)
    then true
    else hd(lst) <= hd(tl lst) andalso isSorted(tl lst)

(*17*)
(*intlist->boolean*)

fun isAnySorted(lst) =
    if null lst orelse null (tl lst)
    then true
    else isSorted(lst) orelse (hd(lst) > hd(tl lst)) andalso isAnySorted(tl lst)

(* use "Extra_Practice_Problems.sml"; *)

(*18*)
(*intlist*intlist->intlist*)

fun sortedMerge(xs, ys) = 
    if null xs
    then ys
    else if null ys
    then xs
    else if hd(xs) > hd(ys)
    then hd(ys)::sortedMerge(xs, tl ys)
    else hd(xs)::sortedMerge(tl xs, ys)

(*19*)
(*intlist->intlist*)

fun qsort(lst) =
    if null lst
    then []
    else if null (tl lst)
    then [hd lst]
    else
        let
            val smaller = qsort(#2(splitAt(tl lst, hd lst)))
            val bigger = qsort(#1(splitAt(tl lst, hd lst)))
        in
            smaller @ [hd lst] @ bigger
        end

(*20*)
(*intlist->intlist*intlist*)

fun divide(xs) =
    let
        fun helper(xs, i, is_odd) =
            if null xs
            then []
            else if is_odd andalso (i mod 2) <> 0
            then hd xs::helper(tl xs, i + 1, is_odd)
            else if not is_odd andalso (i mod 2) = 0
            then hd xs::helper(tl xs, i + 1, is_odd)
            else helper(tl xs, i + 1, is_odd)
    in
        (helper(xs, 1, true), helper(xs, 1, false))
    end

(*OR*)

fun divide1(xs) =
    if null xs
    then ([], [])
    else if null (tl xs)
    then (hd xs::[], [])
    else
        let val (ys, zs) = divide (tl(tl xs))
        in
            (hd xs::ys, hd(tl xs)::zs)
        end

(*21*)
(*intlist->intlist*)

fun not_so_quick_sort(xs) =
    if null xs
    then []
    else if null(tl xs)
    then [hd xs]
    else
        let
            val ys = #1 (divide xs)
            val zs = #2 (divide xs)
        in
           sortedMerge(not_so_quick_sort(ys), not_so_quick_sort(zs))
        end


    
(*use "Extra_Practice_Problems.sml";*)


