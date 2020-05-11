(* 1. Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.)*)
(* Date = Y/M/D *)

(* Helper functions *)

fun getYear(date: int * int * int) = #1 date
fun getMonth(date: int * int * int) = #2 date
fun getDay(date: int * int * int) = #3 date


fun is_older (date1 : int * int * int, date2: int * int * int) = 
    if getYear(date1) < getYear(date2) andalso getMonth(date1) < getMonth(date2) andalso getDay(date1) < getDay(date2)
    then true
    else false

fun is_older1(date1: int * int * int, date2: int * int * int) = 
    if getYear(date1) < getYear(date2) then true
    else if getYear(date1) > getYear(date2) then false
    else if getMonth(date1) < getMonth(date2) then true
    else if getMonth(date1) > getMonth(date2) then false
    else if getDay(date1) < getDay(date2) then true
    else false

(* 2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month. *)

fun number_in_month (dates: (int * int * int) list, month: int) = 
    if null dates
    then 0
    else
        let val tl_number_in_month = number_in_month (tl dates, month: int)
        in 
            if getMonth (hd dates) = month
            then 1 + tl_number_in_month
            else tl_number_in_month
        end

(* 3.  Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)

fun number_in_months (dates1: (int * int * int) list, months: int list) = 
    if null months
    then 0
    else
        number_in_month (dates1, hd months) + number_in_months (dates1, tl months)

(* 4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)

fun dates_in_month (dates2: (int * int * int) list, month1: int) =
    if null dates2
    then []
    else 
        let val tl_in_month = dates_in_month (tl dates2, month1: int)
        in
        if getMonth (hd dates2) = month1
        then (hd dates2) :: tl_in_month
        else tl_in_month
        end

(* 5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@). *)

fun dates_in_months (dates2: (int * int * int) list, months1: int list) = 
    if null months1
    then []
    else 
        dates_in_month(dates2, hd months1) @ dates_in_months(dates2, tl months1)

(* 6. Write a function get_nth that takes a list of strings and an int n and returns the n
th element of the list where the head of the list is 1st. Do not worry about the case 
where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay *)

fun get_nth (lstring: string list, number: int) = 
    if number = 1
    then hd lstring
    else get_nth(tl lstring, number - 1)

(* 7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December.*)

fun date_to_string (dates3: int * int * int) =
    let 
        val months_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October" ,"November", "December"]
        val number_month = getMonth (dates3)
    in 
        get_nth(months_list, number_month) ^ " " ^ Int.toString(getDay (dates3)) ^ ", " ^ Int.toString(getYear (dates3))
    end

(* 8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case. *)

fun number_before_reaching_sum (sum: int, lnumbers: int list) = 
    if hd lnumbers >= sum
    then 0
    else 1 + number_before_reaching_sum (sum - (hd lnumbers), tl lnumbers)

(* 9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem. *)

fun what_month (day_year: int) =
    let
        val days_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
        1 + number_before_reaching_sum (day_year, days_months)
    end

(* 10. Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2. *)

fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month (day1) :: month_range (day1 + 1, day2)

(* 11. Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.*)

fun oldest (dates4: (int * int * int) list) = 
    if null dates4
    then NONE
    else 
        let 
            fun oldest_nonempty (dates4: (int * int * int) list) = 
                if null (tl dates4)
                then hd dates4
                else
                    let 
                        val tl_oldest = oldest_nonempty (tl dates4)
                    in
                        if is_older1 (hd dates4, tl_oldest)
                        then hd dates4
                        else tl_oldest
                    end
        in 
            SOME (oldest_nonempty (dates4))
        end

(* 12. Write functions number_in_months_challenge and dates_in_months_challenge
that are like your solutions to problems 3 and 5 except having a month in 
the second argument multiple times has no more effect than having it once. 
(Hint: Remove duplicates, then use previous work. *)

fun sort_list(months : int list) =
  let
      fun check(month : int, months : int list) =
        if null months
        then month :: []
        else if month < hd months
        then month :: months
        else (hd months) :: check(month, tl months)
  in
      if null months
      then []
      else if null (tl months)
      then months
      else
          let
              val tl_sorted = sort_list(tl months)
          in
              if hd months < hd tl_sorted
              then months
              else check(hd months, tl_sorted)
          end
  end

fun remove_duplicates1(months: int list) = 
    let
        fun check(months: int list, month: int) =
            if null months
            then false
            else 
                if hd months = month
                then true
                else check (tl months, month)
        fun go_through_list(months: int list) =
            if null months
            then []
            else 
                if check(tl months, hd months)
                then go_through_list(tl months)
                else hd months :: go_through_list(tl months)
    in
        go_through_list(sort_list(months))
    end

(* 13. Challenge Problem: Write a function reasonable_date that takes a date and determines if it
describes a real date in the common era. A “real date” has a positive year (year 0 did not exist), a
month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap
years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.
(Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.) *)

fun reasonable_date (date5: int * int * int) =
    let
        fun is_leap_year(year: int) = 
            year mod 400 = 0 andalso year mod 4 = 0 andalso year mod 100 <> 0
        
        val normal_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val leap_months = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

        fun get_nth (days: int list, number: int) = 
            if number = 1
            then hd days
            else get_nth(tl days, number - 1)
    in
        if getYear(date5) < 1 orelse getMonth(date5) < 1 orelse getMonth(date5) > 12 
            orelse getDay(date5) < 1 orelse getDay(date5) > 31
        then false
        else
            if is_leap_year(getYear(date5))
            then getDay(date5) <= get_nth(normal_months, getMonth(date5))
            else getDay(date5) <= get_nth(leap_months, getMonth(date5))
    end










