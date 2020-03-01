(*
    Programming Languages (Coursera / University of Washington)
    
    Assignment 1
*)

(* Author: Xin Lyu *)

(* Q1 - is_older *)
(* 
    Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
    the first argument is a date that comes before the second argument. (If the two dates are the same,
    the result is false.
*)
fun is_older (date_x: int * int * int, date_y: int * int * int): bool = 
    if (#1 date_x < #1 date_y) orelse (#1 date_x = #1 date_y andalso #2 date_x < #2 date_y) orelse (#1 date_x = #2 date_y andalso #2 date_x = #2 date_y andalso #3 date_x < #3 date_y)
    then true
    else false


(* Q2 - number_in_month *)
(*
    Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
    how many dates in the list are in the given month.
*)
fun number_in_month (date_list: (int * int * int) list, month: int): int = 
    if null date_list
    then 0
    else 
        if (#2 (hd date_list)) = month
        then 1 + number_in_month(tl date_list, month)
        else number_in_month(tl date_list, month)


(* Q3 - number_in_months*)
(*
    Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
    and returns the number of dates in the list of dates that are in any of the months in the list of months.
    Assume the list of months has no number repeated. Hint: Use your answer to the previous problem.
*)
fun number_in_months (date_list: (int * int * int) list, months: int list): int = 
    if null date_list
    then 0
    else 
        if null months
        then 0
        else number_in_month(date_list, hd months) + number_in_months(date_list, tl months)
            

(* Q4 - dates_in_month *)
(*
    Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
    list holding the dates from the argument list of dates that are in the month. The returned list should
    contain dates in the order they were originally given.
*)
fun dates_in_month (date_list: (int * int * int) list, month: int): (int * int * int) list =
    if null date_list
    then []
    else 
        if (month < 1) 
        then []
        else if (month > 12)
        then []
        else
            if (#2 (hd date_list)) = month
            then (hd date_list)::dates_in_month(tl date_list, month)
            else dates_in_month(tl date_list, month)
        
        
(* Q5 - dates_in_months *)
(*
    Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
    and returns a list holding the dates from the argument list of dates that are in any of the months in
    the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
    previous problem and SML’s list-append operator (@)
*)
fun dates_in_months (date_list: (int * int * int) list, months: int list): (int * int * int) list = 
    if null date_list
    then []
    else 
        if null months
        then []
        else 
            let
                val dates = dates_in_months(date_list, tl months)
            in
                dates_in_month(date_list, hd months)@dates
            end
        

(* Q6 - get_nth *)
(*
    Write a function get_nth that takes a list of strings and an int n and returns the nth element of the
    list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
    your function may apply hd or tl to the empty list in this case, which is okay.
*)
fun get_nth (words: string list, index: int): string = 
    if null words
    then ""
    else 
        if index <= 0
        then ""
        else 
            if index = 1
            then hd words
            else get_nth(tl words, index-1)
            

(* Q7 - date_to_string *)
(*
    Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
    (for example). Use the operator ^ for concatenating strings and the library function Int.toString
    for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
    Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
    comma following the day and use capitalized English month names: January, February, March, April,
    May, June, July, August, September, October, November, December.
*)
fun date_to_string (year: int, month: int, day: int): string = 
    let
        val months_name = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months_name, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
    end


(* Q8 - number_before_reaching_sum *)
(*
    Write a function number_before_reaching_sum that takes an int called sum, which you can assume
    is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
    You should return an int n such that the first n elements of the list add to less than sum, but the first
    n+1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
    value; it is okay for an exception to occur if this is not the case.
*)
fun number_before_reaching_sum (sum: int, numbers: int list): int = 
    let
        fun get_nth_number_before_reaching_sum (sum: int, numbers: int list, current_step: int): int = 
            if current_step < 1
            then 0
            else
                if (hd numbers) < sum
                then get_nth_number_before_reaching_sum(sum - (hd numbers), tl numbers, current_step + 1)
                else current_step - 1
    in
        get_nth_number_before_reaching_sum(sum, numbers, 1)
    end


(* Q9 - what_month *)
(*
    Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
    what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
    answer to the previous problem.
*)
fun what_month (day: int): int = 
    let 
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, days_in_months) + 1
    end


(* Q10 - month_range *)
(*
    Write a function month_range that takes two days of the year day1 and day2 and returns an int list
    [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
    of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.
*)
fun month_range (day1: int, day2: int): int list = 
    if (day2 < day1)
    then []
    else 
        let
            val month1 = what_month(day1)
            val month2 = what_month(day2) 
            val length = day2 - day1 + 1
            
            fun construct_answer_list (begin_month: int, end_month: int, length: int): int list = 
                if (begin_month < end_month)
                then [begin_month]@construct_answer_list(begin_month+1, end_month, length-1)
                else 
                    if (length <> 0)
                    then [end_month]@construct_answer_list(begin_month, end_month, length-1)
                    else []
        in
            construct_answer_list(month1, month2, length)
        end


(* Q11 - oldest *)
(*
    Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
    evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.
*)
fun oldest (date_list: (int * int * int) list): (int * int * int) option = 
    if null date_list
    then NONE
    else
        let
            fun get_the_oldest_one(current_oldest: (int * int * int), date_list: (int * int * int) list): (int * int * int) =
                if null date_list
                then current_oldest
                else
                    if (is_older(current_oldest, hd date_list))
                    then get_the_oldest_one(current_oldest, tl date_list)
                    else get_the_oldest_one(hd date_list, tl date_list)
        in
            SOME (get_the_oldest_one(hd date_list, tl date_list))
        end


(* Q12 - number_in_months_challenge / dates_in_months_challenge*)
(*
    Challenge Problem: Write functions number_in_months_challenge and dates_in_months_challenge
    that are like your solutions to problems 3 and 5 except having a month in the second argument multiple
    times has no more effect than having it once. (Hint: Remove duplicates, then use previous work.)
*)
fun remove_duplicates_for_int_list (numbers: int list, records: int list): int list = 
    let
        fun check_exists(numbers: int list, num: int): bool = 
            if null numbers
            then false
            else
                if ((hd numbers) = num)
                then true
                else check_exists(tl numbers, num)
    in
        if null numbers
        then records
        else
            if (check_exists(records, hd numbers))
            then remove_duplicates_for_int_list(tl numbers, records)
            else remove_duplicates_for_int_list(tl numbers, records@[hd numbers])
    end

fun number_in_months_challenge (date_list: (int * int * int) list, months: int list): int = 
    let 
        val updated_months = remove_duplicates_for_int_list(months, [])
    in
        number_in_months(date_list, updated_months)
    end

fun dates_in_months_challenge (date_list: (int * int * int) list, months: int list): (int * int * int) list = 
    let
        val updated_months = remove_duplicates_for_int_list(months, [])
    in
        dates_in_months(date_list, updated_months)
    end
