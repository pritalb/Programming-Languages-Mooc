(* Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.) *)
(* val is_older = fn : (int * int * int) * (int * int * int) -> bool *)
fun is_older(date1: int*int*int, date2: int*int*int) =
	let
		val date_1_in_days = #1 date1 * 365 + #2 date1 * 31 + #3 date1
		val date_2_in_days = #1 date2 * 365 + #2 date2 * 31 + #3 date2
	in
	  	if date_1_in_days < date_2_in_days
		then
			true
		else 
			false
	end

(* Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month. *)
(* val number_in_month = fn : (int * int * int) list * int -> int *)

(* some helper functions *)
fun int_in_list(num: int, num_list: int list) =
	if num_list = []
	then
		false
	else
		hd num_list = num orelse int_in_list(num, tl num_list)

fun bool_to_int(bl: bool) =
	if bl
	then
		1
	else
		0

fun number_in_month(dates: (int*int*int) list, month_num: int) =
	let
		val counter = 0
	in
		if dates = []
		then
			counter
		else
			let 
				val current_date = hd dates
				val current_month = #2 current_date
			in
				counter + bool_to_int(current_month = month_num) + number_in_month(tl dates, month_num)
			end
	end

(* Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated *)
(* val number_in_months = fn : (int * int * int) list * int list -> int *)
fun number_in_months(dates: (int*int*int) list, month_nums: int list) =
	let
		val counter = 0
	in
		if dates = []
		then
			counter
		else
			let 
				val current_date = hd dates
				val current_month = #2 current_date
				val date_in_current_month = int_in_list(current_month, month_nums)
			in
				counter + bool_to_int(date_in_current_month) + number_in_months(tl dates, month_nums)
			end
	end

(* Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)
(* val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list *)
fun dates_in_month(dates: (int*int*int) list, month_num: int) =
	if dates = []
	then
		[]
	else
		let 
			val current_date = hd dates
			val current_month = #2 current_date
			val ans = if month_num = current_month then [current_date] else []
		in
			ans @ dates_in_month(tl dates, month_num)
		end

(* Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. *)
(* val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list *)
fun dates_in_months(dates: (int*int*int) list, month_nums: int list) =
	if dates = []
	then
		[]
	else
		let 
			val current_date = hd dates
			val current_month = #2 current_date
			val date_is_in_months = int_in_list(current_month, month_nums)
			val ans = if date_is_in_months then [current_date] else []
		in
			ans @ dates_in_months(tl dates, month_nums)
		end

(* Write a function get_nth that takes a list of strings and an int n and returns the nth element of the
list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay. *)
(* val get_nth = fn : string list * int -> string *)
fun get_nth(s_list: string list, index: int) =
	if index = 1
	then
		hd s_list
	else
		get_nth(tl s_list, index - 1);


(* Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December *)
(* val date_to_string = fn : int * int * int -> string *)
fun month_name(month_num: int) = 
	let
		val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in
		get_nth(months, month_num)
	end

fun date_to_string(year: int, month: int, date: int) =
	let
		val month_n = month_name(month)
	in
		month_n ^ " " ^ Int.toString(date) ^ ", " ^ Int.toString(year) 
	end

(* Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case. *)
(* val number_before_reaching_sum = fn : int * int list -> int *)
fun nbrs_helper(sum: int, int_list: int list, index_counter: int, current_sum: int) =
	let
		val new_sum = current_sum + hd int_list
	in
		if new_sum = sum orelse new_sum > sum
		then
			index_counter
		else
			nbrs_helper(sum, tl int_list, index_counter + 1, new_sum)
	end

fun number_before_reaching_sum(sum: int, int_list: int list) =
	nbrs_helper(sum, int_list, 0, 0)


(* Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem. *)
(* val what_month = fn : int -> int *)
fun smallest_element_greater_than(int_list: int list, n: int, index_counter: int) =
	if n <= hd int_list
	then
		index_counter
	else
		smallest_element_greater_than(tl int_list, n, index_counter + 1)

fun what_month(day: int) =
	let
		val days_upto_a_month = [30, 58, 89, 119, 150, 180, 211, 242, 272, 303, 333, 365]
	in
		smallest_element_greater_than(days_upto_a_month, day, 1)
	end

(* Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2 *)
(* val month_range = fn : int * int -> int list *)


(* Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list *)
(* val oldest = fn : (int * int * int) list -> (int * int * int) option *)