(* Regular Problems *)

fun is_older(date1: int * int * int, date2: int * int * int) =
  let
    val year1 = #1 date1 val month1 = #2 date1 val day1 = #3 date1
    val year2 = #1 date2 val month2 = #2 date2 val day2 = #3 date2
  in
    if year1 <> year2 then year1 < year2
    else
      if month1 <> month2 then month1 < month2
      else day1 < day2
  end

fun number_in_month(dates: (int * int * int) list, month: int) =
  if null dates then 0
  else (if #2 (hd dates) = month then 1 else 0) +
       number_in_month(tl dates, month)

fun number_in_months(dates: (int * int * int) list, months: int list) =
  if null months then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int * int * int) list, month: int) =
  if null dates then []
  else
    let val tailDatesInMonth = dates_in_month(tl dates, month)
    in
      if #2 (hd dates) = month then hd dates :: tailDatesInMonth
      else tailDatesInMonth
    end

fun dates_in_months(dates: (int * int * int) list, months: int list) =
  if null months then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings: string list, n: int) =
  if n = 1 then hd strings
  else get_nth(tl strings, n - 1)

fun date_to_string(date: int * int * int) =
  let
    val year = #1 date
    val month = #2 date
    val day = #3 date
    val months = ["January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November",
                  "December"]
  in
    get_nth(months, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
  end

fun number_before_reaching_sum(sum: int, numbers: int list) =
  if sum <= hd numbers then 0
  else 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)

fun what_month(dayOfYear: int) =
  let val daysInMonths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in 1 + number_before_reaching_sum(dayOfYear, daysInMonths)
  end

fun month_range(day1: int, day2: int) =
  if day1 > day2 then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: (int * int * int) list) =
  if null dates then NONE
  else
    let val tailOldest = oldest(tl dates)
    in
      if isSome tailOldest andalso is_older(valOf tailOldest, hd dates)
      then tailOldest
      else SOME (hd dates)
    end

(* Challenge #1 *)

fun month_in_months(month: int, months: int list) =
  not (null months) andalso (month = hd months orelse
                             month_in_months(month, tl months))

fun deduplicate_months(months: int list) =
  if null months then []
  else
    let val tailDeduplicatedMonths = deduplicate_months(tl months)
    in
      if month_in_months(hd months, tailDeduplicatedMonths)
      then tailDeduplicatedMonths
      else hd months :: tailDeduplicatedMonths
    end

fun number_in_months_challenge(dates: (int * int * int) list,
                               months: int list) =
  number_in_months(dates, deduplicate_months(months))

fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) =
  dates_in_months(dates, deduplicate_months(months))

(* Challenge #2 *)

fun reasonable_date(date: int * int * int) =
  let
    val year = #1 date val month = #2 date val day = #3 date
    val isLeapYear =
      year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
    val daysInFeb = if isLeapYear then 29 else 28
    val daysInMonths = [31, daysInFeb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    fun days_in_month(month: int, daysInMonths: int list) =
      if month = 1 then hd daysInMonths
      else days_in_month(month - 1, tl daysInMonths)
  in
    year > 0 andalso
    (month >= 1 andalso month <= 12) andalso
    (day >= 1 andalso day <= days_in_month(month, daysInMonths))
  end

