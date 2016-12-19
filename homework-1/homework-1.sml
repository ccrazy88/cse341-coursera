(* Regular Problems *)

fun is_older ((year1, month1, day1), (year2, month2, day2)) =
  if year1 <> year2 then year1 < year2
  else if month1 <> month2 then month1 < month2
  else day1 < day2

fun number_in_month ([], _) = 0
  | number_in_month ((_, month, _) :: dates', givenMonth) =
      (if month = givenMonth then 1 else 0) +
      number_in_month (dates', givenMonth)

fun number_in_months (_, []) = 0
  | number_in_months (dates, month :: months') =
      number_in_month (dates, month) + number_in_months (dates, months')

fun dates_in_month ([], _) = []
  | dates_in_month ((year, month, day) :: dates', givenMonth) =
      let val datesInMonth' = dates_in_month (dates', givenMonth)
      in
        if month = givenMonth
          then (year, month, day) :: datesInMonth'
        else datesInMonth'
      end

fun dates_in_months (_, []) = []
  | dates_in_months (dates, month :: months') =
      dates_in_month (dates, month) @ dates_in_months (dates, months')

(* The original homework assignment accepted a nonexhaustive function. *)
fun get_nth (element :: _, 1) = element
  | get_nth (_ :: elements', n) = get_nth (elements', n - 1)

fun date_to_string (year, month, day) =
  let val months = ["January", "February", "March", "April", "May", "June",
                    "July", "August", "September", "October", "November",
                    "December"]
  in get_nth (months, month) ^ " " ^ Int.toString day ^ ", " ^ Int.toString year
  end

(* The original homework assignment accepted a nonexhaustive function. *)
fun number_before_reaching_sum (sum, number :: numbers') =
  if sum <= number then 0
  else 1 + number_before_reaching_sum (sum - number, numbers')

fun what_month dayOfYear =
  let val daysInMonths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in 1 + number_before_reaching_sum (dayOfYear, daysInMonths)
  end

fun month_range (day1, day2) =
  if day1 > day2 then []
  else what_month day1 :: month_range (day1 + 1, day2)

fun oldest [] = NONE
  | oldest (date :: dates') =
      let val oldest' = oldest dates'
      in
        case oldest' of
            NONE => SOME date
          | SOME someOldest' =>
              if is_older (someOldest', date) then oldest' else SOME date
    end

(* Challenge #1 *)

fun month_in_months (_, []) = false
  | month_in_months (givenMonth, month :: months') =
      givenMonth = month orelse month_in_months(givenMonth, months')

fun deduplicate_months [] = []
  | deduplicate_months (month :: months') =
    let val deduplicatedMonths' = deduplicate_months months'
    in
      if month_in_months (month, deduplicatedMonths')
        then deduplicatedMonths'
      else month :: deduplicatedMonths'
    end

fun number_in_months_challenge (dates, months) =
  number_in_months (dates, deduplicate_months months)

fun dates_in_months_challenge (dates, months) =
  dates_in_months (dates, deduplicate_months months)

(* Challenge #2 *)

fun reasonable_date (year, month, day) =
  let
    val isLeapYear =
      year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
    val daysInFeb = if isLeapYear then 29 else 28
    val daysInMonths = [31, daysInFeb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    (* The original homework assignment accepted a nonexhaustive function. *)
    fun days_in_month (1, daysInMonth :: _) = daysInMonth
      | days_in_month (month, _ :: daysInMonths') =
          days_in_month (month - 1, daysInMonths')
  in
    year > 0 andalso
    (month >= 1 andalso month <= 12) andalso
    (day >= 1 andalso day <= days_in_month (month, daysInMonths))
  end

