(* This code is provided for homework #3. *)

exception NoAnswer

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

datatype typ = Anything
             | UnitT
             | IntT
             | TupleT of typ list
             | Datatype of string

(* End of provided code. *)

val only_capitals = List.filter (fn s => Char.isUpper (String.sub (s, 0)))

val longest_string1 =
  List.foldl (fn (s, l) => if String.size s > String.size l then s else l) ""

val longest_string2 =
  List.foldl (fn (s, l) => if String.size s >= String.size l then s else l) ""

fun longest_string_helper isLongerFn =
  List.foldl
    (fn (s, l) => if isLongerFn (String.size s, String.size l) then s else l)
    ""

val longest_string3 = longest_string_helper
  (fn (newSize, currentSize) => newSize > currentSize)

val longest_string4 = longest_string_helper
  (fn (newSize, currentSize) => newSize >= currentSize)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer answerer questions =
  case questions of
      [] => raise NoAnswer
    | question :: questions' =>
        case answerer question of
            NONE => first_answer answerer questions'
          | SOME answer => answer

fun all_answers answerer questions =
  let
    fun aux answerer questions accumulator =
      case questions of
          [] => SOME accumulator
        | question :: questions' =>
            case answerer question of
                NONE => NONE
              | SOME answer => aux answerer questions' (answer @ accumulator)
  in
    aux answerer questions []
  end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) String.size

fun count_some_var (s, p) =
  g (fn _ => 0) (fn variableString => if variableString = s then 1 else 0) p

(* These are helper methods for problem #10. *)

fun all_variable_strings pattern =
  case pattern of
      Variable x => [x]
    | TupleP patterns =>
        List.foldl (fn (innerPattern, variableStrings) =>
          variableStrings @ all_variable_strings innerPattern) [] patterns
    | ConstructorP (_, pattern) => all_variable_strings pattern
    | _ => []

fun has_repeats strings =
  case strings of
      [] => false
    | s :: strings' =>
        if List.exists (fn innerS => s = innerS) strings'
          then true
        else has_repeats strings'

(* End of helper methods for problem #10. *)

val check_pat = not o has_repeats o all_variable_strings

fun match (value, pattern) =
  case (pattern, value) of
      (Wildcard, _) => SOME []
    | (Variable str, _) => SOME [(str, value)]
    | (UnitP, Unit) => SOME []
    | (ConstP const, Const valueConst) =>
        if const = valueConst then SOME [] else NONE
    | (TupleP innerPatterns, Tuple innerValues) =>
        if length innerPatterns = length innerValues
          then all_answers match (ListPair.zip (innerValues, innerPatterns))
        else NONE
    | (ConstructorP (name, innerPattern), Constructor (valueName, innerValue))
      => if name = valueName then match (innerValue, innerPattern) else NONE
    | _ => NONE

fun first_match value patterns =
  SOME (first_answer (fn pattern => match (value, pattern)) patterns)
  handle NoAnswer => NONE
