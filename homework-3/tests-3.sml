use "homework-3.sml";

(* These are the original tests. *)
val test1 = only_capitals ["A", "B", "C"] = ["A", "B", "C"]
val test2 = longest_string1 ["A", "bc", "C"] = "bc"
val test3 = longest_string2 ["A", "bc", "C"] = "bc"
val test4a = longest_string3 ["A", "bc", "C"] = "bc"
val test4b = longest_string4 ["A", "B", "C"] = "C"
val test5 = longest_capitalized ["A", "bc", "C"] = "A"
val test6 = rev_string "abc" = "cba"
val test7 = first_answer
  (fn x => if x > 3 then SOME x else NONE)
  [1, 2, 3, 4, 5] = 4
val test8 = all_answers
  (fn x => if x = 1 then SOME [x] else NONE)
  [2, 3, 4, 5, 6, 7] = NONE
val test9a = count_wildcards Wildcard = 1
val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9c = count_some_var ("x", Variable("x")) = 1
val test10 = check_pat (Variable("x")) = true
val test11 = match (Const(1), UnitP) = NONE
val test12 = first_match Unit [UnitP] = SOME []

(* These are additional tests. *)
val test13 = only_capitals ["Apple", "Banana", "Orange"] =
  ["Apple", "Banana", "Orange"]
val test14 = only_capitals ["apple", "Banana", "orange"] = ["Banana"]
val test15 = only_capitals ["apple", "banana", "orange"] = []
val test16 = longest_string1 [] = ""
val test17 = longest_string1 ["ab", "bc", "cd"] = "ab"
val test18 = longest_string2 [] = ""
val test19 = longest_string2 ["ab", "bc", "cd"] = "cd"
val test20 = longest_string3 [] = ""
val test21 = longest_string3 ["ab", "bc", "cd"] = "ab"
val test22 = longest_string4 [] = ""
val test23 = longest_string4 ["ab", "bc", "cd"] = "cd"
val test24 = longest_capitalized ["A", "bc", "C"] = "A"
val test25 = longest_capitalized ["ab", "bc", "cde"] = ""
val test26 = rev_string "" = ""
val test27 = rev_string "AbbA" = "AbbA"
val test28 = all_answers
  (fn x => if x = 1 then SOME [x] else NONE)
  [1, 1, 1, 1, 1] = SOME [1, 1, 1, 1, 1]
val test29 = count_wildcards (Variable "asdf") = 0
val test30 = count_wildcards UnitP = 0
val test31 = count_wildcards (ConstP 17) = 0
val test32 = count_wildcards (TupleP [UnitP, ConstP 17, Wildcard, Wildcard]) = 2
val test33 = count_wildcards (TupleP [TupleP [Wildcard], Wildcard]) = 2
val test34 = count_wild_and_variable_lengths (ConstructorP ("asdf", UnitP)) = 0
val test35 = count_some_var ("xyz", Variable "x") = 0
val test36 = count_some_var
  ("xyz", TupleP [Variable "x", Wildcard, Variable "xyz", Variable "xyz"]) = 2
val test37 = check_pat (TupleP [Variable("x"), Variable("x")]) = false
