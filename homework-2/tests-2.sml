use "homework-2.sml";

(* These are the original tests. *)
val test1 = all_except_option ("string", ["string"]) = SOME []
val test2 = get_substitutions1 ([["foo"], ["there"]], "foo") = []
val test3 = get_substitutions2 ([["foo"], ["there"]], "foo") = []
val test4 = similar_names ([["Fred", "Fredrick"], ["Elizabeth", "Betty"],
                            ["Freddie", "Fred", "F"]],
                           {first = "Fred", middle = "W", last = "Smith"}) =
  [{first = "Fred", last = "Smith", middle = "W"},
   {first = "Fredrick", last = "Smith", middle = "W"},
   {first = "Freddie", last = "Smith", middle = "W"},
   {first = "F", last = "Smith", middle = "W"}]
val test5 = card_color (Clubs, Num 2) = Black
val test6 = card_value (Clubs, Num 2) = 2
val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test9 = sum_cards [(Clubs, Num 2), (Clubs, Num 2)] = 4
val test10 = score ([(Hearts, Num 2), (Clubs, Num 4)], 10) = 4
val test11 = officiate ([(Hearts, Num 2), (Clubs, Num 4)], [Draw], 15) = 6
val test12 = officiate ([(Clubs, Ace), (Spades, Ace), (Clubs, Ace),
                         (Spades, Ace)],
                        [Draw, Draw, Draw, Draw, Draw],
                        42) = 3
val test13 = ((officiate([(Clubs, Jack), (Spades, Num(8))],
                         [Draw, Discard(Hearts, Jack)],
                         42);
               false) 
              handle IllegalMove => true)

(* These are additional tests. *)
val test14 = all_except_option ("string", ["before", "string", "after"]) =
  SOME ["before", "after"]
val test15 = all_except_option ("string", ["before", "string"]) =
  SOME ["before"]
val test16 = all_except_option ("string", ["string", "after"]) =
  SOME ["after"]
val test17 = all_except_option ("string", ["before", "after"]) =
  NONE
val test18 = get_substitutions1 ([["Fred", "Fredrick"], ["Elizabeth", "Betty"],
                                  ["Freddie", "Fred", "F"]], "Fred") =
  ["Fredrick", "Freddie", "F"]
val test19 = get_substitutions1 ([["Fred", "Fredrick"], ["Jeff", "Jeffrey"],
                                  ["Geoff", "Jeff", "Jeffrey"]], "Jeff") =
  ["Jeffrey", "Geoff", "Jeffrey"]
val test20 = get_substitutions2 ([["Fred", "Fredrick"], ["Elizabeth", "Betty"],
                                  ["Freddie", "Fred", "F"]], "Fred") =
  ["Fredrick", "Freddie", "F"]
val test21 = get_substitutions2 ([["Fred", "Fredrick"], ["Jeff", "Jeffrey"],
                                  ["Geoff", "Jeff", "Jeffrey"]], "Jeff") =
  ["Jeffrey", "Geoff", "Jeffrey"]
val test22 = all_same_color [(Clubs, Ace), (Hearts, Ace), (Hearts, Ace)] = false
val test23 = score ([(Hearts, Num 8), (Clubs, Num 4)], 10) = 6
val test24 = score ([(Hearts, Num 8), (Hearts, Num 4)], 10) = 3
