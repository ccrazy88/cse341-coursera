(* This code is provided for homework #2. *)
fun same_string (s1: string, s2: string) =
  s1 = s2
(* End of provided code. *)

fun all_except_option (s, all) =
  case all of
      [] => NONE
    | head :: all' =>
        if same_string (s, head) then SOME all'
        else
          case all_except_option (s, all') of
              NONE => NONE
            | SOME allExceptOption' => SOME (head :: allExceptOption')

fun get_substitutions1 (substitutions, s) =
  case substitutions of
      [] => []
    | substitution :: substitutions' =>
        case all_except_option (s, substitution) of
            NONE => get_substitutions1 (substitutions', s)
          | SOME allExceptOption' =>
              allExceptOption' @ get_substitutions1 (substitutions', s)

fun get_substitutions2 (substitutions, s) =
  let
    fun aux (substitutions, s, acc) =
      case substitutions of
          [] => acc
        | substitution :: substitutions' =>
            case all_except_option (s, substitution) of
                NONE => aux (substitutions', s, acc)
              | SOME allExceptOption' =>
                  aux (substitutions', s, acc @ allExceptOption')
  in aux (substitutions, s, [])
  end

fun similar_names (substitutions, {first, middle, last}) =
  let
    fun aux (otherFirsts, {first, middle, last}) =
      case otherFirsts of
           [] => []
         | otherFirst :: otherFirsts' =>
             {first = otherFirst, middle = middle, last = last} ::
             aux (otherFirsts', {first = first, middle = middle, last = last})
  in
    {first = first, middle = middle, last = last} ::
    aux (get_substitutions2 (substitutions, first),
         {first = first, middle = middle, last = last})
  end

(* These data types are provided for homework #2. *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw 
exception IllegalMove
(* End of data types provided for homework #2. *)

fun card_color (suit, rank) =
  case suit of
      Clubs => Black
    | Diamonds => Red
    | Hearts => Red
    | Spades => Black

fun card_value (suit, rank) =
  case rank of
      Jack => 10
    | Queen => 10
    | King => 10
    | Ace => 11
    | Num n => n

fun remove_card (cards, card, exc) =
  case cards of
      [] => raise exc
    | head :: cards' =>
        if head = card
          then cards'
        else head :: remove_card(cards', card, exc)

fun all_same_color cards =
  case cards of
      [] => true
    | card :: [] => true
    | first :: second :: cards' =>
        card_color first = card_color second andalso
        all_same_color (second :: cards')

fun sum_cards cards =
  let
    fun aux (cards, acc) =
      case cards of
          [] => acc
        | card :: cards' => aux (cards', acc + card_value card)
  in aux (cards, 0)
  end

fun score (heldCards, goal) =
  let
    fun preliminary_score (heldCards, goal) =
      let val sumOfHeldCards = sum_cards heldCards
      in
        if sumOfHeldCards > goal
          then (sumOfHeldCards - goal) * 3
        else goal - sumOfHeldCards
      end
    val scoreDivisor = if all_same_color heldCards then 2 else 1
  in preliminary_score (heldCards, goal) div scoreDivisor
  end

fun officiate (cardList, moveList, goal) =
  let
    fun aux (cardList, moveList, goal, heldList) =
      case moveList of
          [] => score (heldList, goal)
        | Discard card :: moveList' =>
            aux (cardList, moveList', goal,
                 remove_card (heldList, card, IllegalMove))
        | Draw :: moveList' =>
            case cardList of
                [] => score (heldList, goal)
              | card :: cardList' =>
                  let val heldList' = card :: heldList
                  in
                    if sum_cards heldList' > goal
                      then score(heldList', goal)
                    else aux (cardList', moveList', goal, heldList')
                  end
  in aux (cardList, moveList, goal, [])
  end

(* Challenge #3a *)

fun aces_in_hand cards =
  case cards of
      [] => 0
    | (suit, rank) :: cards' =>
        case rank of
            Ace => 1 + aces_in_hand cards'
          | _ => aces_in_hand cards'

fun replace_aces_with_ones (cards, numberOfOnes) =
  case cards of
      [] => []
    | (suit, rank) :: cards' =>
        case rank of
            Ace =>
              if numberOfOnes > 0
                then (suit, Num 1) :: replace_aces_with_ones (cards',
                                                              numberOfOnes - 1)
              else (suit, rank) :: replace_aces_with_ones (cards', numberOfOnes)
          | _ => (suit, rank) :: replace_aces_with_ones (cards', numberOfOnes)

fun score_challenge (heldList, goal) =
  let
    val acesInHand = aces_in_hand heldList
    fun score_with_aces (heldList, goal, acesAsOne) =
      score (replace_aces_with_ones (heldList, acesAsOne), goal)
    fun score_with_aces_minimum (heldList, goal, acesAsOne, currentMinimum) =
      let
        val score = score_with_aces (heldList, goal, acesAsOne)
        val newMinimum =
          case currentMinimum of
              NONE => SOME score
            | SOME currentMinimumValue =>
                SOME (Int.min (score, currentMinimumValue))
      in
        if acesAsOne = 0 then newMinimum
        else score_with_aces_minimum (heldList, goal, acesAsOne - 1, newMinimum)
      end
  in
    case score_with_aces_minimum (heldList, goal, acesInHand, NONE) of
        NONE => raise IllegalMove
      | SOME minimumValue => minimumValue
  end

(* Forgot to implement officiate_challenge. Whoops! *)

(* Challenge #3b *)

fun card_with_value (cards, value) =
  case cards of
      [] => NONE
    | card :: cards' =>
        if card_value card = value
          then SOME card
        else card_with_value (cards', value)

(* This player stops unless it is compelled to make a move by the requirements
 * of the "careful player" rules. *)
fun careful_player (cardList, goal) =
  let
    fun aux (cardList, goal, moveList, heldList) =
      if score (heldList, goal) = 0
        then moveList
      else
        case cardList of
            [] => moveList
          | nextCard :: cardList' =>
              case card_with_value (heldList, sum_cards heldList +
                                    card_value nextCard - goal) of
                  SOME cardWithValue => moveList @ [Discard cardWithValue, Draw]
                | NONE =>
                    if goal - sum_cards heldList > 10
                      then aux (cardList', goal, moveList @ [Draw],
                                nextCard :: heldList)
                    else moveList
  in aux (cardList, goal, [], [])
  end
