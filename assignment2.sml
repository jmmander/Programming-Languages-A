(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, xs) =
    let fun aux (pre, left) =
        case left of
            []     => NONE
          | hd::tl => if same_string(str, hd) then SOME (pre @ tl) else aux (pre @ [hd], tl)
    in
        aux([], xs)
    end

fun get_substitutions1 (subs, s) =
    case subs of
        []     => []
      | hd::tl => ( case all_except_option(s, hd) of
                        NONE     => get_substitutions1 (tl, s)
                      | SOME sub => sub @ get_substitutions1 (tl, s) )

fun get_substitutions2 (subs, s) =
    let fun aux (subs_left, valid_subs) =
        case subs_left of
            []     => valid_subs
          | hd::tl => ( case all_except_option(s, hd) of
                            NONE     => aux(tl, valid_subs)
                          | SOME sub => aux(tl, valid_subs @ sub) )
    in
        aux(subs, [])
    end

fun similar_names (subs, {first=x, middle=y, last=z}) =
    let fun aux (valid_subs, alt_names) =
        case valid_subs of
            []     => alt_names
          | hd::tl => aux(tl, alt_names @ [{first=hd, middle=y, last=z}])
    in
        aux(get_substitutions2(subs, x), [{first=x, middle=y, last=z}])
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (suit, rank) =
    case suit of
        Spades => Black
      | Clubs  => Black
      | _      => Red

fun card_value (suit, rank) =
    case rank of
        Num i => i
      | Ace   => 11
      | _     => 10

fun remove_card (cs, c, e) =
    let fun aux (pre, left) =
        case left of
            []     => raise e
          | hd::tl => if c = hd then pre @ tl else aux (pre @ [hd], tl)
    in
        aux([], cs)
    end

fun all_same_color cs =
    case cs of
        []           => true
      | c1::[]       => true
      | c1::(c2::tl) => (card_color c1 = card_color c2 andalso all_same_color (c2::tl))

fun sum_cards cs =
    let fun aux (left, sum) =
        case left of
            [] => sum
          | hd::tl => aux(tl, sum + card_value hd)
    in
        aux (cs, 0)
    end

fun score (held, goal) =
    let 
        val sum = sum_cards held
        val prelim_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
        if all_same_color held then prelim_score div 2 else prelim_score
    end

fun officiate (cards_list, moves_list, goal) =
    let fun play (cards, moves, held) =
        if sum_cards held > goal then score (held, goal) else
        case moves of
            []              => score (held, goal)
          | (Discard c)::tl => play (cards, tl, remove_card (held, c, IllegalMove))
          | Draw::tl        => (case cards of
                                    []            => score (held, goal)
                                  | c::cards_left => play (cards_left, tl, c::held))
    in
        play (cards_list, moves_list, [])
    end

