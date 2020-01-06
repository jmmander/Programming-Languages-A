
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(str, lst) =
    case lst of
	[] => NONE
     |  l::ls => if same_string(str, l)
		 then SOME ls
		 else case  all_except_option(str, ls) of
			  NONE  => NONE
			| SOME ls' => SOME (l::ls')

fun get_substitutions1(lst, s) =
    case lst of
	[] => []
       | list1::ls => case all_except_option(s,list1) of
		         NONE => get_substitutions1(ls, s)
		       | SOME x => x @ get_substitutions1(ls, s)

							 
			 
fun get_substitutions2(lst, s) =
    let fun f (lst, s, acc) =
	    case lst of
		[] => acc
	      | list1::ls => case all_except_option(s,list1) of
				NONE => f(ls, s, acc)
			      | SOME x => f(ls, s, x@acc)
    in
	f(lst, s, [])
    end
					       
					       
fun similar_names(sub_list, {first=f,middle=m,last=l}) =
    let
	fun create_names_list(sub_names_list) =
	    case sub_names_list of
		[] => [{first=f,middle=m,last=l}]
	     |  x::xs => ({first=x,middle=m,last=l}::create_names_list(xs))
    in
	create_names_list(get_substitutions2(sub_list, f))       
   end

	
		     
	       
				       
						      
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
	      

fun card_color(card) =
    case card of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts,_)  => Red

fun card_value(card) =
    case rankcard of
	(_,Num n) => n
      | (_,Ace) => 11
      | (_,_) => 10 
			
		     

				    

					     
   
		 

		      
		      
			  
		      
					   


	    
	
	


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
