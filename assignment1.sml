fun is_older (d1: int*int*int, d2: int*int*int) =
    if d1 = d2
    then false
    else (if (#1 d1) = (#1 d2)
	  then (if (#2 d1) = (#2 d2)
		then (#3 d1) < (#3 d2)
		else (#2 d1) < (#2 d2))
	  else (#1 d1) < (#1 d2))		     

fun number_in_month(datelist: (int*int*int) list, month: int) =
    if null datelist
    then 0
    else (if #2 (hd datelist) = month
	  then 1 + number_in_month((tl datelist), month)
	  else  number_in_month((tl datelist), month))

fun number_in_months (datelist: (int*int*int) list, monthlist: int list) =
    if null monthlist
    then 0
    else number_in_month(datelist, (hd monthlist)) + number_in_months(datelist, (tl  monthlist))
	
fun dates_in_month (datelist: (int*int*int) list, month: int) =
    if null datelist
    then []
    else (if #2 (hd datelist) = month
	  then  (hd datelist) :: dates_in_month((tl datelist), month)
	  else  dates_in_month((tl datelist), month))				    

 fun dates_in_months (datelist: (int*int*int) list, monthlist: int list) =
     if null monthlist
     then []		   
     else dates_in_month(datelist, (hd monthlist)) @ dates_in_months(datelist, (tl monthlist))
	   
 fun get_nth(string_list: string list, n: int) =
     if n = 1
     then (hd string_list)
     else get_nth((tl string_list), n-1)
atelist)
	 end)

			  

     




     
	     
	     
	  
	     
		     	     
	 
