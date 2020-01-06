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

 fun date_to_string(date: int*int*int) =
     let val monthlist = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
     in get_nth(monthlist, (#2 date)) ^ " " ^ Int.toString(#3 date) ^  ", " ^ Int.toString(#1 date)
     end
		      
 fun number_before_reaching_sum(sum: int, intlist: int list) =
     if (hd intlist) >= sum
     then 0
     else 1 + number_before_reaching_sum((sum - (hd intlist)), (tl intlist))

 fun what_month(day: int) =
     let val monthlist = [31,28,31,30,31,30,31,31,30,31,30,31]
     in 1 + number_before_reaching_sum(day, monthlist)
     end

 fun month_range(d1: int, d2: int) =
     if d1 > d2
     then []
     else what_month(d1) :: month_range(d1 + 1, d2)
	 		     
fun oldest(datelist : (int*int*int) list) =
    if null datelist
    then NONE
    else (let val tl_ans = oldest(tl datelist)
	 in if isSome tl_ans andalso is_older(valOf tl_ans,(hd datelist)) 
	    then tl_ans
	    else SOME (hd datelist)
	 end)

			  

     
	     
	     
	  
	     
		     	     
	 
