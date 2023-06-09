
fun any(xs : bool list) =
    case xs of 
            [] => false 
        | head :: tail => if head then true else any(tail)

val test1 = any [true,false,true,false]

fun addAllOpt lst = 
    let
      fun temp xs = 
      case xs of 
        [] => 0
    |   head :: tail => case head of
                        NONE => temp (tail)
                        | SOME p => p + temp (tail)
    in
      if temp lst = 0
      then NONE
      else SOME( temp lst)
    end

val test2 = addAllOpt([SOME 1, NONE, SOME 3,SOME 4,NONE,NONE])

fun addOpt (op1,op2) = 
    case (op1,op2) of 
    (NONE,_) => NONE
    |   (_,NONE) => NONE
    |   (SOME p, SOME q) => SOME (p+q)

val test3 = addOpt(SOME 1,SOME 2)
val test4 = addOpt(SOME 1,NONE)

fun repeat (xs1:int list,xs2 : int list) = 
    let 
        fun helper2(lst1,num) = 
            if num  = 0
            then []
            else hd lst1 :: helper2(lst1,num-1)
        fun helper1(lst1,lst2) = 
            if null lst2 then []
            else helper2(lst1,hd lst2) @ helper1(tl lst1,tl lst2)
    in 
        helper1(xs1,xs2)
    end

val test5 = repeat ([1,2,3], [4,0,3])

fun cumsum(ps:int list) = 
    let 
        fun helper(xs,temp) = 
            if null xs then []
            else hd xs + temp :: helper((tl xs),(temp+(hd xs)))

    in 
        helper(ps,0)
    end

val test4 = cumsum [1,4,20]
