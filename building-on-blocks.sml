(* 10. Building On Blocks *)

(* 4 *)

fun eq_int(n:int,m:int):bool = (n=m);

fun is_zero(n)
  = eq_int(n,0);

exception Too_small;

fun pred(n)
  = if eq_int(n,0)
    then raise Too_small
    else n-1;

fun succ(n)
  = n+1;

fun plus(n,m)
  = if is_zero(n)
    then m
    else succ(plus(pred(n),m));


(* 6 *)

datatype num =
	 Zero
	 | One_more_than of num;

fun is_zero(Zero)
  = true
  | is_zero(not_zero)
    =  false;


exception Too_small;

fun pred(Zero)
  = raise Too_small
  | pred(One_more_than(n))
    = n;

fun succ(n)
  = One_more_than(n);

fun plus(n,m)
  = if is_zero(n)
    then m
    else succ(plus(pred(n),m));

(* 7 *)

(* plus(2,3) *)

(* Error: operator and operand don't agree *)

(* 10 *)

plus(
    One_more_than(
	One_more_than(
	    Zero)),
    One_more_than(
	One_more_than(
	    One_more_than(
		Zero))));


(* 18 *)

signature N =
sig
    type number
    exception Too_small
    val succ : number -> number
    val pred : number -> number
    val is_zero : number -> bool
end;


(* 26 *)

functor NumberAsNum()
	:>
	N
=
struct
datatype num =
	 Zero
	 | One_more_than of num
type number = num
exception Too_small
fun succ(n)
  = One_more_than(n)
fun pred(Zero)
  = raise Too_small
  | pred(One_more_than(n))
    = n
fun is_zero(Zero)
  = true
  | is_zero(a_num)
    = false
end;


functor NumberAsInt()
	:>
	N
=
struct
type number = int
exception Too_small
fun succ(n)
  = n + 1
fun pred(n)
  = if eq_int(n,0)
    then raise Too_small
    else n - 1
fun is_zero(n)
  = eq_int(n,0)
end;


(* 33 *)

structure IntStruct =
NumberAsInt();

(* structure IntStruct : N *)

(* 35 *)

structure NumStruct =
NumberAsNum();

(* 40 *)

signature P =
sig
    type number
    val plus :
	(number * number) -> number
end;

(* 41 *)

functor PON(structure a_N : N)
	:>
	P
=
struct
type number = a_N.number
fun plus(n,m)
  = if a_N.is_zero(n)
    then m
    else a_N.succ(
	    plus(a_N.pred(n),m))
end;

(* 47 *)

structure IntArith =
PON(structure a_N = IntStruct);

(* structure IntArith : P *)

(* 51 *)

structure NumArith =
PON(structure a_N = NumStruct);

(* 62 *)

signature N_C_R =
sig
    type number
    exception Too_small
    val conceal : int -> number
    val succ : number -> number
    val pred : number -> number
    val is_zero : number -> bool
    val reveal : number -> int
end;

(* 65 *)

functor NumberAsInt()
	:>
	N_C_R
=
struct
type number = int
exception Too_small
fun conceal(n)
  = n
fun succ(n)
  = n + 1
fun pred(n)
  = if eq_int(n,0)
    then raise Too_small
    else n -1
fun is_zero(n)
  = eq_int(n,0)
fun reveal(n)
  = n
end;

functor NumberAsNum()
	:>
	N_C_R
=
struct
datatype num =
	 Zero
	 | One_more_than of num
type number = num
exception Too_small
fun conceal(n)
  = if eq_int(n,0)
    then Zero
    else One_more_than(
	    conceal(n - 1))
fun succ(n)
  = One_more_than(n)
fun pred(Zero)
  = raise Too_small
  | pred(One_more_than(n))
    = n
fun is_zero(Zero)
  = true
  | is_zero(a_num) =
    false
fun reveal(n)
  = if is_zero(n)
    then 0
    else 1 + reveal(pred(n))
end;

(* 66 *)

structure IntStruct =
NumberAsInt();

structure IntArith =
PON(structure a_N = IntStruct);

structure NumStruct =
NumberAsNum();

structure NumArith =
PON(structure a_N = NumStruct);

(* 71 *)

NumStruct.reveal(
    NumStruct.succ(
	NumStruct.conceal(0)));

(* 84 *)

functor PON(structure a_N : N)
	:>
	P where type number = a_N.number
			      =
			      struct
			      type number = a_N.number
			      fun plus(n,m)
				= if a_N.is_zero(n)
				  then m
				  else a_N.succ(
					  plus(a_N.pred(n),m))
			      end;

(* 88 *)

structure NumArith =
PON(structure a_N = NumStruct);

structure IntArith =
PON(structure a_N = IntStruct);

(* 89 *)

NumStruct.reveal(
    NumArith.plus(
	NumStruct.conceal(1),
	NumStruct.conceal(2)));

(* 93 *)

functor NumberAsInt2()
	:>
	N where type number = int
			      =
			      struct
			      type number = int
			      exception Too_small
			      fun succ(n)
				= n + 1
			      fun pred(n)
				= if eq_int(n,0)
				  then raise Too_small
				  else n - 1
			      fun is_zero(n)
				= eq_int(n,0)
			      end;

(* 95 *)

structure IntStruct2 =
NumberAsInt2();

(* 96 *)

structure IntArith2 =
PON(structure a_N = IntStruct2);

(* 99 *)

IntArith2.plus(
    1,
    2);

(* 114 *)

signature S =
sig
    type number1
    type number2
    val similar :
	(number1 * number2) -> bool
end;

(* 115 *)

functor Same(structure a_N : N
	     structure b_N : N)
	:>
	S where type number1 = a_N.number
				   where type number2 = b_N.number
							=
							struct
							type number1 = a_N.number
							type number2 = b_N.number
							fun sim(n,m)
							  = if a_N.is_zero(n)
							    then b_N.is_zero(m)
							    else sim(a_N.pred(n),
								     b_N.pred(m))
							fun similar(n,m)
							  = ((sim(n,m)
							      handle
							      a_N.Too_small => false)
							     handle
							     b_N.Too_small => false)
							end;

(* 118 *)

structure SimIntNum =
Same(structure a_N = IntStruct
     structure b_N = NumStruct);

(* 119 *)

structure SimNumInt =
Same(structure a_N = NumStruct
     structure b_N = IntStruct);

(* 122 *)

SimNumInt.similar(
    NumStruct.conceal(0),
    IntStruct.conceal(0));


(* 123 *)

SimIntNum.similar(
    IntStruct.conceal(0),
    NumStruct.conceal(1));

(* 124 *)

structure SimNumNum =
Same(structure a_N = NumStruct
     structure b_N = NumStruct);

(* 127 *)

fun new_plus(x,y)
  = NumStruct.reveal(
      NumArith.plus(
	  NumStruct.conceal(x),
	  NumStruct.conceal(y)));

fun new_plus(x,y)
  = IntStruct.reveal(
      IntArith.plus(
	  IntStruct.conceal(x),
	  IntStruct.conceal(y)));


(* 129 *)

signature J =
sig
    val new_plus : (int * int) -> int
end;

(*

functor NP(structure a_N : N_C_R
	   structure a_P : P)
	:>
	J
=
struct
fun new_plus(x,y)
  =  a_N.reveal(
      a_P.plus(
	  a_N.conceal(x),
	  a_N.conceal(y)))
end;

*)

(* 135 *)

functor NP(structure a_N : N_C_R
	   structure a_P : P
	   sharing type
	   a_N.number
	   =
	   a_P.number)
	:>
	J
=
struct
fun new_plus(x,y)
  = a_N.reveal(
      a_P.plus(
	  a_N.conceal(x),
	  a_N.conceal(y)))
end;


structure NPStruct =
NP(structure a_N = NumStruct
   structure a_P = NumArith);

(* 140 *)

structure NPStruct =
NP(structure a_N = NumberAsNum()
   structure a_P =
   PON(structure a_N = a_N));

(* 147 *)

signature T =
sig
    type number
    val times :
	(number * number) -> number
end;

functor TON(structure a_N : N
	    structure a_P : P
	    sharing type
	    a_N.number =
	    a_P.number)
	:>
	T where type number = a_N.number
			      =
			      struct
			      type number = a_N.number
			      fun times(n,m)
				= if a_N.is_zero(m)
				  then m
				  else a_P.plus(n,
						times(n,a_N.pred(m)))
			      end;
