(* 8. Bows and Arrows *)

(* 1 *)

datatype 'a list =
	 Empty
	 | Cons of 'a * 'a list;

(* 3 *)

datatype orapl =
	 Orange
       | Apple;

fun eq_orapl(Orange,Orange)
  = true
  | eq_orapl(Apple,Apple) =
    true
  | eq_orapl(one,another) =
    false;

(* 4 *)

fun eq_int(n:int,m:int):bool = (n=m);

fun subst_int(n,a,Empty)
  = Empty
  | subst_int (n,a,Cons(e,t))
    = if eq_int(a,e)
      then Cons(n,subst_int(n,a,t))
      else Cons(e,subst_int(n,a,t));


fun subst_orapl(n,a,Empty)
  = Empty
  | subst_orapl(n,a,Cons(e,t))
    = if eq_orapl(a,e)
      then Cons(n,subst_orapl(n,a,t))
      else Cons(e,subst_orapl(n,a,t));

(* 9 *)

fun subst(rel,n,a,Empty)
  = Empty
  | subst(rel,n,a,Cons(e,t))
    = if rel(a,e)
      then Cons(n,subst(rel,n,a,t))
      else Cons(e,subst(rel,n,a,t));

(* 22 *)

Cons(15,
     Cons(6,
	  Cons(15,
	       Cons(17,
		    Cons(15,
			 Cons(8,
			      Empty))))));

subst(eq_int,11,15,
      Cons(15,
	   Cons(6,
		Cons(15,
		     Cons(17,
			  Cons(15,
			       Cons(8,
				    Empty)))))));
(* 24 *)

fun less_than(n:int,m:int):bool = (n<m);

less_than(15,17);

(* 27 *)

subst(less_than,11,15,
      Cons(15,
	   Cons(6,
		Cons(15,
		     Cons(17,
			  Cons(15,
			       Cons(8,
				    Empty)))))));


(* 33 *)

fun in_range((small,large),x)
  = if less_than(small,x)
    then less_than(x,large)
    else false;

(* 36 *)

subst(in_range,22,(11,16),
      Cons(15,
	   Cons(6,
		Cons(15,
		     Cons(17,
			  Cons(15,
			       Cons(8,
				    Empty)))))));

(* 40 *)

fun subst_pred(pred,n,Empty)
  = Empty
  | subst_pred(pred,n,Cons(e,t))
    = if pred(e)
      then Cons(n,subst_pred(pred,n,t))
      else Cons(e,subst_pred(pred,n,t));

(* 48 *)

fun is_15(n)
  = eq_int(n,15);

(* 49 *)

subst_pred(is_15,11,
      Cons(15,
	   Cons(6,
		Cons(15,
		     Cons(17,
			  Cons(15,
			       Cons(8,
				    Empty)))))));

(* 52 *)

fun less_than_15(x)
  = less_than(x,15);

(* 55 *)

subst_pred(less_than_15,11,
	   Cons(15,
		Cons(6,
		     Cons(15,
			  Cons(17,
			       Cons(15,
				    Cons(8,
					 Empty)))))));

(* 59 *)

fun in_range_11_16(x)
  = if less_than(11,x)
    then less_than(x,16)
    else false;

(* 62 *)

subst_pred(in_range_11_16,22,
	   Cons(15,
		Cons(6,
		     Cons(15,
			  Cons(17,
			       Cons(15,
				    Cons(8,
					 Empty)))))));

(* 65 *)

fun in_range_c(small,large)(x)
  = if less_than(small,x)
    then less_than(x,large)
    else false;

(* 71 *)

fun in_range_c_11_16(x)
  = if less_than(11,x)
    then less_than(x,16)
    else false;

(* 77 *)

subst_pred(in_range_c(3,16),22,
	   Cons(15,
		Cons(6,
		     Cons(15,
			  Cons(17,
			       Cons(15,
				    Cons(8,
					 Empty)))))));

(* 79 *)

fun subst_c(pred)(n,Empty)
  = Empty
  | subst_c(pred)(n,Cons(e,t))
    = if pred(e)
      then Cons(n,subst_c(pred)(n,t))
      else Cons(e,subst_c(pred)(n,t));

(* 86 *)

fun subst_c_in_range_11_16(n,Empty)
  = Empty
  | subst_c_in_range_11_16(n,Cons(e,t))
    = if in_range_11_16(e)
      then Cons(n,
		subst_c_in_range_11_16(n,t))
      else Cons(e,
		subst_c_in_range_11_16(n,t));

(* 88 *)

fun combine(Empty,Empty)
  = Empty
  | combine(Empty,Cons(b,l2))
    = Cons(b,l2)
  | combine(Cons(a,l1),Empty)
    = Cons(a,l1)
  | combine(Cons(a,l1),Cons(b,l2))
    = Cons(a,combine(l1,Cons(b,l2)));

fun combine(Empty,l2)
  = l2
  | combine(Cons(a,l1),l2)
    = Cons(a,combine(l1,l2));

(* 90 *)

combine(
    Cons(1,
	 Cons(2,
	      Cons(3,
		   Empty))),
    Cons(5,
	 Cons(4,
	      Cons(7,
		   Cons(9,
			Empty)))));

(* 93 *)

fun combine_c(Empty)(l2)
  = l2
  | combine_c(Cons(a,l1))(l2)
    =  Cons(a,combine_c(l1)(l2));

(* 95 *)

fun prefixer_123(l2)
  = Cons(1,
	 Cons(2,
	      Cons(3,
		   l2)));

(* 97 *)

fun waiting_prefix_123(l2)
  = Cons(1,
	 combine_c(
	     Cons(2,
		  Cons(3,
		       Empty)))
		  (l2));

(* 104 *)

fun base(l2)
  = l2;

(* 115 *)

fun combine_s(Empty)
  = base
  | combine_s(Cons(a,l1))
    = make_cons(a,combine_s(l1))
and make_cons(a,f)(l2)
    = Cons(a,f(l2));

(* 116 *)

combine_s(
    Cons(1,
	 Cons(2,
	      Cons(3,
		   Empty))));

(* 117 *)

make_cons(3,base);

fun prefix_3(l2)
  = Cons(3,base(l2));

(* 118 *)

make_cons(2,
	  prefix_3);

fun prefix_23(l2)
  = Cons(2,prefix_3(l2));


(* 119 *)

make_cons(1,
	  prefix_23);

fun prefix_123(l2)
  = Cons(1,prefix_23(l2));
