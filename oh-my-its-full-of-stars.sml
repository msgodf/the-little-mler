(* 6. Oh My, It's Full of Stars! *)

(* 6 *)

datatype fruit =
         Peach
       | Apple
       | Pear
       | Lemon
       | Fig;

datatype tree =
         Bud
         | Flat of fruit * tree
         | Split of tree * tree;


(* 11 *)

fun flat_only(Bud)
  = true
  | flat_only(Flat(f,t)) =
    flat_only(t)
  | flat_only(Split(s,t)) =
    false;

(* val flat_only = fn : tree -> bool *)

(* 12 *)

fun split_only(Bud)
  = true
  | split_only(Flat(f,t)) =
    false
  | split_only(Split(s,t)) =
    if split_only(s)
    then split_only(t)
    else false;

(* val split_only = fn : tree -> bool *)

(* 20 *)

split_only(
    Split(
        Split(
            Bud,
            Split(
                Bud,
                Bud)),
        Split(
            Bud,
            Split(
                Bud,
                Bud))));

(* val it = true : bool *)

(* 22 *)

fun contains_fruit(Bud)
  = false
  | contains_fruit(Flat(f,t)) =
    true
  | contains_fruit(Split(s,t)) =
    if contains_fruit(s)
    then true
    else contains_fruit(t);

(* val contains_fruit = fn : tree -> bool *)

fun contains_fruit(x)
  = if split_only(x)
    then false
    else true;


(* 34 *)

fun less_than(n:int,m:int) = (n < m);

(* var less_than = fn : int * int -> bool *)

fun larger_of(n,m)
  = if less_than(n,m)
    then m
    else n;

(* var larger_of = fn : int * int -> int *)

(* 35 *)

fun height(Bud) =
  0
  | height(Flat(f,t)) =
    1+ height(t)
  | height(Split(s,t)) =
    1 + larger_of(height(s),height(t));

(* var height = fn : tree -> int *)

(* 36 *)

height(Split(Bud,Bud));

(* 39 *)

fun eq_fruit(Peach,Peach)
  = true
  | eq_fruit(Apple,Apple) =
    true
  | eq_fruit(Pear,Pear) =
    true
  | eq_fruit(Lemon,Lemon) =
    true
  | eq_fruit(Fig,Fig) =
    true
  | eq_fruit(a_fruit,another_fruit) =
    false;


(* val eq_fruit = fn : fruit * fruit -> bool *)

(* 41 *)

fun subst_in_tree(n,a,Bud)
  = Bud
  | subst_in_tree(n,a,Flat(f,t)) =
    if eq_fruit(f,a)
    then Flat(n,subst_in_tree(n,a,t))
    else Flat(f,subst_in_tree(n,a,t))
  | subst_in_tree(n,a,Split(s,t)) =
    Split(
	subst_in_tree(n,a,s),
	subst_in_tree(n,a,t));

(* val subst_in_tree = fn : fruit * fruit * tree -> tree *)

(* 43 *)

(* val occurs = fn : fruit * tree -> int *)

fun occurs(a,Bud)
  = 0
  | occurs(a,Flat(f,t)) =
    if eq_fruit(f,a)
    then 1 + occurs(a,t)
    else occurs(a,t)
  | occurs(a,Split(s,t)) =
    occurs(a,s) + occurs(a,t);

(* 51 *)

datatype 'a slist =
	 Empty
	 | Scons of (('a sexp) * ('a slist))
     and
     'a sexp =
     An_atom of 'a
     | A_slist of ('a slist);

(* 54 *)

Scons(An_atom(Fig),
      Scons(An_atom(Fig),
	    Scons(An_atom(Lemon),
		  Empty)));

(* 57 *)

fun occurs_in_slist(a,Empty)
  = 0
  | occurs_in_slist(a,Scons(s,y)) =
    occurs_in_sexp(a,s) + occurs_in_slist(a,y)					
and occurs_in_sexp(a,An_atom(b))
    = if eq_fruit(b,a)
      then 1
      else 0
  | occurs_in_sexp(a,A_slist(y))
    = occurs_in_slist(a,y);

(* val occurs_in_slist = fn : fruit * fruit slist -> int *)
(* val occurs_in_sexp = fn : fruit * fruit sexp -> int *)

(* 58 *)

fun subst_in_slist(n,a,Empty)
  = Empty
  | subst_in_slist(n,a,Scons(s,y)) =
    Scons(
	subst_in_sexp(n,a,s),
	subst_in_slist(n,a,y))
and subst_in_sexp(n,a,An_atom(b))
    = if eq_fruit(b,a)
      then An_atom(n)
      else An_atom(b)
  | subst_in_sexp(n,a,A_slist(y)) =
    A_slist(
	subst_in_slist(n,a,y));

(* var subst_in_slist = fn : fruit * fruit * fruit slist -> fruit slist *)
(* var subst_in_sexp = fn : fruit * fruit * fruit sexp -> fruit sexp *)

(* 59 *)

(*

fun rem_from_slist(a,Empty)
  = Empty
  | rem_from_slist(a,Scons(s,y)) =
    ___
and rem_from_sexp(a,An_atom(b))
    = __
  | rem_from_sexp(a,A_slist(y)) =
    __;

*)

(* 64 *)

(*

fun rem_from_slist(a,Empty)
  = Empty
  | rem_from_slist(a,Scons(s,y)) =
    if eq_fruit_in_atom(a,s)
    then rem_from slist(a,y)
    else Scons(
             rem_from_sexp(a,s),
             rem_from_slist(a,y))
and rem_from_sexp(a,An_atom(b))
    = __
  | rem_from_sexp(a,A_slist(y)) =
    A_slist(rem_from_slist(a,y));

*)

(* 65 *)

fun eq_fruit_in_atom(a,An_atom(s))
  = eq_fruit(a,s)
  | eq_fruit_in_atom(a_fruit,A_slist(y)) =
    false;

(* 68 *)

fun rem_from_slist(a,Empty)
  = Empty
  | rem_from_slist(a,Scons(s,y)) =
    if eq_fruit_in_atom(a,s)
    then rem_from_slist(a,y)
    else Scons(
	    rem_from_sexp(a,s),
	    rem_from_slist(a,y))
and rem_from_sexp(a,An_atom(b))
    = An_atom(b)
  | rem_from_sexp(a,A_slist(y)) =
    A_slist(rem_from_slist(a,y));

(* var rem_from_slist = fn : fruit * fruit slist -> fruit slist *)
(* var rem_from_sexp = fn : fruit * fruit sexp -> fruit sexp *)

(* 76 *)

fun rem_from_slist(a,Empty)
  = Empty
  | rem_from_slist(a,Scons(An_atom(b),y)) =
    if eq_fruit(a,b)
    then rem_from_slist(a,y)
    else Scons(
	    An_atom(b),
	    rem_from_slist(a,y))
  | rem_from_slist(a,Scons(A_slist(x),y)) =
    Scons(
	A_slist(rem_from_slist(a,x)),
	rem_from_slist(a,y));

(* val rem_from_slist = fn : fruit * fruit slist -> fruit slist *)



		   
		  
	

		   
		
	       
		     
	     

