(* 9. Oh No! *)

(* 4 *)

datatype 'a list =
	 Empty
	 | Cons of 'a * 'a list;

(* 5 *)

datatype box =
	 Bacon
	 | Ix of int;

(* 9 *)

fun is_bacon(Bacon)
  = true
  | is_bacon(Ix(n))
    = false;

(* val is_bacon = fn : box -> bool *)

fun where_is(Empty)
  = 0
  | where_is(Cons(a_box,rest))
    = if is_bacon(a_box)
      then 1
      else 1 + where_is(rest);

(* val where_is = fn : box list -> int *)


(* 14 *)

exception No_bacon of int;

fun where_is(Empty)
  = raise No_bacon(0)
  | where_is(Cons(a_box,rest))
    = if is_bacon(a_box)
      then 1
      else 1 + where_is(rest);

(* val where_is = fn : box list -> int *)

(* 17 *)

(*
where_is(
    Cons(Ix(5),
	 Cons(Ix(13),
	      Cons(Ix(8),
		   Empty))));

*)

(* uncaught exception No_bacon *)


(* 31 *)

(where_is(
    Cons(Ix(5),
	 Cons(Ix(13),
	      Cons(Ix(8),
		   Empty))))
 handle
 No_bacon(an_int)
 => an_int);

(* val = 0 : int *)

(* 35 *)

(where_is(
      Cons(Ix(5),
	   Cons(Bacon,
		Cons(Ix(8),
		     Empty))))
handle
No_bacon(an_int)
=> an_int);

(* val it = 2 : int *)

(* 41 *)

Cons(Ix(5),
     Cons(Ix(4),
	  Cons(Bacon,
	       Cons(Ix(2),
		    Cons(Ix(3),
			 Empty)))));

(* 49 *)

(* I test this once find exists *)


(*
find(2,
     Cons(Ix(5),
	  Cons(Ix(4),
	       Cons(Bacon,
		    Cons(Ix(2),
			 Cons(Ix(3),
			      Empty))))));

*)

(* 66 *)

exception Out_of_range;

fun eq_int(n:int,m:int):bool = (n=m);

fun list_item(n,Empty)
  = raise Out_of_range
  | list_item(n,Cons(abox,rest))
    = if eq_int(n,1)
      then abox
      else list_item(n - 1,rest);

(* val list_item = fn : int * 'a list -> 'a *)

(* 67 *)

fun find(n,boxes)
  = check(n,boxes,list_item(n,boxes))
and check(n,boxes,Bacon)
    = n
  | check(n,boxes,Ix(i))
    = find(i,boxes);

(* val find = fn : int * box list -> int *)
(* val check = fn : int * box list * box -> int *)

(*
find(2,
     Cons(Ix(5),
	  Cons(Ix(4),
	       Cons(Bacon,
		    Cons(Ix(2),
			 Cons(Ix(3),
			      Empty))))));
*)

(* never terminates! *)

(* 75 *)

(*

find(1,
     Cons(Ix(5),
	  Cons(Ix(4),
	       Cons(Bacon,
		    Cons(Ix(2),
			 Cons(Ix(7),
			      Empty))))));
*)

(* uncaught exception Out_of_range *)

(* 84 *)

fun find(n,boxes)
  = (check(n,boxes,list_item(n,boxes))
     handle
     Out_of_range
	 => find(n div 2,boxes))
and check(n,boxes,Bacon)
    = n
  | check(n,boxes,Ix(i))
    = find(i,boxes);

(* 86 *)

find(1,
     Cons(Ix(5),
	  Cons(Ix(4),
	       Cons(Bacon,
		    Cons(Ix(2),
			 Cons(Ix(7),
			      Empty))))));

(* val it = 3 :int *)

(* 98 *)

fun path(n,boxes)
  = Cons(n,
	 (check(n,boxes,list_item(n,boxes))
	  handle
	  Out_of_range
	  => path(n div 2,boxes)))
and check(n,boxes,Bacon)
    = Empty
  | check(n,boxes,Ix(i))
    = path(i,boxes);

(* val path = fn : int * box list -> int list *)
(* val check = fn :int * box list * boz -> int list *)

fun path(n,boxes)
  = Cons(n,
	 (check(boxes,list_item(n,boxes))
	  handle
	  Out_of_range
	  => path(n div 2,boxes)))
and check(boxes,Bacon)
    = Empty
  | check(boxes,Ix(i))
    = path(i,boxes);

(* val path = fn : int * box list -> int list *)
(* val check = fn : box list * box -> int list *)

(* 100 *)

path(1,
     Cons(Ix(5),
	  Cons(Ix(4),
	       Cons(Bacon,
		    Cons(Ix(2),
			 Cons(Ix(7),
			      Empty))))));
