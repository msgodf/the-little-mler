(* 7. Functions Are People, Too *)

(* 1 *)

fun identity(x)
  = x;


(* 5 *)

fun true_maker(x)
  = true;

(* 7 *)

datatype bool_or_int =
	 Hot of bool
	 | Cool of int;


(* 11 *)

fun hot_maker(x)
  = Hot;

(*val hot_maker = fn : 'a -> bool -> bool_or_int *)


(* 20 *)

fun help(f)
  = Hot(
      true_maker(
	  if true_maker(5)
	  then f
	  else true_maker));
