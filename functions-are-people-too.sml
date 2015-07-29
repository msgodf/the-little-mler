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

(* val help = fn : ('a -> bool) -> bool_or_int *)

(* 38 *)

datatype chain =
	 Link of (int * (int -> chain));


(* 50 *)

fun ints(n)
  = Link(n + 1, ints);

(*val ints = fn : int -> chain *)

(* 51 *)

ints(0);

(* val it = Link(1,fn) : chain *)

(* 53 *)

ints(5);

(* val it = Link (6,fn) : chain *)

(* 58 *)

fun skips(n)
  = Link(n + 2, skips);

(* val skips = fn : int -> chain *)

(* 59 *)

skips(8);

(* val it = Link (10,fn) : chain *)

(* 60 *)

skips(17);

(* val it = Link(19,fn) : chain *)

(* 61 *)

fun eq_int(n:int,m:int):bool = (n=m);

fun divides_evenly(n,c)
  = eq_int((n mod c),0);

(* val divides_evenly = fn : int * int -> bool *)

fun is_mod_5_or_7(n)
  = if divides_evenly(n,5)
    then true
    else divides_evenly(n,7);

(* val is_mod_5_or_7 = fn : int -> bool *)


(* 62 *)

fun some_ints(n)
  = if is_mod_5_or_7(n + 1)
    then Link(n + 1,some_ints)
    else some_ints(n + 1);

(*val some_ints = fn : int -> chain *)

(* 63 *)

some_ints(1);

(* val it = Link(5,fn) : chain *)

some_ints(17);

(* val it = Link (20,fn) : chain *)

some_ints(116);

(* val it = Link (119,fn) : chain *)

(* 71 *)

some_ints(0);

(* val it = Link (5,fn) : chain *)

(* 83 *)

fun chain_item(n,Link(i,f))
  = if eq_int(n,1)
    then i
    else chain_item(n-1,f(i));

(* val chain_item = fn : int * chain -> int *)

(* 84 *)

chain_item(1, some_ints(0));

(* val it = 5 : int *)

chain_item(6, some_ints(0));

(* val it = 20 : int *)

chain_item(37, some_ints(0));

(* val it = 119 : int *)

(* 93 *)

fun is_prime(n)
  = has_no_divisors(n,n - 1)
and has_no_divisors(n,c)
    = if eq_int(c,1)
      then true
      else
	  if divides_evenly(n,c)
	  then false
	  else has_no_divisors(n,c - 1);

(* val is_prime = fn : int -> bool *)


(* val has_no_divisors = fn : int * int -> bool *)


(* 96 *)

fun primes(n)
  = if is_prime(n + 1)
    then Link(n + 1,primes)
    else primes(n + 1);

(* val primes = fn : int -> chain *)

chain_item(12,primes(1));

(* val it = 37 : int *)

(* 98 *)

fun fibs(n)(m)
  = Link(n + m,fibs(m));

(* val fibs = fn : int -> int -> chain *)

(* 106 *)

(* Link(0,fibs(1)); *)

(* 109 *)

fun fibs_1(m)
  = Link (1 + m,fibs(m));

(* val fibs_1 = fn : int -> chain *)

(* 112 *)

fibs_1(1);

(* val it = Link(2,fn) : chain *)

(* 115 *)

fibs_1(2);

(* val it = Link(3,fn) : chain *)

(* 117 *)

fun fibs_2(m)
  = Link(2 + m,fibs(m));

