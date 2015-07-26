(* 4. Look to the Stars *)

(* 4 *)

datatype meza =
         Shrimp
       | Calamari
       | Escargots
       | Hummus;

(* 5 *)

datatype main =
         Steak
       | Ravioli
       | Chicken
       | Eggplant;

datatype salad =
         Green
       | Cucumber
       | Greek;

(* 6 *)

datatype dessert =
         Sundae
       | Mousse
       | Torte;

(* 9 *)

(Calamari, Ravioli, Greek, Sundae);

(* val it = (Calamari,Ravioli,Greek,Sundae) : meza * main * salad * dessert *)

(* 11 *)

(Hummus, Steak, Green, Torte);

(* val it = (Hummus,Steak,Green,Torte) : meza * main * salad * dessert *)

(* 18 *)

(Shrimp, Sundae);
             
(* val it = (Shrimp,Sundae) : meza * dessert *)

(* 25 *)

fun add_a_steak(Shrimp)
  = (Shrimp, Steak)
  | add_a_steak(Calamari) =
    (Calamari, Steak)
  | add_a_steak(Escargots) =
    (Escargots, Steak)
  | add_a_steak(Hummus) =
    (Hummus, Steak);

(* val add_a_steak = fn : meza -> meza * main *)

(* 27 *)

fun add_a_steak(x)
  = (x, Steak);

(* val add_a_steak = fn : 'a -> 'a * main *)

(* 28 *)

add_a_steak(Escargots);

(* val it = (Escargots,Steak) : meza * main *)

(* 29 *)

add_a_steak(5);

(* val it = (5,Steak) : int * main *)

(* 38 *)

(*

fun remove_anchovy(Crust)
  = Crust
  | remove_anchovy(Anchovy(x)) =
    remove_anchovy(x)
  | remove_anchovy(C(x)) =
    C(remove_anchovy(x));
  
*)

(* 42 *)

fun eq_main(Steak, Steak)
  = true
  | eq_main(Steak, Ravioli) =
    false
  | eq_main(Steak, Chicken) =
    false
  | eq_main(Steak, Eggplant) =
    false
  | eq_main(Ravioli, Steak) =
    false
  | eq_main(Ravioli, Ravioli) =
    true
  | eq_main(Ravioli, Chicken) =
    false
  | eq_main(Ravioli, Eggplant) =
    false
  | eq_main(Chicken, Steak) =
    false
  | eq_main(Chicken, Ravioli) =
    false
  | eq_main(Chicken, Chicken) =
    true
  | eq_main(Chicken, Eggplant) =
    false
  | eq_main(Eggplant, Steak) =
    false
  | eq_main(Eggplant, Ravioli) =
    false
  | eq_main(Eggplant, Chicken) =
    false
  | eq_main(Eggplant, Eggplant) =
    true;

(* val eq_main = fn : main * main -> bool *)

(* 46 *)

fun eq_main(Steak, Steak) =
  true
  | eq_main(Ravioli, Ravioli) =
    true
  | eq_main(Chicken, Chicken) =
    true
  | eq_main(Eggplant, Eggplant) =
    true
  | eq_main(a_main, another_main) =
    false;

(* val eq_main = fn : main * main -> bool *)

(* 54 *)

fun has_steak(a_meza, Steak, a_dessert)
  = true
  | has_steak(a_meza, a_main, a_dessert) =
    false;

(* val has_steak = fn : 'a * main * 'b -> bool *)

(* 56 *)

has_steak(5, Steak, true);

(* val it = true : bool *)

(* 66 *)

fun has_steak(a:meza, Steak, d:dessert): bool =
  true
  | has_steak(a:meza, ns, d:dessert): bool =
    false;

(* val has_steak = fn : meza * main * dessert -> bool *)

(* 67 *)

fun add_a_steak(x)
  = (x, Steak);

(* val add_a_steak = fn : 'a -> 'a * main *)

fun add_a_steak(x:meza):(meza * main)
  = (x, Steak);

(* val add_a_steak = fn : meza -> meza * main *)

