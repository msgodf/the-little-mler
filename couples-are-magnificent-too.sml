(* 5. Couples Are Magnificent, Too *)

(* 1 *)

datatype 'a pizza =
         Bottom
         | Topping of ('a * ('a pizza));

(* 3 *)

datatype fish =
         Anchovy
       | Lox
       | Tuna;

(* 

datatype fish pizza =
         Bottom
         | Topping of (fish * (fish pizza));
*)

(* 4 *)

Topping(Anchovy,
        Topping(Tuna,
                Topping(Anchovy,
                        Bottom)));

(* val it = Topping (Anchovy,Topping (Tuna,Topping #)) : fish pizza *)

(* 5 *)

Topping(Tuna,
        Topping(Anchovy,
                Bottom));

(* val it = Topping (Tuna,Topping (Anchovy,Bottom)) : fish pizza *)

(* 6 *)

Topping(Anchovy,
        Bottom);

(* val it = Topping (Anchovy,Bottom) : fish pizza *)

(* 11 *)

fun rem_anchovy(Bottom)
  = Bottom
  | rem_anchovy(Topping(Anchovy,p)) =
    rem_anchovy(p)
  | rem_anchovy(Topping(Tuna,p)) =
    Topping(Tuna, rem_anchovy(p))
  | rem_anchovy(Topping(Lox,p)) =
    Topping(Lox, rem_anchovy(p));

(* val rem_anchovy = fn : fish pizza -> fish pizza *)

(* 13 *)

fun rem_anchovy(Bottom)
  = Bottom
  | rem_anchovy(Topping(Anchovy,p)) =
    rem_anchovy(p)
  | rem_anchovy(Topping(t,p)) =
    Topping(t,rem_anchovy(p));

(* 14 *)

fun rem_tuna(Bottom)
  = Bottom
  | rem_tuna(Topping(Anchovy,p)) =
    Topping(Anchovy,rem_tuna(p))
  | rem_tuna(Topping(Tuna,p)) =
    rem_tuna(p)
  | rem_tuna(Topping(Lox,p)) =
    Topping(Lox,rem_tuna(p));

(* val rem_tuna = fn : fish pizza -> fish pizza *)

(* 21 *)

fun rem_tuna(Bottom)
  = Bottom
  | rem_tuna(Topping(Tuna,p)) =
    rem_tuna(p)
  | rem_tuna(Topping(t,p)) =
    Topping(t,rem_tuna(p));

(* 28 *)

fun rem_fish(x,Bottom)
  = Bottom
  | rem_fish(Tuna,Topping(Tuna,p)) =
    rem_fish(Tuna,p)
  | rem_fish(Tuna,Topping(t,p)) =
    Topping(t,rem_fish(Tuna,p))
  | rem_fish(Anchovy,Topping(Anchovy,p)) =
    rem_fish(Anchovy,p)
  | rem_fish(Anchovy,Topping(t,p)) =
    Topping(t,rem_fish(Anchovy,p))
  | rem_fish(Lox,Topping(Lox,p)) =
    rem_fish(Lox,p)
  | rem_fish(Lox,Topping(t,p)) =
    Topping(t,rem_fish(Lox,p));

(* val rem_fish = fn : fish * fish pizza -> fish pizza *)

(* 30 *)

fun rem_fish(x,Bottom)
  = Bottom
  | rem_fish(Tuna,Topping(Tuna,p)) =
    rem_fish(Tuna,p)
  | rem_fish(Tuna,Topping(Anchovy,p)) =
    Topping(Anchovy,rem_fish(Tuna,p))
  | rem_fish(Tuna,Topping(Lox,p)) =
    Topping(Lox,rem_fish(Tuna,p))
  | rem_fish(Anchovy,Topping(Anchovy,p)) =
    rem_fish(Anchovy,p)
  | rem_fish(Anchovy,Topping(Lox,p)) =
    Topping(Lox,rem_fish(Anchovy,p))
  | rem_fish(Anchovy,Topping(Tuna,p)) =
    Topping(Tuna,rem_fish(Anchovy,p))
  | rem_fish(Lox,Topping(Lox,p)) =
    rem_fish(Lox,p)
  | rem_fish(Lox,Topping(Anchovy,p)) =
    Topping(Anchovy,rem_fish(Lox,p))
  | rem_fish(Lox,Topping(Tuna,p)) =
    Topping(Tuna,rem_fish(Lox,p));


(* 38 *)

fun eq_fish(Anchovy,Anchovy)
  = true
  | eq_fish(Anchovy,Lox) =
    false
  | eq_fish(Anchovy,Tuna) =
    false
  | eq_fish(Lox,Anchovy) =
    false
  | eq_fish(Lox,Lox) =
    true
  | eq_fish(Lox,Tuna) =
    false
  | eq_fish(Tuna,Anchovy) =
    false
  | eq_fish(Tuna,Lox) =
    false
  | eq_fish(Tuna,Tuna) =
    true;

(* val eq_fish = fn : fish * fish -> bool *)

fun eq_fish(Anchovy,Anchovy)
  = true
  | eq_fish(Lox,Lox) =
    true
  | eq_fish(Tuna,Tuna) =
    true
  | eq_fish(a_fish,another_fish) =
    false;

(* 39 *)

eq_fish(Anchovy,Anchovy);

(* val it = true : bool *)

eq_fish(Anchovy,Tuna);

(* val it = false : bool *)

(* 40 *)

fun rem_fish(x,Bottom)
  = Bottom
  | rem_fish(x,Topping(t,p)) =
    if eq_fish(t,x)
    then rem_fish(x,p)
    else Topping(t,(rem_fish(x,p)));
                                    
(* val rem_fish = fn : fish * fish pizza -> fish pizza *)

(* 44 *)

rem_fish(Anchovy,
         Topping(Anchovy,
                 Bottom));

(* val it = Bottom : fish pizza *)

(* 57 *)

fun eq_int(n:int,m:int) =
  (n = m);

(* val eq_int = fn : int * int -> bool *)

fun rem_int(x,Bottom)
  = Bottom
  | rem_int(x,Topping(t,p)) =
    if eq_int(t,x)
    then rem_int(x,p)
    else Topping(t,(rem_int(x,p)));

(* val rem_int = fn : int * int pizza -> int pizza *)

(* 66 *)

fun subst_fish(n,a,Bottom)
  = Bottom
  | subst_fish(n,a,Topping(t,p)) =
    if eq_fish(t,a)
    then Topping(n,subst_fish(n,a,p))
    else Topping(t,subst_fish(n,a,p));

(* val subst_fish = fn : fish * fish * fish pizza -> fish pizza *)

fun subst_int(n,a,Bottom)
  = Bottom
  | subst_int(n,a,Topping(t,p)) =
    if eq_int(t,a)
    then Topping(n,subst_int(n,a,p))
    else Topping(t,subst_int(n,a,p));

(* val subst_int = fn : int * int * int pizza -> int pizza *)

(* 67 *)

eq_int(17,0);

(* val it = false : bool *)

                
