(* 2. Matchmaker, Matchmaker *)

(* 1 *)

datatype shish_kebab =
         Skewer
         | Onion of shish_kebab
         | Lamb of shish_kebab
         | Tomato of shish_kebab;

(* 2 *)

Skewer;

(* val it = Skewer : shish_kebab *)

(* 3 *)

Onion(
    Skewer);

(* val it = Onion Skewer : shish_kebab *)

(* 4 *)

Onion(
    Lamb(
        Onion(
            Skewer)));

(* val it = Onion (Lamb (Onion Skewer)) : shish_kebab *)

(* 15 *)

fun only_onions(Skewer)
  = true
  | only_onions(Onion(x))
    = only_onions(x)
  | only_onions(Lamb(x))
    = false
  | only_onions(Tomato(x))
    = false;

(* val only_onions = fn : shish_kebab -> bool *)

(* 24 *)

only_onions(
    Onion(
        Onion(
            Skewer)));

(* val it = true : bool *)

(* 26 *)

(* Does *)

only_onions(
    Onion(
        Onion(
            Skewer)));

(* match *)

only_onions(Skewer);

(* 63 *)

fun is_vegetarian(Skewer)
  = true
  | is_vegetarian(Onion(x)) =
    is_vegetarian(x)
  | is_vegetarian(Lamb(x)) =
    false
  | is_vegetarian(Tomato(x)) =
    is_vegetarian(x);

(* val is_vegetarian = fn : shish_kebab -> bool *)

(* 64 *)

datatype 'a shish =
         Bottom of 'a
         | Onion of 'a shish
         | Lamb of 'a shish
         | Tomato of 'a shish;

(* 67 *)

datatype rod =
         Dagger
       | Fork
       | Sword;

(* 68 *)

datatype plate =
         Gold_plate
       | Silver_plate
       | Brass_plate;

(* 69 *)

Onion(
    Tomato(
        Bottom(Dagger)));

(* val it = Onion (Tomato (Bottom Dagger)) : rod shish *)

(* 71 *)

Onion(
    Tomato(
        Bottom(Gold_plate)));

(* val it =  Onion (Tomato (Bottom Gold_plate)) : plate shish *)

(* 73 *)

fun is_veggie(Bottom(x))
  = true
  | is_veggie(Onion(x)) =
    is_veggie(x)
  | is_veggie(Lamb(x)) =
    false
  | is_veggie(Tomato(x)) =
    is_veggie(x);

(* val is_veggie = fn : 'a shish -> bool *)

(* 74 *)

(* is_veggie(Onion(Fork)); *)

(* Error: operator and operand don't agree [tycon mismatch]
   operator domain: 'Z shish
   operand:         rod
   in expression:
     Onion Fork 
 *)

(* 85 *)

is_veggie(
    Onion(
        Tomato(
            Bottom(52))));

(* val it = true : bool *)

(* 98 *)

fun what_bottom(Bottom(x))
  = x
  | what_bottom(Onion(x)) =
    what_bottom(x)
  | what_bottom(Lamb(x)) =
    what_bottom(x)
  | what_bottom(Tomato(x)) =
    what_bottom(x);

(* val what_bottom = fn : 'a shish -> 'a *)


