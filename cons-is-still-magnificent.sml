(* 3. Cons Is Still Magnificent *)

(* 1 *)

datatype pizza =
         Crust
         | Cheese of pizza
         | Onion of pizza
         | Anchovy of pizza
         | Sausage of pizza;

(* 2 *)

Anchovy(
    Onion(
        Anchovy(
            Anchovy(
                Cheese(
                    Crust)))));

(* val it = Anchovy (Onion (Anchovy (Anchovy (Cheese #)))) : pizza *)

(* 10 *)

fun remove_anchovy(Crust)
  = Crust
  | remove_anchovy(Cheese(x)) =
    Cheese(remove_anchovy(x))
  | remove_anchovy(Onion(x)) =
    Onion(remove_anchovy(x))
  | remove_anchovy(Anchovy(x)) =
    Crust
  | remove_anchovy(Sausage(x)) =
    Sausage(remove_anchovy(x));

(* val remove_anchovy = fn : pizza -> pizza *)

(* 11 *)

remove_anchovy(
    Anchovy(
        Crust));

(* val it = Crust : pizza *)

(* 12 *)

remove_anchovy(
    Anchovy(
        Anchovy(
            Crust)));

(* val it = Crust : pizza *)

(* 15 *)

Onion(
    remove_anchovy(
        Cheese(
            Anchovy(
                Anchovy(
                    Crust)))));

(* val it = Onion (Cheese Crust) : pizza *)

(* 24 *)

remove_anchovy(
    Cheese(
        Anchovy(
            Cheese(
                Crust))));

(* val it = Cheese Crust : Pizza *)

(* 31 *)

fun remove_anchovy(Crust)
  = Crust
  | remove_anchovy(Cheese(x)) =
    Cheese(remove_anchovy(x))
  | remove_anchovy(Onion(x)) =
    Onion(remove_anchovy(x))
  | remove_anchovy(Anchovy(x)) =
    remove_anchovy(x)
  | remove_anchovy(Sausage(x)) =
    Sausage(remove_anchovy(x));
              
remove_anchovy(
    Cheese(
        Anchovy(
            Cheese
                Crust)));


(* 43 *)

fun top_anchovy_with_cheese(Crust)
  = Crust
  | top_anchovy_with_cheese(Cheese(x)) =
    Cheese(top_anchovy_with_cheese(x))
  | top_anchovy_with_cheese(Onion(x)) =
    Onion(top_anchovy_with_cheese(x))
  | top_anchovy_with_cheese(Anchovy(x)) =
    Cheese(
        Anchovy(
            top_anchovy_with_cheese(x)))
  | top_anchovy_with_cheese(Sausage(x))=
    Sausage(top_anchovy_with_cheese(x));

(* val top_anchovy_with_cheese = fn : pizza -> pizza *)

(* 45 *)

top_anchovy_with_cheese(
    remove_anchovy(
        Onion(
            Anchovy(
                Cheese(
                    Anchovy(
                        Crust))))));

(* val it = Onion (Cheese Crust) : pizza *)

(* top_anchovy_with_cheese didn't add any cheese, because remove_anchovy
   removed all the anchovy first *)

(* 46 *)

remove_anchovy(
    top_anchovy_with_cheese(
        Onion(
            Anchovy(
                Cheese(
                    Anchovy(
                        Crust))))));

(* val it = Onion (Cheese (Cheese (Cheese Crust))) : pizza *)

(* 49 *)

fun subst_anchovy_by_cheese(x)
  = remove_anchovy(
      top_anchovy_with_cheese(x));

(* val subst_anchovy_by_cheese = fn : pizza -> pizza *)

(* 53 *)

fun subst_anchovy_by_cheese(Crust)
  = Crust
  | subst_anchovy_by_cheese(Cheese(x)) =
    Cheese(subst_anchovy_by_cheese(x))
  | subst_anchovy_by_cheese(Onion(x)) =
    Onion(subst_anchovy_by_cheese(x))
  | subst_anchovy_by_cheese(Anchovy(x)) =
    Cheese(subst_anchovy_by_cheese(x))
  | subst_anchovy_by_cheese(Sausage(x)) =
    Sausage(subst_anchovy_by_cheese(x));

(* val subst_anchovy_by_cheese = fn : pizza -> pizza *)


           
                         
