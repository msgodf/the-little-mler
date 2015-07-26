(* 1. Building Blocks *)

(* 16 *)

datatype seasoning =
         Salt
       | Pepper;

(* 21 *)

datatype num =
         Zero
         | One_more_than of num;

(* 23 *)

One_more_than(Zero);

(* val it = One_more_than Zero : num) *)

(* 25 *)

One_more_than(
    One_more_than(
        Zero));

(* val it = One_more_than (One_more_than Zero) *)

(* 26 *)

(* One_more_than(0); *)

(* 27 *)

One_more_than(
    One_more_than(
        One_more_than(
            One_more_than(
                Zero))));

(* 32 *)

datatype 'a open_faced_sandwich =
         Bread of 'a
         | Slice of 'a open_faced_sandwich;

(* 33 *)

Bread(0);

(* val it = Bread 0 : int open_faced_sandwich *)

(* 34 *)

Bread(true);

(* val it = Bread true : bool open_faced_sandwich *)

(* 45 *)

Bread(
    One_more_than(
        Zero));
     
(* val it = Bread (One_more_than Zero) : num open_faced_sandwich *)

(* 46 *)

Bread(Bread(0));

(* val it = Bread (Bread 0) : int open_faced_sandwich open_faced_sandwich *)


                              
