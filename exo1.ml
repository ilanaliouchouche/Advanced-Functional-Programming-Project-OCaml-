(* Question 1-1 *)

let f a b = a + b

let ajout_deux g = if g = 0 then f 2 0 else f (2 + 1) g - 1 ;; 
let ajout_deux g = if g = 0 then f 2 0 else f (2 + 1) (g - 1) ;;

(* Question 1-2 *)

let rec somme_derniers l1 l2 = match l1 with 
  | [] -> (match l2 with 
      | [] -> 0 
      | x2 :: [] -> x2 
      | hd2 :: tl2 -> somme_derniers [] tl2)
  | x1 :: [] -> (match l2 with
      | [] -> x1 
      | x2 :: [] -> x1 + x2 
      | hd2 :: tl2 -> somme_derniers [ x1 ] tl2)
  | hd1 :: tl1 -> somme_derniers tl1 l2 ;;

somme_derniers [0; 1; 2] [1; 2; 3] ;;

(* Question 1-3 *)

let x a b = a + b;;
let f y z = 
  let v = y + 5 in 
  if z > y then z
  else x v 0
;;

f 2 1 ;;

(* Question 1-4 *)

(*1*) let y = 5.3 ;;
(*2*) let u z =
(*3*)  z -. y ;; (* z : l2 , y : l1*)
(*4*) let y = 
  (*5*) let y = 
  (*6*)    y +. 2.5   (* y : l1 *)
  (*7*) in let z = u y (* y : l5 , u : l2 *)
  (*8*) in y *. z  (* y : l5 , z : l7 *)
  (*9*) in let u = 2.0 in 
(*10*)  y +. y +. u ;; (* y : l5 , u : l9*)







               
  



   










  



   















               
  



   










  



   














