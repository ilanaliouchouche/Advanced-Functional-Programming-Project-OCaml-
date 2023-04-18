type grid = int list list;;

(* Question 3-1 *)

let g_example : grid = [[-2; 0; 1; 4]; [7; 2; -3; -4]; [6; -1; 3; 5]];;

(* Question 3-2 *)

let rec length l = 
  match l with
  | [] -> 0 
  | _ :: t -> 1 + length t;; 

let height g = 
  match g with
  | [] -> failwith "La grille est vide"
  | h :: t -> length h;;

height g_example;; 

(* Question 3-3 *)

let rec check_well_formed g = 
  match g with
  | [] | [[]] -> false
  | [a] -> true
  | h :: t -> if length h = height t then true else false;;

let wf_grid_exn g =
  if check_well_formed g = false then failwith "La grille n'est pas bien formée";;

wf_grid_exn g_example;; 
  

(* Question 3-4 *)

(* La réponse est 17. *)

(* Question 3-5 *)

let rotate_up arr = 
  match arr with
  | [] -> [] 
  | h :: t -> t @ [h];;

rotate_up [-2; 0; 1; 4];; 

(* Question 3-6 *)

let rotate_down arr = 
  List.rev (rotate_up (List.rev arr));;

rotate_down [-2; 0; 1; 4];; 

(* Question 3-7 *)

let rec add a b =
  match a, b with
  | [], [] -> []
  | [], r -> []
  | l, [] -> []
  | hd1 :: tl1, hd2 :: tl2 -> (hd1 + hd2) :: (add tl1 tl2);;


let rec max a b c = 
  match a, b, c with
  | [], [], [] -> []
  | [], _, _ -> []
  | _, [], _ -> []
  | _, _, [] -> []
  | hd1 :: tl1, hd2 :: tl2, hd3 :: tl3 -> Stdlib.max hd1 (Stdlib.max hd2 hd3) :: (max tl1 tl2 tl3);;


let best_option a b = 
  let next_up = add a (rotate_up b) in
  let next_same = add a b in
  let next_down = add a (rotate_down b) in 
  max next_up next_same next_down;;

best_option [-2; 0; 1; 4] [7; 2; -3; -4];;

(* Question 3-8 *) 

let rec get_sum a b =
  match b with
  | [] -> a
  | h :: t -> 
      get_sum (best_option a h) t;;

let sum g = 
  match g with
  | [] -> []
  | h :: t -> get_sum h t;;

sum [[-2; 0; 1; 4]; [7; 2; -3; -4]; [6; -1; 3; 5]];;

(* Question 3-9 *) 

let rec max_list arr = 
  match arr with
  | [] -> failwith "La liste est vide"
  | h :: t -> 
      match t with
      | [] -> h
      | _ -> Stdlib.max h (max_list t);; 

max_list [11; 13; 8; 7];;
    
(* Question 3-10 *) 

let rec solve g =
  max_list (sum g);; 

solve g_example;;
           
  
  
