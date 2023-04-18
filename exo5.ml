type command = Up | Down | Left | Right | Seq of command list

(* Question 5-1 *)

let rec evalpos1 c =
  match c with
  | Up -> 0, 1
  | Down -> 0, -1
  | Left -> -1, 0
  | Right -> 1, 0
  | Seq (ls) -> match ls with
    | [] -> 0, 0
    | h :: t -> 
        match t with
        | [] -> evalpos1 h
        | t -> 
            let (x, y) = evalpos1 (Seq t) in
            match h with
            | Up when y < 100 -> x, y + 1 
            | Down when y > -100 -> x, y - 1
            | Left when x > -100 -> x - 1, y
            | Right when x < 100 -> x + 1, y
            | _ -> failwith "La tortue est hors jeu !"
  
;;

evalpos1 (Seq [Up; Up; Left; Down; Right; Right]);;

type order = Up | Down | Left | Right
type command = Dir of order | Repeat of (int * order) | Seq of command list | Infinite of order

(* Question 5-2 *) 
  
let rec evalpos2 c =
  match c with
    
  | Seq (ls) -> 
      begin
        match ls with
      
        | [] -> 0, 0
        | h :: t -> 
        
            match t with
          
            | [] -> evalpos2 h
            | t -> 
                let (x, y) = evalpos2 (Seq t) in 
                match h with 
                | Dir(order) -> 
                    begin
                      match order with 
                      | Up when y < 100 -> x, y + 1 
                      | Down when y > -100 -> x, y - 1
                      | Left when x > -100 -> x - 1, y
                      | Right when x < 100 -> x + 1, y
                      | _ -> failwith "La tortue est hors jeu !"
                    end     
                | Repeat(n, order) -> 
                    begin
                      match order with 
                      | Up when y + n <= 100 -> x, y + n
                      | Down when y - n >= -100 -> x, y - n
                      | Left when x - n >= -100 -> x - n, y
                      | Right when x+ n <= 100 -> x + n, y
                      | _ -> failwith "La tortue est hors jeu !"
                    end
                | _ -> 0, 0
      end
      
  | Dir(order) -> 
      begin
        match order with 
        | Up -> 0, 1
        | Down -> 0, -1
        | Left -> -1, 0
        | Right -> 1, 0
      end     
  | Repeat(n, order) -> 
      begin
        match order with 
        | Up when n <= 100 -> 0, n
        | Down when n <= 100 -> 0, -n
        | Left when n <= 100 -> -n, 0
        | Right when n <= 100 -> n, 0
        | _ -> failwith "La tortue est hors jeu !" 
      end 
  | _ -> 0, 0
;;


evalpos2 (Seq [Repeat (2, Up); Dir Up; Dir Left; Dir Down;  Dir Right; Dir Right]);; 

(* Question 5-3 *) 
  
let rec evalpos3 c =
  match c with
    
  | Seq (ls) -> 
      begin
        match ls with
      
        | [] -> 0, 0
        | h :: t -> 
        
            match t with
          
            | [] -> evalpos3 h
            | t -> 
                let (x, y) = evalpos3 (Seq t) in 
                match h with 
                | Dir(order) -> 
                    begin
                      match order with 
                      | Up when y < 100 -> x, y + 1 
                      | Down when y > -100 -> x, y - 1
                      | Left when x > -100 -> x - 1, y
                      | Right when x < 100 -> x + 1, y
                      | _ -> failwith "La tortue est hors jeu !"
                    end     
                | Repeat(n, order) -> 
                    begin
                      match order with 
                      | Up when y + n <= 100 -> x, y + n
                      | Down when y - n >= -100 -> x, y - n
                      | Left when x - n >= -100 -> x - n, y
                      | Right when x+ n <= 100 -> x + n, y
                      | _ -> failwith "La tortue est hors jeu !"
                    end
                | Infinite(order) ->
                    failwith "La tortue est hors jeu !" 
                | _ -> 0, 0
      end
      
  | Dir(order) -> 
      begin
        match order with 
        | Up -> 0, 1
        | Down -> 0, -1
        | Left -> -1, 0
        | Right -> 1, 0
      end     
  | Repeat(n, order) -> 
      begin
        match order with 
        | Up when n <= 100 -> 0, n
        | Down when n <= 100 -> 0, -n
        | Left when n <= 100 -> -n, 0
        | Right when n <= 100 -> n, 0
        | _ -> failwith "La tortue est hors jeu !"
      end
  | Infinite(order) ->
      failwith "La tortue est hors jeu !"
;;

let safety c =
  try
    let _ = evalpos3 c in
    true
      
  with 
  | _ -> false 
;;

safety (Seq [Repeat (2, Up); Dir Up; Dir Left; Dir Down;Dir Right; Dir Right]);;


