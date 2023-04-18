(* Question 4-1 *) 
           
let rec fibo n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | n when n > 1 -> fibo (n - 1) + fibo (n - 2)
  | n -> failwith "N est négatif" ;;

fibo 1;;
fibo 2;;
fibo 6;;
fibo 10;;
fibo 12;;

(* Question 4-2 *)

let count = ref 0;; 
             
let rec fibocount n =
  count := !count + 1; 
  
  match n with
  | 0 -> 0
  | 1 -> 1
  | n when n > 1 -> fibocount (n - 1) + fibocount (n - 2)
  | n -> failwith "N est négatif" ;;


fibocount(1);;
!count;;
fibocount(2);;
!count;; 
fibocount(6);;
!count;;
fibocount(10);;
!count;;
fibocount(12);;
!count;; 


(* Question 4-3 *)

let f = ref (List.init 1024 (fun x -> -1));; 
let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;


let rec fibocheck n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | n -> if List.nth !f n != -1 then List.nth !f n else 
        let temp = memofibocount n in
        f := replace !f n temp;
        List.nth !f n
    
and 
  memofibocount n =
  count := !count + 1; 
  match n with
  | 0 -> 0
  | 1 -> 1
  | n -> fibocheck (n-1) + fibocheck (n-2);; 


(* Question 4-4 *) 

count := 0 ;;

memofibocount(1);;
!count;;
memofibocount(2);;
!count;; 
memofibocount(6);;
!count;;
memofibocount(10);;
!count;;
memofibocount(12);;
!count;; 

f;;


(* 
 fibocount(1);; 
 fibocount(2);; 
 fibocount(6);; 
 fibocount(10);; 
 fibocount(12);;

    La complexité avec la technique de mémoïsation est linéaire 0(n) alors que
    l'originale fait deux appels recursifs ce qui rend la complexité élevée, 
    donc on a plus de temps d'attente pour la Q4-1. D'ailleurs on a comme preuve
    de différence de compléxité le fait que count renvoie 15 avec memofibocount 
    et 671 avec fibocount.(en allant seulement jusqu'à n = 12).
*)


           
  
  

           
  
  
