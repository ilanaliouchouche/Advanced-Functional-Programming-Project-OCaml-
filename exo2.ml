(* Question 2-1 *)

let f1 (x , y ) z =
  let g a b = a < b in
  if z then g x else g y ;; 

(* x : 'a
   y : 'a
   z : bool
   g : bool
   a : 'a
   b : 'a
   f1 : bool
    
Cette fonction g prend les paramètres d'entrée a et b, les compare et renvoie 
true si a<b, sinon false.
La comparaison de deux variables est possible pour tous les types de données 
lorsque les types de données sont identiques, 
ainsi les types de données de a et b peuvent être de n'importe 
quel type et le type de la valeur renvoyée est bool.
Ensuite z est un test donc un booléen puis on a g x XOR g y donc x et y ont le
même type que a et b.

*)

(* Question 2-2 *)

let list_sum p = List.fold_left ( fun x y -> if p y then x + 1
                                  else x ) 0;;
let list_or = List.fold_left ( fun x y -> x || y ) false ;;

let a = [];; (* a : 'a list *)
(*list_sum a*) ;; (* erreur ici, il nous faut la fonction test *) (* a :('a -> bool) normalement *)
list_or a ;; (* a : bool list *)

let a = ref [];; (* a :'a list ref *)
(*list_sum !a*) ;;  (* erreur ici, même raison que pour la ligne 5 (33) *) (* a : ('a -> bool) normalement *)
list_or !a ;; (* a : bool list ref *)

(* ->> list_sum prend en argument une fonction booléenne unaire appliquée
   à chaque élément d'une liste par le biais du fold_left. Si celle-ci est
   satisfaite alors on accummule, puis finalement on rend l'accumulateur donc INT.
     ('a -> bool)->'a list->int
   - list_or, L'accumulateur commence à être défini sur faux. Pour chaque élément
   d'une liste boolééne ( car on compare avec l'acc qui est un booléen),
   nous calculons le OU logique de l'élément testé pour l'égalité et l'accumulateur. 
   Si au moins un vrai se produit, le résultat sera vrai.
     bool list -> bool 
       
   ->> confère le code. 
*)

(* Question 2-3 *)

type 'a arbre_binaire =
    Feuille
  | Noeud of 'a * 'a arbre_binaire * 'a arbre_binaire ;;

let rec map_arbre t f = 
  match t with
  | Feuille -> Feuille
  | Noeud (root, left, right) -> Noeud (f root, map_arbre left f, map_arbre right f);;
(* 'a arbre_binaire -> ('a -> 'b) -> 'b arbre_binaire*)

let rec forall_arbre t f = 
  match t with
  |Feuille -> true 
  | Noeud(root, left, right) -> f root && forall_arbre left f && forall_arbre right f;;
(* 'a arbre_binaire -> ('a -> bool) -> bool *)

(*tests*)
let t1 = Feuille;;
let t2 = Noeud(3, Feuille, Feuille);;
let t = Noeud(1, t1, t2);;

let f a = a > 0 ;; 
let f1 a = a == 120 ;;
let f2 a = 2 ;;
map_arbre t f2;;
forall_arbre t f;;
forall_arbre t f1 ;;

               
  



   










  



   















               
  



   










  



   














