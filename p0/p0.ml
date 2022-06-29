(*        EJECICIO 1        *)


(* mapdoble (function x -> x) (function x -> -x) [1;1;1;1;1];;*)
(* Implemente la función mapdoble *)

let rec mapdoble f1 f2 l = match l with
[] -> []
| h::t->f1(h)::(mapdoble f2 f1 t);;



(* Indique el tipo de la función mapdoble *)
(* val mapdoble : ('a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list = <fun> *)

(* Indique el valor de: mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];; *)   
(* Error: This expression has type string but an expression was expected of type int *)

(* Indique el tipo de: let y = function x -> 5 in mapdoble y;; *)
(* - : ('_weak1 -> int) -> '_weak1 list -> int list = <fun> *)





(*        EJECICIO 2        *)


(* Defina una función primero_que_cumple *)

let rec primero_que_cumple p l = match l with
[] -> raise (Not_found)
| (h::t) -> if (p h) then h
	    else primero_que_cumple p t;;
	    
	 
	    
(* Indique el tipo de la función primero_que_cumple *)
(* val primero_que_cumple : ('a -> bool) -> 'a list -> 'a = <fun> *)



(* Defina una función existe *)

let existe p l =
	try primero_que_cumple p l with 
	| Not_found -> false
	| _ -> true;;

	
(* Define una función primero_que_cumple para pares clave-valor *)

let rec asociado p l =
  snd(primero_que_cumple (function (a,_) -> a = p) l );;





(*        EJECICIO 3        *)

type 'a arbol_binario = 
      Vacio
    | Nodo of 'a * 'a arbol_binario * 'a arbol_binario;;

    
(* in_orden t;; *)

let rec in_orden = function
  | Vacio -> []
  | Nodo (value, left, right) -> in_orden left @ [value] @ in_orden right;; 


(*pre_orden t;; *)

let rec pre_orden = function
  | Vacio -> []
  | Nodo (value, left, right) -> [value] @ pre_orden left @ pre_orden right;;


(* post_orden t;; *)  

let rec post_orden = function
  | Vacio -> []
  | Nodo (value, left, right) -> post_orden left @ post_orden right @ [value] ;; 


(* anchura t;; *)
  
let anchura t =
  let rec aux l1 l2 r = match l1, l2 with
	    [], [] -> List.rev r
	    | [], _ -> aux (List.rev l2) [] r
	    |(Vacio :: t, _) -> aux t l2 r
	    | (Nodo (value, left, right) :: t, _ )-> aux t (right :: left :: l2) (right :: r)
    in aux [t] [] [];;





(*        EJECICIO 4        *)


type 'a conjunto = Conjunto of 'a list;;
let conjunto_vacio = Conjunto [];;


(* pertenece : 'a -> 'a conjunto -> bool *)
let rec pertenece x (Conjunto c) = match c with
	[] -> false
	| h::t -> if x=h then true
			  else pertenece x (Conjunto t);;


(* agregar : 'a -> 'a conjunto -> 'a conjunto *)
let agregar x (Conjunto c) =
	if (pertenece x (Conjunto c)) then (Conjunto c)
	else Conjunto (x::c);;


(* conjunto_of_list : 'a list -> 'a conjunto *)
let conjunto_of_list l= 
	let rec aux init acc = match init with
		[] -> Conjunto acc
		| h::t -> if (pertenece h (Conjunto t)) then aux t acc 
			   else  aux t (h::acc) 
	in aux l [];;
	

(* suprimir : 'a -> 'a conjunto -> 'a conjunto *)
let suprimir x (Conjunto c) =
	let rec f aux conj =match conj with
		Conjunto (h::t) -> if x=h then Conjunto (aux@t)
				  else f (h::aux) (Conjunto t)
		| Conjunto [] -> Conjunto c
	in f [] (Conjunto c);;
		

(* cardinal : 'a conjunto -> int *)
let cardinal (Conjunto c) =
	let rec aux a conj = match conj with
		[] -> a
		|h::t -> aux(a+1) t
		in aux 0 c;;

(* union : 'a conjunto -> 'a conjunto -> 'a conjunto *)
let union (Conjunto c1) (Conjunto c2) =			
	let rec f aux conj1 = match conj1 with
		[] -> (Conjunto aux)
		| h1::t1 -> if (pertenece h1 (Conjunto aux)) then f aux t1
			     else f (h1::aux) t1
	in f c1 c2;;

(* interseccion : 'a conjunto -> 'a conjunto -> 'a conjunto *)
let rec interseccion (Conjunto c1) (Conjunto c2) = match c1 with
	[] -> Conjunto []
	| h::t -> if pertenece h (Conjunto c2) then
                         agregar h (interseccion (Conjunto t) (Conjunto c2))
                       else
                         interseccion (Conjunto t) (Conjunto c2);;
	
(* diferencia : 'a conjunto -> 'a conjunto -> 'a conjunto *)
let diferencia (Conjunto c1) (Conjunto c2) =				
	let rec f aux conj1 conj2 = match conj1,conj2 with
		[],_ -> Conjunto aux
		| h1::t1,h2::t2 -> if h1<>h2 then f aux conj1 t2
				    else f aux t1 conj2
		| h1::t1,[] -> f (h1::aux) t1 c2
	in f [] c1 c2;; 

                         
(* incluido : 'a conjunto -> 'a conjunto -> bool *)
let rec incluido (Conjunto c1) (Conjunto c2) = match c1 with
	[] -> true
	| h::t -> if (pertenece h (Conjunto c2)) then incluido (Conjunto t) (Conjunto c2)
			  else false;;

(* igual : 'a conjunto -> 'a conjunto -> bool *)
let igual (Conjunto c1) (Conjunto c2) = 
	if((diferencia (Conjunto c1) (Conjunto c2))= Conjunto []) then true else false;;



(* producto_cartesiano : 'a conjunto -> 'b conjunto -> ('a * 'b) conjunto *)
let producto_cartesiano (Conjunto c1) (Conjunto c2) = 
	let rec f aux conj1 conj2 = match conj1,conj2 with
		[],_ -> Conjunto (aux)
		| h1::_,h2::t2 -> f ((h1,h2)::aux) conj1 t2
		| _::t1,[] -> f aux t1 c2
	in f [] c1 c2;;
	
	
(* list_of_conjunto : 'a conjunto -> 'a list *)	
let list_of_conjunto = function (Conjunto c) -> c;;
