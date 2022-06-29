(*Funciones proporcionadas*)

(*cadena_of_string y cadena_of_file -> simbolo list*)
(*er_of_string y er_of_file -> tipo er*)
(*af_of_string y af_of_file -> tipo af*)
(*dibuja_af -> visualizar grafica*)
(*escaner_af -> verifica si es aceptada por automata finito*)

(*Cargar la libreria*)

(*# load "talf.cma";;*)
open Conj;;
open Auto;;
open Ergo;;
open Graf;;


(* Funcion traza_af*)

let print_list lista =  
   let rec f aux l = match l with
      [] -> aux
      | (Estado h)::t -> f (aux^h) t
      in f "" lista;;

let print_simbolo lista =
let rec f aux l = match l with
      [] -> aux
      | (Terminal h)::t -> f (aux^h) t
      | (No_terminal h)::t -> f(aux^h) t
      in f "" lista;;


let traza_af cadena (Af (_, _, inicial, _, finales) as a) =

   let rec aux w acum = match w with

        (Conjunto [], _) ->
           "No es valido"

      | (actuales, []) ->
           if not (es_vacio (interseccion actuales finales)) then acum
           else "No es valido"

      | (actuales, simbolo :: t) ->
           aux ((epsilon_cierre (avanza simbolo actuales a) a), t) (acum^" -> ("^(print_list (list_of_conjunto actuales))^","^(print_simbolo t)^")")

   in
      aux ((epsilon_cierre (Conjunto [inicial]) a), cadena) ""
   ;;



(*Funcion cartesiano_af*)

let def_estados x =
   let rec f aux list = match list with
   [] -> aux
   | h::t -> f (fst(h)^snd(h)^" "^aux) t
   in f "" (List.rev(x));;

let unir_arco x aux = match x with
(Arco_af ((Estado x), (Estado y), (Terminal z))) -> aux^x^" "^y^" "^z^";"
|(Arco_af ((Estado x), (Estado y), (No_terminal z))) -> aux^x^" "^y^" "^z^";";;

let definir_arcos lista =
   let rec f aux list = match list with
   [] -> aux
   |(Arco_af ((Estado x), (Estado y), (Terminal z))) ::t -> (f (unir_arco (Arco_af ((Estado x), (Estado y), (Terminal z))) aux) t)
   |(Arco_af ((Estado x), (Estado y), (No_terminal z))) ::t -> (f (unir_arco (Arco_af ((Estado x), (Estado y), (No_terminal z))) aux) t)
   in f "" lista;;


let division x = match x with
(Arco_af (Estado a, Estado b, Terminal c)) -> [a;b;c]
| (Arco_af (Estado a, Estado b, No_terminal c)) -> [a;b;c];;


let convertir_a_lista (Conjunto a) = 
   let rec f aux lista = match lista with
   [] -> aux
   | h::t -> f ((division h)::aux) t
   in f [] a;;


let juntar x y = x^y;;

let rec salida conjunto x s = match conjunto with
   [[]] -> "Elemento no encontrado"
   | [a;b;c]::t -> if (a=x & c=s) then b
                   else (salida t x s);;

let rec list_car ch = match ch with
    | "" -> []
    | ch -> (String.make 1 (String.get ch 0 )) :: (list_car (String.sub ch 1 ( (String.length ch)-1) ) );;

let aux_list_car lista = 
   let rec f aux list = match list with
      [] -> aux
      | h::t -> (f ((list_car h)::aux) t)
      in f [] lista;;


let aux1 (Conjunto c1) (Conjunto c2) =
   let rec f aux conj1 conj2 = match conj1, conj2 with
   [], _ -> (def_estados (List.rev(aux)))
   | (Estado h1)::_, (Estado h2)::t2 -> f ((h1,h2)::aux) conj1 t2
   | _::t1,[] -> f aux t1 c2
   in f [] c1 c2;;

let aux2 (Conjunto c1) (Conjunto c2) =
   let rec f aux conj1 conj2 = match conj1, conj2 with
   [], _ -> (def_estados (List.rev(aux)))
   | (Terminal h1)::_, (Terminal h2)::t2 -> f ((h1,h2)::aux) conj1 t2
   | (Terminal h1)::_, (No_terminal h2)::t2 -> f ((h1,h2)::aux) conj1 t2
   | (No_terminal h1)::_, (Terminal h2)::t2 -> f ((h1,h2)::aux) conj1 t2
   | (No_terminal h1)::_, (No_terminal h2)::t2 -> f ((h1,h2)::aux) conj1 t2
   | _::t1,[] -> f aux t1 c2
   in f [] c1 c2;;

let aux3 (Estado a1) (Estado a2) = a1^a2;;

let aux4 (Conjunto c1) (Conjunto c2) final simbolos =
   let rec f aux conj1 conj2 lista simb = match lista, simb with
      [], h::t -> (definir_arcos aux)
      | [a;b]::_, x::t -> f ((Arco_af ((Estado (a^b)), (Estado ((salida conj1 a x)^(salida conj2 b x))), (Terminal x)))::aux) conj1 conj2 lista t
      | _::t, [] -> f aux conj1 conj2 t simb
   in f [] (convertir_a_lista (Conjunto c1)) (convertir_a_lista (Conjunto c2)) (aux_list_car (String.split_on_char ' ' final)) (String.split_on_char ' ' simbolos);;


let cartesiano_af (Af (estados1, lenguaje1, inicial1, arcos1, final1) as a1) (Af (estados2, lenguaje2, inicial2, arcos2, final2) as a2) =
   let f st1 st2 = match st1, st2 with
      [],_ -> af_of_string (string_of_cadena st2)
      | _, [] -> af_of_string (string_of_cadena st1)
      | _, _ -> af_of_string((aux1 estados1 estados2)^ ";" ^ (aux2 lenguaje1 lenguaje2) ^ ";" ^ (aux3 inicial1 inicial2) ^ ";" ^ (aux1 final1 final2) ^ ";" ^ (aux4 arcos1 arcos2) ^ ";")
   in f (cadena_of_string (string_of_af a1)) (cadena_of_string (string_of_af a2));;



(* Opcional *)

(* Sobre el lenguaje {a,b,c}*)
(* El automata a1 aceptara aquellas cadenas que empiecen por a y acaben por c*)
(* El automata a2 aceptara aquellas cadenas que contengan exactamente dos b*)
(* El automata a12 acepta cadenas que cumplen ambas propiedades*)




