(*Cargar la libreria*)

(*# load "talf.cma";;*)
open Conj;;
open Auto;;
open Ergo;;
open Graf;;


(*Definiciones dadas*)
type regla_gic =
Regla_gic of (simbolo * simbolo list);;
type gic =
Gic of (simbolo conjunto * simbolo conjunto * regla_gic conjunto *
simbolo);;
type arco_ap =
Arco_ap of (estado * estado * simbolo * simbolo * simbolo list);;
type ap =
Ap of (estado conjunto * simbolo conjunto * simbolo conjunto *
estado * arco_ap conjunto * simbolo * estado conjunto);;
type simbolo =
Terminal of string
| No_terminal of string;;

(*ap_of_gic : Auto.gic -> Auto.ap*)

let preparar_conjunto(Conjunto conj) = (Conjunto ((Auto.No_terminal "")::conj));;

let procesar_regla (Regla_gic (x,y)) = (Arco_ap(Estado "q1", Estado "q1", Terminal "", x, y));;

let reglas_terminales ter =
    let rec f aux lista = match lista with
        [] -> aux
        | h::t -> (f ((Arco_ap (Estado "q1", Estado "q1", h, h, []))::aux) t)
        in f [] ter;;

let obtener_reglas (Conjunto c) (Conjunto t) =
    let rec f aux lista = match lista with 
    [] -> (Conjunto (List.rev ((Arco_ap(Estado "q1",Estado "q2", Terminal "", No_terminal "", [No_terminal ""]))::aux)))
    | h::t -> (f (procesar_regla(h)::aux) t)
    in f ((Arco_ap(Estado "q0",Estado "q1",Terminal "", No_terminal "", [No_terminal ""]))::(reglas_terminales t)) c;;


let ap_of_gic (Gic (a,b,c, d)) = 
Ap (Conjunto[Estado "q0"; Estado "q1"; Estado "q2"],
         b,
         (preparar_conjunto a),
         Estado "q0",
         (obtener_reglas c b),
         No_terminal "",
         Conjunto [Estado "q2"]);;

(*Conjunto de reglas activadas al procesar dicha cadena*)

let rec lista_de_cadena ch = match ch with
    "" -> []
    | ch -> (String.make 1 (String.get ch 0 )) :: (lista_de_cadena (String.sub ch 1 ( (String.length ch)-1) ) );;

let eliminar st = (String.sub st 1 ((String.length st)-1));;

let obtener_arcos automata =
    let rec f aux partes cont = match partes, (cont>4) with
        [], _ -> (List. rev aux)
        | h::t, false -> (f aux t (cont+1))
        | h::t, true -> if((compare (eliminar h) "")==0) then (f aux t (cont+1))
                        else (f ((eliminar h)::aux) t (cont+1))
    in f [] (String.split_on_char ';' automata) 0;;

let obtener_estado_inicial automata =
    let rec f partes cont = match partes, (cont==3) with
        [], _ -> raise (Not_found)
        | h::t, false -> (f t (cont+1))
        | h::t, true -> (eliminar h) 
    in f (String.split_on_char ';' automata) 0;;

let arco_correcto arc ch st simb_pila=
    let rec f arco car estado cont pila = match arco, cont with
    [],_ -> false
    | h::t, 1 ->    if ((compare h estado)==0) then (f t car estado (cont+1) pila)
                    else false
    | h::t, 3 ->    if ((compare h car)==0) then (f t car estado (cont+1) pila)
                    else false
    | h::t, 4 ->    if((compare h pila)==0) then true
                    else false
    | h::t, _ -> (f t car estado (cont+1) pila)
    in f arc ch st 1 simb_pila;;

let obtener_estado_fin_arco arco =
    let rec f arc cont = match arc, cont with
    [], _ -> raise(Not_found)
    | h::t, 2 -> h
    | h::t, _ -> f t (cont+1)
    in f arco 1;; 

let imprimir_pila pila =
    let rec f aux lista = match lista with
    [] -> aux
    | h::t -> (f (aux^" "^h) t)
    in f "" pila;;

let resultado_del_arco arco =
    let rec f arc cont = match arc, cont with
        [], _ -> raise(Not_found)
        | h::t, 5 -> h
        | h::t, _ -> (f t (cont+1))
        in f arco 1;;


let pila_para_imprimir arco pila=
    if((compare (resultado_del_arco arco) "epsilon")==0) then (imprimir_pila (List.tl pila))
    else ((resultado_del_arco arco)^(imprimir_pila pila));;

let obtener_string arco pila car =
    car^": "^(List.hd arco)^" -> "^(obtener_estado_fin_arco arco)^ "-> Pila: "^(pila_para_imprimir arco pila);;

let rec anadir_a_salida car arcos estado_actual pila = match arcos with
    [] -> "error2"
    | h::t ->   if(arco_correcto (String.split_on_char ' ' h) car estado_actual (List.hd pila)) then (obtener_string (String.split_on_char ' ' h) pila car)
                else (anadir_a_salida car t estado_actual pila);;


let rec obtener_estado_actual ch arc st pila= match arc with
    [] -> "error3"
    | h::t ->   if(arco_correcto (String.split_on_char ' ' h) ch st (List.hd pila)) then (obtener_estado_fin_arco (String.split_on_char ' ' h))
                else (obtener_estado_actual ch t st pila);;

let obtener_imprimir final = 
    let rec f aux cad = match cad with
    [] -> aux
    | h::t -> f (aux^h^" \n ") t
    in f "" final;;


let ultimo_paso aux arc st = 
    (obtener_imprimir (List.rev aux))^st^"->"^(obtener_estado_actual "epsilon" arc st ["zeta"])^"-> Pila: zeta";;


let resultado_pila ch arco st pila =
    if((compare (resultado_arco ch arco st pila) "epsilon")==0) then (List.tl pila)
    else (resultado_arco ch arco st pila)::pila;;

let proceso_a_seguir cadena arcos estado_actual pila =
    let rec f aux ch arc st pil = match ch, pil with
        [], ["zeta"] -> (ultimo_paso aux arc st)
        | h::t, _ -> (f ((anadir_a_salida h arc st pil)::aux) t arc (obtener_estado_actual h arc st pil) (resultado_pila h arc st pil))
        in f [] cadena arcos estado_actual pila;;



let rec resultado_arco ch arc st pila= match arc with
    [] -> raise(Not_found)
    | h::t ->   if(arco_correcto (String.split_on_char ' ' h) ch st (List.hd pila)) then (resultado_del_arco (String.split_on_char ' ' h))
                else (resultado_arco ch t st pila);;



let reglas cadena automata = 
    if(escaner_ap (cadena) automata) 
        then (proceso_a_seguir (String.split_on_char ' ' (string_of_cadena cadena)) (obtener_arcos (string_of_ap automata)) 
        (obtener_estado_inicial (string_of_ap automata)) ["zeta"])
    else "La cadena no es valida para este automata";;