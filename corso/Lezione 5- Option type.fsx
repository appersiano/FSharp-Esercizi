//Lezione 5

(*
ESERCIZIO 1
===========

Definire la funzione sommaArea che, date due figure fig1 e fig2,
restituisce la somma delle areee delle due figure, se  definita.
Il risultato deve essere un option type.
Per calcolare l'area, usare la funzione areaOpt definita a lezione. 

Esempi: 

let sum1 = sommaArea ( Rettangolo(2,5) , (Quadrato 10) ) ;;
//  val sum1 : float option = Some 110.0

let sum2 = sommaArea ( Rettangolo(2,-5),  (Quadrato 10) ) ;;
// val sum2 : float option = None

let sum3 = sommaArea ( Rettangolo(2, 5), (Quadrato -10) ) ;;
// val sum3 : float option = None

let sum4 =  sommaArea ( Triangolo(10,5), Triangolo(3,5)) ;;
// val sum4  : float option = Some 32.5
*)

//Precedentemente definiti a lezione
type figura = 
   | Rettangolo of  int * int      // base * altezza
   | Quadrato   of  int            // lato
   | Triangolo  of  int * int  ;;  // base * altezza

let area fig =
   match fig with
   | Rettangolo(b,h) -> float ( b * h )   
   | Quadrato lato   -> float (lato * lato )
   | Triangolo(b,h)  -> float ( b * h )  / 2.0 ;;

let areaOpt fig =
   match fig with
   | Rettangolo (b,h) | Triangolo(b,h)  ->
       if b < 0 || h < 0 then None
       else Some (area fig)
   | Quadrato lato ->
       if lato < 0 then None
       else Some (area fig) ;;

///////////////////////////////////////////////////////

let sommaArea (fig1,fig2) =
    match (areaOpt(fig1),areaOpt(fig2)) with
    | None,_ -> None
    | _,None -> None
    | _,_ -> let area1 = areaOpt (fst(fig1,fig2))
             let area2 = areaOpt (snd(fig1,fig2))
             Some (Option.get(area1) + Option.get(area2) )



(*
ESERCIZIO 2
===========

Definire le seguenti funzioni su liste in cui utilizzano option type;
non vanno usate le funzioni predefinite su liste.

Per ogni funzione viene  proposta qualche proprieta' da verificare con QuickCheck.

a) head_tailOpt: 'a list -> ('a option, 'a list option)


Restituisce la coppia (None,None) se la lista e' vuota, 
altrimenti la coppia composta dalla testa della lista e la coda della lista.

Esempi:

let h1 = head_tailOpt [ "uno" ; "due" ; "tre" ] ;;          
// val h1 : string option * string list option  =   ( Some "uno", Some ["due"; "tre"] )

let h2 = head_tailOpt ([] : int list) ;;
// val h2 : int option * int list option = (null, null)

Ricordarsi che non e' possibile applicare una funzione polimorfa a un argomento polimorfo,
per questo e' necessario assegnare alla lista vuota un tipo non polimorfo.
Notare che il valore  (None,None) di h2 e' stampato dall'interprete come (null, null).

In F# sono definite le analoghe funzioni

  List.head : 'a list -> 'a
  List.tail : 'a list -> 'a

che sulla lista vuota sollevano una eccezione.

QuickCheck
^^^^^^^^^^

Verifichiamo le seguente proprieta'  prop_ht_o di una lista ls.

*  Se  il valore head_tailOpt ls e' definito e 

      head_tailOpt ls = ( Some x , Some xs )

   allora  ls = x :: xs .


let prop_ht_o ls =
  match ( head_tailOpt ls) with
    Some x, Some xs -> ls = (x :: xs)
    | _,None | None,_ -> true ;;


Check.Quick prop_ht_o ;;

//////////////////////////////////////////////////////////
*)

//a)
#r @"C:\Users\alessandro.persiano\Box Sync\FPUsingF#exercise\FsCheck.dll";;
open FsCheck;;

let head_tailOpt list =
    match list with
    | [] -> None,None
    | x::xs -> Some(x),Some(xs)

let prop_ht_o ls =
  match ( head_tailOpt ls) with
    Some x, Some xs -> ls = (x :: xs)
    | _,None | None,_ -> true ;;


Check.Quick prop_ht_o ;;

(*/b)
lastOpt : 'a list -> 'a option   // funzione ricorsiva

Data una lista ls, last ls restituisce l'ultimo elemento di ls, se definito
(ossia, se la lista non e' vuota).
   
Esempi:

let l1 = lastOpt [ "uno" ; "due" ; "tre" ] ;;
// val l1 : string option = Some "tre"

let l2 = lastOpt ( [ ] : int list ) ;;
// val l2 : int option = None
    

QuickCheck
^^^^^^^^^^

Verifichiamo la seguente proprieta' prop_last di una lista ls:

* Se il valore lastOpt ls e' definito allora
    
      lastOpt ls  = head ( reverse ls ) 

Per calcolare head e reverse usiamo funzioni predefinite List.head e List.rev 

let prop_last ls =
  match (lastOpt ls) with
    Some x -> x = ( List.rev ls |> List.head )
    | None -> true ;;

Check.Quick prop_last  ;;

////////////////////////////////////////////////////////////
*)

let rec lastOpt list =
    match list with
    | [] -> None
    | [x] -> Some(x)
    | x::xs -> lastOpt(xs)

//con le funzioni predefinite
let lastOpt2 list =  
    match List.isEmpty list with
    | true -> None
    | false -> List.rev list |> List.head |> Some

//Fscheck proprietà

let prop_last ls =
  match (lastOpt ls) with
    Some x -> x = ( List.rev ls |> List.head )
    | None -> true ;;

Check.Quick prop_last  ;;

(*
////////////////////////////////////////////////////////////

c) catOpt: 'a option list -> 'a list // funzione ricorsiva

Restisuisce la lista degli elementi x tale che Some x e' nella lista,
eliminando i  None.

Esempi:
 
let lc1 = catOpt ( [Some 1 ; None ; Some 2 ; Some 3 ; None] ) ;;                          
// val lc1 : int list = [1; 2; 3]

let lc2 = catOpt ( [ None ; Some "cane" ; None ; None ; Some "gatto" ; Some "topo"] ) ;;  
// val lc2 : string list = ["cane"; "gatto"; "topo"]

QuickCheck
^^^^^^^^^^

Verifico la seguente proprieta' prop_cat di una lista ls:

*  length ls   >= length ( catOpt ls )

Per calcolare la lunghezza di una lista, usiamo la funzione predefinita  List.length

let prop_cat ls =
  ( List.length ls ) >= (catOpt ls |> List.length)  

Check.Quick prop_cat ;;


////////////////////////////////////////////////


let catOpt list =
    let rec go (l1,l2) =
        match fst(l1,l2) with
        | [] -> l2
        | x::xs -> match x with
                    | None -> go (xs,l2)
                    | Some(x) -> go (xs,(Option.get(x)::l2))
    go list,[]

*)

let catOpt list =
    let rec go (l1,l2) =
        match (l1,l2) with
        | [],_ -> l2
        | x::xs,_ when x = None -> go (xs,l2)
        | x::xs,_ -> go (xs,(l2@[Option.get(x)]))
    go (list,[])

//verifico proprietà
let prop_cat ls =
  ( List.length ls ) >= (catOpt ls |> List.length)  

(*
d) mynth : ('a list * int) -> 'a option  // funzione ricorsiva

Data una lista xs = [x0 ; x1 ; x2 ; ...]  e un intero n >= 0, 

  mynth (xs, n)

restituisce, se definito, l'elemento  xn (elemento di indice n).


Esempi:

let y1 = mynth (['a'..'z'], 0) ;;
// val y1 : char option = Some 'a'

let y2 = mynth (['a'..'z'], 2) ;;
// val y2 : char option = Some 'c'

let y3 = mynth (['a'..'z'], 30) ;;
// val y3 : char option = None

Confrontare con la funzione predefinita List.nth


QuickCheck
^^^^^^^^^^

i) Proprieta' prop_model della coppia  (ls : 'a list , n  : int):

*   Se  il valore mynth(ls,n) e' definito allora
    mynth(ls,n) da' lo stesso risultato che si ottiene usando List.nth


let mynth_prop_model (ls,n) =
  match mynth(ls,n) with
    | None -> true
    | Some m -> m = List.nth  ls n ;;

Check.Quick mynth_prop_model ;;
*)

let mynth (list,n) =
    let rec go (l1,counter) =
        match (l1,counter) with
        | x::xs,c when c = n -> Some x
        | x::xs,c -> go (xs,c+1)
        | _ -> None
    go (list,0)

//Verifico le proprietà
let mynth_prop_model (ls,n) =
  match mynth(ls,n) with
    | None -> true
    | Some m -> m = List.nth  ls n ;;

Check.Quick mynth_prop_model ;;


(*
ESERCIZIO 3 
===========

Definiamo i seguenti tipi:

type tagged = Int of int | Bool of bool;;

type tList = tagged list;;  // type abbreviation

Notare che il tipo tagged permette di rappresentare valori che possono essere int o bool,
useremo spesso questo tipo di definizioni.

Esempi di liste di tipo tList sono:

let tl1  = [ Int 0 ] ;;
let tl2  = [ Int 0 ; Int 1 ; Bool true ; Int 4 ; Bool false ] ;; 
let tl3  = [ Int 3 ;  Bool (4>6) ; Int (44-66) ; Bool ( 10 = 5 + 5 )  ];;

Definire la funzione ricorsiva

    printTl : tList -> string

che, data una lista tl : tList, restituisce una stringa che descrive il contenuto di tl
(valore e tipo di ciascun elemento) come mostrato negli esempi sotto.

Per evitare di duplicare codice, si consiglia di definire la funzione

  printVal : tagged -> string

che restituisce la stringa che descrive un valore di tipo tagged.

Esempi:

let s1 = printTl tl1 ;;
// val s1 : string =  "0 : int"

let s2 = printTl tl2 ;;
// val s2 : string = "0 : int; 1 : int; true : bool; 4 : int; false : bool"

let s3 = printTl tl3 ;;
//  val s3 : string =  "3 : int; false : bool; -22 : int; true : bool"
*)

type tagged = Int of int | Bool of bool;;

type tList = tagged list;;  // type abbreviation


let printVal x =
    match x with
    | Int x -> string(x) + " int"
    | Bool x -> string(x) + " bool"

let rec printTl list =
    match list with
    | [] -> ""             
    | x::xs -> let b = if xs = [] then "" else "; " 
               printVal x + b + printTl xs    
    
