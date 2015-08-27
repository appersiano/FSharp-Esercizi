//Lezione 9

(*
ESERCIZIO 1	
===========

Consideriamo le definizioni

let f = fun x -> x + 1 ;;
let g = fun x -> x  +1 ;;

Le funzioni f e g sono uguali?
Cosa calcola g?

Dare qualche esempio di termine t tale per cui l'applicazione

  g t

ha senso.
*)

//Le funzioni f e g sono uguali?
//No, f rappresente l'incremento di 1, g prendo come parametro una f e gli applica +1 come secondo parametro

//Cosa calcola g?
//g calcola la funzione parziale

//Esempio di applicazione
// g f

(*
ESERCIZIO 2	
===========

2.1) Definire la funzione ricorsiva map tale che, 
data una funzione f e una lista ls aventi tipo

  f : 'a -> 'b      ls : 'a list

il valore di
 
   map f ls

e' la lista di tipo 'b list ottenuta applicando a ogni elemento x di ls la funzione f.

Quindi:

 map  f [ x0 ; x1 ; .... ; xn ]   =   [ f x0 ; f x1 ; ... ; f xn ]

Il tipo di map e':

   map : ('a -> 'b) -> 'a list -> 'b list

Notare che map e' una funzione higher-order.
*)

let rec map f list =
    match list with
    | [] -> []
    | x::xs -> f x :: map f xs;;

(*
2.2) Sia l1 la lista contenente i numeri da 1 a 10.

Applicando map a una opportuna funzione f e alla lista l1 costruire le seguenti liste
(scrivere f come funzione anonima):

l2 =  [1; 4; 9; 16; 25; 36; 49; 64; 81; 100] // quadrati dei primi 10 numeri

l3 = [(1, "dispari"); (2, "pari"); (3, "dispari"); (4, "pari"); (5, "dispari"); (6, "pari"); 
      (7, "dispari"); (8, "pari"); (9, "dispari"); (10, "pari")]

*)

let l1 = [1..10]

let l2 = map (fun x -> x*x) l1

let l3 = map (fun x -> if x % 2 = 0 then (x,"pari") else (x,"dispari") ) l1

(*
2.3) Consideriamo la lista

let names = [ ("Mario", "Rossi") ; ("Anna Maria", "Verdi") ; ("Giuseppe", "Di Gennaro")] ;;

Applicando map a una opportuna funzione e alla lista names, costruire la lista
 
names1 =  ["Dott. Mario Rossi"; "Dott. Anna Maria Verdi"; "Dott. Giuseppe Di Gennaro"]
*)

let names = [ ("Mario", "Rossi") ; ("Anna Maria", "Verdi") ; ("Giuseppe", "Di Gennaro")] ;;

map (fun x -> "Dott." + fst(x) + " " + snd(x) ) names

(*
i)    prop_map f (ls : int list)

map e List.map calcolano gli stessi valori
(piu' precisamente, le applicazioni 'map f ls' e 'List.map f ls' producono la stessa lista).
*)

let prop_map f (list : int list) =
    map f list = List.map f list

(*
ii)   prop_map_pres_len f (ls :int list)

La lista  'map f ls' ha la stessa lunghezza di ls
(per calcolare la lunghezza di una lista usare List.length)
*)

let prop_map_pres f (list : int list) =
    List.length list = List.length (map f list)

(*
ESERCIZIO 3	
===========

3.1) Definire la funzione ricorsiva filter tale che, 
 data una funzione pred (predicato)  e una lista ls  aventi tipo

  pred : 'a -> bool     ls :  'a list

il  valore di

   filter pred ls
   
e' la lista di tipo 'a list contenente gli elementi di ls che verificano pred.
La lista risultante contiene quindi gli elementi x di ls tali che pred x e' true  (pred funge da filtro).

Il tipo di filter e':

 filter: ('a -> bool) -> 'a list -> 'a list

ed e'  una funzione higher-order
*)

let rec filter pred list =
    match list with
    | [] -> []
    | x::xs when pred x -> x :: filter pred xs
    | x::xs -> filter pred xs

(*
3.2) Usando fiter, definire la funzione

   mult3 : int  -> int list

che costruisce la lista dei multipli di 3 compresi fra 1 e n
(applicare in modo opportuno filter sulla lista [1 .. n]).
*)

let mult3 n =
    let list = [1..n]
    filter (fun x -> x % 3 = 0) list;;
    
(*

QuickCheck
^^^^^^^^^^
Definire e  verificare con QuickCheck le seguenti  proprieta' di filter:

i) prop_filter pred (ls : int list) 

filter e List.filter calcolano gli stessi valori.

ii)  prop_filter_len pred (ls :int list)

La lista   'filter pred ls' non puo' essere piu' lunga della lista ls.
*)

let prop_filter pred list =
    List.filter pred list = filter pred list;;

let prop_filter_len pred (list : int list) =
    List.length list >= List.length (filter pred list)

(*
4.1) Definire la funzione ricorsiva filter1 analoga a filter in cui pero'

   filter1 pred ls = ( lsTrue, lsFalse )

dove:

- lsTrue    contiene gli elementi di ls che verificano pred
- lsFalse   contiene gli elementi di ls che non verificano pred

Il tipo di filter1 e':

   ('a -> bool) -> 'a list -> 'a list * 'a list
*)

let filter1 pred list =
    let rec go (l,l1,l2) =
        match l with
        | [] -> l1,l2
        | x::xs when pred x -> go (xs,l1@[x],l2)
        | x::xs -> go (xs,l1,l2@[x])
    go (list,[],[])

(*
4.2) Usando filter1 e le definizioni nell'esercizio precedente, costruire le coppie di liste

p1 =  ( [3; 6; 9; 12; 15; 18] , [1; 2; 4; 5; 7; 8; 10; 11; 13; 14; 16; 17; 19; 20] )
 //  ( multipli di 3 , non-multipli di 3 ) 
*)
filter1 (fun x -> x % 3 = 0) [1..20];;

(*
4.3) Usando filter1, definire la funzione
  
   multNonmult : int -> int list * int list

che, dato un intero n, partiziona la lista [1 .. n] 
nella coppia di liste  

    ( multipli di 3 , non-multipli di 3 ) 

Ad esempio:

   multNonmult 16 =   ( [3; 6; 9; 12; 15] , [1; 2; 4; 5; 7; 8; 10; 11; 13; 14; 16] )
*)

let multNoMult n =
    filter1 (fun x -> x % 3 = 0) [1..n];;

(*
QuickCheck
^^^^^^^^^^

Definire e  verificare con QuickCheck le seguenti  proprieta' di filter1:

i) prop_filter1_len pred (ls : int list)

Sia  (xs,ys) il risultato di 'filter1 pred ls'.
Allora la lista 'xs @ ys' ha la stessa lunghezza di ls.

*)

let prop_filter1_len pred (ls : int list) =
    List.length ls = List.length (fst(filter1 pred ls) @ snd(filter1 pred ls))
    
(*
t prop_filter1_app pred (ls :int list)

Sia  (xs,ys) il risultato di 'filter1 pred ls'.
Allora, ordinando le liste ls e  'xs @ ys', si ottiene la stessa lista
(per ordinare le liste usare List.sort).
*)

let prop_filter1_app pred (ls :int list) =
   List.sort ls = List.sort (fst(filter1 pred ls) @ snd(filter1 pred ls))

(*

ESERCIZIO 5	
===========

Definire la funzione 

     divisori : int -> int list

che, dato un intero n > 0, restituisce la lista dei suoi divisori.

Esempio:

 let d100 =  divisori 100 ;;
// val d100 : int list = [1; 2; 4; 5; 10; 20; 25; 50; 100]

Usando la funzione divisori, definire la funzione isPrime che determina se un intero  e' primo.
Notare che e' sufficiente scrivere una espressione booleana.
*)

let divisori n =
    let rec div (x,l1) =
        match x with
        | 0 -> l1
        | _ when n % x = 0 -> div (x-1,x::l1)
        | _ -> div (x-1,l1)
    div (n,[])

let isPrime x = List.length (divisori x) = 2
