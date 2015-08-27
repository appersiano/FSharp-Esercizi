//Lezione 16-17 Sequenze
 
let nat =  Seq.initInfinite (fun x -> x) ;;

(*
MAP
===

i) Definire la funzione ricorsiva higer-order

    map : ('a -> 'b) -> seq<'a> -> seq<'b>

che effettua la map di una sequenza infinita.

Piu' precisamente, data una sequenza infinita 

   sq = seq [ e0 ; e1 ; e2 ; .... ]   : seq<'a>

e una funzione f : 'a -> 'b, vale:

    map sq f   =  seq [ f(e0) ; f(e1) ; f(e2) ; .... ]   : seq<'b>

Notare che si *assume* che sq sia infinita. 
Questo implica che:

- la testa di sq e' sempre definita;
- la coda si sq e' a sua volta infinita,  quindi la chiamata ricorsiva sulla coda e' corretta
  (la condizione sul parametro sequenza e' verificata).
*)

let map f s = 
    let rec go n =
        seq{ 
            yield f (Seq.nth n s) 
            yield! go (n+1) 
            }
    go 1;;

(*
ii)  Applicare map alla sequenza infinita nat dei naturali 
per generare la sequenza infinita squares  dei quadrati dei naturali.
  
Verificare che la lista dei primi 15 elementi di squares e':

[0; 1; 4; 9; 16; 25; 36; 49; 64; 81; 100; 121; 144; 169; 196; 225; 256; 289; 324; 361]
 *)
 
nat |> map (fun x -> x*x) |> Seq.take 15 |> Seq.toList;;   

(*
iii)  Modificare la definizione di map in modo che funzioni correttamente 
anche su sequenze finite.

Rispetto a prima, occorre anche considerare il caso in cui la sequenza sq e' vuota
(infatti, dopo un certo numero di chiamate ricorsive su una sequenza finita, si genera la sequenza vuota).
Puo' essere utile usare la funzioni

   Seq.isEmpty : seq<'a> -> bool  // verifica se una  sequenza e' vuota
   Seq.empty   :  seq<'a>         // costante che definisce la sequenza vuota
*)

//sequenza finita // FA FARE
let seqd = seq{ yield 1
                yield 2
                yield 3
            }

let map2 f s = 
    let rec go n =
        seq{                              
                    yield f (Seq.nth n s) 
                    if Seq.nth (n+1) s <> Seq.empty then
                        yield! go (n+1) 
            }
    go 0;;

(*

FILTER
======

i) Definire la funzione ricorsiva

 filter : ('a -> bool) -> seq<'a> -> seq<'a>

che, dato un predicato pred : 'a -> bool e una sequenza infinita sq,
genera la sequenza degli elementi di sq che verificano sq.

ii) Applicare filter alla sequenza infinita nat dei naturali
per generare la sequenza dei multipli di 3 (0, 3, 6, ...)

Verificare che la lista dei primi 20 elementi della sequenza generata e'

 [0; 3; 6; 9; 12; 15; 18; 21; 24; 27; 30; 33; 36; 39; 42; 45; 48; 51; 54; 57]

 *)

let pred x = (x % 3 = 0)


let filter p s =
    let rec go n =
        seq { if p (Seq.nth n s) then yield (Seq.nth n s) 
              yield! go (n+1)
            }
    go 0;;


(*
SEQUENZA DI FIBONACCI
=====================

Lo scopo dell'esercizio e' definire la sequenza infinita

   fibSeq : seq<int> 

dei numeri di Fibonacci.

i) Definire la funzione ricorsiva

   fibFrom :  int -> int -> seq<int>

che, dati due interi a e b, genera la sequenza di Fibonacci i cui primi due numeri sono a e b,
ossia la sequenza di interi  x0, x1, x2, ... tale che

    x0  =  a
    x1  =  b
    xn  =  x(n-2) + x(n-1) per ogni  n >= 2

Ad esempio, la lista dei primi 10 elementi di

  fibFrom 5 10

e'  

 [5; 10; 15; 25; 40; 65; 105; 170; 275; 445]

Suggerimento
^^^^^^^^^^^^

Notare che
  
   fibFrom a b 

e' la sequenza 

  a ; b ; a + b  ; b + (a+b) ;  (a+b) + (b+(a+b)) ; .... 

Osservare come e' fatta la sottosequenza che parte da b:

  b ; a + b  ; b + (a+b) ;  (a+b) + (b+(a+b)) ; .... 
  *)

let rec fibFrom a b =
    seq {
        yield a
        yield b
        yield! fibFrom (a+b) (b+(a+b))
    }

(*
ii) Definire la funzione

  fib : int -> int

che, dato n >= 0, calcola il numero di Fibonacci di indice n.

Esempi:

fib 0 ;;   // 1
fib 1 ;;   // 1
fib 2 ;;   // 2
fib 3 ;;   // 3
fib 4 ;;   // 5
fib 10 ;;  // 89

Poiche' i numeri di Fibonacci crescono esponenzialmente,
conviene rappresentarli usando il tipo uint64 invece di int;
le costanti di tipo  uint64 hanno suffisso UL 
(ad esempio, la costante 1 di tipo uint64 e' 1UL).
*)

let fib n = Seq.nth n (fibFrom 0 1)
    

(*
SEQUENZA DELLE SOMME DI UNA SEQUENZA
====================================

i) Definire la funzione
   
   sumSeq : seq<int> -> seq<int>

che, data una sequenza infinita di interi,

  n0, n1, n2, n3, .....

costruisce la sequenza delle somme 

 n0, n0 + n1, n0 + n1 + n2, n0 + n1 + n2 + n3, ....

Suggerimento
^^^^^^^^^^^^

Consideriamo la sottosequenza che parte da n0 + n1
   
  n0 + n1, n0 + n1 + n2, n0 + n1 + n2 + n3, ...
  
Tale sequenza puo' essere ottenuta come sumSeq della sequenza infinita

  n0 + n1, n2,  n3, ...
*)

let sumSeq s =
    let rec go n tots =
        seq {
            let sum =  (Seq.nth n s) + tots
            yield sum
            yield! go (n+1) sum
        }     
    go 0 0;;

// 0 + 0
// 1 + 0
// 2 + 1


//Esercizio 2
(*
CRIVELLO DI ERATOSTENE
======================

Scopo dell'esercizio e'  definire la sequenza infinita dei numeri primi
implementando la procedura nota come "crivello di Eratostene".
Tale procedura  lavora su insiemi infiniti;  nelle implementazioni in genere 
si fissa un limite superiore sui numeri da considerare.
Con le sequenze si puo' lavorare direttamente sugli  insiemi infiniti.


i) Definire la funzione 

    sift : int -> seq<int> -> seq<int>
  
che, dati un intero a > 0 e  una sequenza infinita di interi sq,
restituisce la sequenza di interi  ottenuta eliminando da sq i multipli di a.

Ad esempio

let nat = sequenza infinita dei numeri naturali 0, 1, 2, 3, ...
let sq1 = sift 2 nat
let sq2 = sift 3 nat

Verificare che:

- La lista dei primi 10 elementi di sq1 e'

    [1; 3; 5; 7; 9; 11; 13; 15; 17; 19]

- La lista dei primi 15 elementi di sq2 e'

    [1; 2; 4; 5; 7; 8; 10; 11; 13; 14; 16; 17; 19; 20; 22]
*)

let sift a s =
    let rec go n =
        seq {
             let element = Seq.nth n s
             if element % a <> 0 then yield element
             yield! go (n+1)
        }
    go 0;;

let sq1 = sift 2 nat
sq1 |> Seq.take 10 |> Seq.toList

let sq2 = sift 3 nat
sq2 |> Seq.take 15 |> Seq.toList

(*
i) Definire la funzione ricorsiva

    sieve : seq<int> -> seq<int>

che applica a una sequenza infinita di interi  il crivello di Eratostene.
Piu' precisamente, data una sequenza sq di interi, va eseguito il seguente processo infinito:

------------------------------------------------------------------------------------
1. x0  :=  primo elemento di sq 
2. Cancella da sq i multipli propri di x0
   (ossia, x0 rimane in sq e sono cancellati da sq i multipli di x0 maggiori di x0) 
3. Ripeti il processo da 1  
------------------------------------------------------------------------------------

Al passo 2 usare la funzione sift.
*)

let sieve seq1 =
    let rec go n s =
        seq {
            let a = Seq.nth n s           
            yield a
            let aseq = sift a s                           
            //yield! aseq
            yield! go (n) aseq
        }
    go 0 seq1;;

(*
iii) Sia nat2 la sequenza infinita degli interi n >= 2.
La sequenza infinita primes dei numeri primi puo' essere costruita applicando sieve a nat2.

Verificare che la lista dei primi 10 numeri primi e'
 
 [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
 *)

let nat2 = Seq.initInfinite (fun x -> x + 2)

nat2 |> sieve |> Seq.take 10 |> Seq.toList;;

(*
iv) L'implementazione fornita del Crivello di Eratostene e' poco efficiente,
in quanto le sequenza usate vengono continuamente rigenerate.

Per migliorare l'efficienza si puo' usare il meccanismo di caching, 
che evita di calcolare piu' volte lo stesso elemento di una sequenza.

Si puo' procedere come segue:

a) Si definisce la versione cached della funzione sift

let siftC a sq = Seq.cache  ( sift a sq )
*)

let siftC a sq = Seq.cache  ( sift a sq )

(*
b) Si definisce la funzione sieveC, analoga a sieve, in cui pero' vengono usate le funzioni siftC e sieveC.
*)

let sieveC seq1 =
    let rec go n s =
        seq {
            let a = Seq.nth n s           
            yield a
            let aseq = siftC a s                           
            //yield! aseq
            yield! go (n) aseq
        }
    go 0 seq1;;

(*
c) Si definisce la sequenza cached dei numeri primi usando sieveC:
*)

let primesC = Seq.cache (sieveC nat2)

(*

*)

type lazyBool = unit -> bool
