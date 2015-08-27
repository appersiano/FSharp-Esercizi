(*
1) Scrivere una funzione costo che calcola il costo di un prodotto.

La funzione costo ha come argomento una tupla

   ( cod , prezzoDi, scontoDi )

dove:

-  cod : string
   codice del prodotto di cui si vuole calcolare il costo.
-  prezzoDi : string -> float
   funzione che, dato il codice di un prodotto (stringa), ne calcola il prezzo (float).  
-  scontoDi : string -> int
   funzione che, dato il codice di un prodotto (stringa), ne calcola la percentuale di sconto da applicare
   (intero compreso fra 0 e 100).

Notare che il tipo della funzione costo e'
  string * (string -> float) * (string -> int)  -> float
*)

// prezzoDi : 100 =  x :: scontoDi

let costo (cod : string, prezzoDi, scontoDi ) = 
    let pr = prezzoDi cod
    let sc = scontoDi cod
    pr - ( pr * (float sc) / 100.0 );;
