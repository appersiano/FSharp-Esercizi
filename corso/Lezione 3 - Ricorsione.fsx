//Lezione 3 - Ricorsione

(*
 Dati due interi m>=0 e n>= 0, la definizione ricorsiva del MCD fra m e n e':

MCD(m,n) =  n    se m=0

         =  MCD(n % m, m)   se m > 0 


Definire una funzione ricorsiva  mcd : int * int -> int
che calcola MCD(m,n) (si assume  m>=0 e n>= 0)

Usando la funzione mcd, definire una funzione (non ricorsiva)

   simplify : int * int -> int * int

che semplifica una frazione.

Piu' precisamente, dati due interi a >= 0 e  b>0, 
simplify (a,b) = (c,d) se e solo se c/d e' la frazione ottenuta semplificando a/b.

Esempi:

simplify (15,9) ;; 
val it : int * int = (5, 3)

simplify (7,5) ;; 
val it : int * int = (7, 5)
*)

let rec mcd (m,n) = 
    match (m,n) with
    | (0,n) -> n
    | (m,n) -> mcd (n % m, m);;

let simplify (n,d) = 
    match mcd(n,d) with
    | x -> (n/x,d/x)

let rec sum1 x =
    match x with
    | 0 -> 0
    | n -> n + sum1 ( n - 1 )

let rec sum2 (x,y) =   
    match (x,y) with
    | (a,b) when b < a -> failwith "y deve essere > x"
    | (a,b)  when a = b -> a
    | (a,b) -> a + sum2 ( x + 1, y );;

    
let rec fib x = 
    match x with
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n - 2) + fib (n - 1)
