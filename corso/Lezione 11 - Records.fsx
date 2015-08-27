//Lezione 11

type valutazione = {stud : string ;  matr : int ; voto : int} ;;

let vals = [
  {stud="Bianchi"; matr=101 ; voto = 24} ;
  {stud="Brambilla"; matr=150 ; voto = 28} ;
  {stud="Rossi"; matr=153 ; voto = 15} ;
  {stud="Verdi"; matr=110 ; voto = 28} ;
  {stud="Brambilla"; matr=107 ; voto = 17} ;
  {stud="Verdi"; matr=190 ; voto = 28} ;
  {stud="Ferrari"; matr=160 ; voto = 18} ;
  {stud="Verdi"; matr=180 ; voto = 18} ;
  {stud="Rossi"; matr=111 ; voto = 15};
  {stud="Gialli"; matr=135 ; voto = 15} ;
  {stud="Neri"; matr=122 ; voto = 10}
  ] ;;

(*
1) Definire la funzione

   getMatrVoti : valutazione list -> (int * int) list


che, data una lista di valutazioni, estrae le coppie (matricola,voto)
contenute nella lista

Esempio:

let voti = getMatrVoti vals ;; 
// [(101, 24); (150, 28); (153, 15); (110, 28); (107, 17); (190, 28); (160, 18); (180, 18); (111, 15); (135, 15); (122, 10)]
*)

let getMatrVoti list =
    let rec loop l1 l2 =
        match l1 with
        | [] -> l2
        | x::xs -> match x with
                   {stud = _ ; matr = m ; voto = v} -> loop xs (l2@[(m,v)])
    loop list []

//Usando le funzioni HO
let getMatrVotiHO l = List.foldBack (fun x acc -> (x.matr,x.voto)::acc) l []

(*
 Uno studente e' promosso se ha ottenuto un voto >= 18.

Definire la funzione

   isPromosso : valutazione -> bool

che determina se lo studente nella valutazione e' promosso.

Esempio

 isPromosso {stud="Rossi"; matr=103 ; voto = 15} ;; // false 
 isPromosso {stud="Ferrari"; matr=190 ; voto = 18} ;; // true
 *)

let isPromosso s = s.voto >= 18

    
(*
Usando la funzione isPromosso e una opportuna funzione higher-order, 
definire la funzione
  
    getPromossi : valutazione list -> valutazione list

che estrae da una lista di valutazioni la sottolista con le valutazioni
degli studenti promossi e l'analoga funzione

    getBocciati : valutazione list -> valutazione list
*)

let getPromossi list = List.filter (fun x -> isPromosso x ) list
let getBocciati list = List.filter (fun x -> not (isPromosso x) ) list

(*
Usando le funzioni definite sopra costruire le liste:

plist :  lista delle coppie (matr,voto) degli studenti promossi
// [(101, 24); (150, 28); (110, 28); (190, 28); (160, 18); (180, 18)]


blist :  lista delle coppie (matr,voto) degli studenti bocciati
//  [(153, 15); (107, 17); (111, 15); (135, 15); (122, 10)]
*)

let plist = vals |> getPromossi |> getMatrVoti
let blist = vals |> getBocciati |> getMatrVoti

(*
3) La funzione

  List.sortWith : ('a -> 'a -> int) -> 'a list -> 'a list

permette di ordinare una lista usando una  funzione di ordinamento

    compareTo : 'a -> 'a -> int
    
passata come parametro.
Piu' precisamente,  la lista e' ordinata usando l'ordinamento < tale che

    x1 < x2    se e solo se    compareTo x1 x2 <= -1 


Definire la funzione

    mysort : valutazione list -> valutazione list

che ordina una lista di valutazioni in questo modo:
- ordine decrescente rispetto al voto
- a parita' di voto, ordine crescente rispetto alla matricola.

Per definire mysort, applicare List.sortWith una opportuna funzione

    cmp :  valutazione -> valutazione -> int


Costruire la lista

risultati : coppie (matr,voto) nella lista ordinata con mysort
// [(110, 28); (150, 28); (190, 28); (101, 24); (160, 18); (180, 18); (107, 17); (111, 15); (135, 15); (153, 15); (122, 10)]
*)

let cmp val1 val2 =
    let min = val1.voto < val2.voto
    match min with
    | true -> -1
    | false -> let m = val1.matr < val2.matr               
               match m with
               | true -> -1
               | false -> 1
               


let mysort x = List.sortWith cmp x

vals |> mysort |> getMatrVotiHO |> List.rev;;


(*
Definire la funzione iterativa

    isuml :  int list -> int

che calcola la somma degli elementi di una lista di interi.

Usare la funzione ausiliaria  

     sumlA : int list * int   -> int

in cui nel secondo parametro viene accumulata la somma degli elementi.

Confrontare sperimentalmentele   isuml e  

let rec suml = function
    | [] -> 0
    | x :: xs -> x + suml xs;;

Nell'interprete dare il comando 

 #time

che, dopo ogni computazione, stampa alcuni dati sulle risorse utilizzate (tempo CPU, uso garbage collector, ecc.)

Provare ad eseguire delle chiamate della forma

  suml [ 1 ..K ]      
 isuml [ 1 .. K ]

con K intero grande a piacere.
Tenere presente che le liste sono costruite nello heap.
*)


let isuml list =
    let rec sumlA (l1,t) =
        match l1 with
        | [] -> t
        | x::xs -> sumlA (xs,(x+t))
    sumlA (list,0)
