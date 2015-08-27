//Lezione 13

//apply.txt
(*

Consideriamo la funzione

   apply : ('a -> 'a -> 'a) -> 'a list -> 'a list -> 'a list

definita come segue.

Siano

  ls1 = [x0 ; x1 ; x2 ; .... ]
  ls2 = [y0 ; y1 ; y2 ; .... ] 
 
due liste di tipo 'a e  f una funzione di tipo  'a -> 'a -> 'a.
L'applicazione 

     apply f ls1 ls2

costruisce la lista

  [ f x0 y0 ; f x1 y1 ; f x2 y2 ; ..... ]

ottenuta applicando f agli elementi della lista nella stessa posizione.
Se una lista termina, la lista costruita e' completata con gli elementi della lista piu' lunga.

Esempi:

  list1  =  [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
  list2  =  [1; 2; 3; 4; 5]

apply (+) list1 list1 ;;  //  [2; 4; 6; 8; 10; 12; 14; 16; 18; 20]
apply (+) list1 list2 ;;  //  [2; 4; 6; 8; 10; 6; 7; 8; 9; 10]
apply (+) list2 list1 ;;  //  [2; 4; 6; 8; 10; 6; 7; 8; 9; 10]
*)

let list1 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
let list2  =  [1; 2; 3; 4; 5]

(*
i) Definire la funzione ricorsiva 

    apply1 : ('a -> 'a -> 'a) -> 'a list -> 'a list -> 'a list

che implementa apply (ricorsione semplice senza funzioni ausiliarie)
*)


let rec apply f l1 l2 =
        match (l1,l2) with
        | [],y::ys -> y::apply f [] ys
        | x::xs,[] -> x::apply f xs []
        | x::xs,y::ys -> (f x y)::apply f xs ys
        | _ -> []

(*
ii) Definire la funzione 

    apply2 : ('a -> 'a -> 'a) -> 'a list -> 'a list -> 'a list
   
corrispondente alla versione iterativa di apply.

Occorre definire una funzione ricorsiva ausiliaria applyA con ricorsione in coda,
in cui la lista e' costruita gradualmente mediante un parametro che funge da accumulatore.
*)

let Iapply f l1 l2 =
        let rec applyA l1 l2 acc =
            match (l1,l2) with
            | [],y::ys -> applyA [] ys (y::acc)
            | x::xs,[] -> applyA xs [] (x::acc)
            | x::xs,y::ys -> applyA xs ys ((f x y)::acc)
            | _ -> List.rev acc
        applyA l1 l2 []

Iapply (+) list1 list1 ;;  //  [2; 4; 6; 8; 10; 12; 14; 16; 18; 20]
Iapply (+) list1 list2 ;;  //  [2; 4; 6; 8; 10; 6; 7; 8; 9; 10]
Iapply (+) list2 list1 ;;  //  [2; 4; 6; 8; 10; 6; 7; 8; 9; 10]
