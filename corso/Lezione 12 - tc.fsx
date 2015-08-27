//Lezione 12

//da fare incluso nella lezione
// EXERCISE: REWRITE TPCHK WITH VALUE CARRYING EXCEPTIONS, AND A UTILITY TO PRINT THEM OUT

//Esercizio tc.txt

//Si considerino i tipi intero e liste di interi
type tp =
    | INT
    | LSINT

//ed un linguaggio di espressioni che contenga constanti intere, somme, la
//lista vuota, l'operazione di cons,  le operazioni di testa e di coda di una lista:
type exp =      
  | C of int
  | Sum of exp * exp
  | Nil of (exp list)
  | Cons of  (exp * exp)
  | Head of  exp 
  | Tail of  exp 

let rec tpcheck e =
    match e with
    | C e1 -> Some INT    
    | Sum (e1,e2) -> let (t1,t2) = (tpcheck e1 , tpcheck e2 )
                     if t1 = Some INT && t2 = Some INT then t1
                     else None
    | Nil e1 -> Some LSINT
    | Cons (e1,e2) -> let (t1,t2) = (tpcheck e1 , tpcheck e2 )
                      if t1 = Some INT && t2 = Some LSINT then Some LSINT
                      else None
    | Head e1 -> let t1 = tpcheck e1 
                 if t1 = Some LSINT then Some INT
                 else None
    | Tail e1 -> let t1 = tpcheck e1 
                 if t1 = Some LSINT then Some LSINT
                 else None

//Type checker using Exception
exception T1ERR of (exp * tp) 
exception T2ERR of (exp * tp) 

let rec tpcheckEx e =
    match e with
    | C e1 -> C e1    
    | Sum (e1,e2) -> match (tpcheck e1 , tpcheck e2 ) with
                     | (C n1,C n2) -> C (n1 + n2)
                      (_,_) -> raise NotAInt 
                     
    | Nil e1 -> Some LSINT
    | Cons (e1,e2) -> let (t1,t2) = (tpcheck e1 , tpcheck e2 )
                      if t1 = Some INT && t2 = Some LSINT then Some LSINT
                      else None
    | Head e1 -> let t1 = tpcheck e1 
                 if t1 = Some LSINT then Some INT
                 else None
    | Tail e1 -> let t1 = tpcheck e1 
                 if t1 = Some LSINT then Some LSINT
                 else None

let rec eval e=
    match e with
    | C e1 -> C e1        
    | Sum(e1,e2) -> let (C n1) = eval e1 
                    let (C n2) = eval e2 
                    C (n1 + n2)
    | Nil e1 -> Nil e1
    | Cons (e1,e2) -> let (C n1) = eval e1
                      let (Nil n2) = eval e2
                      Nil ((C n1)::n2)    
    | Head e1 -> let (Nil e1) = eval e1                        
                 List.head e1
    | Tail e1 -> let (Nil e1) = eval e1                        
                 Nil (List.tail e1)

    //Test
    let a1 = eval ( Sum( C 3, C 9 ) )// = C 12
    let a2 = eval ( Cons (a1,Nil []) ) // = Nil [C 12]
    let a3 = eval ( Head ( a2 ) ) // = C 12
    let a4 = eval ( 
                    Tail ( 
                        Cons ( a1, 
                            Cons ( 
                                ( Sum (C 2, C 2) 
                                , Nil []
                                ) 
                                ) 
                             ) ) ) 
