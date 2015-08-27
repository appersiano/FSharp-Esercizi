/////////////////////////
// Esercizi Capitolo 4 //
/////////////////////////

// Es 4.1
//Declare function upto: int -> int list such that upto n = [1; 2; …; n].
let upTo n =
    let rec go x list=
        match x with
        | 0 -> list
        | x -> go (x-1) (x::list)
    go n [];

//4.2
let downTo n =
    let rec go x list=
        match x with
        | 0 -> list
        | x -> go (x-1) (list@[x])
    go n [];

//4.3
let isEven n = n % 2 = 0;;

//lista dei numeri pari fino ad n
let evenN1 n =
    let rec go x list =
        match x with
        | x when x > n -> list
        | x when isEven x -> go (x+1) (list@[x])       
        | _ -> go (x+1) list
    go 1 [];;

//Lista dei primi n numeri pari
let evenN n =
    let rec go (x,countEven : int) list =
        match x,countEven with
        | x,c when snd(x,c) = n -> list
        | x,c when isEven (fst(x,c)) -> go (x+1,c+1) (list@[x])       
        | _ -> go (x+1,countEven) list
    go (1,0) [];;

//4.4 recupera da stackoverflow
let rec altsum2 = function
    | [] -> 0
    | x0::xs -> x0 - altsum2 xs ;;

//4.5
let rec rmOdd list=
    match list with
    |[] -> []
    |x:: y:: xs  -> x:: rmOdd  (xs)
    |x:: xs -> x:: rmOdd (xs)

//4.6
let rec rmEven list=
    match list with
    |[] -> []
    |x::xs when x % 2 = 0  -> x:: rmEven  (xs)  
    |x::xs ->  rmEven(xs)

//4.7
let rec multiplicity x list=
    match list with
    | [] -> 0
    | x1::xs when x1 = x -> 1 + multiplicity x xs
    | x1::xs -> multiplicity x xs

//4.8
let split (list : 'a list)=
    let rec go l (l1,l2)=
        match l with            
        | [] -> (l1,l2)
        | [x] -> (l1@[x],l2)        
        | x1::x2::xs -> go xs (l1@[x1],l2@[x2])
    go list ([],[]);;

//4.9
let zip l1 l2=
    if List.length l1 <> List.length l2 then
        failwith "Le liste sono di lunghezza diversa"
    else
        let rec go (a,b) lu=
            match (a,b) with          
            | (x::xs,y::ys) -> go (xs,ys) (lu@[(x,y)])
            | _,_-> lu
        go (l1,l2) [];;
    ;;

//4.10
let rec prefix l1 l2 =
    match l1,l2 with    
    | x::xs,y::ys when ys = [] && xs <> [] -> false
    | x::xs,y::ys when x = y -> true && prefix xs ys
    | x::xs,y::ys when x <> y -> false && prefix xs ys    
    | _,_ -> true

//4.11
//1
let rec count (list,x) =
    match list with
    | [] -> 0
    | x1::xs when x1 = x -> 1 + count(xs,x)
    | x1::xs -> count (xs,x)

//2
//insert
let rec insert (list : int list,x) =
    match list,x with
    | [],vl -> []
    | x1::[],vl when x1 <= vl -> x1::vl::[]    
    | x1::x2::xs,vl when x1 <= vl && vl <= x2 -> x1::x::x2::xs        
    | x1::x2::xs,vl -> x1::insert(x2::xs,x)    
    | _ -> []   
        
//3
//intersect
let rec intersect (l1,l2) =
    match l1,l2 with
    | [],[] -> [] : int list
    | x::xs as xl,y::ys when x = y -> x::intersect(xl,ys) 
    | x::xs,ys -> intersect(xs,ys)
    | _ -> []

//4
//plus
let rec plus (l1,l2) =
    match l1,l2 with
    | [],[] -> [] : int list
    | x::xs,y::ys when x <= y -> x::y::plus(xs,ys)
    | x::xs,y::ys when x >= y -> y::x::plus(xs,ys)
    | _ -> []

//5
//minus 
//DA RIVEDERE , NON FUNZIONA
let rec minus (l1,l2) =
    match l1,l2 with
    | [],[] -> []
    | x::xs,[] -> x::minus(xs,l2) 
    | x::xs,y::ys when x = y -> minus(xs,ys)                
    | x::xss as xs,y::ys when x <> y -> minus(xs,ys)   
    | _ -> []

  // minus ([1;1;1;2;2],[1;1;2;3])

//Es 4.12 
let rec sum p list =
    match list with  
    | [] -> 0
    | x::xs when p x -> x + sum p xs
    | x::xs -> sum p xs

//> sum (fun x -> x > 1) [1;2;3];;
//val it : int = 5

//4.13
//a - smallest
let smallest list =
    let rec finds (l1,x)=
        match l1,x with
        | [],s -> s
        | x::xs,s when x < s -> finds (xs,x)
        | x::xs,s -> finds (xs,s)
    finds (list,List.nth list 0)

//b - delete
let delete (a,list) =
    let rec del (c,l1,lres) =
        match (c,l1,lres) with
        | 1,l1,lres          -> lres
        | 0,x::xs,lres when x = a -> del (1,xs,lres@xs)
        | 0,x::xs,lres            -> del (0,xs,lres@[x]) 
        | _,_,_ -> failwith "Inserisci una lista con n>0 elementi"  
    del (0,list,[])

//c - sort
let weaksort list =
    let rec ws l1 l2_s=
        match l1 with
        | []    -> l2_s
        | x::xs -> let sm = (smallest (x::xs))
                   ws (delete (sm,x::xs)) (l2_s@[sm])
    ws list [];;

//Rircorda....esiste List.sort !

//4.14
let smallest list =
    let rec finds (l1,x)=
        match l1,x with
        | [],s -> Some s
        | x::xs,s when x < s -> finds (xs,x)
        | x::xs,s -> finds (xs,s)
    finds (list,List.nth list 0)

//4.15
let revrev ll =
    let rec rev_list l1 lreversed= 
        match l1 with
        | [] -> lreversed
        | x::xs -> rev_list xs lreversed@[x]
    
    let rec rev_all ll res =
        match ll with
        | [] -> res
        | x::xs -> rev_all xs ((rev_list x [])::res)
    rev_all ll [];;


//4.16,4.17 --> da fare
