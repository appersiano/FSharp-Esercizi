/////////////////////////
// Esercizi Capitolo 2 //
/////////////////////////

//Esercizio 2.1
let f = function
    | n when (n % 2 = 0 || n % 3 = 0) && n % 5 <> 0 -> true
    | _ -> false;;

    //f(24) = true
    //f(27) = true
    //f(29) = false
    //f(30) = false

//Esercizio 2.2
let rec pow = function
    | (_,0) -> ""
    | (s,n) -> s + pow(s,n-1);;

//Esercizio 2.3
let isIthChar (str:string,i,ch)= 
    match str.[i] = ch with
    | true -> true
    | _ -> false        

//Esercizio 2.4
let rec occFromIth(str : string,i,ch : char) =
    match (str,i,ch) with
    | (s,i,_) when i >= s.Length -> 0
    | (s,i,c) -> if str.[i] = ch
                    then 1 + occFromIth(s,i+1,ch) 
                    else 0 + occFromIth(s,i+1,ch);;
                
//Esercizio 2.5
let occInIth (str,c) = occFromIth(str,0,c);;
       
//Esercizio 2.6
// true if and only if d is not a divisor of n
let notDivisible(d,n) = n % d <> 0;;

//Esercizio 2.7

        //Esercizio 2.7.1
        let rec test = function
            | (a,b,c) when a<=b -> failwith "a must be <= of b"            
            | (a,b,c) -> notDivisible(a, c) && test(a + 1, b, c)
                    
        //Esercizio 2.7.2
        //da ottimizzare, sarebbe meglio fermarsi al primo true
        //se si trova un numero che è un divisore...perchè andare avanti?
        let prime n =
            let rec divide x =
                match x with
                | 0 -> false                              
                | 1 -> true                
                | x -> notDivisible(x,n) && divide (x-1)
            divide (n-1)

        //Esercizio 2.7.3
        let nextprime n = 
            let rec check i =
                match prime(i) with
                | true -> i
                | _ -> check (i+1)          
            check (n+1);;

//Esercizio 2.8
let rec bin(n, k) =
    match (n, k) with
    | (row, 0) -> 1
    | (row, col) when col = n -> 1
    | (row, col) -> bin(n - 1, k - 1) + bin(n - 1, k)

//Esercizio 2.9
let rec f = function
    | (0,y) -> y
    | (x,y) -> f(x-1, x*y)

// 1. Determine the type of f.
// val f: int * int -> int
// Guardando la prima riga del pattern matching vediamo che il primo elemento della tupla è 0 (tipo int)
// inoltre, guardando la seconda riga del pattern matching, il secondo elemento è un tipo di cui è definita l'operazione *
// essendo che abbiamo x*y, e dato che in F# non possiamo semplicemente effettuare operazioni come 2*3.12, per inferenza
// concludiamo che y è di tipo int

//2. For which arguments does the evaluation of f terminate?
// Termina per tutte le tuple con (x > 0, y appertenete ad Z)

//3. Write the evaluation steps for f(2, 3).
//[1] match 2* -> f ( 2-1 , 2*3 )
//[-]          -> f ( 1 , 6 )
//[2] match 2* -> f ( 1-1 , 1*6 )
//[-]          -> f ( 0 , 6 )
//[2] match 1* -> f ( 0 , 6 ) -> 6

//4. What is the mathematical meaning of f(x, y)?
// y^x  funzione somma y per x volte

//Esercizio 2.10
let test(c,e) = if c then e else 0;;
// 1. What is the type of test?
// Possiamo subito notare che essendoci else 0 il risultato della funziona saro di tipo int,
// essendo c messo dopo l'if vuol dire che si tratta di un boolean duque
// boolean * int -> int

// 2. What is the result of evaluating test(false, fact(−1))?
// 0 , fact -1 non viene mai valutato in quando presente dopo l'if

// 3. Compare this with the result of evaluating
//[1] if [false] then...
//[2] else -> return 0

//2.11
let VAT n x = float(n) * ( 1.0 + (x / 100.0) );;

let unVAT n x = float(n) / ( 1.0 + (x / 100.0) );; 

//2.12 , 2.13
//Personlamente non ho idea di come farli
