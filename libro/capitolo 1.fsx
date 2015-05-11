/////////////////////////
// Esercizi Capitolo 1 //
/////////////////////////

//Es 1.1
let g n = n + 4;;

//Es 1.2
let h (x,y) = System.Math.Sqrt(x*x+y*y);;

//Es 1.3
let gn = function
    | n ->n + 4

let hf1 = function
    | (x, y) -> System.Math.Sqrt(x*x + y*y)

//Es 1.4
let rec f = function
    | 0 -> 0
    | n -> n + f (n-1);;

//Es 1.5
let rec fibonacci = function
    | 0 -> 0
    | 1 -> 1
    | n -> fibonacci (n-2) + fibonacci (n-1);;

//Es 1.6
let rec sum = function
    | (m,0) -> m
    | (m,n) -> m + n + sum (m,n-1);;

//Es 1.7
a. float * int
b. int
c. float
d. float * int

//Es 1.8

    | a -> 5
env | f -> funzione aggiungi 1
    | g -> funzione aggiungi 6

//f3 = 4 ; g 3 = 9
