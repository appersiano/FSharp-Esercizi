/////////////////////////
// Esercizi Capitolo 3 //
/////////////////////////

// Es 3.1
// Triple

let (.<.) (hours,minutes,f :string) (hours2,minutes2,f2) = 
    let first = hours * 60 + minutes
    let second = hours2 * 60 + minutes2

    if f = f2 then
        if first < second then true else false        
    elif f < f2 then true else false
    ;;
     
//Records
type Meridiano = AM | PM
type Orario = {hours : int; minutes : int; meridien : Meridiano};;

let (.<.) (a : Orario) (b : Orario )=
    let first = a.hours * 60 + a.minutes
    let second = b.hours * 60 + b.minutes

    if a.meridien = b.meridien then
        if first < second then true else false        
    elif a.meridien < b.meridien then true else false
    ;;

//let orario1 = { hours =  2; minutes = 15; meridien = AM };;
//let orario2 = { hours =  2; minutes = 15; meridien = PM };;

//Esercizio 3.2
// 12 pence -> 1 shilling
// 20 shilling -> 1 pound

let (.+.) (pounds, shillings, pence) (pounds2, shillings2, pence2) = 
    let (pence, c_pence) = 
        let sum = pence + pence2
        (sum % 12, sum / 12)
    let (shillings, c) = 
        let sum = shillings + shillings2 + c_pence
        (sum % 20, sum / 20)

    let t_pound = pounds + pounds2 + c
    (t_pound, shillings, pence)
    ;;

// Records
type EnglishMoney = { pounds : int ; shillings : int ; pences : int }

let m1 = { pounds = 12 ; shillings = 3 ; pences = 10 }
let m2 = { pounds = 12 ; shillings = 3 ; pences = 10 }

let (.+.) m1 m2 = 
    let (pence, c_pence) = 
        let sum = m1.pences + m2.pences
        (sum % 12, sum / 12)
    let (shillings, c) = 
        let sum = m1.shillings + m2.shillings + c_pence
        (sum % 20, sum / 20)

    let t_pound = m1.pounds + m2.pounds + c
    (t_pound, shillings, pence)
    ;; 

//Manca da implementare la sottrazione

//Es 3.3

//addizione
let (..+) (a,b) (c,d) = (a + c, b + d)
//moltiplicazione
let (..*) (a,b) (c,d) = (a * c - b * d, b * c + a * d)
//sottrazione
let (-&) (a,b) (c,d) = (a  - c, b - d)
//divisione
let (-/) (a,b) (c,d) = 
    let divisore = c**2.0 + b**2.0
    ( (a * c + b * d) / divisore, (b * c - a * d) / divisore )

//Es 3.4
//3.4.1
type StraightLine = float * float
//3.4.2
//let x = (1.0,2.0) : StraightLine;;
let mirrorX x : StraightLine = 
    (-fst(x), - snd(x))

let mirrorY x : StraightLine = 
    (-fst(x), snd(x))

//3.4.3
let formattaStraightLine (x : StraightLine) =
    let a = fst(x)
    let b = snd(x)
    printfn "y = %fx + %f" a b


//3.5
type Solution =
    | TwoRoots of float * float
    | OneRoot of float
    | NoSolution

let solve (a, b, c)=
    let discriminant = b*b - 4.0*a*c
    match discriminant with
    | x when x < 0.0 -> NoSolution
    | _ -> let sqrtDiscriminant = sqrt(discriminant)
           let x1 = (-b + sqrtDiscriminant)/ 2.0 * a
           if discriminant = 0.0 then
            OneRoot(x1)
            else
            let x2 = (-b - sqrtDiscriminant)/ 2.0 * a
            TwoRoots(x1, x2)

//3.6 guarda su

//3.7
let area x =
    match x with
    | _ when not (isShape x) -> failwith "not a legal shape"
    | Circle r -> System.Math.PI * r * r
    | Square a -> a*a
    | Triangle(a, b, c) -> 42.0
