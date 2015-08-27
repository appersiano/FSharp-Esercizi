/////////////////////////
// Esercizi Capitolo 5 //
/////////////////////////

//Es. 5.1
let rev l = List.foldBack (fun x acc -> acc@[x]) l [];;

//Es 5.2
let revrev_fb ll = List.foldBack (fun x acc -> acc@[rev x] ) ll [];;

let revrev_f ll = List.fold (fun acc x -> rev x::acc ) [] ll;;

//Es 5.3
let sum_fb p list = List.foldBack (fun x acc -> if p x then x + acc else acc ) list 0;;

let sum_f p list = List.fold (fun acc x -> if p x then x + acc else acc ) 0 list;;

//5.4
let downto1 f n e =
    if n <= 0 then
        e
    else
        List.foldBack f [1..n] e
    ;; 

let fact n = downto1 (*) n 1

let build f n = downto1 f n [] 
//build (fun x e -> [x]@e) 4;;
