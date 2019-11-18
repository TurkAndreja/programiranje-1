(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root m n = (m > 0) && (n > 0 ) && ((m = n * n) || (n = m * m))

let pack3 x y z = (x, y, z)

let sum_if_not f list =
  let rec sum f acc list =
  match list with
  | [] -> acc
  | x :: xs -> if f x then sum f acc xs else sum f (x + acc) xs
  in
  sum f 0 list

(*let apply sez_funkcij list =
  let aux sez_funkcij list acc =
    match sez_funkcij, list with 
    | 
    | (f :: fs, x :: xs) -> aux *)


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = Predavanja | Vaje

type srecanje = {predmet: string; vrsta: vrsta_srecanja; trajanje: int}

type urnik = srecanje list list

let vaje = {predmet = "Analiza 2a"; vrsta = Vaje; trajanje = 3}
let predavanje = {predmet = "Programiranje 1"; vrsta = Predavanja; trajanje = 2}

let urnik_profesor = [[{predmet = "Analiza 2a"; vrsta = Vaje; trajanje = 2}];[]; [{predmet = "Analiza 2a"; vrsta = Predavanja; trajanje = 1}]; [];[]; [{predmet = "Analiza 2a"; vrsta = Vaje; trajanje = 1}]]

(*let je_preobremenjen urnik =
  match urnik with
  | []
  | x :: xs -> (
      match x with
      | []
      | {_, vrsta, trajanje} :: ss 
  )


let bogastvo () = failwith "dopolni me"