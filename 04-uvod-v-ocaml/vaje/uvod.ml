
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [square] vrne kvadrat podanega celega števila.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # square 2;;
 - : int = 4
[*----------------------------------------------------------------------------*)

let square x = x*x;; (*tole ;;, da ve, da mora to pognat *)

square 2;; 
  
(*----------------------------------------------------------------------------*]
 Funkcija [middle_of_triple] vrne srednji element trojice.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # middle_of_triple (true, false, true);;
 - : bool = false
[*----------------------------------------------------------------------------*)

(* let rec middle_of_triple x, y, z = y;; (* dela tako z (x, y, z), kot x, y, z *) *)

let middle_of_triple (_, b, _) = b
let middle_of_triple x =
  let (_,b,_) = x
  in b (*funkcija sprejme tuple*)


(*----------------------------------------------------------------------------*]
 Funkcija [starting_element] vrne prvi element danega seznama. V primeru
 prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # starting_element [1; 2; 3; 4];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let starting_element seznam = function
  | [] -> failwith "ERROR"
  | x :: _ -> x (*seznam odpakiramo: vzamemo prvi element in kar ostane(xs ponavadi označmo x :: xs, ampak ker tega xs dela ne rabmo, kar _) *)

(*function pove, da je  to funkcija ene sprem. in je niti ne rabmo napisat, ampak jo razelimo na en mejni primer, ostalo pa tisto, kar funkcija v resnici naredi *)

let starting_element' sez =
  match sez with
    | [] -> failwith "Ne bo šlo"
    | x :: _ -> x  (*če vrstici s praznim seznamom in x zamenjam, bo vse vredu, ker se ne izključujeta niti ne prekivata, zato lahko naredim to spodaj  *)

    let starting_element' sez =
  match sez with
    | x :: _ -> x 
    | _ -> failwith "Ne bo šlo"

    (*a = starting element [1;2;3] bo a int 4
    če pa b = starting element 12345 pa bo reku ocaml, da gre to za funkcijo iz seznama v seznam (sam ugotovi) in te opomni, da ne gre, ker rabi seznam za argument
    ne sme biti nekompatibilnosti med spremenljivkami, ki jih def in uporabljaš
    ocaml nam ne pusti napačnih stvari notr dajat- napačnih tipov npr*)

(*----------------------------------------------------------------------------*]
 Funkcija [multiply] zmnoži vse elemente seznama. V primeru praznega seznama
 vrne vrednost, ki je smiselna za rekurzijo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # multiply [2; 4; 6];;
 - : int = 48
[*----------------------------------------------------------------------------*)

let rec multiply = function
  | [] -> 1
  | x :: xs -> x * multiply xs

(*----------------------------------------------------------------------------*]
 Napišite funkcijo ekvivalentno python kodi:

  def sum_int_pairs(pair_list):
      if len(pair_list) == 0:
        return []
      else:
        x, y = pair_list[0]
        return [x + y] + sum_int_pairs(pair_list[1:])

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_int_pairs [(1, -2); (3, 4); (0, -0)];;
 - : int list = [-1; 7; 0]
[*----------------------------------------------------------------------------*)

let rec sum_int_pairs = function
  | [] -> []
  |(x, y) :: xys  -> x + y :: sum_int_pairs xys (*če daš [x, y] je to isto kot ([x, y]) in ti vzame, kot da seznam razpakira v dva seznama*))

(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get k = function
  | [] -> failwith "Index out of range"
  | x :: xs when k = 0 -> x
  | x :: xs -> get (k - 1) xs

let rec get k = function
  | [] -> failwith "Index out of range"
  | x :: xs -> if (k = 0) then x else get (k-1) xs (* xs je še en argument. ki ti ga function implicitno vzame *)

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double = function
  | [] -> []
  | x :: xs -> x :: x :: double xs (*sez je lahk al prazen al pa take oblike x :: xs ;; = x :: (x :: double xs)*)
  | x :: xs -> [x;x] @ double xs
(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k sez =
  if k = 0 then
    x :: sez
  else
    match sez with
      | y :: ys -> x :: insert x (k-1) ys

(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k sez =
   if k <= 0 then 
   ([], sez)
   else 
    match sez with
    | x :: xs -> 
      let (prej, potem) = divide (k - 1) xs in
      (x :: prej, potem)
    | [] -> ([], [])


(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate n sez = 
  let prvih_n, ostalo = divide n sez in
  ostalo @ prvih_n

(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)
let rec obrni sez =
  let rec obrni' acc = function
  | [] -> acc
  | x :: xs -> obrni' (x :: acc) xs
  in obrni' [] sez


let rec remove x sez = 
  let rec remove' acc x = function
  | [] -> obrni acc
  | y :: ys when y is not x -> remove' (y :: acc) ys
  | y :: ys -> remove' acc ys
  in remove' [] x sez
    

(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_palindrome = ()

(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components = ()

(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let rec second_largest = ()
