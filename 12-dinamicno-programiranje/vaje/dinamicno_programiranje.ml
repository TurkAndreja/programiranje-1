(* ========== Vaje 6: Dinamično programiranje  ========== *)


(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

(* matriki okoli daj same ničle, da se ne sprašuješ vedno, če si že na robu*)


(*let max_cheese matrix = function

  let sirina = Array.length matrix.(0) in
  let visina = Array.length matrix in

  let aux x y = function
    tren_vr = matrix.(x).(y)
    desno = matrix.(x + 1).(y) if (x + 1) <=
    spodaj = matrix.(x).(y + 1)
*)
  
let max_cheese cheese = 
  let height = Array.length cheese in
  let width = Array.length cheese.(0) in
  let memo = Array.make_matrix height width 0 in
  let poracunaj_celico (vr:int) (st:int) = (*bo le spremenila, torej vrnila nič*)
    if st = -1 then () else (
    let desno = if st = width - 1 then 0 else memo.(vr).(st + 1) in 
    let dol = if vr = height - 1 then 0 else memo.(vr + 1).(st) in 
    memo.(vr).(st) <- cheese.(vr).(st) + (max desno dol);
    poracunaj_celico vr (st - 1)
    )
  let rec vrsticno (vr:int) = (*za vrstico vr bo ustrezno popravila memo, nočemo pa da nam kr neki vrača, zato bo vrnila unit, ko bo nrdila vrstico vr, bo šla na vr - 1*)
    if vr = -1 then ( (*rešimo se hitro, kdaj se konča*)
    else (
      (*let trenutna = cheese.(vr) in () (*lahk še funkcijo za po stolpcu dodam pred to po vrsticah, lahko pa jo tu dodam*)*)
      poracunaj_celico vr (width - 1);
      vrsticno(vr - 1)
    ) 
  in
  vrsticno (height - 1);
  memo.(0).(0) (*ker je array podan po vrsticah, je tu lažje po vrsticah iti*) (*v pythonu naredimo le dve gnezdeni zanki, na izpitu pri dim prog lahko delaš v poljubnem jeiku*)

(*časovna zahtevnost je vsaj n krat m, ker gremo čez vse celice, iz vsake smo sam eno levo in eno desno pogledal in to je to, torej je skupak ravno n krat m*)
(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)

let alternating_towers visina =

let rec na_vrhu_modri =

(*----------------------------------------------------------------------------*]
 Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
 poljubne izdelke, katerih skupna masa ne presega [max_w] kilogramov. Napišite
 funkcijo [best_value articles max_w], ki poišče največjo skupno ceno, ki jo
 lahko odnesemo iz trgovine, kjer lahko vsak izdelek vzamemo večkrat, nato pa
 še funkcijo [best_value_uniques articles max_w], kjer lahko vsak izdelek
 vzamemo kvečjemu enkrat.

 Namig: Modul [Array] ponuja funkcije kot so [map], [fold_left], [copy] in
 podobno, kot alternativa uporabi zank.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # best_value articles 1.;;
 - : float = 10.95
 # best_value_unique articles 1.;;
- : float = 7.66
[*----------------------------------------------------------------------------*)

(* Articles are of form (name, price, weight) *)
let articles = [|
	("yoghurt", 0.39, 0.18);
	("milk", 0.89, 1.03);
  ("coffee", 2.19, 0.2);
  ("butter", 1.49, 0.25);
  ("yeast", 0.22, 0.042);
  ("eggs", 2.39, 0.69);
  ("sausage", 3.76, 0.50);
  ("bread", 2.99, 1.0);
  ("Nutella", 4.99, 0.75);
  ("juice", 1.15, 2.0)
|]


(*----------------------------------------------------------------------------*]
 Cena sprehoda po drevesu je vsota vrednosti v vseh obiskanih vozliščih.
 Poiščite vrednost najdražjega sprehoda od korena do listov drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_path Empty ;;
 - : 'a option = None
 # max_path test_tree;;
- : int option = Some 21
[*----------------------------------------------------------------------------*)

type 'a tree
 = Empty
 | Node of ('a tree) * 'a * ('a tree)

let leaf x = Node (Empty, x, Empty)

let test_tree = Node( Node(leaf 0, 2, leaf 13), 5, Node(leaf 9, 7, leaf 4))

(*----------------------------------------------------------------------------*]
 Cena sprehoda po drevesu je vsota vrednosti v vseh obiskanih vozliščih.
 Poiščite najdražji sprehod od korena do listov drevesa: Funkcija pot vrne v 
 obliki seznama smeri, katere je potrebno izbrati za najdražji sprehod.

 Napišite tudi funkcijo, ki sprehod pretvori v elemente sprehoda
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_path_trace Empty ;;
 - : 'a list = []
 # max_path_trace test_tree;;
- : direction list = [Right, Left]
 # reconstruct test_tree (max_path_trace test_tree);;
- : int list = [5; 7; 9]
[*----------------------------------------------------------------------------*)

type direcion 
  = Left
  | Right