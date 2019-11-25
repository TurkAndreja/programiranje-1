(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
(*vozlišće v drevesu je node, tako mu rečemo*)
type 'a tree 
  = Empty
  | Node of ('a tree) * 'a * ('a tree) (*tkole nastavmo, da lepo vidmo, katero je levo in katero desno. namest tistga v sredi je lahk karkol, nps 'a list, al pa kr še eno drevo*)

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11 (*teli k nimajo več otrok, so listi*)
[*----------------------------------------------------------------------------*)

(*teli k nimajo več otrok, so listi*)

let make_leaf x = Node (Empty, x, Empty) (*vzame tip in vrne drevo nad tem tipom, pomožna funkcija, ki zelo priročna*)

let test_tree = Node (Node (make_leaf 0 ,2, Empty), 5, Node (make_leaf 6, 7, make_leaf 11))

(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec mirror = function
  | Empty -> Empty
  | Node (left, el, right) -> Node (mirror right, el, mirror left) (*ni repna*)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec height = function
  | Empty -> 0
  | Node (left, _, right) -> 1 + max (height left) (height right) (* če ne narediš oklepajev za argumente pri Node, ti ga ne bo prepoznalo kot drevo !!!*)

let rec size = function
  | Empty -> 0
  | Node (r, _, l) -> 1 + size r + size l

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

(*drevo si lahk vbistvu predstavljamo seznam*)
(*če preveš tip Empty je to neko drevo nad a 'a tree*)
let rec map_tree f = function
  | Empty -> Empty
  | Node (l, x, r) -> Node ( map_tree f l, f x, map_tree f r)
  (*lahk dokažemo, da map_tree od kompozitum f in g, je map tree od f + maptree od g*)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
  | Empty -> []
  | Node (l, x, r) -> (list_of_tree l) @ (x :: list_of_tree r) (*@ je ful draga. kr tist seznam, ki ga priključuje, gre najprej čez celega in gre od zadi na desno, torej na začetek desnega seznama ga dodajat, zato to s seznami ni časovno vredu*)

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)

(*ali pa pogledamo, če je seznam linearno urejen - gledamo po dva pa po dva: najprej 1. 2., če ne false, če ja, prvega vn vržemo in gledamo 2. 3.*)
let rec is_ordered = function
| x :: y:: xs -> if x <= y then is_ordered (y::xs) else false (*rep rek*)
| x::y::xs -> x <= y && is_ordered (y::xs) (*tudi ta repna, ker ocaml kr zavrže prvi del, če ne drži in ostane le še drugi. poleg tega gre na splošno najprej prvi del preden gre druzga zračunat*)
| _ -> true
  (*| [] -> true*)
  (* lepš zadnja možnost | x :: [] (* xs when list.len xs is 1 ne bo vredu, ker še enkrat več se sprehodiš in lin odvisno*)*)

let is_bst bst = is_ordered (list_of_tree bst)

let is_between lower_b x upper_b = match (lower_b, upper_b) with
  | None, None -> true
  | None, Some up' -> 
  |
  |

let rec is_bst tree =
  let rec is_sub_bst trenutni_min trenutni_max = function (*kaj so prve meje - lahk greš enga desno levo in +/- 1, none ne mporemo dat kr notr, ker je none neprimerljiv in bo zaštekal*)
    | Empty -> true
    | Node (l, x, r) -> is is_between trenutni_min (*(trenutni_min < x) && (x < trenutni_max) && is_sub_bst (trenutni_min, x, l) && is_sub_bst (x, trenutni_max, r)*) (*najprej nastavimo, če x vredu, da če slučajno ni, še ne računa vsega druzga*)
  in
  true
(*PREPIŠI !!!*)
(*zato primerjam na 'a option, pri čemer 'a linearno urejen, None pa dam pod vse v Hessejevem diagramu in je manjša od vseh, pol pa še en None' lahk dodam, da bo nad vsemi (glej zvezek), pri none bo pa vedno true*)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec insert v = function
| Empty -> make_leaf v
| Node (l, x, r) when v < x -> Node (insert v l, x, r)
| Node (l, x, r) when x < v -> Node (l, x, insert v r)
| t -> t

let rec member v = function
  | Empty -> false
  | Node (l, x, r) -> 
    if v < x then 
    member v l else 
    if x < v then
    member v r
    else true

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

(*let member2 bst = List.mem (list_of_tree bst) ; zelo šibek program*)


(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

(*naslednjik korena je najmanjši na desni in predhodnik največji iz leve in te dva najdemo da gremo skos levo in skos desno*)

(*nrdim seznam in ga še okol obrnem, da se mi ne bo treba sprehajat po seznamu maximumov, da pridem do največjega čist na konc*)

let succ bst =
  let minimal = function
  | Empty -> None (*zadn element na levi bo imel levega soseda Empty*)
  | Node(Empty, x, _) -> Some xs
  | Node(l, _, _) -> minimal l (*mora biti pred prejšnjo vrstico*)
  in
  match bst with
  | Empty -> None (*ga ni*)
  | Node (l, x, r) -> minimal r

  (*za desno podobno, pa v drugo smer matchas*)
  (* tole lak s slovarji implementiram, prvi element je neki, ostalo kr neki in gledam prve, potem pa some neki dajam ven*)

(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

      
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

