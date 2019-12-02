(* ========== Vaja 8: Moduli  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
"Once upon a time, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.

 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.

 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.

 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 primitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.

 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."

 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Definirajte signaturo [NAT], ki določa strukturo naravnih števil. Ima osnovni 
 tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
 Hkrati naj vsebuje pretvorbe iz in v OCamlov [int] tip.

 Opomba: Funkcije za pretvarjanje ponavadi poimenujemo [to_int] and [of_int],
 tako da skupaj z imenom modula dobimo ime [NAT.of_int], ki nam pove, da 
 pridobivamo naravno število iz celega števila.
[*----------------------------------------------------------------------------*)

module type NAT = sig (*ta tip t je ponavadi tak glavni tip modula in not bo mel vrednost zero, ki bo tipa t in neko funkcijo, ki bo sprejela dva argumenta tipa t in vrnila bool: povemo kako bo stvar delala*)
  type t

  val eq   : t -> t -> bool
  val zero : t
  (* Dodajte manjkajoče! *) (*dodamo, da bo nekaj berljivo:pretvarjanje*)
  val to_int : t -> int (*pri N dobimo to zastonj, le pazit mormo, da ni negativnih; implementiramo ih lahk tut z Peamovimi aksiomi*)
  val of_int : int -> t 
  val ena : t
  val sestej : t -> t -> t
  val odstej : t -> t -> t
  val zmnozi : t -> t -> t 

end (*delamo strukturo, ki se obnaša kot naravna števila*)
(*ljudje bodo videli samo tole:glavne stvari, kar smo spodaj definirali pa ne: osnovno, kako se obnašajo naravna števila: kar pa se dejansko dogaja, lahko pa mal popravljaš, brez da bi vedeli*)


(*----------------------------------------------------------------------------*]
 Napišite implementacijo modula [Nat_int], ki zgradi modul s signaturo [NAT],
 kjer kot osnovni tip uporablja OCamlov tip [int].

 Namig: Dokler ne implementirate vse funkcij v [Nat_int] se bo OCaml pritoževal.
 Temu se lahko izognete tako, da funkcije, ki še niso napisane nadomestite z 
 [failwith "later"], vendar to ne deluje za konstante. (*da ti scompila in pride čez. čeprav še nisi napisal*)
[*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct

  type t = int (*tale eq že ve, da bo vzel dva tja in vrnil bool, ker to v def*)
  let eq = (=) (*eq x y = x = y; tisto lažje, jo kar enačiš z (=) funkcijo-ne primeš direkt točke, ampak kar enačiš funkcije- to včasih hitreje*)
  let zero = 0 
  let ena = 1 (* tu smo vzeli 0 za nar št brez naslednika? ;zakaj rabimo potem let ena = 1*)
  let sestej = (+)
  let zmnozi = ( * ) (*pazi, moraš nujno imeti presledke, da ni komentar*)
  let odstej x y = max 0 (x - y)
  let to_int n = n
  let of_int i = i 
  (* Dodajte manjkajoče! *)

end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo [NAT], ki temelji na Peanovih aksiomih:
 https://en.wikipedia.org/wiki/Peano_axioms
   
 Osnovni tip modula definirajte kot vsotni tip, ki vsebuje konstruktor za ničlo
 in konstruktor za naslednika nekega naravnega števila.
 Večino funkcij lahko implementirate s pomočjo rekurzije. Naprimer, enakost
 števil [k] in [l] določimo s hkratno rekurzijo na [k] in [l], kjer je osnoven
 primer [Zero = Zero].

[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct (*naravno število bi lahk tudi predstavljal s seznami, torej npr. z dolžinami seznamov*)

  type t = Zero | Succ of t 
  let rec eq x y = 
    match x, y with
    | Zero, Zero -> true
    (*| (Zero, Nas (n)) | (Nas (m), Zero) -> false (*pri ali dej v oklepaje obe strani*) *)
    | Succ (n), Succ (m) -> eq n m
    | _ -> false 

  let zero = Zero (* To morate spremeniti! *)
  (* Dodajte manjkajoče! *)
  let ena = Succ zero

  let rec to_int n = match n with
    | Zero -> 0
    | Succ k -> 1 + (to_int k) (*tuki sestej ne dela, če ga dasm namest +*)

  let rec of_int x = match x with
    | 0 -> Zero
    | x' when x' > 0 -> Succ (of_int (x' - 1))
    | _ -> failwith "Negativnih števil ne sprejemam" (* lahk bi dal tle tut int -> t option in v tem primeru pač ne bi ničesar vrglo*)

  let rec sestej m n =
    match m with (*vseen, katerega matchamo*)
    | Zero -> n
    | Succ k -> Succ (sestej k n)
     
  let rec odstej m n = (*DN a - b = n+ - m+ = n - m in potem lovim ničlo*)
    (*match m n with
    | (Zero, _) | (_, Zero) -> Zero
    | Succ (x), Succ (y) -> odstej *)

  let rec zmnozi m n =
    match m with
    | Zero -> Zero
    | Succ(x) -> sestej n (zmnozi n x)

end

(*----------------------------------------------------------------------------*]
 V OCamlu lahko module podajamo kot argumente funkcij, z uporabo besede
 [module]. Funkcijo, ki sprejme modul torej definiramo kot

 # let f (module M : M_sig) = ...

 in ji podajamo argumente kot 

 # f (module M_implementation);;

 Funkcija [sum_nat_100] sprejme modul tipa [NAT] in z uporabo modula sešteje
 prvih 100 naravnih števil. Ker funkcija ne more vrniti rezultata tipa [NAT.t]
 (saj ne vemo, kateremu od modulov bo pripadal, torej je lahko [int] ali pa
  variantni tip) na koncu vrnemo rezultat tipa [int] z uporabo metode [to_int].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_nat_100 (module Nat_int);;
 - : int = 4950
 # sum_nat_100 (module Nat_peano);;
 - : int = 4950
[*----------------------------------------------------------------------------*)

let sum_nat_100 (module Nat : NAT) = (*lahk tut gremo z vsoto aritm. zap n(n+1)/2*)
  let rec sum acc current_nat = 
    if Nat.eq current_nat (Nat.of_int 100) then
    acc else
    sum (Nat.sestej current_nat (acc)) (Nat.add Nat.one current_nat)
  Nat.to_int (sum Nat.zero Nat.zero)
  (*linearna hitrost, ker se čez vse zapeljemo*)
  (*važn e. če damo nekje skos veliko oz majhno stvar*)
  (*linearna krat linearna je kvadratna potem*)
  (* če bi nam nekdo povedal cifro in bi jo mogl še zgradit z nar št, že takoj dobimo kvadratno*)
  

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Now we follow the fable told by John Reynolds in the introduction.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte signaturo modula kompleksnih števil.
 Potrebujemo osnovni tip, test enakosti, ničlo, enko, imaginarno konstanto i,
 negacijo, konjugacijo, seštevanje in množenje. 
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t 

  val eq : t -> t -> bool
  val zero = t
  val ena = t
  val i = t
  val neg = t -> t
  val conj = t -> t
  val sestej = t -> t -> to_int
  val zmnozi = t -> t -> t
  (* Dodajte manjkajoče! *)
end

(*----------------------------------------------------------------------------*]
 Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število realno in imaginarno komponento.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = failwith "later"
  (* Dodajte manjkajoče! *)

end

(*----------------------------------------------------------------------------*]
 Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število radij in kot (angl. magnitude in argument).
   
 Priporočilo: Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga 
 pustite za konec (lahko tudi za konec stoletja).
[*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  (* Pomožne funkcije za lažje življenje. *)
  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.

  let eq x y = failwith "later"
  (* Dodajte manjkajoče! *)

end

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Na vajah z iskalnimi drevesi smo definirali tip slovarjev 
 [('key, 'value) dict], ki je implementiral [dict_get], [dict_insert] in
 [print_dict]. Napišite primerno signaturo za slovarje [DICT] in naredite
 implementacijo modula z drevesi (kot na prejšnjih vajah). 
 
 Modul naj vsebuje prazen slovar [empty] in pa funkcije [get], [insert] in
 [print] (print naj ponovno deluje zgolj na [(string, int) t].
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funkcija [count (module Dict) list] prešteje in izpiše pojavitve posameznih
 elementov v seznamu [list] s pomočjo izbranega modula slovarjev.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count (module Tree_dict) ["b"; "a"; "n"; "a"; "n"; "a"];;
 a : 3
 b : 1
 n : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let count (module Dict : DICT) list = ()