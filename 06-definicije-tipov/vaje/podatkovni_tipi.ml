(* ========== Vaja 3: Definicije Tipov  ========== *)

let rec reverse list =
  let rec reverse' list acc =
    match list with
    | [] -> acc
    | x :: xs -> reverse' xs (x :: acc)
  in
  reverse' list []

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute.
 Oglejmo si dva pristopa k izboljšavi varnosti pri uporabi valut.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type euro = Euro of float
type dollar = Dollar of float
(*tip vsote razpakiraš in delaš na vsaki stvari posebej*)

(*----------------------------------------------------------------------------*]
 Definirajte tipa [euro] in [dollar], kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število.
 Nato napišite funkciji [euro_to_dollar] in [dollar_to_euro], ki primerno
 pretvarjata valuti (točne vrednosti pridobite na internetu ali pa si jih
 izmislite).

 Namig: Občudujte informativnost tipov funkcij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)

let dollar_to_euro_ration = 1.0 /. 1.10 (* !!! PAZI !!! (deljeno); konstante prej definiraj in jih ne piši kar tako v funkcijo *)

let dollar_to_euro (Dollar d) = Euro (d *. dollar_to_euro_ration) (*v ocamlu razpakiramo nov tip Dollar x, vne x. Mi hočemo funkcijo, i gre iz tipa dollar v tip euro *)
let euro_to_dollar (Euro e) = Dollar (e /. dollar_to_euro_ration) (*lepo je, če imaš dollar d in euro e, da atribut malo povezan z tipom *)

(*----------------------------------------------------------------------------*]
 Definirajte tip [currency] kot en vsotni tip z konstruktorji za jen, funt
 in švedsko krono. Nato napišite funkcijo [to_pound], ki primerno pretvori
 valuto tipa [currency] v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
        Ocaml sam opozori, da je potrebno popraviti funkcijo [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)

type currency =
  | Yen of float (*jota 1*)
  | Pound of float (*jota 2*)
  | Crown of float (*jota 3*)

let to_pound c = match c with
  | Crown cr -> Pound (cr *. 0.3)  (*lahk imamo function ali pa damo argument*)
  | Yen y -> Pound (y *. 0.9) (*jo že takoj kot seznam razpakiram*)
  | Pound p -> Pound p
  
(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip [list] predstavimo s konstruktorjem za prazen seznam
 [Nil] (oz. [] v Ocamlu) in pa konstruktorjem za člen [Cons(x, xs)] (oz.
 x :: xs v Ocamlu).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tip [intbool_list] z konstruktorji za:
  1.) prazen seznam,
  2.) člen z celoštevilsko vrednostjo,
  3.) člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)

type intbool_list = 
  | Nil
  | Int of int * intbool_list (*rekurzivno:int je na prvem mestu in pol naprej seznam, lahk pa je bool na prvem mestu in pol seznam (ga rekurzivno def, tako kot običajen seznam - glava + ostalo*)
  | Bool of bool * intbool_list

let primer = Int (5, Bool(true, Bool(false, Int(7, Nil))))
(*----------------------------------------------------------------------------*]
 Funkcija [intbool_map f_int f_bool ib_list] preslika vrednosti [ib_list] v nov
 [intbool_list] seznam, kjer na elementih uporabi primerno od funkcij [f_int]
 oz. [f_bool].
[*----------------------------------------------------------------------------*)

let rec intbool_map_rep f_int f_bool ib_list =
  let rec intbool_map_rep' f_int f_bool ib_list acc =
  match ib_list with
    | Nil -> reverse acc
    | Int (i, is) -> intbool_map_rep' f_int f_bool is (f_int i :: acc) (*ne dela, če napišeš Int i, is; mporaš dati v oklepaj Int (i, is)*)
    | Bool (b, bs) -> intbool_map_rep' f_int f_bool bs (f_bool b :: acc)
  in
  intbool_map_rep' f_int f_bool ib_list []


  let rec intbool_map f_int f_bool = function (*zadnji argument, torej list v tem primeru, bo funkcija vzela implicitno in ga bo matchala kar takoj*)
    | Nil -> Nil
    | Int (x, xs) -> Int (f_int x, intbool_map f_int f_bool xs)
    | Bool (b, bs) -> Bool (f_bool b, intbool_map f_int f_bool bs)

(*funkcije sta drugačnmega tipa, POGLEJ !!! morda prvi ni prav, ker hočemo dobiti v kodomeni tipe inboola !!!*)

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_reverse] obrne vrstni red elementov [intbool_list] seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let rec intbool_reverse ib_list = (*tukaj ni function, ker še nekaj definiraš prej, ne matchas direkt !!! *)
  let rec intbool_reverse' acc = function
    | Nil -> acc
    | Int (i, is) -> intbool_reverse' (Int (i, acc)) is (*tukaj daj is kot argument, ga kar napiši, ker se je ta implicitnoi spremenil*)
    | Bool (b, bs) -> intbool_reverse' (Bool (b, acc)) bs
  in
  intbool_reverse' Nil ib_list (*ne bo delal s [] !!!*)

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_separate ib_list] loči vrednosti [ib_list] v par [list]
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let rec intbool_separate = ()

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)



(*----------------------------------------------------------------------------*]
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 [magic], ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire,
 frost in arcane.

 Ko se čarodej zaposli na akademiji, se usmeri v zgodovino, poučevanje ali
 raziskovanje oz. historian, teacher in researcher. Definirajte tip
 [specialisation], ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type boool = TTrue | FFalse

type magic = Fire | Frost | Arcane
type specialisation = Historian | Teacher | Researcher
(*nimajo nobenih dodatnih informacij*)

(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
 status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)

type status =
  | Newbie
  | Student of magic * int
  | Employed of magic * specialisation

type wizard = {ime: string; status: status}

(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)

type magic_counter = {fire: int ; frost: int ; arcane: int}

(*let update counter magic_type = counter.fire (*vrne int pri fire*) *)

let update counter magic_type = function
  | Fire -> {fire = counter.fire + 1 ; frost = counter.frost ; arcane = counter.arcane}
  | Fire -> {fire = counter.fire + 1} (*vzem sam counter pa mu sam tole posodob pa ostalo pust - lažje*)

let update ({fire = fire2 ; frost ; arcane} as counter) = function (*kar je mel magic caunter za fore, si jo shranim v spremenljicko fire2*)
  |Fire -> {counter with fire = fire2}
  |Frost -> {counter with frost = frost + 1}
  |Arcane -> {counter with arcane = arcane + 1}
  (*zdaj imam tu vse spremenljivke že v začetku shranjene, ampak problem, ker pol counter ni definiran, zato v as counter? oklepaji?*) 

(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let rec count_magic lst=  (*nrdi neki za enga čarovnika in za staro stanje*)
  let folder counter {status} = (*ime spustimo, ker na sne zanima*)
    match status with
      | Newbie -> counter
      | Studebt (magic, _) -> update counter magic
      | Employed (magic, _) -> update counter magic
in
list.fold_left folder {fire=0 ; frost=0 ; arcane =0} lst
(*rekurzivno bi si morali acc ročno nosit argument s sabo, tu pa fold to dela namesto mene*)

(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let rec find_candidate = ()
