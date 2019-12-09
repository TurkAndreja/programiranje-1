(* ========== Vaje 5: Urejanje  ========== *)


(*----------------------------------------------------------------------------*]
 Funkcija [randlist len max] generira seznam dolžine [len] z naključnimi
 celimi števili med 0 in [max].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let l = randlist 10 10 ;;
 val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]
[*----------------------------------------------------------------------------*)

let rec randlist len max =
  if len <= 0 then [] else Random.int max :: (randlist (len - 1) max) (*seznam negativne dolžine je prazen seznam*)

(*rabimo ga, da bomo lahko testiral naše sorte na naključnem listu*)
(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Sedaj lahko s pomočjo [randlist] primerjamo našo urejevalno funkcijo (imenovana
 [our_sort] v spodnjem primeru) z urejevalno funkcijo modula [List]. Prav tako
 lahko na manjšem seznamu preverimo v čem je problem.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 let test = (randlist 100 100) in (our_sort test = List.sort compare test);;
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)



(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Vstavljanjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert y xs] vstavi [y] v že urejen seznam [xs] in vrne urejen
 seznam. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 9 [0; 2];;
 - : int list = [0; 2; 9]
 # insert 1 [4; 5];;
 - : int list = [1; 4; 5]
 # insert 7 [];;
 - : int list = [7]
[*----------------------------------------------------------------------------*)

(*ZELO POMEMBNO, DA SPREJME SEZNAM ŽE UREJENIH - ZDAJ MORMO NOV ELEMENT SAMO VSTAVIT NA PRAVO MESTO V ŽE UREJENEM*)

let rec insert y = function 
  | [] -> [y]
  | x :: xs when y > x -> x :: insert y xs
  | x :: xs -> y :: x :: xs

let insert y xs = (*po vrsti gledamo, kdaj bo neki manjše in neki večje od y - iščemo prelomnico; manjše si skos vlečemo s sabo: če pridemo do konca jih vrnemo sicer naredimo ustrezno.*)
  let rec insert_aux acc xs = match rest with (*to je zelo počasi - kvadratno, ker dodajamo na konec seznama  *)
    | [] -> List.rev (y :: acc) (*manjsi @ [y] *)
    | x :: xs -> insert_aux (x :: acc (*tole v oklepaju je časovno veliko bolj slabš! manjsi @ [x]*)) xs (*ker se bo ta veja največkrat izvajala, je treba razmislt, kako bom to vejo čim hitrej naredu, in pol po potrebi še ostale spremenimo*)
    | x :: xs when y > x -> List.rev_append (y :: acc) (*(y :: manjsi) @ restmanjsi @ [y] @ rest*) (x :: xs) (*obrne taprvega in appenda k tadrugmu, pa še tail rec je in hitra*)
  in
  insert_aux [] xs

(* časovna zahtevnost je kvadratna *)

(*let insert y xs =
  let rec insert_aux acc = function
    | [] -> list.rev (y :: acc)
    | x::xs ->*)
    
    
  

(*----------------------------------------------------------------------------*]
 Prazen seznam je že urejen. Funkcija [insert_sort] uredi seznam tako da
 zaporedoma vstavlja vse elemente seznama v prazen seznam.
[*----------------------------------------------------------------------------*)

let insert_sort sez =
 let rec insert_sort' acc = function
 | [] -> List.rev acc
 | x :: xs -> x :: insert_sort' acc xs
 in
 insert_sort' [] sez



(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [min_and_rest list] vrne par [Some (z, list')] tako da je [z]
 najmanjši element v [list] in seznam [list'] enak [list] z odstranjeno prvo
 pojavitvijo elementa [z]. V primeru praznega seznama vrne [None]. 
[*----------------------------------------------------------------------------*)


(* DOKONČAJ

let min_and_rest sez =  (*vrne minimum iz seznama in ostalo, to nrdimo tko, da vzamemo min in ostale odstranmo*)
  
  let rec najdi_min acc = function (* ne vemo, če je min sploh noter*, None damo nad vse v urejenosti - krkoli je manjše od None, torej moramo acc pomatchat*)
  | [] -> None
  | x :: xs -> 
    match acc with
    | None -> najdi_min x xs
    | _ -> najdi_min (min acc x) xs

*)

  (* zdaj pa še remove !!! *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri urejanju z izbiranjem na vsakem koraku ločimo dva podseznama, kjer je prvi
 že urejen, drugi pa vsebuje vse elemente, ki jih je še potrebno urediti. Nato
 zaporedoma prenašamo najmanjši element neurejenega podseznama v urejen
 podseznam, dokler ne uredimo vseh. 

 Če pričnemo z praznim urejenim podseznamom, vemo, da so na vsakem koraku vsi
 elementi neurejenega podseznama večji ali enaki elementom urejenega podseznama,
 saj vedno prenesemo najmanjšega. Tako vemo, da moramo naslednji najmanjši člen
 dodati na konec urejenega podseznama.
 (Hitreje je obrniti vrstni red seznama kot na vsakem koraku uporabiti [@].)
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort] je implementacija zgoraj opisanega algoritma.
 Namig: Uporabi [min_and_rest] iz prejšnje naloge.
[*----------------------------------------------------------------------------*)



(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem na Tabelah
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri delu z tabelami (array) namesto seznami, lahko urejanje z izbiranjem 
 naredimo "na mestu", t.j. brez uporabe vmesnih kopij (delov) vhoda. Kot prej
 tabelo ločujemo na že urejen del in še neurejen del, le da tokrat vse elemente
 hranimo v vhodni tabeli, mejo med deloma pa hranimo v spremenljivki
 [boundary_sorted]. Na vsakem koraku tako ne izvlečemo najmanjšega elementa
 neurejenga dela tabele temveč poiščemo njegov indeks in ga zamenjamo z
 elementom na meji med deloma (in s tem dodamo na konec urejenega dela).
 Postopek končamo, ko meja doseže konec tabele.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [swap a i j] zamenja elementa [a.(i)] and [a.(j)]. Zamenjavo naredi
 na mestu in vrne unit.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let test = [|0; 1; 2; 3; 4|];;
 val test : int array = [|0; 1; 2; 3; 4|]
 # swap test 1 4;;
 - : unit = ()
 # test;;
 - : int array = [|0; 4; 2; 3; 1|]
[*----------------------------------------------------------------------------*)

let swap a i j = (* a[i], a [j] = a[j], a[i] v pythonu*)
  (*a.(i) <- a.(j)
  a.(j) <- a.(i) ne bo vredu*)
  let z = a.(i) in
  a.(i) <- a.(j); (*; reče izvedi to in rezultat vrzi stran, če pa bi bil nek rezultat nekega tipa. te ocaml opomni, če res hočeš*)
  a.(j) <- z
(*; reče izvedi to in rezultat vrzi stran, če pa bi bil nek rezultat nekega tipa. te ocaml opomni, če res hočeš*)

(*----------------------------------------------------------------------------*]
 Funkcija [index_min a lower upper] poišče indeks najmanjšega elementa tabele
 [a] med indeksoma [lower] and [upper] (oba indeksa sta vključena).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 index_min [|0; 2; 9; 3; 6|] 2 4 = 4
[*----------------------------------------------------------------------------*)

(*ne uporabljaj for zanke v o'camlu, če se le da*)
let index_min a lower upper =
  let cur_ind = ref lower in
  for i = (lower + 1) to upper do
    if a.(i) < a.(!cur_ind) then  (*vedno si pogledam trenutni min iz arraya, to bi se včasih bolje splačalo shranit trenutni min, npr. list*)
      cur_ind := i
    else () (*else ni nujen ?*)
  done
  !cur_ind

(*testni primer je tu narobe*)

(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort_array] implementira urejanje z izbiranjem na mestu. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Namig: Za testiranje uporabi funkciji [Array.of_list] in [Array.to_list]
 skupaj z [randlist].
[*----------------------------------------------------------------------------*)

(*skos bom klical funkcija swap pa index min - od 0 do konca in potem swapam ta index z ničlo vv mojem arrayu po 1*)

