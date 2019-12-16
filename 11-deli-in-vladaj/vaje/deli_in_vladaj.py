###############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end].
#
# Primer: za [start = 0] in [end = 8] tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
###############################################################################

def pivot(a, start, end): #delamo samo na delčku tabele a = table[start: end + 1]

    pivot = a[start]
    left_i = start #start + 0
    right_i = end #start + end
    
    #prestavlamo vse razen pivota, ta ostane na prvem mestu, potem pa samo zamenjamo sredino z pivotom?
    while left_i < right_i: #tuki pazi, ne =!, ker se lahko v enem krogu dvakrat premakne in zamudiš enakost, ker gresta drg čez druzga
        if a[left_i + 1] < pivot:
            left_i += 1
        elif a[right_i] >= pivot:
            right_i -= 1
        else: 
            a[left_i + 1], a[right_i] = a[right_i], a[left_i + 1] #ju ne zmanjšamo in bo šel še enkrat pogledat, če je zdej vredu
            #zdej pa še pivot na začetku damo v sredo z zamenjavo
            #če nista enaka vzamemo desnega ?

    a[start] = a[right_i] 
    a[right_i] = pivot

    return right_i

    #ful je pomembno, da pivot dobro izberemo. zdaj seznam še ni urejen. delili ga bomo na polovice in vsako pivotirali. pivot na polovicah izbiramo naključno


###############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti
# element po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da
# jo rešite brez da v celoti uredite tabelo [a].
###############################################################################

def kth_element(a, k, start = 0, end = None):
    if end is None:
        end = len(a) - 1
    x = pivot(a, start, end)
    if x == k:
        return a[x]
    elif x > k:
        return kth_element(a, k, start, x - 1)    
    else:
        return kth_element(a, (k - x), x + 1, end)
    # moramo samo še en pogoj dopisat, da bo vedu, kdaj se ustavit + nekje je nek indeks narobe


###############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> quicksort(a)
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def quicksort(a, start = 0, end = None):
    if end is None:
        end = len(a) - a
    if start >= end:
        return #nehamo
    else:
        x = pivot(a, start, end) #x je na pravem mestu, še levi in desni seznam od njega posortiramo
        quicksort(a, start, x - 1)
        quicksort(a, x + 1, end)

    #quicksort =q(lower)
    #quicksprt2 = q(upper)
    # quicksort @ [x] @ quicksort2


###############################################################################
# Če imamo dve urejeni tabeli, potem urejeno združeno tabelo dobimo tako, da
# urejeni tabeli zlijemo. Pri zlivanju vsakič vzamemo manjšega od začetnih
# elementov obeh tabel. Zaradi učinkovitosti ne ustvarjamo nove tabele, ampak
# rezultat zapisujemo v že pripravljeno tabelo (ustrezne dolžine).
# 
# Funkcija naj deluje v času O(n), kjer je n dolžina tarčne tabele.
# 
# Sestavite funkcijo [zlij(target, begin, end, list_1, list_2)], ki v del 
# tabele [target] med start in end zlije tabeli [list_1] in [list_2]. V primeru, 
# da sta elementa v obeh tabelah enaka, naj bo prvi element iz prve tabele.
# 
# Primer:
#  
#     >>> list_1 = [1,3,5,7,10]
#     >>> list_2 = [1,2,3,4,5,6,7]
#     >>> target = [-1 for _ in range(len(list_1) + len(list_2))]
#     >>> zlij(target, 0, len(target), list_1, list_2)
#     >>> target
#     [1,1,2,3,3,4,5,5,6,7,7,10]
#
###############################################################################

def zlij(target, begin, end, list_1, list_2):

    len1 = len(list_1)
    len2 = len(list_2)
    mesto1 = 0
    mesto2 = 0

    while mesto1 < len1 and mesto2 < len2: #dokler primerjamo 
        if list_1[mesto1] < list_2[mesto2]:
            target[begin + mesto1 + mesto2] = list_1[mesto1]
            mesto1 += 1
        else:
            target[begin + mesto1 + mesto2] = list_2[mesto2]
            mesto2 += 1
    
    while mesto1 < len1: #prvo izpraznemo
        target[begin + mesto1 + mesto2] = list_1(mesto1)
        mesto1 += 1

    while mesto2 < len2: #še drugo izpraznimo
        target[begin + mesto1 + mesto2] = list_2(mesto2)
        mesto2 += 1

    #v ocamlu si dam na vsakega od seznamov na koncu neskončnost, ki je večja od vseh in naredim primerjavo

#vsako cifro pogledamo dvakrat => linearna časovna zahtevnost


###############################################################################
# Tabelo želimo urediti z zlivanjem (merge sort). 
# Tabelo razdelimo na polovici, ju rekurzivno uredimo in nato zlijemo z uporabo
# funkcije [zlij].
#
# Namig: prazna tabela in tabela z enim samim elementom sta vedno urejeni. !!!
#
# Napišite funkcijo [mergesort(a)], ki uredi tabelo [a] s pomočjo zlivanja.
# Za razliko od hitrega urejanja tu tabele lahko kopirate, zlivanje pa je 
# potrebno narediti na mestu.
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
# >>> mergesort(a)
# [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def mergesort(a, start = 0, end = None):
    #dolzina = len(a)
    if end == None:
        end= len(a)
    #polovica = dolzina // 2

    if len(a) > 1:
        polovica = (start + end) // 2
        mergesort(a, start, polovica)
        mergesort(a, polovica, end)
    pass





#T(n) = n(pivotiram) + 2T(n/2)(grem levo in desno) + n(zlivanje) = log