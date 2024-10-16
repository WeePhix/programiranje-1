(* ========== Vaja 2: Uvod v funkcijsko programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Vektorje predstavimo kot seznam števil s plavajočo vejico.
[*----------------------------------------------------------------------------*)

type vector = float list

(*----------------------------------------------------------------------------*]
Definirajte enotske vektorje `i`, `j` in `k` v treh dimenzijah.
[*----------------------------------------------------------------------------*)

let i = [1.; 0.; 0.]
let j = [0.; 1.; 0.]
let k = [0.; 0.; 1.]

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razteg : float -> vector -> vector`, ki vektor, 
predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

let rec razteg a v = List.map (fun x -> a*.x) v

(*----------------------------------------------------------------------------*]
Napišite funkcijo `sestej : vector -> vector -> vector`, ki vrne vsoto dveh 
vektorjev.
[*----------------------------------------------------------------------------*)

let rec sestej u v =
  let w = List.combine u v in
  List.map (fun (x1, x2) -> x1 +. x2) w

(*----------------------------------------------------------------------------*]
Napišite funkcijo `skalarni_produkt : vector -> vector -> float`, ki izračuna 
skalarni produkt dveh vektorjev
[*----------------------------------------------------------------------------*)

let rec skalarni_produkt u v =
  let w = List.combine u v in
  let prods = List.map (fun (x1, x2) -> x1 *. x2) w in
  List.fold_left (+.) 0. prods

(*----------------------------------------------------------------------------*]
Napišite funkcijo `norma : vector -> float`, ki vrne evklidsko normo vektorja.
[*----------------------------------------------------------------------------*)

let rec norma v = sqrt (skalarni_produkt v v)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `projeciraj : vector -> vector -> vector`, ki izračuna 
projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let rec projeciraj a b = razteg ((skalarni_produkt a b) /. (norma a)**2.) b

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML 
oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.

Primer:
`ovij "h1" "Hello, world!"`

[*----------------------------------------------------------------------------*)

let rec ovij tag contents = "<" ^ tag ^ ">" ^ contents ^ "</" ^ tag ^ ">"

(*----------------------------------------------------------------------------*]
Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število 
presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za ustrezno število presledkov.

Primer:
`zamakni 4 "Hello, world!"`

[*----------------------------------------------------------------------------*)

let rec zamakni n string =
  let lines = String.split_on_char '\n' string in
  let padding = "\n" ^ String.make n ' ' in
  String.concat padding lines

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne 
niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:

Primer:
`ul ["ananas"; "banana"; "čokolada"]`

[*----------------------------------------------------------------------------*)

let rec ul elements = ovij "ul" (zamakni 4 (("\n" ^ (String.concat "\n"(List.map (ovij "li") (List.map (fun str -> "\n    " ^ str ^ "\n") elements))))) ^ "\n")



(*----------------------------------------------------------------------------*]
Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme niz, 
ki vsebuje vejico, loči na del pred in del za njo.

Primer:
`razdeli_vrstico "mleko, 2"`

[*----------------------------------------------------------------------------*)

let rec razdeli_vrstico line =
  let sides = String.split_on_char ',' line in
  List.hd sides, String.trim (List.hd (List.tl sides))

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`, 
ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike 
"izdelek, vrednost", in vrne seznam ustreznih parov.

Primer:
`pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"`

[*----------------------------------------------------------------------------*)

let rec pretvori_v_seznam_parov str =
  let arr_str = String.split_on_char '\n' str in
  List.map razdeli_vrstico arr_str

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list -> (string * 'b) list`,
ki dano funkcijo uporabi na vseh drugih komponentah elementov seznama.

Primer:
```ml
let seznam = [("ata", "mama"); ("teta", "stric")] in 
pretvori_druge_komponente String.length seznam
```

[*----------------------------------------------------------------------------*)

let rec pretvori_druge_komponente f tuples_list =
  let a, b = List.split tuples_list in
  List.combine a (List.map f b)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki 
sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni 
znesek nakupa.

Primer:
```ml
let nakupovalni_seznam = "mleko, 2\njabolka, 5"
and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
izracunaj_skupni_znesek cenik nakupovalni_seznam
```

[*----------------------------------------------------------------------------*)

let rec izracunaj_skupni_znesek costs_text shopping_text =
  let costs_arr = pretvori_v_seznam_parov costs_text in
  let ingredients, quantities_str = List.split (pretvori_v_seznam_parov shopping_text) in
  let quantities_float = List.map float_of_string quantities_str in
  let prices = List.map float_of_string (List.map (fun ing -> List.assoc ing costs_arr) ingredients) in
  List.fold_left (+.) 0. (List.map2 (fun x y -> x *. y) prices quantities_float)


let shop_list = "apple, 5\ncherry, 7\nbanana, 2"
let price_list = "bread, 3\ncherry, 0.5\napple, 1\nbanana, 1.5"