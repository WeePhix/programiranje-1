(*----------------------------------------------------------------------------*
 # 4. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste napisali svoj simulator Turingovih strojev. Zaradi
 preprostosti bomo za abecedo vzeli kar znake tipa `char`, za prazni znak bomo
 izbrali presledek `' '`, stanja pa bomo predstavili z nizi. Za možne premike
 zafiksiramo tip `direction`:
[*----------------------------------------------------------------------------*)

type direction = Left | Right
type state = string

(*----------------------------------------------------------------------------*
 ## Implementacija trakov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Tape`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip v obe smeri neomejenih trakov in glavo na danem mestu;
 - `make`, ki naredi nov trak z znaki iz niza ter glavo na prvem znaku;
 - `read`, ki vrne znak pod glavo;
 - `write`, ki pod glavo zapiše dani znak;
 - `move`, ki glavo premakne v dano smer;
 - `print`, ki izpiše vsebino traku (brez presledkov na začetku in koncu) ter
 pod njim z `^` označi mesto glave.

 Zadnji dve funkciji naj vrneta nov trak, obstoječega pa naj pustita
 nespremenjenega.

 Ker je tip `t` abstrakten, si lahko privoščite poljubno implementacijo, zato
 poskrbite tako za učinkovitost kot za preglednost kode.
[*----------------------------------------------------------------------------*)

module type TAPE = sig
  type t

  val make : string -> t
  val move : direction -> t -> t
  val read : t -> char
  val write : char -> t -> t
  val print : t -> unit
end


let rec reverse acc = function
    | [] -> acc
    | x :: xs -> reverse (x :: acc) xs

module Tape : TAPE = struct
  type t = char list * char list
  let make inp : t =
    let rec aux acc = function
    | "" -> [] 
    | s when String.length s = 1 -> s.[0] :: acc
    | s -> aux (s.[(String.length s - 1)] :: acc) (String.sub s 0 (String.length s - 1))
    in
      [], (aux [] inp)
  let move d (t : t) : t =
    let (left, right) = match t, d with
    | (l, []), Right -> (' ' :: l, [])
    | (l, x::xs), Right -> (x :: l, xs)
    | ([], r), Left -> ([], ' ' :: r)
    | (x::xs, r), Left -> (xs, x :: r)
    in
    
    (* trims leading whitespaces in a list *)
    let rec trim = function
      | [] -> []
      | ' ' :: xs -> trim xs
      | l -> l
    in
    let final = left |> reverse [] |> trim |> reverse [], right |> reverse [] |> trim |> reverse [] in
    if snd final = [] then fst final, [' '] else final

  let read (t : t) =
    match snd t with
    | [] -> ' '
    | x :: xs -> x
  let write c t = match t with
  | l, [] -> l, [c]
  | l, x::xs -> l, c :: xs
  let print (t : t) =
    let left, right = reverse [] (fst t), snd t in
    let _ = List.map print_char (left @ right) in
    let _ = print_newline () in
    print_string ((String.make (List.length left) ' ') ^ "^" ^ "\n")
end

let primer_trak = Tape.(
  make "ABCDE"
  |> move Left
  |> move Left
  |> move Right
  |> move Right
  |> move Right
  |> move Right
  |> write '!'
  |> print
)
(*
AB!DE
  ^
*)
(* val primer_trak : unit = () *)

(*----------------------------------------------------------------------------*
 ## Implementacija Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Machine`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip Turingovih strojev;
 - `make`, ki naredi nov stroj z danim začetnim stanjem in seznamom preostalih
 stanj ter prazno prehodno funkcijo;
 - `initial`, ki vrne začetno stanje stroja;
 - `add_transition`, ki prehodno funkcijo razširi s prehodom $(q, a) \mapsto
 (q', a', d)$;
 - `step`, ki za dano stanje in trak izvede en korak stroja, če je to mogoče.

 Zadnji dve funkciji naj vrneta spremenjene vrednosti, obstoječe argumente pa
 naj pustita nespremenjene. Prav tako pri zadnjih dveh funkcijah lahko
 predpostavite, da ju bomo klicali le na poprej podanih stanjih.

 Tudi tu je tip `t` abstrakten, zato poskrbite za učinkovitost in preglednost
 kode.
[*----------------------------------------------------------------------------*)

open Map
module OrderedKey = struct
  type t = state * char
  let compare = compare
  let make s c : t = (s, c)
end
module States = Map.Make(OrderedKey)


module type MACHINE = sig
  type t
  val make : state -> state list -> t
  val initial : t -> state
  val add_transition : state -> char -> state -> char -> direction -> t -> t
  val step : t -> state -> Tape.t -> (state * Tape.t) option
end


module Machine : MACHINE = struct
  type t = {
    initial : state;
    states : state list;
    transitions : (state * char * direction) States.t
    }
  let make s ss : t = {initial = s; states = ss; transitions = States.empty}
  let initial (t: t) = t.initial
  let add_transition (s : state) (c : char) (s' : state) (c' : char) (d: direction) (t : t) : t =
    {t with transitions = States.add (OrderedKey.make s c) (s', c', d) t.transitions}
  let step (t : t) (state : state) (tape : Tape.t) =
    let change = States.find_opt (OrderedKey.make state (Tape.read tape)) t.transitions in
    match change with
    | None -> None
    | Some (s, c, d) -> Some (s, Tape.move d (Tape.write c tape))
end
(*----------------------------------------------------------------------------*
 Primer stroja "Binary Increment" na <http://turingmachine.io> lahko
 implementiramo kot:
[*----------------------------------------------------------------------------*)

let binary_increment =
  Machine.(
    make "right" [ "carry"; "done" ]
    |> add_transition "right" '1' "right" '1' Right
    |> add_transition "right" '0' "right" '0' Right
    |> add_transition "right" ' ' "carry" ' ' Left
    |> add_transition "carry" '1' "carry" '0' Left
    |> add_transition "carry" '0' "done" '1' Left
    |> add_transition "carry" ' ' "done" '1' Left
  )

(* val binary_increment : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 Zapišite funkciji `slow_run` in `speed_run` tipa `Machine.t -> str -> unit`, ki
 simulirata Turingov stroj na traku, na katerem je na začetku zapisan dani niz.
 Prva naj izpiše trakove in stanja pri vseh vmesnih korakih, druga pa naj izpiše
 le končni trak. Slednjo bomo uporabljali tudi pri meritvi učinkovitosti
 izvajanja.
[*----------------------------------------------------------------------------*)

let slow_run machine tape =
  let actual_tape = Tape.make tape in
  let rec aux = function
  | None -> ()
  | Some (s, t) ->
    let _ = Tape.print t in
    let _ = print_string (s ^ "\n") in
    aux (Machine.step machine s t)
  in
  aux (Some (Machine.initial machine, actual_tape))
let primer_slow_run =
  slow_run binary_increment "1011"
(*
1011
^
right
1011
 ^
right
1011
  ^
right
1011
   ^
right
1011
    ^
right
1011
   ^
carry
1010
  ^
carry
1000
 ^
carry
1100
^
done
*)
(* val primer_slow_run : unit = () *)

let speed_run machine tape =
  let initial = Machine.initial machine in
  let actual_tape = Tape.make tape in
  let rec aux acc = function
  | None -> acc
  | Some (s, t) ->
    let new_state = Machine.step machine s t in
    aux (t :: acc) (new_state)
  in
  match aux [] (Some (initial, actual_tape)) with
  | [] -> Tape.print actual_tape
  | x :: xs -> Tape.print (x)

let primer_speed_run =
  speed_run binary_increment "1011"
(*
1100
^
*)
(* val primer_speed_run : unit = () *)

(*----------------------------------------------------------------------------*
 ## Krajši zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ko definiramo Turingov stroj, prehode običajno združujemo najprej po stanjih,
 nato pa še po znakih. Prav tako pri dosti prehodih samo premikamo glavo, trak
 in stanje pa pustimo pri miru. Zapišite funkcije:

 - `for_state`
 - `for_character`
 - `for_characters`
 - `move`
 - `switch_and_move`
 - `write_and_move`
 - `write_switch_and_move`

 s katerimi bi lahko zgornji primer na krajše zapisali kot spodaj.
 Implementacijo in tipe ugotovite sami.
[*----------------------------------------------------------------------------*)


(* let binary_increment' =
  Machine.make "right" ["carry"; "done"]
  |> for_state "right" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "carry" Left
  ]
  |> for_state "carry" [
    for_character '1' @@ switch_and_move "carry" Left;
    for_characters "0 " @@ write_switch_and_move '1' "done" Left
  ]   *)
(* val binary_increment' : Machine.t = <abstr> *)

let rec for_state s (transitions : (state -> Machine.t -> Machine.t) list) m =
  match transitions with
  | [] -> m
  | x :: xs -> for_state s xs (x s m)

let for_character c (t : state -> char -> Machine.t -> Machine.t) s m = t s c m

let for_characters text (t : state -> char -> Machine.t -> Machine.t) s m =
  let rec aux acc = function
    | "" -> acc
    | st when String.length st = 1 -> for_character st.[0] t s acc
    | st when String.length st > 1 -> aux (for_character st.[0] t s acc) (String.sub st 1 (String.length st - 1))
    | _ -> assert false
  in
  aux m text

let move d s c m = Machine.add_transition s c s c d m
let switch_and_move s' d s c m = Machine.add_transition s c s' c d m
let write_and_move c' d s c m = Machine.add_transition s c s c' d m
let write_switch_and_move c' s' d s c m = Machine.add_transition s c s' c' d m
  


let binary_increment' =
  Machine.make "right" ["carry"; "done"]
  |> for_state "right" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "carry" Left
  ]
  |> for_state "carry" [
    for_character '1' @@ write_and_move '0' Left;
    for_characters "0 " @@ write_switch_and_move '1' "done" Left
  ]

let _ = speed_run binary_increment' "101110111"


let _ = print_string "\n\n\n=====================================\n=====================================\n=====================================\n\n\n"
(*----------------------------------------------------------------------------*
 ## Primeri Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste sestavljali stroje, ki bodo iz začetnega niza na traku na
 različne načine izračunali nov niz. Pri tem lahko predpostavite, da je začetni
 niz sestavljen iz ničel in enic, preostanek traku pa je prazen. Na koncu
 izvajanja naj bo glava na začetku novega niza, z izjemo tega niza pa naj bo
 trak prazen. Ni pa treba, da se izračunani niz začne na istem mestu na traku,
 kot se je začel prvotni niz.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Obračanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki začetni niz obrne na glavo.
[*----------------------------------------------------------------------------*)

let reverse = Machine.make "read" ["fst0>"; "fst1>"; "w0<"; "w1<"; "w0>"; "w1>"; "r>"; "c0<"; "c1<"; "c0>"; "c1>"; "ret"; "done"] |>
  for_state "read" [
    for_character '0' @@ write_switch_and_move 'o' "fst0>" Right;
    for_character '1' @@ write_switch_and_move 'i' "fst1>" Right
  ] |>
  for_state "fst0>" [
    for_character ' ' @@ switch_and_move "w0<" Left;
    for_character '0' @@ write_and_move 'o' Right;
    for_character '1' @@ write_and_move 'i' Right
  ] |>
  for_state "fst1>" [
    for_character ' ' @@ switch_and_move "w1<" Left;
    for_character '0' @@ write_and_move 'o' Right;
    for_character '1' @@ write_and_move 'i' Right
  ] |>
  for_state "w0<" [
    for_character 'o' @@ write_switch_and_move '0' "c0<" Left;
    for_character 'i' @@ write_switch_and_move '0' "c1<" Left;
    for_characters "01 " @@ switch_and_move "ret" Left
  ] |>
  for_state "w1<" [
    for_character 'o' @@ write_switch_and_move '1' "c0<" Left;
    for_character 'i' @@ write_switch_and_move '1' "c1<" Left;
    for_characters "01 " @@ switch_and_move "ret" Left
  ] |>
  for_state "w0>" [
    for_character 'o' @@ write_switch_and_move '0' "r>" Right;
    for_character 'i' @@ write_switch_and_move '0' "r>" Right;
    for_characters "01 " @@ switch_and_move "ret" Left
  ] |>
  for_state "w1>" [
    for_character 'o' @@ write_switch_and_move '1' "r>" Right;
    for_character 'i' @@ write_switch_and_move '1' "r>" Right;
    for_characters "01 " @@ switch_and_move "ret" Left
  ] |>
  for_state "r>" [
    for_character 'o' @@ switch_and_move "c0>" Right;
    for_character 'i' @@ switch_and_move "c1>" Right;
    for_characters "01" @@ switch_and_move "ret" Left
  ] |>
  for_state "c0<" [
    for_characters "oi" @@ move Left;
    for_characters " 01" @@ switch_and_move "w0>" Right
  ] |>
  for_state "c1<" [
    for_characters "oi" @@ move Left;
    for_characters " 01" @@ switch_and_move "w1>" Right
  ] |>
  for_state "c0>" [
    for_characters "oi" @@ move Right;
    for_characters " 01" @@ switch_and_move "w0<" Left
  ] |>
  for_state "c1>" [
    for_characters "oi" @@ move Right;
    for_characters " 01" @@ switch_and_move "w1<" Left
  ] |>
  for_state "ret" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ switch_and_move "done" Right
  ]



let primer_reverse = speed_run reverse "0000111001"
(* 
1001110000          
^
*)
(* val primer_reverse : unit = () *)

(*----------------------------------------------------------------------------*
 ### Podvajanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki podvoji začetni niz.
[*----------------------------------------------------------------------------*)

let duplicate = Machine.make "fstread" ["fst0"; "fst1"; "replace"; "left"; "c0"; "c1"; "done"] |>
  for_state "fstread" [
    for_character '0' @@ write_switch_and_move 'o' "fst0" Right;
    for_character '1' @@ write_switch_and_move 'i' "fst1" Right
  ] |>
  for_state "fst0" [
    for_character '0' @@ write_and_move 'o' Right;
    for_character '1' @@ write_switch_and_move 'i' "fst1" Right;
    for_character ' ' @@ write_switch_and_move '0' "replace" Left
  ] |>
  for_state "fst1" [
    for_character '0' @@ write_switch_and_move 'o' "fst0" Right;
    for_character '1' @@ write_and_move 'i' Right;
    for_character ' ' @@ write_switch_and_move '1' "replace" Left
  ] |>
  for_state "replace" [
    for_character 'o' @@ write_switch_and_move '0' "left" Left;
    for_character 'i' @@ write_switch_and_move '1' "left" Left
  ] |>
  for_state "left" [
    for_characters "01" @@ move Left;
    for_character 'o' @@ write_switch_and_move '0' "c0" Right;
    for_character 'i' @@ write_switch_and_move '1' "c1" Right;
    for_character ' ' @@ switch_and_move "done" Right
  ] |>
  for_state "c0" [
    for_character '0' @@ move Right;
    for_character '1' @@ write_switch_and_move '0' "c1" Right;
    for_character ' ' @@ write_switch_and_move '0' "left" Left
  ] |>
  for_state "c1" [
    for_character '1' @@ move Right;
    for_character '0' @@ write_switch_and_move '1' "c0" Right;
    for_character ' ' @@ write_switch_and_move '1' "left" Left
  ]

let primer_duplicate = speed_run duplicate "010011"
(* 
001100001111
^
*)
(* val primer_duplicate : unit = () *)

(*----------------------------------------------------------------------------*
 ### Eniški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki na začetku na traku sprejme število $n$, zapisano
 v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih natanko $n$ enic.
[*----------------------------------------------------------------------------*)

let to_unary = Machine.make "truncate" ["right"; "decrement"; "carry"; "increment"; "return"; "clear"; "done"] |>
  for_state "truncate" [
    for_character '0' @@ write_and_move ' ' Right;
    for_character '1' @@ switch_and_move "right" Right;
    for_character ' ' @@ switch_and_move "done" Left
  ] |>
  for_state "right" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ write_switch_and_move '|' "decrement" Left
  ] |>
  for_state "decrement" [
    for_character '0' @@ write_and_move '1' Left;
    for_character '1' @@ write_switch_and_move '0' "carry" Right;
    for_character ' ' @@ switch_and_move "clear" Right
  ] |>
  for_state "carry" [
    for_characters "01" @@ move Right;
    for_character '|' @@ switch_and_move "increment" Right
  ] |>
  for_state "increment" [
    for_character '1' @@ move Right;
    for_character ' ' @@ write_switch_and_move '1' "return" Left
  ] |>
  for_state "return" [
    for_characters "1" @@ move Left;
    for_character '|' @@ switch_and_move "decrement" Left
  ] |>
  for_state "clear" [
    for_character '1' @@ write_and_move ' ' Right;
    for_character '|' @@ write_switch_and_move ' ' "done" Right
  ]


let primer_to_unary = speed_run to_unary "1010"
(* 
1111111111
^
*)
(* val primer_to_unary : unit = () *)

(*----------------------------------------------------------------------------*
 ### Dvojiški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku
 sprejme število $n$ enic, na koncu pa naj bo na traku zapisano število $n$ v
 dvojiškem zapisu.
[*----------------------------------------------------------------------------*)

let to_binary = Machine.make "start" [] |>
  for_state "start" [
    for_character '1' @@ write_switch_and_move '|' "increment" Left;
    for_character ' ' @@ write_switch_and_move '0' "done_zero" Left
  ] |>
  for_state "done_zero" [
    for_character ' ' @@ switch_and_move "done" Right
  ] |>
  for_state "increment" [
    for_character '1' @@ write_and_move '0' Left;
    for_characters " 0" @@ write_switch_and_move '1' "return_binary" Right
  ] |>
  for_state "return_binary" [
    for_characters "01" @@ move Right;
    for_character '|' @@ switch_and_move "return_unary" Right
  ] |>
  for_state "return_unary" [
    for_character '1' @@ move Right;
    for_character ' ' @@ switch_and_move "take" Left
  ] |>
  for_state "take" [
    for_character '1' @@ write_switch_and_move ' ' "carry" Left;
    for_character '|' @@ write_switch_and_move ' ' "done_return" Left
  ] |>
  for_state "carry" [
    for_character '1' @@ move Left;
    for_character '|' @@ switch_and_move "increment" Left
  ] |>
  for_state "done_return" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ switch_and_move "done" Right
  ]

let primer_to_binary = speed_run to_binary (String.make 42 '1')
(* 
101010                                           
^
*)
(* val primer_to_binary : unit = () *)