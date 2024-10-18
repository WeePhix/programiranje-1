let rec accending_list_gen i n =
  match i with
  | _ when i = n -> []
  | _ when i < n -> i :: (accending_list_gen (i+1) n)
  | _ -> []


let rec filter_mapi_rec f i l =
    match (l, i) with
    | (lh::lt, ih::it) when Option.is_some (f ih lh) -> Option.get (f ih lh) :: filter_mapi_rec f it lt
    | (lh::lt, ih::it) -> filter_mapi_rec f it lt
    | _ -> []

    
(*^^^ POMOZNE FUNKCIJE ^^^*)


let rec stevke b n =
  let r = n mod b in
  match n with
  | _ when n < b -> n :: []
  | _ -> stevke b ((n - r) / b) @ r :: []


let rec take n l =
  match l with
  | [] -> []
  | _ when n = 0 -> []
  | h::t -> h::(take (n-1) t)


let rec drop_while p l = 
  match l with
  | h::t when p h -> drop_while p t
  | _ -> l


let rec filter_mapi f l =
  filter_mapi_rec f (accending_list_gen 0 (List.length l)) l