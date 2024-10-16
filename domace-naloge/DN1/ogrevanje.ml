let rec stevke b n =
  let o = n mod b in
  let m = (n - o) / b in
  if (m <= b) then [m; o] else List.flatten [stevke b m; [o]]


let rec take n l =
  if n = 0 || l = []
    then []
  else if n = 1
    then [List.hd l]
  else
    [List.hd l] @ take (n-1) (List.tl l)


let rec drop_while p l =
  if (p (List.hd l)) then [List.hd l] @ drop_while p (List.tl l)
  else []

let filter_mapi p l =
  let rec index_list i n = if i = n then [] else [i] @ index_list (i+1) n in
  let indices = index_list 0 (List.length l) in

  let rec checker si sl =
    if sl = [] then []
    else
      let p_on_el = p List.hd si List.hd sl in
      if not (p_on_el = None) then
      [List.hd si; p_on_el] @ checker (List.tl si) (List.tl sl)
    else checker (List.tl si) (List.tl sl) in
  checker indices l

let l = [1;2;3;4;5;6;7;8;9]