let rec stevke b n =
  let o = n mod b in
  let m = (n - o) / b in
  if (m <= b) then [m; o] else List.flatten [stevke b m; [o]]