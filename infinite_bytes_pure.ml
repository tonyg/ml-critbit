let integer_length i =
  let rec loop i n = if i = 0 then n else loop (i lsr 1) (n + 1) in
  loop i 0

(* let infinite_bytes_eq a b = *)
(*   let limit = max (String.length a) (String.length b) in *)
(*   let rec check i = *)
(*     if i = limit then true *)
(*     else if infinite_bytes_ref a i = infinite_bytes_ref b i then check (i + 1) *)
(*     else false *)
(*   in check 0 *)

(* let infinite_bytes_diffbitpos a b = *)
(*   let limit = max (String.length a) (String.length b) in *)
(*   let rec find_differing_byte i = *)
(*     if i = limit *)
(*     then -1 (\* they're the same (infinite zeros on the end!) byte string *\) *)
(*     else *)
(*       let delta = (infinite_bytes_ref a i) lxor (infinite_bytes_ref b i) in *)
(*       if delta = 0 *)
(*       then find_differing_byte (i + 1) *)
(*       else *)
(* 	let bit = 8 - (integer_length delta) (\* 0th bit is high *\) in *)
(* 	i * 8 + bit *)
(*   in find_differing_byte 0 *)

let infinite_bytes_eq a b =
  let la = String.length a in
  let lb = String.length b in
  let switch = min la lb in
  let limit = max la lb in
  let rec check x i =
    if i = limit then true
    else if String.unsafe_get x i = '\000' then check x (i + 1)
    else false
  in
  let rec check_lower i =
    if i = switch then
      (if la < lb then check b i
       else if lb < la then check a i
       else true)
    else if String.unsafe_get a i = String.unsafe_get b i then check_lower (i + 1)
    else false
  in
  if la = lb
  then String.compare a b = 0
  else check_lower 0

let infinite_bytes_diffbitpos a b =
  let la = String.length a in
  let lb = String.length b in
  let switch = min la lb in
  let limit = max la lb in
  let rec find_first_nonzero x i =
    if i = limit
    then -1 (* they're the same (infinite zeros on the end!) byte string *)
    else
      let delta = int_of_char (String.unsafe_get x i) in
      if delta = 0
      then find_first_nonzero x (i + 1)
      else
	let bit = 8 - (integer_length delta) (* 0th bit is high *) in
	i * 8 + bit
  in
  let rec find_differing_byte i =
    if i = switch then
      (if la < lb then find_first_nonzero b i
       else if lb < la then find_first_nonzero a i
       else -1) (* they're the same (infinite zeros on the end!) byte string *)
    else
      let delta = int_of_char (String.unsafe_get a i) lxor int_of_char (String.unsafe_get b i) in
      if delta = 0
      then find_differing_byte (i + 1)
      else
	let bit = 8 - (integer_length delta) (* 0th bit is high *) in
	i * 8 + bit
  in find_differing_byte 0
