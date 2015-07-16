open Infinite_bytes

type elt = string (* really, bytes rather than string *)

type index = int (* non-negative *)

type node =
  | Leaf of elt
  | Node of index * node * node

type t =
  | Empty
  | Nonempty of int * node

let empty = Empty

let is_empty t = match t with
  | Empty -> true
  | Nonempty _ -> false

let infinite_bytes_ref bs n =
  if n >= String.length bs
  then 0 (* treat byte-strings as followed by an infinite suffix of zeroes *)
  else int_of_char (String.unsafe_get bs n)

let is_bit_set n bit = (n land (1 lsl bit)) <> 0

let bit_ref bs n =
  let byte_index = n / 8 in
  let bit_index = 7 - (n mod 8) in
  is_bit_set (infinite_bytes_ref bs byte_index) bit_index

type insertion =
  | Pos of index
  | Kid of node

let join a b =
  let pos = infinite_bytes_diffbitpos a b in
  if pos = -1
  then Kid (Leaf a) (* they're the same (infinite zeros on the end!) byte string *)
  else Pos pos

let mem k t = match t with
  | Empty -> false
  | Nonempty (_, n) ->
    let rec walk n = match n with
      | Leaf bs -> infinite_bytes_eq bs k
      | Node (index, zero, one) -> walk (if bit_ref k index then one else zero)
    in walk n

let add k t = match t with
  | Empty -> Nonempty (1, Leaf k)
  | Nonempty (old_count, n) ->
    let splice_key p sib =
      if bit_ref k p
      then Node (p, sib, Leaf k)
      else Node (p, Leaf k, sib)
    in
    let rec walk n = match n with
      | Leaf bs -> join bs k
      | Node (index, zero, one) ->
	let maybe_splice kid stitch =
	  let kid' = walk kid in
	  match kid' with
	    | Pos p -> if p < index then kid' else stitch (splice_key p kid)
	    | Kid kid'' -> if kid == kid'' then Kid n else stitch kid''
	in
	if bit_ref k index
	then maybe_splice one (fun sib -> Kid (Node (index, zero, sib)))
	else maybe_splice zero (fun sib -> Kid (Node (index, sib, one)))
    in Nonempty (old_count + 1, (* BUG: incorrect when k already exists in t *)
		 (match walk n with
		   | Pos p -> splice_key p n
		   | Kid n' -> n'))

let remove k t = match t with
  | Empty -> Empty
  | Nonempty (old_count, n) ->
    let removed_count = ref 0 in
    let rec walk n = match n with
      | Leaf bs -> if infinite_bytes_eq bs k then (removed_count := 1; None) else Some n
      | Node (index, zero, one) ->
	if bit_ref k index
	then match walk one with None -> Some zero | Some one' -> Some (Node (index, zero, one'))
	else match walk zero with None -> Some one | Some zero' -> Some (Node (index, zero', one))
    in match walk n with
      | None -> Empty
      | Some n' -> Nonempty (old_count - !removed_count, n')

let cardinal t = match t with
  | Empty -> 0
  | Nonempty (count, _) -> count

let elements t = match t with
  | Empty -> []
  | Nonempty (_, n) ->
    let rec walk n acc = match n with
      | Leaf bs -> bs :: acc
      | Node (_, zero, one) -> walk zero (walk one acc)
    in walk n []

let of_list xs = List.fold_right add xs empty
