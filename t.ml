open Critbit

module StringSet = Set.Make(String)

let to_string t = "[" ^ (String.concat "; " (elements t)) ^ "]"

let dump t = print_string (to_string t); print_char '\n'

let time thunk =
  let start = Unix.gettimeofday() in
  let result = thunk() in
  let delta = Unix.gettimeofday() -. start in
  (delta, result)

let mean_of n scale label thunk =
  let rec loop remaining acc result =
    if remaining > 0
    then
      let (delta, result) = time thunk in
      loop (remaining - 1) (acc +. delta) (Some result)
    else (acc, result)
  in
  let (total, result) = loop n 0.0 None in
  let mean = total /. (float_of_int n) in
  let scaled_mean = mean /. (float_of_int scale) in
  Printf.printf "%s: %g µs/op (%g kHz; mean of %d runs)\n%!"
    label
    (scaled_mean *. 1000000.0)
    (0.001 /. scaled_mean)
    n;
  match result with None -> raise Not_found | Some v -> v

let str4 i =
  let padding = 10000 in
  let s = String.make (padding + 4) 'x' in
  let base = 9000 in
  String.set s (base + 0) (char_of_int (i lsr 24));
  String.set s (base + 1) (char_of_int ((i lsr 16) land 255));
  String.set s (base + 2) (char_of_int ((i lsr 8) land 255));
  String.set s (base + 3) (char_of_int (i land 255));
  s

let rec range_fold f n seed =
  if n = 0
  then seed
  else range_fold f (n - 1) (f (n - 1) seed)

let main () =
  (* let x   = add "ac" (add "ab" (add "aa" empty)) in *)
  (* let x'  = add "aa" (add "ac" (add "ab" empty)) in *)
  (* let x'' = add "aa" (add "ac" (add "ab" x')) in *)
  (* dump x; *)
  (* dump x'; *)
  (* dump x''; *)

  let max_count = 50000 in
  let nrepeats = 15 in

  let str4s = range_fold (fun n a -> str4 n :: a) max_count [] in
  let str4s_probe = range_fold (fun n a -> str4 n :: a) max_count [] in
  let missing = str4 (max_count * 2) in

  let str4s_fold_gen ss f seed =
    let rec loop xs seed = match xs with
      | [] -> seed
      | x :: xs' -> loop xs' (f x seed)
    in loop ss seed
  in

  let str4s_fold f seed = str4s_fold_gen str4s f seed in
  let str4s_probe_fold f seed = str4s_fold_gen str4s_probe f seed in

  mean_of nrepeats max_count "Baseline"
    (fun () -> str4s_fold (fun n a -> let _ = n in a) ());


  let full_c = mean_of nrepeats max_count "Critbit   - insertion"
    (fun () -> str4s_fold (fun n a -> add n a) empty) in
  let full_s = mean_of nrepeats max_count "StringSet - insertion"
    (fun () -> str4s_fold (fun n a -> StringSet.add n a) StringSet.empty) in

  let _ = mean_of nrepeats max_count "Critbit   - removal"
    (fun () -> str4s_probe_fold (fun n a -> remove n a) full_c) in
  let _ = mean_of nrepeats max_count "StringSet - removal"
    (fun () -> str4s_probe_fold (fun n a -> StringSet.remove n a) full_s) in

  mean_of nrepeats max_count "Critbit   - positive membership"
    (fun () -> str4s_probe_fold (fun n a -> let _ = mem n full_c in a) ());
  mean_of nrepeats max_count "StringSet - positive membership"
    (fun () -> str4s_probe_fold (fun n a -> let _ = StringSet.mem n full_s in a) ());

  mean_of nrepeats max_count "Critbit   - negative membership"
    (fun () -> str4s_probe_fold (fun n a -> let _ = mem missing full_c in a) ());
  mean_of nrepeats max_count "StringSet - negative membership"
    (fun () -> str4s_probe_fold (fun n a -> let _ = StringSet.mem missing full_s in a) ());

  mean_of nrepeats max_count "Critbit   - cardinality"
    (fun () -> str4s_probe_fold (fun n a -> let _ = cardinal full_c in a) ());
  (* mean_of nrepeats max_count "StringSet - cardinality" *)
  (*   (fun () -> str4s_probe_fold (fun n a -> let _ = StringSet.cardinal full_s in a) ()); *)

  ();;

main ()
