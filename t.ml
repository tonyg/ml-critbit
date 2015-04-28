open Critbit

module StringSet = Set.Make(String)

let to_string t = "[" ^ (String.concat "; " (elements t)) ^ "]"

let dump t = print_string (to_string t); print_char '\n'

let time thunk =
  let start = Unix.gettimeofday() in
  let result = thunk() in
  let delta = Unix.gettimeofday() -. start in
  (delta, result)

let rec sum_of samples = match samples with
  | [] -> 0.0
  | x :: xs -> x +. sum_of xs

let mean_of samples = sum_of samples /. (float_of_int (List.length samples))

let sqr_sub mean x =
  let x_minus_mean = x -. mean in
  x_minus_mean *. x_minus_mean

let unbiased_sample_variance_of samples =
  let mean = mean_of samples in
  sum_of (List.map (sqr_sub mean) samples) /. (float_of_int (List.length samples - 1))

let sample_standard_deviation_of samples = sqrt (unbiased_sample_variance_of samples)

let standard_error_of_mean_of samples =
  sample_standard_deviation_of samples /. sqrt (float_of_int (List.length samples))

let stats_header additional_info =
  Printf.printf "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n%!"
    "label"
    "q0_microsec"
    "q1_microsec"
    "q2_microsec"
    "q3_microsec"
    "q4_microsec"
    "lo_95ci_microsec"
    "mean_microsec"
    "hi_95ci_microsec"
    "q0_rate_kHz"
    "q1_rate_kHz"
    "q2_rate_kHz"
    "q3_rate_kHz"
    "q4_rate_kHz"
    "lo_95ci_rate_kHz"
    "mean_rate_kHz"
    "hi_95ci_rate_kHz"
    "nrepeats"
    "problem_size"
    (String.concat "," additional_info)

let stats n scale additional_info label thunk =
  let rec loop remaining =
    if remaining > 0
    then
      let (delta, _) = time thunk in
      delta :: loop (remaining - 1)
    else []
  in
  let result = thunk() in (* discard a single run to warm up *)
  let samples = loop n in
  let q0 = 0 in
  let q1 = n / 4 in
  let q2 = n / 2 in
  let q3 = n * 3 / 4 in
  let q4 = n - 1 in
  let sorted_samples = List.sort compare samples in
  let q0_val = List.nth sorted_samples q0 in
  let q1_val = List.nth sorted_samples q1 in
  let q2_val = List.nth sorted_samples q2 in
  let q3_val = List.nth sorted_samples q3 in
  let q4_val = List.nth sorted_samples q4 in
  let mean = mean_of samples in
  let standard_error_of_mean = standard_error_of_mean_of samples in
  let half_ci = 1.96 *. standard_error_of_mean in
  let lo_ci = mean -. half_ci in
  let hi_ci = mean +. half_ci in
  let scale_f = float_of_int scale in
  let scaled_q0_val = q0_val /. scale_f in
  let scaled_q1_val = q1_val /. scale_f in
  let scaled_q2_val = q2_val /. scale_f in
  let scaled_q3_val = q3_val /. scale_f in
  let scaled_q4_val = q4_val /. scale_f in
  let scaled_mean = mean /. scale_f in
  let scaled_lo_ci = lo_ci /. scale_f in
  let scaled_hi_ci = hi_ci /. scale_f in
  let microseconds = 1000000.0 in
  let one_millisecond = 0.001 in
  Printf.printf "%s,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g,%d,%d,%s\n%!"
    label
    (scaled_q0_val *. microseconds)
    (scaled_q1_val *. microseconds)
    (scaled_q2_val *. microseconds)
    (scaled_q3_val *. microseconds)
    (scaled_q4_val *. microseconds)
    (scaled_lo_ci *. microseconds)
    (scaled_mean *. microseconds)
    (scaled_hi_ci *. microseconds)
    (one_millisecond /. scaled_q4_val)
    (one_millisecond /. scaled_q3_val)
    (one_millisecond /. scaled_q2_val)
    (one_millisecond /. scaled_q1_val)
    (one_millisecond /. scaled_q0_val)
    (one_millisecond /. scaled_hi_ci)
    (one_millisecond /. scaled_mean)
    (one_millisecond /. scaled_lo_ci)
    n
    scale
    (String.concat "," additional_info);
  result

let str4 padding base i =
  let s = String.make (padding + 4) 'x' in
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

  let (max_count, nrepeats, padding, base) =
    match Sys.argv with
      | [| _ |] -> (50000, 15, 10000, 9000)
      | [| _; c; n; p; b |] -> (int_of_string c, int_of_string n, int_of_string p, int_of_string b)
      | _ ->
	(Printf.printf "Usage: t.native | t.native problem_size nrepeats padding base\n";
	 raise Exit)
  in

  stats_header ["padding"; "base"];
  let run_experiment label thunk =
    stats nrepeats max_count [string_of_int padding; string_of_int base] label thunk
  in

  let str4s = range_fold (fun n a -> str4 padding base n :: a) max_count [] in
  let str4s_probe = range_fold (fun n a -> str4 padding base n :: a) max_count [] in
  let missing = str4 padding base (max_count * 2) in

  let str4s_fold_gen ss f seed =
    let rec loop xs seed = match xs with
      | [] -> seed
      | x :: xs' -> loop xs' (f x seed)
    in loop ss seed
  in

  let str4s_fold f seed = str4s_fold_gen str4s f seed in
  let str4s_probe_fold f seed = str4s_fold_gen str4s_probe f seed in

  run_experiment "Baseline"
    (fun () -> str4s_fold (fun n a -> let _ = n in a) ());


  let full_c = run_experiment "Critbit insertion"
    (fun () -> str4s_fold (fun n a -> add n a) empty) in
  let full_s = run_experiment "StringSet insertion"
    (fun () -> str4s_fold (fun n a -> StringSet.add n a) StringSet.empty) in
  let full_h = Hashtbl.create max_count in
  run_experiment "Hashtbl insertion"
    (fun () -> str4s_fold (fun n a -> Hashtbl.add full_h n ()) ());

  run_experiment "Critbit positive membership"
    (fun () -> str4s_probe_fold (fun n a -> let _ = mem n full_c in a) ());
  run_experiment "StringSet positive membership"
    (fun () -> str4s_probe_fold (fun n a -> let _ = StringSet.mem n full_s in a) ());
  run_experiment "Hashtbl positive membership"
    (fun () -> str4s_probe_fold (fun n a -> let _ = Hashtbl.mem full_h n in ()) ());

  run_experiment "Critbit negative membership"
    (fun () -> str4s_probe_fold (fun n a -> let _ = mem missing full_c in a) ());
  run_experiment "StringSet negative membership"
    (fun () -> str4s_probe_fold (fun n a -> let _ = StringSet.mem missing full_s in a) ());
  run_experiment "Hashtbl negative membership"
    (fun () -> str4s_probe_fold (fun n a -> let _ = Hashtbl.mem full_h missing in ()) ());

  let _ = run_experiment "Critbit removal"
    (fun () -> str4s_probe_fold (fun n a -> remove n a) full_c) in
  let _ = run_experiment "StringSet removal"
    (fun () -> str4s_probe_fold (fun n a -> StringSet.remove n a) full_s) in
  run_experiment "Hashtbl removal"
    (fun () -> str4s_probe_fold (fun n a -> Hashtbl.remove full_h n) ());

  (* run_experiment "Critbit cardinality" *)
  (*   (fun () -> str4s_probe_fold (fun n a -> let _ = cardinal full_c in a) ()); *)
  (* run_experiment "StringSet cardinality" *)
  (*   (fun () -> str4s_probe_fold (fun n a -> let _ = StringSet.cardinal full_s in a) ()); *)

  ();;

main ()
