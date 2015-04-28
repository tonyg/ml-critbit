open Ocamlbuild_plugin ;;

let _ = dispatch begin function
  | After_rules ->
    flag ["c"; "compile"] (S [A"-ccopt"; A"-Os"]);
    dep ["link"; "ocaml"; "use_infinite_bytes"] ["infinite_bytes.o"]
  | _ -> ()
end
