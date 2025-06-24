open Habu
open Driver
open Settings

let print_version () =
  print_endline "habu 0.0.1";
  print_endline "Copyright 2025, Joel Reymont"
;;

let () = Random.self_init ()
let filename = ref ""
let settings = ref { default_settings with debug_fun = Printf.eprintf "%s\n%!" }

let args =
  [ ( "--debug"
    , Arg.Unit (fun () -> settings := { !settings with debug = true })
    , "Print debugging information to stderr" )
  ; ( "--version"
    , Arg.Unit
        (fun () ->
          print_version ();
          exit 0)
    , "Print version and exit" )
  ]
;;

let usage = Printf.sprintf "Usage: %s [OPTIONS] <Sleigh file>" Sys.argv.(0)

let () =
  if Array.length Sys.argv = 1 then (
    Arg.usage args usage;
    exit 1
  )
;;

let () = Arg.parse args (fun f -> filename := f) usage
let ( let* ) = Result.bind

let () =
  let f () =
    let* tree, text, buffer = parse_file !filename in
    Tree.lift tree text buffer
  in
  match f () with
  | Error msg ->
    Printf.eprintf "Could not process %s.\n%s%!\n" !filename msg;
    exit 1
  | Ok _ -> print_endline "All good!"
;;
