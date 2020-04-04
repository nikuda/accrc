open Config

let main () =
  print_endline config.dir_path;
  Files.watch config

let () = main ()
