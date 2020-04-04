open Config

let main () =
  print_endline config.dir_path;
  Files.read_files config

let () = main ()
