open Config

let main () =
  Printf.printf "Source directory: %s\n" config.dir_path;
  Files.watch config
  (* Files.read_files config *)

let () = main ()
