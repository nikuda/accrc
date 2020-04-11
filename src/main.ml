open Config
open Cmdliner

let watch poll_interval =
  Printf.printf "Watching %s with %d sec interval\n" config.dir_path poll_interval;
  Files.watch config poll_interval

let watch_cmd =
  let doc = "Watch directory with $(docv) in seconds." in
  Arg.(value & opt int 10 & info ["w"; "watch"] ~docv:"POLL_INTERVAL" ~doc)

let watch_t = Term.(const watch $ watch_cmd)

let info =
  let doc = "race control" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <nikuda at gmail.com>." ]
  in
  Term.info "accrc" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (watch_t, info)
