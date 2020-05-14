module C = Configurator.V1

let default : C.Pkg_config.package_conf =
  { libs   = []
  ; cflags = []
  }

let f c =
  let conf =
    match C.Pkg_config.get c with
    | None -> default
    | Some pc ->
       match (C.Pkg_config.query pc ~package:"sqlite3") with
       | None -> default
       | Some deps -> deps
  in
  C.Flags.write_lines "c_flags.sexp" conf.cflags


let () =
  C.main ~name:"foo" f
