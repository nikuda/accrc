open Config

type verb = Normal | Quiet | Verbose
type copts = { debug : bool; verb : verb }

let str = Printf.sprintf
let opt_str sv = function None -> "None" | Some v -> str "Some(%s)" (sv v)
let opt_str_str = opt_str (fun s -> s)
let verb_str = function
  | Normal -> "normal" | Quiet -> "quiet" | Verbose -> "verbose"

let pr_copts oc copts = Printf.fprintf oc
    "debug = %B\nverbosity = %s\n"
    copts.debug (verb_str copts.verb)

let init copts = Printf.printf
    "%a" pr_copts copts

let watch _ interval =
  let poll_interval =
    match interval with
      | Some v -> v
      | None -> config.sleep_time
  in
  Printf.printf "Watching %s with %d sec interval\n" config.dir_path poll_interval;
  Files.watch config poll_interval

let help _ man_format cmds topic = match topic with
| None -> `Help (`Pager, None) (* help about the program. *)
| Some topic ->
    let topics = "topics" :: "patterns" :: "environment" :: cmds in
    let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
    | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
    | `Ok _ ->
        let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
        `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page)

open Cmdliner

(* Help sections common to all commands *)

let help_secs = [
 `S Manpage.s_common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
 `P "Use `$(mname) help patterns' for help on patch matching."; `Noblank;
 `P "Use `$(mname) help environment' for help on environment variables.";
 `S Manpage.s_bugs; `P "Check bug reports at http://bugs.example.org.";]

(* Options common to all commands *)

let copts debug verb = { debug; verb }
let copts_t =
  let docs = Manpage.s_common_options in
  let debug =
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc)
  in
  let verb =
    let doc = "Suppress informational output." in
    let quiet = Quiet, Arg.info ["q"; "quiet"] ~docs ~doc in
    let doc = "Give verbose output." in
    let verbose = Verbose, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [Normal] [quiet; verbose])
  in
  Term.(const copts $ debug $ verb)

(* Commands *)

let init_cmd =
  let doc = "initialise $(mname) in current directory" in
  let exits = Term.default_exits in
  let man = [
    `S Manpage.s_description;
    `P doc;
    `Blocks help_secs; ]
  in
  Term.(const init $ copts_t),
  Term.info "init" ~doc ~sdocs:Manpage.s_common_options ~exits ~man

let watch_cmd =
  let interval =
    let doc = "Poll logfiles every INTERVAL seconds" in
    Arg.(value & opt (some int) None & info ["p"; "poll"]
      ~docv:"INTERVAL" ~doc)
  in
  let doc = "watch for changes" in
  let exits = Term.default_exits in
  let man =
    [`S Manpage.s_description;
     `P "Creates a patch from changes in the working tree. If you specify
         a set of files ...";
     `Blocks help_secs; ]
  in
  Term.(const watch $ copts_t $ interval ),
  Term.info "watch" ~doc ~sdocs:Manpage.s_common_options ~exits ~man

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about $(mname) commands" in
  let man =
    [`S Manpage.s_description;
     `P "Prints help about darcs commands and other subjects...";
     `Blocks help_secs; ]
  in
  Term.(ret (const help $ copts_t $ Arg.man_format $ Term.choice_names $topic)),
  Term.info "help" ~doc ~exits:Term.default_exits ~man

let default_cmd =
  let doc = "race control" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man = help_secs in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info "accrc" ~version:"v0.1.0" ~doc ~sdocs ~exits ~man

let cmds = [init_cmd; watch_cmd; help_cmd]

let () = Term.(exit @@ eval_choice default_cmd cmds)
