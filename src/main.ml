open Models

let show_time ms =
  let (m, s, ms) = (ms / 60000, (ms mod 60000) / 1000, ms mod 1000) in
  string_of_int m ^ ":" ^ string_of_int s ^ ":" ^ string_of_int ms

let lines ?encoding (src : [`Channel of in_channel | `String of string]) =
  let rec loop d buf acc = match Uutf.decode d with
  | `Uchar u ->
      begin match Uchar.to_int u with
      | 0x000A ->
          let line = Buffer.contents buf in
          Buffer.clear buf;
          Buffer.add_string acc line;
          loop d buf acc
      | _ ->
          Uutf.Buffer.add_utf_8 buf u;
          loop d buf acc
      end
  | `End ->
      (Buffer.add_string acc (Buffer.contents buf)); Buffer.contents acc
  | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep; loop d buf acc
  | `Await -> assert false
  in
  let nln = `Readline (Uchar.of_int 0x000A) in
  loop (Uutf.decoder ~nln ?encoding src) (Buffer.create 512) (Buffer.create 512)

let open_file str =
  let inf = open_in str in
  try
    let utf8_file = lines (`Channel inf) in
    flush stdout;
    close_in inf;
    utf8_file
  with e ->
    close_in_noerr inf;
    raise e

let main () =
  let open Session in
  let file_string = open_file "./data/Round1_Suzuka_R1.json" in
  let json = Yojson.Basic.from_string file_string in
  let r = Session.parse json in
  print_string (Track.to_string r.track);
  print_string " - ";
  print_string ((SessionType.to_string r.session_type) ^ " " ^ (string_of_int r.index));
  print_string (if r.result.SessionResult.is_wet_session then " - WET" else "");
  print_newline ();
  print_endline ("Best lap: " ^ (show_time r.result.SessionResult.best_lap));
  List.iteri (fun i t -> print_endline ("Best S" ^ string_of_int (i + 1) ^ ":  " ^ (show_time t))) r.result.SessionResult.best_splits;
  List.iteri (fun i c -> print_endline (string_of_int (i + 1) ^ ".\t" ^
      c.Leaderboard.driver.Driver.first_name ^ " " ^ c.Leaderboard.driver.Driver.last_name ^ "\t\t" ^
      c.Leaderboard.driver.Driver.short_name ^ " " ^ CupCategory.to_string c.Leaderboard.cup_category ^ " " ^
      string_of_int c.Leaderboard.car_number ^ "\t\t" ^
      Car.to_string c.Leaderboard.car_model)) r.result.SessionResult.leaderboard

let () = main ()
