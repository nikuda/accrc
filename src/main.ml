open Models

let show_time ms =
  let (m, s, ms) = (ms / 60000, (ms mod 60000) / 1000, ms mod 1000) in
  string_of_int m ^ ":" ^ string_of_int s ^ ":" ^ string_of_int ms

let main () =
  let open Session in
  let json = Yojson.Basic.from_channel stdin in
  let r = Session.parse json in
  let p = Uutf.String.encoding_guess "this is a test" in
  let x pp =
    match pp with
      | (`UTF_16BE, _) -> "`UTF_16BE"
      | (`UTF_16LE, _) -> "`UTF_16LE"
      | (`UTF_8, _)  -> " `UTF_8 "
  in
  print_string (Track.to_string r.track);
  print_string " - ";
  print_string ((SessionType.to_string r.session_type) ^ " " ^ (string_of_int r.index));
  print_string (if r.result.SessionResult.is_wet_session then " - WET" else "");
  print_newline ();
  print_string (x p);
  print_newline ();
  print_endline ("Best lap: " ^ (show_time r.result.SessionResult.best_lap));
  List.iteri (fun i t -> print_endline ("Best S" ^ string_of_int (i + 1) ^ ":  " ^ (show_time t))) r.result.SessionResult.best_splits;
  List.iteri (fun i c -> print_endline (string_of_int (i + 1) ^ ".\t" ^
      c.Leaderboard.driver.Driver.first_name ^ " " ^ c.Leaderboard.driver.Driver.last_name ^ "\t\t" ^
      c.Leaderboard.driver.Driver.short_name ^ " " ^ CupCategory.to_string c.Leaderboard.cup_category ^ " " ^
      string_of_int c.Leaderboard.car_number ^ "\t\t" ^
      Car.to_string c.Leaderboard.car_model)) r.result.SessionResult.leaderboard

let () = main ()
