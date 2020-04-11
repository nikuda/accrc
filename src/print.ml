open Models
open Session

let show_time ms =
  let (m, s, ms) = (ms / 60000, (ms mod 60000) / 1000, ms mod 1000) in
  string_of_int m ^ ":" ^ string_of_int s ^ ":" ^ string_of_int ms

let show_pos pos =
  let p = pos + 1 in
  let pad_p =
    match p with
    | x when x < 10 -> String.concat "" [" "; string_of_int x]
    | _ -> string_of_int p
  in
    String.concat "" [" "; pad_p; ".  "]

let show_title r =
  print_string (Track.to_string r.track);
  print_string " - ";
  print_string (SessionType.to_string r.session_type);
  print_string (if r.result.SessionResult.is_wet_session then " - WET" else "");
  print_string (" - " ^ Utils.string_of_tm (snd r.time));
  print_newline ()

let show_result r =
  show_title r;
  print_endline ("Best lap: " ^ (show_time r.result.SessionResult.best_lap));
  List.iteri (fun i t -> print_endline ("Best S" ^ string_of_int (i + 1) ^ ":  " ^ (show_time t))) r.result.SessionResult.best_splits;
  List.iteri (fun i c -> print_endline (show_pos i ^
      c.Leaderboard.driver.Driver.first_name ^ " " ^ c.Leaderboard.driver.Driver.last_name ^ "\t\t" ^
      c.Leaderboard.driver.Driver.short_name ^ " " ^ CupCategory.to_string c.Leaderboard.cup_category ^ " " ^
      string_of_int c.Leaderboard.car_number ^ "\t\t" ^
      Car.to_string c.Leaderboard.car_model)) r.result.SessionResult.leaderboard
