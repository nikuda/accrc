open Utils
open Models
open Session
open SessionResult
open Leaderboard
open Driver

let show_debug config msg =
  if
    config.Config.debug
  then
    print_endline msg
  else
    ()

let show_time sec =
  let (m, s, ms) = (sec / 60000, (sec mod 60000) / 1000, sec mod 1000) in
  Printf.sprintf "%d:%02d:%03d" m s ms

let show_pos pos =
  let p = pos + 1 in
  let pad_p =
    match p with
    | x when x < 10 -> String.concat "" [" "; string_of_int x]
    | _ -> string_of_int p
  in
    String.concat "" [" "; pad_p; ".  "]

let show_log t r =
  let track_name, _ = Track.to_tuple r.track in
  Printf.printf "%s - " (Time.string_of_tm t);
  Printf.printf "%s - %s\n"  (SessionType.to_string r.session_type) track_name;
  flush stdout

let show_session_info r =
  let track_name, track_year = Track.to_tuple r.track in
  let weather = if r.result.SessionResult.is_wet_session then " (WET)" else "" in
  Printf.printf "\n Track: %s %d%s\n\n" track_name track_year weather

let show_session_bests r =
  let show_best_split i t = Printf.printf "S%d %s    " (i + 1) (show_time t) in
  Printf.printf "  Best:    LAP %s    " (show_time r.result.best_lap);
  List.iteri show_best_split r.result.best_splits;
  print_endline "";
  flush stdout

let show_leaderboard r =
  let show_leaderboard_pos i c =
    let driver_full_name = Printf.sprintf "%s %s" c.driver.first_name c.driver.last_name in
    let car_name, car_model, _ = Car.to_tuple c.car_model in
    Printf.printf "%3d. %3d " (i + 1) c.car_number;
    Printf.printf "%s %-24s" c.driver.short_name driver_full_name;
    Printf.printf "%-13s %-16s " car_name car_model;
    Printf.printf "%-10s\n" (CupCategory.to_string c.cup_category)
  in
  print_endline "";
  List.iteri show_leaderboard_pos r.result.leaderboard;
  print_endline "";
  flush stdout

let show_result config r =
  match config.Config.verb with
  | Config.Verbose ->
    show_session_info r;
    show_session_bests r;
    show_leaderboard r;
  | _ -> ()
