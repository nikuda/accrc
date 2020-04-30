open Models
open Session
open SessionResult
open Leaderboard
open Driver

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

let show_title r =
  let track_name, track_year = Track.to_tuple r.track in
  let weather = if r.result.SessionResult.is_wet_session then "WET" else "" in
  Printf.printf "%s -- " (Utils.string_of_tm (snd r.time));
  Printf.printf "%-12s %d - %s %s\n"
    track_name track_year (SessionType.to_string r.session_type) weather;
  flush stdout

let show_best_lap r =
  Printf.printf "Best lap: %s\n" (show_time r.result.best_lap);
  flush stdout

let show_best_splits r =
  let show_best_split i t = Printf.printf "Best S%d: %s\n" (i + 1) (show_time t) in
  List.iteri show_best_split r.result.best_splits;
  flush stdout

let show_leaderboard r =
  let show_leaderboard_pos i c =
    let driver_full_name = c.driver.first_name ^ " " ^ c.driver.last_name in
    let car_name, car_model, car_year = Car.to_tuple c.car_model in
    Printf.printf "%3d. %-24s" (i + 1) driver_full_name;
    Printf.printf "%s %3d " c.driver.short_name c.car_number;
    Printf.printf "%-10s" (CupCategory.to_string c.cup_category);
    Printf.printf "%-13s %-16s %d\n" car_name car_model car_year
  in
  List.iteri show_leaderboard_pos r.result.leaderboard;
  flush stdout

let show_result config r =
  show_title r;
  match config.Config.verb with
  | Config.Verbose ->
    show_best_lap r;
    show_best_splits r;
    show_leaderboard r;
  | _ -> ()
