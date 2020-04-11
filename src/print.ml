open Models
open Session
open SessionResult
open Leaderboard
open Driver

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
  let weather = if r.result.SessionResult.is_wet_session then " - WET" else "" in
  Printf.printf "%s - %s" (Track.to_string r.track) (SessionType.to_string r.session_type);
  Printf.printf "%s - %s\n" weather (Utils.string_of_tm (snd r.time))

let show_best_lap r =
  Printf.printf "Best lap: %s\n" (show_time r.result.best_lap)

let show_best_splits r =
  let show_best_split i t = Printf.printf "Best S%d: %s\n" (i + 1) (show_time t) in
  List.iteri show_best_split r.result.best_splits

let show_leaderboard r =
  let show_leaderboard_pos i c =
    let driver_full_name = c.driver.first_name ^ " " ^ c.driver.last_name in
    let car_name, car_model, car_year = Car.to_tuple c.car_model in
    Printf.printf "%3d. %-24s" (i + 1) driver_full_name;
    Printf.printf "%s %3d " c.driver.short_name c.car_number;
    Printf.printf "%-10s" (CupCategory.to_string c.cup_category);
    Printf.printf "%-16s %s %d\n" car_name car_model car_year
  in
  List.iteri show_leaderboard_pos r.result.leaderboard

let show_result r =
  show_title r;
  show_best_lap r;
  show_best_splits r;
  show_leaderboard r;
  flush stdout
