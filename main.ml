open Yojson.Basic.Util

type session_type =
  | Race
  | Qualifying
  | Practice

let parse_session_type json =
  let str = json |> member "sessionType" |> to_string in
  match str with
  | "R" -> Race
  | "Q" -> Qualifying
  | "P" -> Practice
  | _ -> failwith "Unknown session type"

let session_type_to_string session_type =
  match session_type with
  | Race -> "Race"
  | Qualifying -> "Qualifying"
  | Practice -> "Practice"

module Session : sig
  type t = {
    track_name : string;
    session_type : session_type;
  }
  val create : session_type -> string -> t
end = struct
  type t = {
    track_name : string;
    session_type : session_type;
  }
  let create session_type track_name = { session_type; track_name }
end

let parse_session json =
  let track_name = json |> member "trackName" |> to_string in
  let session_type = parse_session_type json in
  Session.create session_type track_name

let main () =
  let json = Yojson.Basic.from_channel stdin in
  let r = parse_session json in
  let open Session in
  print_endline (session_type_to_string r.session_type)

let () = main ()
