open Yojson.Basic.Util

module SessionType = struct
  type t =
    | Race
    | Qualifying
    | Practice

  let from_string str =
    match str with
    | "R" -> Race
    | "Q" -> Qualifying
    | "P" -> Practice
    | _ -> failwith ("Unknown session type: " ^ str)

  let to_string session_type =
    match session_type with
    | Race -> "Race"
    | Qualifying -> "Qualifying"
    | Practice -> "Practice"

  let parse json =
    json |> member "sessionType" |> Yojson.Basic.Util.to_string |> from_string
end

module Track = struct
  type name =
    | BrandsHatch of int
    | Monza of int

  let from_string str =
    match str with
    | "brands_hatch" -> BrandsHatch 2018
    | "brands_hatch2019" -> BrandsHatch 2019
    | _ -> failwith ("Unknown track name: " ^ str)

  let to_string name =
    let n, y =
      match name with
      | BrandsHatch year -> ("Brands Hatch", year)
      | Monza year -> ("Monza", year)
    in
    n ^ " " ^ string_of_int y

  let parse json =
    json |> member "trackName" |> Yojson.Basic.Util.to_string |> from_string
end

module Session = struct
  type t = {
    index: int;
    track_name : Track.name;
    session_type : SessionType.t;
  }

  let create index session_type track_name = { index; session_type; track_name }

  let parse json =
    let index = json |> member "sessionIndex" |> to_int in
    let track_name = Track.parse json in
    let session_type = SessionType.parse json in
    create index session_type track_name
end


let main () =
  let json = Yojson.Basic.from_channel stdin in
  let r = Session.parse json in
  let open Session in
  (SessionType.to_string r.session_type) ^ " " ^ (string_of_int r.index) ^ " - " ^ (Track.to_string r.track_name) |> print_endline

let () = main ()
