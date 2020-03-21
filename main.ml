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
  type t =
    | Monza of int
    | Zolder of int
    | BrandsHatch of int
    | Silverstone of int
    | PaulRicard of int
    | Misano of int
    | Spa of int
    | Nurburgring of int
    | Barcelona of int
    | Hungaroring of int
    | Zandvoort of int
    | Kyalami of int
    | MountPanorama of int
    | Suzuka of int
    | LagunaSeca of int


  let from_string str =
    match str with
    | "monza" -> Monza 2018
    | "monza_2019" -> Monza 2019
    | "zolder" -> Zolder 2018
    | "zolder_2019" -> Zolder 2019
    | "brands_hatch" -> BrandsHatch 2018
    | "brands_hatch_2019" -> BrandsHatch 2019
    | "silverstone" -> Silverstone 2018
    | "silverstone_2019" -> Silverstone 2019
    | "paul_ricard" -> PaulRicard 2018
    | "paul_ricard_2019" -> PaulRicard 2019
    | "misano" -> Misano 2018
    | "misano_2019" -> Misano 2019
    | "spa" -> Spa 2018
    | "spa_2019" -> Spa 2019
    | "nurburgring" -> Nurburgring 2018
    | "nurburgring_2019" -> Nurburgring 2019
    | "barcelona" -> Barcelona 2018
    | "barcelona_2019" -> Barcelona 2019
    | "hungaroring" -> Hungaroring 2018
    | "hungaroring_2019" -> Hungaroring 2019
    | "zandvoort" -> Zandvoort 2018
    | "zandvoort_2019" -> Zandvoort 2019
    | "kyalami_2019" -> Kyalami 2019
    | "mount_panorama_2019" -> MountPanorama 2019
    | "suzuka_2019" -> Suzuka 2019
    | "laguna_seca_2019" -> LagunaSeca 2019
    | _ -> failwith ("Unknown track name: " ^ str)

  let to_string track =
    let name, year =
      match track with
      | Monza year -> ("Monza", year)
      | Zolder year -> ("Zolder", year)
      | BrandsHatch year -> ("Brands Hatch", year)
      | Silverstone year -> ("Silverstone", year)
      | PaulRicard year -> ("Paul Ricard", year)
      | Misano year -> ("Misano", year)
      | Spa year -> ("Spa-Francorchamps", year)
      | Nurburgring year -> ("Nürburgring", year)
      | Barcelona year -> ("Barcelona", year)
      | Hungaroring year -> ("Hungaroring", year)
      | Zandvoort year -> ("Zandvoort", year)
      | Kyalami year -> ("Kyalami", year)
      | MountPanorama year -> ("Mount Panorama", year)
      | Suzuka year -> ("Suzuka", year)
      | LagunaSeca year -> ("Laguna Seca", year)
    in
    name ^ " " ^ string_of_int year

  let parse json =
    json |> member "trackName" |> Yojson.Basic.Util.to_string |> from_string
end

module SessionResult = struct
  type t = {
    best_lap: int;
    best_splits: int list;
  }

  let parse json =
    let result = json |> member "sessionResult" |> to_assoc in
    let best_lap =
      try List.assoc "bestlap" result |> to_int
      with Not_found -> failwith ("Parsing error: bestlap")
    in
    let best_splits =
      try List.assoc "bestSplits" result |> to_list |> List.map to_int
      with Not_found -> failwith ("Parsing error: bestSplits")
    in
    { best_lap; best_splits }
end

module Session = struct
  type t = {
    index: int;
    track_name : Track.t;
    session_type : SessionType.t;
    result : SessionResult.t;
  }

  let create index session_type track_name result =
    { index; session_type; track_name; result }

  let parse json =
    let index = json |> member "sessionIndex" |> to_int in
    let track_name = Track.parse json in
    let session_type = SessionType.parse json in
    let session_result = SessionResult.parse json in
    create index session_type track_name session_result
end

type lap = Lap of int * int * int

let to_time ms =
  Lap (ms / 60000, (ms mod 60000) / 1000, ms mod 1000)

let show_time lap =
  match lap with
  | Lap (m, s, ms) -> string_of_int m ^ ":" ^ string_of_int s ^ ":" ^ string_of_int ms

let main () =
  let open Session in
  let json = Yojson.Basic.from_channel stdin in
  let r = Session.parse json in
  (SessionType.to_string r.session_type) ^ " " ^ (string_of_int r.index) ^ " - " ^ (Track.to_string r.track_name) |> print_endline;
  print_endline (show_time (to_time r.result.SessionResult.best_lap));
  List.iter (fun t -> print_endline (show_time (to_time t))) r.result.SessionResult.best_splits

let () = main ()
