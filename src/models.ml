open Yojson.Basic.Util
open Utils

module Car = struct
  type t =
    | Porsche of string * int
    | Mercedes of string * int
    | Ferrari of string * int
    | Audi of string * int
    | Lamborghini of string * int
    | McLaren of string * int
    | BMW of string * int
    | Bentley of string * int
    | Nissan of string * int
    | AstonMartin of string * int
    | Jaguar of string * int
    | Lexus of string * int
    | Honda of string * int

  let from_int x =
    match x with
    | 0 -> Porsche ("991 GT3", 2018)
    | 1 -> Mercedes ("AMG GT3", 2018)
    | 2 -> Ferrari ("488 GT3", 2018)
    | 3 -> Audi ("R8 LMS", 2018)
    | 4 -> Lamborghini ("Huracan GT3", 2018)
    | 5 -> McLaren ("650S GT3", 2018)
    | 6 -> Nissan ("GT-R Nismo GT3", 2018)
    | 7 -> BMW ("M6 GT3", 2018)
    | 8 -> Bentley ("Continental GT3", 2018)
    | 9 -> Porsche ("991.2 GT3 Cup", 2018)
    | 10 -> Nissan ("GT-R Nismo GT3", 2017)
    | 11 -> Bentley ("Continental GT3", 2016)
    | 12 -> AstonMartin ("Vantage V12 GT3", 2018)
    | 13 -> Lamborghini ("Gallardo R-EX", 2018)
    | 14 -> Jaguar ("G3", 2018)
    | 15 -> Lexus ("RC F GT3", 2018)
    | 16 -> Lamborghini ("Huracan Evo", 2019)
    | 17 -> Honda ("NSX GT3", 2019)
    | 18 -> Lamborghini ("Huracan SuperTrofeo", 2018)
    | 19 -> Audi ("R8 LMS Evo", 2019)
    | 20 -> AstonMartin ("Vantage AMR GT3", 2019)
    | 21 -> Honda ("NSX Evo", 2019)
    | 22 -> McLaren ("720S GT3", 2019)
    | 23 -> Porsche ("911 II GT3 R", 2019)
    | _ -> failwith ("Unknown car code: " ^ string_of_int x)

  let fmt =
    format_of_string "%s\t\t%s %d"

  let to_tuple car =
    match car with
    | Porsche (model, year) -> ("Porsche", model, year)
    | Mercedes (model, year) ->("Mercedes", model, year)
    | Ferrari (model, year) -> ("Ferrari", model, year)
    | Audi (model, year) -> ("Audi", model, year)
    | Lamborghini (model, year) -> ("Lamborghini", model, year)
    | McLaren (model, year) -> ("McLaren", model, year)
    | BMW (model, year) -> ("BMW", model, year)
    | Bentley (model, year) -> ("Bentley", model, year)
    | Nissan (model, year) -> ("Nissan", model, year)
    | AstonMartin (model, year) -> ("Aston Martin", model, year)
    | Jaguar (model, year) -> ("Jaguar", model, year)
    | Lexus (model, year) -> ("Lexus", model, year)
    | Honda (model, year) -> ("Honda", model, year)

  let to_string car =
    let brand, model, year = to_tuple car in
    Printf.sprintf fmt brand model year
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

  let fmt =
    format_of_string "%s %d"

  let to_tuple track =
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

  let to_string track =
    let name, year = to_tuple track
    in Printf.sprintf fmt name year

  let parse json =
    json |> member "trackName" |> Yojson.Basic.Util.to_string |> from_string
end

module Driver = struct
  type t = {
    id : string;
    first_name : string;
    last_name : string;
    short_name : string;
  }

  let parse json =
    {
      id = json |> member "playerId" |> to_string;
      first_name = json |> member "firstName" |> to_string;
      last_name = json |> member "lastName" |> to_string;
      short_name = json |> member "shortName" |> to_string;
    }
end

module SessionType = struct
  type t =
    | Race of int
    | Qualifying of int
    | Practice of int

  let from_string str =
    let session_number =
      try int_of_string (Char.escaped str.[1])
      with _ -> 0
    in
    match str.[0] with
    | 'R' -> Race session_number
    | 'Q' -> Qualifying session_number
    | 'P' -> Practice session_number
    | _ -> failwith ("Unknown session type: " ^ str)

  let to_string session_type =
    let show_num n = if n > 0 then " " ^ string_of_int n else "" in
    match session_type with
    | Race n -> "Race" ^ show_num n
    | Qualifying n -> "Qualifying " ^ show_num n
    | Practice n -> "Practice " ^ show_num n

  let parse json =
    json |> member "sessionType" |> Yojson.Basic.Util.to_string |> from_string
end

module CupCategory = struct
  type t =
    | Overall
    | ProAm
    | Am
    | Silver
    | National

  let from_int x =
    match x with
    | 0 -> Overall
    | 1 -> ProAm
    | 2 -> Am
    | 3 -> Silver
    | 4 -> National
    | _ -> failwith ("Unknown cup category: " ^ string_of_int x)

  let to_string cup_category =
     match cup_category with
    | Overall -> "Overall"
    | ProAm -> "ProAm"
    | Am -> "Am"
    | Silver -> "Silver"
    | National -> "National"
end

module Leaderboard = struct
  type entry = {
    car_id : int;
    car_number : int;
    car_model : Car.t;
    cup_category : CupCategory.t;
    driver_id : int;
    driver : Driver.t;
  }

  type t = entry list

  let parse_leaderboard json =
    let car = json |> member "car" in
    {
      car_id = car |> member "carId" |> to_int;
      car_number = car |> member "raceNumber" |> to_int;
      car_model = car |> member "carModel" |> to_int |> Car.from_int;
      cup_category = car |> member "cupCategory" |> to_int |> CupCategory.from_int;
      driver_id = json |> member "currentDriverIndex" |> to_int;
      driver = json |> member "currentDriver" |> Driver.parse
    }

  let parse json  =
    List.map parse_leaderboard json
end

module SessionResult = struct
  type t = {
    best_lap: int;
    best_splits: int list;
    is_wet_session: bool;
    leaderboard: Leaderboard.t;
  }

  let to_wet_session x =
    if x == 1 then true else false

  let parse json =
    let result = json |> member "sessionResult" in
    let best_lap = result |> member "bestlap" |> to_int in
    let best_splits = result |> member "bestSplits" |> to_list |> List.map to_int in
    let is_wet_session = result |> member "isWetSession" |> to_int |> to_wet_session in
    let leaderboard = result |> member "leaderBoardLines" |> to_list |> Leaderboard.parse in
    { best_lap; best_splits; is_wet_session; leaderboard }
end

module Session = struct
  type t = {
    name: string;
    meta: string;
    index: int;
    track : Track.t;
    session_type : SessionType.t;
    result : SessionResult.t;
    time: float * Unix.tm
  }

  let create name meta index session_type track result time =
    { name; meta; index; session_type; track; result; time }

  let parse filename json =
    let name = json |> member "serverName" |> to_string in
    let meta = json |> member "metaData" |> to_string in
    let index = json |> member "sessionIndex" |> to_int in
    let session_type = SessionType.parse json in
    let track = Track.parse json in
    let result = SessionResult.parse json in
    let time = Time.tm_of_filename filename in
    create name meta index session_type track result time
end
