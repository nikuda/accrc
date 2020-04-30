type verb = Normal | Quiet | Verbose

type config =
  { dir_path: string;
    sleep_time: int;
    debug: bool;
    verb: verb;
  }

let makeConfig debug verb =
  { dir_path = "./data/"; sleep_time = 10; debug; verb; }
