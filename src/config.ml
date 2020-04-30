open Sqlite3

type verb = Normal | Quiet | Verbose

type config =
  { dir_path: string;
    sleep_time: int;
    debug: bool;
    verb: verb;
  }

let makeConfig debug verb =
  { dir_path = "./data/"; sleep_time = 10; debug; verb; }

let mydb = db_open "test.db"

let x =
  let create_table_sql = "CREATE TABLE contacts (
   contact_id INTEGER PRIMARY KEY,
   first_name TEXT NOT NULL,
   last_name TEXT NOT NULL,
   email text NOT NULL UNIQUE,
   phone text NOT NULL UNIQUE
  );" in
  match exec mydb create_table_sql with
  | Rc.OK -> print_endline "Ok"
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg mydb)
