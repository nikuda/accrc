open Sqlite3

let create_table_sql =
  "CREATE TABLE drivers (
    id INTEGER PRIMARY KEY,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    short_name text NOT NULL,
    player_id text NOT NULL UNIQUE
  );"

let init () =
  let mydb = db_open "accrc.db" in
  match exec mydb create_table_sql with
  | Rc.OK -> print_endline "Ok"
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg mydb)
