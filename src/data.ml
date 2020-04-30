open Sqlite3

let create_table_sql =
  "CREATE TABLE contacts (
    contact_id INTEGER PRIMARY KEY,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email text NOT NULL UNIQUE,
    phone text NOT NULL UNIQUE
  );"

let init () =
  let mydb = db_open "test.db" in
  match exec mydb create_table_sql with
  | Rc.OK -> print_endline "Ok"
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg mydb)

