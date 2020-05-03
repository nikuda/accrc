open Sqlite3
open Models
open Session

let query config queries  =
  let mydb = db_open "accrc.db" in
  let query = String.concat "\n" queries in
  match exec mydb query with
  | Rc.OK -> Print.show_debug config "Ok"
  | r -> Print.show_debug config ((Rc.to_string r) ^ (errmsg mydb))

(* Init *)

let create_table_sessions =
  "CREATE TABLE sessions (
    id INTEGER PRIMARY KEY,
    datetime DATETIME NOT NULL,
    updated DATETIME NOT NULL
  );"

let create_table_drivers =
  "CREATE TABLE drivers (
    id INTEGER PRIMARY KEY,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    short_name text NOT NULL,
    player_id text NOT NULL UNIQUE
  );"

let init_queries = [create_table_sessions; create_table_drivers]

let init config =
  query config init_queries


(* Insert *)

let insert_sessions datetime  =
  Printf.sprintf "INSERT INTO sessions (datetime, updated) VALUES(%f, %f)" datetime datetime

let add_result config result =
  let add_query = (insert_sessions (fst result.time) ) in
  print_endline add_query;
  query config [add_query]
