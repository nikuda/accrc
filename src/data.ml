open Sqlite3

open Utils
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
    datetime DATETIME UNIQUE NOT NULL,
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

let insert_sessions datetime updated =
  Printf.sprintf
    "INSERT INTO sessions (datetime, updated)
     VALUES(
      \"%s\",
      \"%s\"
    );"
    datetime updated

let add_result config result file_mtime =
  let datetime = Time.string_of_tm (snd result.time) in
  let updated = Time.string_of_tm (Time.tm_of_mtime (Unix.localtime file_mtime)) in
  let add_query = insert_sessions datetime updated in
  print_endline add_query;
  query config [add_query]
