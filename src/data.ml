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
    name TEXT NOT NULL,
    meta TEXT NOT NULL,
    started DATETIME UNIQUE NOT NULL,
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

let insert_sessions started updated =
  Printf.sprintf
    "INSERT INTO sessions (name, meta, started, updated)
     VALUES(
      \"%s\",
      \"%s\",
      \"%s\",
      \"%s\"
    );"
    started updated

let add_result config result file_mtime =
  let started = Time.string_of_tm (snd result.time) in
  let updated = Time.string_of_tm (Unix.localtime file_mtime) in
  let add_query = insert_sessions result.name result.meta started updated in
  if config.Config.debug then print_endline add_query else ();
  query config [add_query]
