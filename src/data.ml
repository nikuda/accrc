open Sqlite3

open Utils
open Models
open Session

let query config queries  =
  let mydb = db_open "accrc.db" in
  let query = String.concat "\n" queries in
  if config.Config.debug then print_endline query else ();
  match exec mydb query with
  | Rc.OK -> Print.show_debug config "Ok"
  | r -> Print.show_debug config ((Rc.to_string r) ^ (errmsg mydb))

(* Core tables *)

let create_table_series =
  "CREATE TABLE series (
    id INTEGER PRIMARY KEY,
    name TEXT UNIQUE NOT NULL
  );"

let create_table_events =
  "CREATE TABLE events (
    id INTEGER PRIMARY KEY,
    name TEXT UNIQUE NOT NULL
  );"

let create_table_sessions =
  "CREATE TABLE sessions (
    id INTEGER PRIMARY KEY,
    type TEXT NOT NULL,
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

(* Assoc tables *)

let create_table_series_events =
  "CREATE TABLE series_events (
    id INTEGER PRIMARY KEY,
    series INTEGER NOT NULL,
    event INTEGER NOT NULL,
    FOREIGN KEY(series) REFERENCES series(id),
    FOREIGN KEY(event) REFERENCES events(id)
  );"

let create_table_events_sessions =
  "CREATE TABLE events_sessions (
    id INTEGER PRIMARY KEY,
    event INTEGER NOT NULL,
    session INTEGER NOT NULL,
    FOREIGN KEY(event) REFERENCES events(id),
    FOREIGN KEY(session) REFERENCES sessions(id)
  );"

(* Init *)

let init_queries =
  [
    create_table_series;
    create_table_events;
    create_table_sessions;
    create_table_drivers;
    create_table_series_events;
    create_table_events_sessions
  ]

let init config =
  query config init_queries


(* Insert *)

let insert_series =
  Printf.sprintf
    "INSERT INTO series (name)
      VALUES(\"%s\")
      ON CONFLICT(name) DO NOTHING;"

let insert_events =
  Printf.sprintf
    "INSERT INTO events (name)
      VALUES(\"%s\")
      ON CONFLICT(name) DO NOTHING;"

let insert_sessions =
  Printf.sprintf
    "INSERT INTO sessions (type, started, updated)
     VALUES(
      \"%s\",
      \"%s\",
      \"%s\"
    );"

let add_result config result file_mtime =
  let started = Time.string_of_tm (snd result.time) in
  let updated = Time.string_of_tm (Unix.localtime file_mtime) in
  let add_series_query = insert_series result.meta in
  let add_events_query = insert_events result.name in
  let add_sessions_query = insert_sessions
    (SessionType.to_string result.session_type)
    started updated
  in
  query config [add_series_query; add_events_query; add_sessions_query]
