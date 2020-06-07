open Sqlite3

open Utils
open Models
open Session

let open_db () = db_open "accrc.db"

let query config ?cb queries =
  let mydb = db_open "accrc.db" in
  let query = String.concat "\n" queries in
  if config.Config.debug then print_endline query else ();
  match exec mydb ?cb query with
  | Rc.OK -> Print.show_debug config "Ok"
  | r -> Print.show_debug config ((Rc.to_string r) ^ (errmsg mydb))

(* Core tables *)

let create_table_series =
  "CREATE TABLE IF NOT EXISTS series (
    id INTEGER PRIMARY KEY,
    name TEXT UNIQUE NOT NULL
  );"

let create_table_events =
  "CREATE TABLE IF NOT EXISTS events (
    id INTEGER PRIMARY KEY,
    series_id INTEGER NOT NULL,
    name TEXT NOT NULL,
    started DATETIME UNIQUE NOT NULL,
    FOREIGN KEY(series_id) REFERENCES series(id)
  );"

let create_table_sessions =
  "CREATE TABLE IF NOT EXISTS sessions (
    id INTEGER PRIMARY KEY,
    event_id INTEGER NOT NULL,
    type TEXT NOT NULL,
    started DATETIME UNIQUE NOT NULL,
    updated DATETIME NOT NULL,
    FOREIGN KEY(event_id) REFERENCES events(id)
  );"

let create_table_drivers =
  "CREATE TABLE IF NOT EXISTS drivers (
    id INTEGER PRIMARY KEY,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    short_name text NOT NULL,
    player_id text NOT NULL UNIQUE
  );"

(* Init *)

let init_queries =
  [
    create_table_series;
    create_table_events;
    create_table_sessions;
    create_table_drivers;
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
    "INSERT INTO events (series_id, name, started)
    VALUES(
      (SELECT id FROM \"series\" WHERE name = \"%s\"),
      \"%s\",
      \"%s\"
    )
    ON CONFLICT DO NOTHING;"

let insert_sessions =
  Printf.sprintf
    "INSERT INTO sessions (event_id, type, started, updated)
    VALUES(
      (SELECT id FROM events ORDER BY id DESC LIMIT 1),
      \"%s\",
      \"%s\",
      \"%s\"
    )
    ON CONFLICT(started) DO NOTHING;"

let transaction queries =
  let query = String.concat "\n" queries in
  Printf.sprintf
    "BEGIN TRANSACTION;
    %s
    COMMIT;" query

let add_result config result file_mtime =
  let started = Time.string_of_tm (snd result.time) in
  let updated = Time.string_of_tm (Unix.localtime file_mtime) in
  let add_sessions_query = insert_sessions
    (SessionType.to_string result.session_type)
    started
    updated
  in
  let queries, cb =
    if result.index = 0 then
      begin
        let add_series_query = insert_series result.meta in
        let add_events_query = insert_events result.meta result.name started in
        let cb row _ = Array.iter (fun opts ->
          match opts with Some x -> print_endline x | None -> ()) row in
        ([ add_series_query; add_events_query; add_sessions_query; ], Some cb)
      end
    else ([ add_sessions_query; ], None)
  in
  let t = transaction queries in
  query config ?cb:cb [t]

(* Select *)

let select_sessions =
  "SELECT type, started, updated FROM sessions;"

let get_sessions ?cb config = 
  query config ?cb:cb [select_sessions]

let select_all_test =
  "SELECT s.name, e.name, ss.type FROM series_events as se, events_sessions as es
  JOIN series as s ON se.series_id = s.id
  JOIN events as e ON se.event_id = e.id
  JOIN sessions as ss ON se.event_id = es.event_id AND ss.id = es.session_id;"
