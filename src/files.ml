open Config
open Models

let lines ?encoding (src : [`Channel of in_channel | `String of string]) =
  let rec loop d buf acc = match Uutf.decode d with
  | `Uchar u ->
      begin match Uchar.to_int u with
      | 0x000A ->
          let line = Buffer.contents buf in
          Buffer.clear buf;
          Buffer.add_string acc line;
          loop d buf acc
      | _ ->
          Uutf.Buffer.add_utf_8 buf u;
          loop d buf acc
      end
  | `End ->
      (Buffer.add_string acc (Buffer.contents buf));
      Buffer.contents acc
  | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep;
      loop d buf acc
  | `Await -> assert false
  in
  let nln = `Readline (Uchar.of_int 0x000A) in
  loop (Uutf.decoder ~nln ?encoding src) (Buffer.create 512) (Buffer.create 512)

let open_file str =
  let inf = open_in str in
  try
    let utf8_file = lines (`Channel inf) in
    flush stdout;
    close_in inf;
    utf8_file
  with e ->
    close_in_noerr inf;
    raise e

let get_path config file =
  Filename.concat config.dir_path file

let parse_result config file =
  let file_string = open_file (get_path config file) in
  let json = Yojson.Basic.from_string file_string in
  Session.parse file json


(* Cache *)

let create_cache config =
  let file_cache = Hashtbl.create 10000 in
  let add_to_cache t s u = 
    let file_name = Utils.Time.to_filename 
      (Option.value t ~default:"") 
      (Option.value s ~default:"") 
    in
    let file_mtime = Utils.Time.to_epoch 
      (Option.value u ~default:"") 
    in 
    Hashtbl.add file_cache file_name file_mtime;
    Print.show_debug config 
      @@ Printf.sprintf "[CACHE] filename %s mtime %f" file_name file_mtime
  in 
  let populate_cache row _ =
    begin match row with
    | [| t; s; u; |] -> add_to_cache t s u
    | _ -> ()
    end
  in 
  Data.get_sessions ?cb:(Some populate_cache) config;
  file_cache

let add config file_cache file_name file_stat =
  let result = parse_result config file_name in
  Hashtbl.add file_cache file_name file_stat.Unix.st_mtime;
  Data.add_result config result file_stat.Unix.st_mtime;
  Printf.printf "[%s] " "NEW";
  Print.show_log (Unix.localtime file_stat.Unix.st_mtime) result;
  Print.show_result config result

let update config file_cache file_name file_stat =
  let result = parse_result config file_name in
  Hashtbl.replace file_cache file_name file_stat.Unix.st_mtime;
  Printf.printf "[%s] " "UPD";
  Print.show_log (Unix.localtime file_stat.Unix.st_mtime) result

let iter_files config file_cache file_name =
  let file_stat = Unix.stat (get_path config file_name) in
  match Hashtbl.find_opt file_cache file_name with
  | None -> add config file_cache file_name file_stat;
  | Some cur_file_mtime ->
      if cur_file_mtime < file_stat.Unix.st_mtime
      then update config file_cache file_name file_stat
      else ()

let read_files config file_cache =
  let dir = Sys.readdir config.dir_path in
  Array.sort compare dir;
  Array.iter (iter_files config file_cache) dir

(* Commands *)

let read config =
  let file_cache = create_cache config in
  read_files config file_cache

let watch config poll_interval =
  let file_cache = create_cache config in
  let rec loop () =
    read_files config file_cache;
    Unix.sleep poll_interval;
    loop ()
  in loop ()
