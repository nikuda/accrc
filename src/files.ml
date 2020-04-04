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
