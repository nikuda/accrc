open Unix

module Time = struct
  let to_tm_base yy mm dd hour min sec =
    Unix.mktime {tm_sec=sec; tm_min=min; tm_hour=hour;
                 tm_mday=dd; tm_mon=(mm - 1); tm_year=yy;
                 tm_wday=0; tm_yday=0; tm_isdst=false}

  let tm_of_datetime datetime = 
    let to_tm yy = to_tm_base (yy - 1900) in
    Scanf.sscanf datetime "%d-%02d-%02d %02d:%02d:%02d" to_tm

  let tm_of_filename filename =
    let to_tm yy = to_tm_base (yy + 100) in
    Scanf.sscanf filename "%02d%02d%02d_%02d%02d%02d" to_tm

  let string_of_tm tm =
    Printf.sprintf "%d-%02d-%02d %02d:%02d:%02d"
      (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec
  
  let to_filename t s = 
    let to_str tm = 
      Printf.sprintf "%d%02d%02d_%02d%02d%02d"
        (tm.tm_year - 100) (tm.tm_mon + 1) tm.tm_mday
        tm.tm_hour tm.tm_min tm.tm_sec
    in 
    Printf.sprintf "%s_%s.json" (to_str (snd (tm_of_datetime s))) t

  let to_epoch u = 
    fst (tm_of_datetime u)
end
