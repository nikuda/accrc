open Unix

let epoch_seconds filename =
  let to_tm yy mm dd hour min ss =
    Unix.mktime {Unix.tm_sec=ss; tm_min=min; tm_hour=hour;
                 tm_mday=dd; tm_mon=mm; tm_year=(2000 + yy);
                 tm_wday=0; tm_yday=0; tm_isdst=false}
  in Scanf.sscanf filename "%02d%02d%02d_%02d%02d%02d" to_tm


let string_of_tm tm =
  Printf.sprintf "%02d-%02d-%d %02d:%02d:%02d"
    tm.tm_mday tm.tm_mon tm.tm_year
    tm.tm_hour tm.tm_min tm.tm_sec
