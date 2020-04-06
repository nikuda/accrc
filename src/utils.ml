open Unix

let epoch_seconds date =
  let to_tm yy mm dd hour min ss =
    Unix.mktime {Unix.tm_sec=ss; tm_min=min; tm_hour=hour;
                 tm_mday=dd; tm_mon=mm; tm_year=(2000+yy);
                 tm_wday=0; tm_yday=0; tm_isdst=false}
  in Scanf.sscanf date "%02d%02d%02d_%02d%02d%02d" to_tm


let string_of_tm tm =
  string_of_int tm.tm_mday ^ "-" ^
  string_of_int tm.tm_mon ^ "-" ^
  string_of_int tm.tm_year ^ " " ^
  string_of_int tm.tm_hour ^ ":" ^
  string_of_int tm.tm_min ^ ":" ^
  string_of_int tm.tm_sec
