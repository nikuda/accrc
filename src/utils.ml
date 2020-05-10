open Unix

module Time = struct
  let tm_of_filename filename =
    let to_tm yy mm dd hour min sec =
      Unix.mktime {tm_sec=sec; tm_min=min; tm_hour=hour;
                   tm_mday=dd; tm_mon=mm; tm_year=(yy + 100);
                   tm_wday=0; tm_yday=0; tm_isdst=false}
    in Scanf.sscanf filename "%02d%02d%02d_%02d%02d%02d" to_tm

  let string_of_tm tm =
    Printf.sprintf "%d-%02d-%02d %02d:%02d:%02d"
      (1900 + tm.tm_year) tm.tm_mon tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec

  let tm_of_mtime tm =
    snd (Unix.mktime
      {tm_sec=tm.tm_sec; tm_min=tm.tm_min; tm_hour=tm.tm_hour;
       tm_mday=tm.tm_mday; tm_mon=(1 + tm.tm_mon); tm_year=tm.tm_year;
       tm_wday=0; tm_yday=0; tm_isdst=false})
end
