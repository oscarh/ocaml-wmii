
let localtime () =
    let tm = Unix.localtime (Unix.time ()) in
    let year = string_of_int (tm.Unix.tm_year + 1900) in
    let month = Printf.sprintf "%02d" (tm.Unix.tm_mon + 1) in
    let day = Printf.sprintf "%02d" tm.Unix.tm_mday in
    let hour = Printf.sprintf "%02d" (tm.Unix.tm_hour) in
    let minute = Printf.sprintf "%02d" tm.Unix.tm_min in
    year ^ "-" ^ month ^ "-" ^ day ^ " " ^ hour  ^ ":" ^ minute
