(* Connection *)
let adrs_exp = Str.regexp "unix!\\(.+\\)"
let wmii_address =
    let adrs = Sys.getenv "WMII_ADDRESS" in
    if Str.string_match adrs_exp adrs 0 then Str.matched_group 1 adrs
    else adrs

let user = Sys.getenv "USER"
let conn = Ixpc.connect wmii_address
let rootfid = Ixpc.attach conn user "/"

(* Core functions *)

let write file data =
   let fid, iounit = Ixpc.walk_open conn rootfid false file Ixpc.oWRITE in
   let len = Int32.of_int (String.length data) in
   ignore(Ixpc.write conn fid iounit Int64.zero len data);
   Ixpc.clunk conn fid

let read file =
   let fid, iounit = Ixpc.walk_open conn rootfid false file Ixpc.oREAD in
   Ixpc.read conn fid iounit Int64.zero (Int32.of_int 4096)

let dmenu out_str =
   let dmenu_cmd =
      "dmenu -b -nb #eeeeee -nf #222222 -sb #335577 -sf #ffffff" in
   let c_in, c_out = Unix.open_process dmenu_cmd in
   output_string c_out out_str;
   close_out c_out;
   let buffer = String.create 1024 in
   let len = input c_in buffer 0 1014 in
   String.sub buffer 0 len

let current_tags () =
   let data = read "/tag" in
   let files = Ixpc.unpack_files data in
   List.fold_left 
   (
      fun str stat -> 
         match stat.Fcall.name with
         | "sel" -> ""
         | name -> name ^ "\n" ^ str
   ) "" files

(* Key bind functions *)
let spawn cmd =
   Sys.command (cmd ^ "&");
   ()

let focus dir =
   write "/tag/sel/ctl" ("select " ^ dir)

let send dir =
   write "/tag/sel/ctl" ("send sel " ^ dir)

let mode m =
   write "/tag/sel/ctl" ("colmode sel " ^ m)

let sel_tag _ =
   let tags = current_tags () in
   let new_tag = dmenu tags in 
   write "/ctl" ("view " ^ new_tag)

let set_tag _ =
   let cid = read "/client/sel/ctl" in
   let tags = current_tags () in
   let new_tag = dmenu tags in 
   write ("/client/" ^ cid ^ "/tags") new_tag

let launch _ =
   let cmd = dmenu "firefox\ngajim" in
   spawn cmd;
   ()

let create_client cid =
   let current_tag = read "/tag/sel/ctl" in
   write ("/client/" ^ cid ^ "/tags") current_tag
