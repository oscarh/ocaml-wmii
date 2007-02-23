(* Connection *)
let adrs_exp = Str.regexp "unix!\\(.+\\)"
let wmii_address =
    let adrs = Sys.getenv "WMII_ADDRESS" in
    if Str.string_match adrs_exp adrs 0 then Str.matched_group 1 adrs
    else adrs

let user = Sys.getenv "USER"
let conn = Ixpc.connect wmii_address
let rootfid = Ixpc.attach conn user "/"

(* Action menu *)
let (actions : (string, (unit -> unit)) Hashtbl.t) = Hashtbl.create 10

(* Core functions *)

let write conn rootfid file data =
   let fid, iounit = Ixpc.walk_open conn rootfid false file Ixpc.oWRITE in
   let len = Int32.of_int (String.length data) in
   ignore(Ixpc.write conn fid iounit Int64.zero len data);
   Ixpc.clunk conn fid

let read conn rootfid file = 
   let fid, iounit = Ixpc.walk_open conn rootfid false file Ixpc.oREAD in
   let data = Ixpc.read conn fid iounit Int64.zero (Int32.of_int 4096) in
   Ixpc.clunk conn fid;
   data

let create conn rootfid file =
    let perm = Int32.shift_left (Int32.of_int 0x2) 6 in
    let index = try String.rindex file '/' with Not_found -> 0 in
    let dir = String.sub file 0 index in
    let file = 
        String.sub file (index + 1) ((String.length file) - (index + 1)) in
    print_string ("creating: " ^ file ^ " in: " ^ dir);
    print_newline ();
    let fid = Ixpc.walk conn rootfid false dir in
    let _ = Ixpc.create conn fid file perm Ixpc.oWRITE in
    Ixpc.clunk conn fid

let remove conn rootfid file =
    let fid = Ixpc.walk conn rootfid false file in
    Ixpc.remove conn fid

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
   let data = read conn rootfid "/tag" in
   let files = Ixpc.unpack_files data in
   List.fold_left 
   (
      fun str stat -> 
         match stat.Fcall.name with
         | "sel" -> ""
         | name -> name ^ "\n" ^ str
   ) "" files

let quit () =
   write conn rootfid "/ctl" "quit"

(* Misc helper functions *)
let read_dir dir =
   try 
      let handle = Unix.opendir dir in
      let rec read_file acc =
         try 
            let new_acc = match Unix.readdir handle with
                | ".." -> acc
                | "." -> acc
                | file -> if file.[0] = '.' then acc else file :: acc in
            read_file new_acc
         with End_of_file -> acc in
      let files = read_file [] in
      Unix.closedir handle;
      files
   with Unix.Unix_error (_, "opendir", _) -> []

let path_delimiter = Str.regexp ":"
let programs () =
    let path = Str.split path_delimiter (Sys.getenv "PATH") in
    let read acc dir =
       read_dir dir @ acc in
    List.fold_left read [] path

let program_str =
   let buff = Buffer.create 2048 in
   let progs = List.sort String.compare (programs ()) in
   Buffer.add_string buff (List.hd progs);
   let add program = Buffer.add_string buff ("\n" ^ program) in
   List.iter add (List.tl progs);
   Buffer.contents buff

(* Key bind functions *)
let kill _ =
   write conn rootfid "/client/sel/ctl" "kill"

let spawn cmd =
   ignore(Sys.command (cmd ^ "&"));
   ()

let focus dir =
   write conn rootfid "/tag/sel/ctl" ("select " ^ dir)

let send dir =
   try 
      write conn rootfid "/tag/sel/ctl" ("send sel " ^ dir) 
   with Ixpc.IXPError _ -> ()

let mode m =
   write conn rootfid "/tag/sel/ctl" ("colmode sel " ^ m)

let view_tag tag = 
   write conn rootfid "/ctl" ("view " ^ tag)

let sel_tag _ =
   let tags = current_tags () in
   let new_tag = dmenu tags in 
   view_tag new_tag

let set_tag _ =
   let cid = read conn rootfid "/client/sel/ctl" in
   let tags = current_tags () in
   let new_tag = dmenu tags in 
   write conn rootfid ("/client/" ^ cid ^ "/tags") new_tag

let launch _ =
   match dmenu program_str with
   | "" -> ()
   | cmd -> spawn cmd

let action_menu _ =
   let action_str = if Hashtbl.length actions > 0 then
      let build_str str action = str ^ "\n" ^ action in
      let action_list = 
         Hashtbl.fold (fun action _ l -> action :: l) actions [] in
      List.fold_left build_str (List.hd action_list) (List.tl action_list)
   else
      "" in
   match dmenu action_str with
   | ""-> ()
   | action -> 
      try 
         let cb = Hashtbl.find actions action in
         cb ()
      with Not_found -> ()

let create_client cid =
   let current_tag = read conn rootfid "/tag/sel/ctl" in
   write conn rootfid ("/client/" ^ cid ^ "/tags") current_tag

(* TODO Remove these and have them in a conf file... in some way *)
let normcolors = "#222222 #eeeeee #666666"
let focuscolors = "#ffffff #335577 #447799"

let create_tag tag =
   let tag_file = "/lbar/" ^ tag in
   create conn rootfid tag_file;
   write conn rootfid tag_file (normcolors ^ tag)

let destroy_tag tag =
   try 
   remove conn rootfid ("/lbar/" ^ tag)
   with Ixpc.IXPError _ -> ()

let tagbar_click arg =
   let len = String.length arg in
   let index = String.rindex arg ' ' in 
   let tag = String.sub arg (index+1) (len-index-1) in
   view_tag tag

let focus_tag tag =
   Printf.printf "Focusing %s\n" tag;
   flush stdout;
   try 
   let tag_file = "/lbar/" ^ tag in
   write conn rootfid tag_file (focuscolors ^ tag)
   with Ixpc.IXPError _ -> ()

let unfocus_tag tag =
   Printf.printf "Unfocusing %s\n" tag;
   flush stdout;
   try 
   let tag_file = "/lbar/" ^ tag in
   write conn rootfid tag_file (normcolors ^ tag)
   with Ixpc.IXPError _ -> ()
