open Printf

(* Connection *)
let adrs_exp = Str.regexp "unix!\\(.+\\)"
let wmii_address =
    let adrs = Sys.getenv "WMII_ADDRESS" in
    if Str.string_match adrs_exp adrs 0 then Str.matched_group 1 adrs
    else adrs

let user = Sys.getenv "USER"
let conn = Ixpc.connect wmii_address
let rootfid = Ixpc.attach conn user "/"

(* Activate/deactivate debug *)
let debug_channel = ref None

(* Action menu *)
let (actions : (string, (unit -> unit)) Hashtbl.t) = Hashtbl.create 10

let debug str =
   match !debug_channel with
   | Some channel -> 
      output_string channel str;
      flush channel
   | None -> ()

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
    let fid = Ixpc.walk conn rootfid false dir in
    let _ = Ixpc.create conn fid file perm Ixpc.oWRITE in
    Ixpc.clunk conn fid

let remove conn rootfid file =
    let fid = Ixpc.walk conn rootfid false file in
    Ixpc.remove conn fid

let dmenu ?prompt:(prompt="") out_str =
   let dmenu_cmd =
      "dmenu" ^ 
    (match prompt with
    | "" -> ""
    |  _ -> " -p \"" ^ prompt ^ "\"") 
    ^ " -b -nb #eeeeee -nf #222222 -sb #335577 -sf #ffffff" in
   let c_in, c_out = Unix.open_process dmenu_cmd in
   output_string c_out out_str;
   close_out c_out;
   let buffer = String.create 1024 in
   let len = input c_in buffer 0 1014 in
   close_in c_in;
   String.sub buffer 0 len

let current_tags () =
   let data = read conn rootfid "/tag" in
   let files = Ixpc.unpack_files data in
   List.rev (List.fold_left 
   (
      fun name_list stat -> 
         match stat.Fcall.name with
         | "sel" -> []
         | name -> name :: name_list 
   ) [] files)

let client_tags () =
   Util.split_string (read conn rootfid "/client/sel/tags") '+'

let current_tag () =
   try
   read conn rootfid "/tag/sel/ctl"
   with Ixpc.IXPError _ -> ""

let quit () =
   write conn rootfid "/ctl" "quit"

let rec do_filtering l filter filtered =
   match l with
   | [] -> filtered
   | head :: tail -> 
      match String.compare head filter with
      | 0 -> do_filtering tail filter filtered 
      | _ -> do_filtering tail filter (head :: filtered)

let rec filter l filter_l =
   match filter_l with
   | [] -> l
   | head :: tail -> 
         let new_l = do_filtering l head [] in
         filter new_l tail

let send_to_tag cid tag =
   write conn rootfid ("/client/" ^ cid ^ "/tags") tag

(* Misc helper functions *)
let list_to_str ?ignore:(ignore = []) ?prefix:(pre = "") str_list =
   let filtered_list = filter str_list ignore in
   List.fold_left
   (
      fun str_list str ->
         let pre_str = pre ^ str in
         match str_list with
         | "" ->  pre_str 
         | _ -> str_list ^ "\n" ^ pre_str
   ) "" filtered_list


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

let programs () =
    let path = Util.split_string (Sys.getenv "PATH") ':' in
    let read acc dir = read_dir dir @ acc in
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
   (* 
    * Catching exception due to 
    * http://flyspray.otur.se/?do=details&task_id=12  
    *)
   try write conn rootfid "/tag/sel/ctl" ("select " ^ dir)
   with Ixpc.IXPError _ -> ()

let send dir =
   try write conn rootfid "/tag/sel/ctl" ("send sel " ^ dir) 
   with Ixpc.IXPError _ -> ()

let mode m =
   write conn rootfid "/tag/sel/ctl" ("colmode sel " ^ m)

let view_tag tag = 
   write conn rootfid "/ctl" ("view " ^ tag)

let sel_tag _ =
   let current = [current_tag ()] in
   let tags = current_tags () in
   let tags_str = list_to_str ~ignore:current tags in
   let new_tag = dmenu ~prompt:"View tag:" tags_str in 
   view_tag new_tag

let set_tag _ =
   try 
      let cid = read conn rootfid "/client/sel/ctl" in
      let client_tags = client_tags () in
      let tags = current_tags () in
      let regular_tags = list_to_str tags in
      let plus_tags = list_to_str ~ignore:client_tags ~prefix:"+" tags in
      let minus_tags = 
         if List.length client_tags > 1 then
            list_to_str ~prefix:"-" client_tags
         else "" in
      let tags_str =  list_to_str [minus_tags ; plus_tags ; regular_tags] in
      let new_tag = dmenu ~prompt:"Set tag:" tags_str in 
      send_to_tag cid new_tag;
   with _ -> ()

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

let create_tag args =
   let tag = List.hd args in
   let tag_file = "/lbar/" ^ tag in
   create conn rootfid tag_file;
   write conn rootfid tag_file (normcolors ^ tag)

let destroy_tag args =
   let tag = List.hd args in
   try 
      remove conn rootfid ("/lbar/" ^ tag)
   with Ixpc.IXPError msg -> 
      debug (sprintf "\nERROR:\nIXPError %s\n\n" msg)

let tagbar_click args =
   match args with
   | [_; tag] -> view_tag tag
   | _ -> ()

let focus_tag args =
   let tag = List.hd args in
   flush stdout;
   try 
      let tag_file = "/lbar/" ^ tag in
      write conn rootfid tag_file (focuscolors ^ tag)
   with Ixpc.IXPError _ -> ()

let unfocus_tag args =
   let tag = List.hd args in
   flush stdout;
   try 
      let tag_file = "/lbar/" ^ tag in
      write conn rootfid tag_file (normcolors ^ tag)
   with Ixpc.IXPError _ -> ()
