(* Global *)
let (event_hash : (string,  (string list -> unit)) Hashtbl.t) = Hashtbl.create 10
let key_hash = Hashtbl.create 30
let plugin_actions = Hashtbl.create 10

(* WMII settings *)
let status_file = "/rbar/status"

let running = ref true (* So that we can stop the status loop *)

(* Help functions *)
let add_event event func =
   Hashtbl.add event_hash event func

let update_events () =
   Hashtbl.clear event_hash;
   let reg (event, func) = add_event event func in
   List.iter reg Wmii_conf.events

let update_keys () =
   Hashtbl.clear key_hash;
   let reg (key, cb, arg) = Hashtbl.add key_hash key (cb, arg) in
   List.iter reg Wmii_conf.keys;
   let key_string key _ key_str = key ^ "\n" ^ key_str in
   let new_keys = Hashtbl.fold key_string key_hash "" in
   Wmii.write Wmii.conn Wmii.rootfid "keys" new_keys

let update_actions () =
   Hashtbl.clear Wmii.actions;
   let add_actions (name, cb) = Hashtbl.add Wmii.actions name cb in
   List.iter add_actions Wmii_conf.actions

let update_plugin_actions () =
   Hashtbl.clear plugin_actions;
   let add_actions (name, callback) = 
      Hashtbl.add plugin_actions name callback in
   List.iter add_actions (Wmii_conf.status_callbacks)

(* Event *)

let setup_bars () = (* Remove them if we are restarted *)
    let data = Wmii.read Wmii.conn Wmii.rootfid "/rbar/" in
    let dirs = Ixpc.unpack_files data in
    let remove_file stat =
       let file = "/rbar/" ^ stat.Fcall.name in
       try Wmii.remove Wmii.conn Wmii.rootfid file with _ -> () in
    List.iter remove_file dirs 

let restart status_thread _ =
   running := false;
   Thread.join status_thread

let rigth_bar_click args =
   match args with
   | [button; name] -> 
      (try
         let cb = Hashtbl.find plugin_actions name in
          cb (int_of_string button)
      with _ -> ()) (* TODO log error *)
   | _ -> ()

let handle_event event args =
   try
      let event_fun = Hashtbl.find event_hash event in
      Printf.printf "Event found in hash: \"%s\"\n" event;
      flush stdout;
      event_fun args
   with Not_found ->  
      Printf.printf "Event not found in hash: \"%s\"\n" event;
      flush stdout;
      ()  

let handle_key key =
   print_string ("Got key: \"" ^ key ^ "\"");
   print_newline ();
   try 
      let func, arg = Hashtbl.find key_hash key in
      print_string "Found key in hash\n";
      func arg
   with Not_found -> ()

let match_event event args =
   print_string ("Event: " ^ event ^ "\n");
   let rec print_list l =
      match l with
      | hd :: tl -> print_string (hd ^ "\n"); print_list tl
      | [] -> () in
   print_list args;
   match event with
   | "Key" -> handle_key (List.hd args)
   | _ -> handle_event event args

let rec split_event str i acc =
   if i < (String.length str) - 2 then (* beware of \n *)
      (if str.[i] = ' ' then
         let rest = String.sub str (i + 1) (String.length str - (i + 1)) in
         split_event rest 0 ((String.sub str 0 i) :: acc)
      else
         split_event str (i + 1) acc)
   else 
      let strlen = String.length str in
      List.rev (String.sub str 0 (strlen - 1) :: acc)

let event_loop () =
   Printf.printf "Event loop start\n";
   flush stdout;
   let fid, iounit =
      Ixpc.walk_open Wmii.conn Wmii.rootfid false "event" Ixpc.oREAD in
   let rec read offset =
      let data = Ixpc.read Wmii.conn fid iounit offset (Int32.of_int 4096) in
      Printf.printf "Got event data: %s\n" data;
      let read_len = String.length data in 
      let tokens = split_event data 0 [] in
      match_event (List.hd tokens) (List.tl tokens);
      if read_len > 0 && !running then
         read (Int64.add offset  (Int64.of_int read_len)) in
   read Int64.zero;
   Ixpc.clunk Wmii.conn fid

let xwrite conn rootfid file data =
   (try 
      let fid = Ixpc.walk conn rootfid false file in
      ignore (Ixpc.stat conn fid);
      Ixpc.clunk conn fid
   with Ixpc.IXPError _ -> Wmii.create conn rootfid file);
   Wmii.write conn rootfid file data

(* Staus loop *)
let status_loop () =
   let conn = Ixpc.connect Wmii.wmii_address in
   let rootfid = Ixpc.attach conn Wmii.user "/" in
   let status = Wmii_conf.plugin_status () in
   let xwrite_status (file, data) =
      xwrite conn rootfid ("/rbar/" ^ file) data in
   List.iter xwrite_status status;
   let rec loop () =
      let status = Wmii_conf.plugin_status () in
      let write_status (file, data) =
         Wmii.write conn rootfid ("/rbar/" ^ file) data in
      List.iter write_status status;
      Thread.delay Wmii_conf.status_interval;
      if !running then loop () in
   Thread.create loop ()

(* Main startup *)
let main () =
   Wmii.write Wmii.conn Wmii.rootfid "event" "Start wmiirc";

   Wmii.write Wmii.conn Wmii.rootfid "ctl" ("font " ^ Wmii_conf.font);
   Wmii.write Wmii.conn Wmii.rootfid "ctl" ("focuscolors " ^ Wmii_conf.focuscolors);
   Wmii.write Wmii.conn Wmii.rootfid "ctl" ("normcolors " ^ Wmii_conf.normcolors);
   Wmii.write Wmii.conn Wmii.rootfid "ctl" ("grabmod " ^ Wmii_conf.modkey);
   Wmii.write Wmii.conn Wmii.rootfid "ctl" "border 1";

   Wmii.write Wmii.conn Wmii.rootfid "tagrules" Wmii_conf.tagrules;
   Wmii.write Wmii.conn Wmii.rootfid "colrules" Wmii_conf.colrules;

   print_string "set up fonts and colours";
   print_newline ();

   (* create a file in tag *)

   update_keys ();
   update_events ();
   update_actions ();
   update_plugin_actions ();

   add_event "RightBarClick" rigth_bar_click;

   setup_bars ();

   let status_thread = status_loop () in
   add_event "Start" (restart status_thread);
   event_loop ()

let _ = main ()
