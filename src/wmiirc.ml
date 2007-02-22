(* Global *)
let event_hash = Hashtbl.create 10
let key_hash = Hashtbl.create 20

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

(* Event *)

let handle_event event_type event_arg =
   try
      let event_fun = Hashtbl.find event_hash event_type in
      Printf.printf "Event found in hash: \"%s\"\n" event_type;
      flush stdout;
      event_fun (event_arg)
   with Not_found ->  
      Printf.printf "Event not found in hash: \"%s\"\n" event_type;
      flush stdout;
      ()  

let handle_key key =
   try 
      let func, arg = Hashtbl.find key_hash key in
      func arg
   with Not_found -> ()

let handle_raw_event event =
   let len = String.length event in 
   try
      let index = String.index event ' ' in
      let event_type = String.sub event 0 index in
      let event_arg = String.sub event (index+1) (len-index-1) in 
      Printf.printf "Event_type: \"%s\"\n" event_type;
      Printf.printf "Event_arg: \"%s\"\n" event_arg;

      match event_type with
      | "Key" -> handle_key event_arg;
      | _ -> handle_event event_type event_arg

   with Not_found ->
      Printf.printf "Event_type: \"%s\"\n" event;
      Printf.printf "Event_arg: \"\"\n";
      handle_event event ""

let setup_bars () = (* Remove them if we are restarted *)
    try Wmii.remove Wmii.conn Wmii.rootfid "/rbar/status" with Ixpc.IXPError _ -> ();
    Wmii.create Wmii.conn Wmii.rootfid "/rbar/status"

let restart status_thread _ =
   running := false;
   Thread.join status_thread

let event_loop () =
   Printf.printf "Event loop start\n";
   flush stdout;
   let fid, iounit = Ixpc.walk_open Wmii.conn Wmii.rootfid false "event" Ixpc.oREAD in

   let rec read offset =
      let data = Ixpc.read Wmii.conn fid iounit offset (Int32.of_int 4096) in
      Printf.printf "Got event data: %s\n" data;
      let read_len = String.length data in 
      let rec parse_event str =
         let len = String.length str in
         if len > 0 then 
            let index = String.index str '\n' in 
            let raw_event = String.sub str 0 index in
            handle_raw_event raw_event;
            let rest = String.sub str (index+1) (len-index-1) in
            parse_event rest in
      parse_event data;
      if read_len > 0 && !running then
         read (Int64.add offset  (Int64.of_int read_len)) in
   read Int64.zero;
   Ixpc.clunk Wmii.conn fid

(* Staus loop *)
let status_loop () =
   let conn = Ixpc.connect Wmii.wmii_address in
   let rootfid = Ixpc.attach conn Wmii.user "/" in
   (try 
      let statusfid = Ixpc.walk conn rootfid false "/rbar/status" in
      ignore (Ixpc.stat conn statusfid);
      Ixpc.clunk conn statusfid
   with Ixpc.IXPError message -> Wmii.create conn rootfid "/rbar/status");
   let rec loop () =
      Wmii.write conn rootfid status_file (Wmii_conf.status ());
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

   setup_bars ();

   let status_thread = status_loop () in
   add_event "Start" (restart status_thread);
   event_loop ()

let _ = main ()
