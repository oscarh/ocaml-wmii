(* Global *)
let event_hash = Hashtbl.create 10
let key_hash = Hashtbl.create 10

(* WMII settings *)
let status_file = "/rbar/status"

(* Help functions *)
let update_events () =
   let reg (event, func) =  
      Hashtbl.add event_hash event func in
   List.iter reg Wmii_conf.events

let handle_event event_type event_arg =
   try
      let event_fun = Hashtbl.find event_hash event_type in
         Printf.printf "Event found in hash\n";
         flush stdout;
         event_fun (event_arg)
   with Not_found ->  
      Printf.printf "Event not found in hash\n"; 
      flush stdout;
      ()  

let update_keys () =
   let reg (key, cb, arg) = 
      Hashtbl.add key_hash key (cb, arg) in
   List.iter reg Wmii_conf.keys;

   let new_keys = Hashtbl.fold 
   (
      fun key _ key_str -> 
            key ^ "\n" ^ key_str
   ) 
   key_hash
   "" in
      Wmii.write Wmii.conn Wmii.rootfid "keys" new_keys
 
(* Event *)
let handle_key key =
   try 
   let func, arg = Hashtbl.find key_hash key in
   func arg
   with Not_found -> ()

let event_loop () =
   Printf.printf "Read event start: \n";
   flush stdout;
   let fid, iounit = Ixpc.walk_open Wmii.conn Wmii.rootfid false "event" Ixpc.oREAD in

   let rec read offset =
      let event = Ixpc.read Wmii.conn fid iounit offset (Int32.of_int 4096) in

      Printf.printf "Read event: %s\n" event;

      let len = String.length event in 
      let index = String.index event ' ' in 
      let event_type = String.sub event 0 index in
      let event_arg = String.sub event (index+1) (len-index-2) in 

      Printf.printf "Event_type: \"%s\"\n" event_type;
      Printf.printf "Event_arg: \"%s\"\n" event_arg;

      (match event_type with
      | "Key" -> handle_key event_arg;
      | _ -> handle_event event_type event_arg);

      if len > 0 then
         read (Int64.add offset  (Int64.of_int len)) in
   read Int64.zero;
   Ixpc.clunk Wmii.conn fid


(* Staus loop *)
let status_loop () =
   let conn = Ixpc.connect Wmii.wmii_address in
   let rootfid = Ixpc.attach conn Wmii.user "/" in
    let rec loop () =
        Wmii.write conn rootfid status_file (Wmii_conf.status ());
        Thread.delay Wmii_conf.status_interval;
        loop () in
    Thread.create loop ()

let setup_bars () = (* Remove them if we are restarted *)
    try Wmii.remove Wmii.conn Wmii.rootfid "/rbar/status" with Ixpc.IXPError _ -> ();
    Wmii.create Wmii.conn Wmii.rootfid "/rbar/status"

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

   (* create a file in tag *)

   update_keys ();
   update_events ();

   setup_bars ();

   let status_thread = status_loop () in
   event_loop ()

let _ = main ()
