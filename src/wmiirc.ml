(* User Settings *)

let terminal = "xrvt"

let status_interval = 1.0 (* Seconds *)

(* WMII settings *)
let status_file = "/rbar/status"

(* User defined functions *)
let status () =
    "Welcome to OCaml-wmii!"

(* Help functions *)
let write file data =
    ()

let read file =
    ()

(* Event loop *)
let rec event_loop () =
    
    event_loop ()

(* Staus loop *)
let status_loop () =
    let rec loop () =
        write status_file (status ());
        Thread.delay status_interval;
        loop () in
    Thread.create loop ()

(* Main startup *)
let main () =
    let status_thread = status_loop () in
    event_loop ()

let _ = main ()
