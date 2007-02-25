open Wmii

(** User Settings **)
(* Look and feel *)
let font = "-*-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"
let normcolors = "#222222 #eeeeee #666666"
let focuscolors = "#ffffff #335577 #447799"
let backgroundcolors = "#333333"

let default_tag = "default"
let terminal = "xterm"
let status_interval = 1.0 (* Seconds *)

(* Rules *)
let tagrules = 
   "/MPlayer.*/ -> ~\n" ^
   "/.*/ -> !\n" ^
   "/.*/ -> default\n"

let colrules = 
   "/.*/ -> 50+50"

(* Debug settings *)
let debug_file = "/tmp/wmii_debug.log"
let debug = true

(** Key bindings **)
(* Control keys *)
let modkey = "Mod1"                 (* activate key bindings *)
let modshift = "Shift"              (* access second level bindings *)

(* Movement *)
let left = "h"                      (* move selection / window left *)
let right = "l"                     (* move selection / window right *)
let up = "k"                        (* move selection / window up *)
let down = "j"                      (* move selection / window down *)
let toggle = "space"                (* toggle between floating and managed *)
                                    (* layer                               *)

let kill_key = modshift ^ "-" ^ "c" (* kill a client *)
let action_key = "a"                (* action menu *)

(* Column modes *)
let default = "d"
let stack = "s"
let max = "m"

(* Tag *)
let tag = "t"

(* Actions *)
let program = "p"                   (* Show program list *)
let termkey = "x"                   (* Launch terminal *)

(* Activate the key bindings *)
let keys =
   [
      (modkey ^ "-" ^ right, focus, "right");
      (modkey ^ "-" ^ left, focus, "left");
      (modkey ^ "-" ^ up, focus, "up");
      (modkey ^ "-" ^ down, focus, "down");
      (modkey ^ "-" ^ toggle, focus, "toggle");
      (modkey ^ "-" ^ modshift ^ "-" ^ right, send, "right");
      (modkey ^ "-" ^ modshift ^ "-" ^ left, send, "left");
      (modkey ^ "-" ^ modshift ^ "-" ^ up, send, "up");
      (modkey ^ "-" ^ modshift ^ "-" ^ down, send, "down");
      (modkey ^ "-" ^ modshift ^ "-" ^ toggle, send, "toggle");
      (modkey ^ "-" ^ default, mode, "default");
      (modkey ^ "-" ^ stack, mode, "stack");
      (modkey ^ "-" ^ max, mode, "max");
      (modkey ^ "-" ^ tag, sel_tag, "");
      (modkey ^ "-" ^ modshift ^ "-" ^ tag, set_tag, "");
      (modkey ^ "-" ^ program, launch, "");
      (modkey ^ "-" ^ termkey, spawn, terminal);
      (modkey ^ "-" ^ kill_key, kill, "");
      (modkey ^ "-" ^ action_key, action_menu, "");
   ]

(* Register for events *)
let events =
   [
      ("CreateTag", create_tag);
      ("DestroyTag", destroy_tag);
      ("LeftBarClick", tagbar_click);
      ("FocusTag", focus_tag);
      ("UnfocusTag", unfocus_tag);
   ]

(* Action menu *)
let actions =
   [
      ("quit", quit);
      ("wmiirc", fun () -> spawn Sys.executable_name);
   ]

(* 
 * Plug-in status
 *
 * This will be called periodically (depending on status_interval) and should
 * return a list of type (string * string).
 * The elements in the tuple are (filename, status).
 * The int argument to the callback is the number of the mouse button pressed.
 * The status is shown in the rbar, ordered by the filename.
 *)
let plugin_status () =
    let date = Date.localtime () in
    let msgs = "Msgs: " ^ Util.safe_read Gajim.msg_count in
    let battery_percent = Util.safe_read Acpi.battery_percent in
    let power_state = Util.safe_read Acpi.power_state in

    let battery = "Battery: " ^ battery_percent ^ " (" ^ power_state ^ ")" in

    [
        ("0_gajim", msgs);
        ("1_date", date); 
        ("2_battery", battery);
    ]

(* 
 * Plug-in callbacks 
 *
 * Will be read on start-up. Connects mouse clicks on right bar to callbacks.
 * The names should be the same as the names in the list returned by 
 * plugin_status.
 * The functions must be of type int -> unit. 
 * The argument is the number representing the mouse button pressed.
 *)
let status_callbacks =
    [
        ("0_gajim", Gajim.plugin_cb);
    ]
