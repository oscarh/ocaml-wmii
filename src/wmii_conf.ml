(* User Settings *)
open Wmii

let font = "-*-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"
let normcolors = "#222222 #eeeeee #666666"
let focuscolors = "#ffffff #335577 #447799"
let backgroundcolors = "#333333"

let default_tag = "default"

(* Key bindings *)
let modkey = "Mod1"
let modshift = "Shift"

let left = "h"
let right = "l"
let up = "k"
let down = "j"
let toggle = "space" (* toggle between floating and managed layer *)
let action = "a"

let kill_key = modshift ^ "-" ^ "c"
let action_key = "a"

(* Column modes *)
let default = "d"
let stack = "s"
let max = "m"

(* Tag *)
let tag = "t"

(* Program *)
let program = "p"
let termkey = "x"

let terminal = "xterm"

let status_interval = 1.0 (* Seconds *)

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

let events =
   [
      ("CreateTag", create_tag);
      ("DestroyTag", destroy_tag);
      ("LeftBarClick", tagbar_click);
      ("FocusTag", focus_tag);
      ("UnfocusTag", unfocus_tag);
   ]

let actions =
   [
      ("quit", quit);
   ]

let tagrules = 
   "/MPlayer.*/ -> ~\n" ^
   "/.*/ -> !\n" ^
   "/.*/ -> default\n"

let colrules = 
   "/.*/ -> 50+50"

let safe_read func =
    try 
        func ()
    with _ ->
        "-"

(* 
 * Status
 *
 * This will be called periodically (depending on status_interval) and should
 * return a list of type (string * string * (unit -> unit) option).
 * The elements in the tuple are (filename, status, callback).
 * The status is listed in the rbar, ordered by the filename.
 *)
let status () =
    let tm = Unix.localtime (Unix.time ()) in
    let year = string_of_int (tm.Unix.tm_year + 1900) in
    let month = Printf.sprintf "%02d" (tm.Unix.tm_mon + 1) in
    let day = Printf.sprintf "%02d" tm.Unix.tm_mday in
    let hour = Printf.sprintf "%02d" (tm.Unix.tm_hour) in
    let minute = Printf.sprintf "%02d" tm.Unix.tm_min in
    let timestr = year ^ "-" ^ month ^ "-" ^ day ^ " " ^ hour  ^ ":" ^ minute in
    
    let msgs = "Msgs: " ^ safe_read Gajim.msg_count in
    let battery_percent = safe_read Acpi.battery_percent in
    let power_state = safe_read Acpi.power_state in

    let battery = "Battery: " ^ battery_percent ^ " (" ^ power_state ^ ")" in

    [
        ("0_gajim", msgs, None);
        ("1_date", timestr, None);
        ("2_battery", battery, None);
    ]
