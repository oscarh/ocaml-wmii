(* User Settings *)
open Wmii

let font = "-*-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"
let normcolors = "#222222 #eeeeee #666666"
let focuscolors = "#ffffff #335577 #447799"
let backgroundcolors = "#333333"

let default_tag = "default"

(* Key bindings *)
let modkey = "Mod1"
let left = "h"
let right = "l"
let up = "k"
let down = "j"
let toggle = "space" (* toggle between floating and managed layer *)

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
      (modkey ^ "-Shift-" ^ right, send, "right");
      (modkey ^ "-Shift-" ^ left, send, "left");
      (modkey ^ "-Shift-" ^ up, send, "up");
      (modkey ^ "-Shift-" ^ down, send, "down");
      (modkey ^ "-Shift-" ^ toggle, send, "toggle");
      (modkey ^ "-" ^ default, mode, "default");
      (modkey ^ "-" ^ stack, mode, "stack");
      (modkey ^ "-" ^ max, mode, "max");
      (modkey ^ "-" ^ tag, sel_tag, "");
      (modkey ^ "-Shift-" ^ tag, set_tag, "");
      (modkey ^ "-" ^ program, launch, "");
      (modkey ^ "-" ^ termkey, spawn, terminal);
      (modkey ^ "-Shift-" ^ "c", kill, "");
   ]

let events =
   [
      ("CreateTag", create_tag);
      ("DestroyTag", destroy_tag);
      ("LeftBarClick", tagbar_click);
      ("FocusTag", focus_tag);
      ("UnfocusTag", unfocus_tag);
   ]

let tagrules = 
   "/MPlayer.*/ -> ~\n" ^
   "/.*/ -> !\n" ^
   "/.*/ -> default\n"

let colrules = 
   "/.*/ -> 50+50"

let gajim_msgs () =
    let cmd = "gajim-remote get_unread_msgs_number 2> /dev/null" in
    let chan = Unix.open_process_in cmd in
    try
        let count = input_line chan in
        ignore (Unix.close_process_in chan);
        count
    with _ -> "-"

(* Status *)
let status () =
    let tm = Unix.gmtime (Unix.time ()) in
    let year = string_of_int (tm.Unix.tm_year + 1900) in
    let month = Printf.sprintf "%02d" (tm.Unix.tm_mon + 1) in
    let day = Printf.sprintf "%02d" tm.Unix.tm_mday in
    let hour = Printf.sprintf "%02d" (tm.Unix.tm_hour + 1) in
    let minute = Printf.sprintf "%02d" tm.Unix.tm_min in
    let timestr = year ^ "-" ^ month ^ "-" ^ day ^ " " ^ hour  ^ ":" ^ minute in

    let msgs = "Msgs: " ^ (gajim_msgs ()) in

    msgs ^ " | " ^ timestr
