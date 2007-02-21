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
      (modkey ^ "-Shift-" ^ right, send, "right");
      (modkey ^ "-Shift-" ^ left, send, "left");
      (modkey ^ "-Shift-" ^ up, send, "up");
      (modkey ^ "-Shift-" ^ down, send, "down");
      (modkey ^ "-" ^ default, mode, "default");
      (modkey ^ "-" ^ stack, mode, "stack");
      (modkey ^ "-" ^ max, mode, "max");
      (modkey ^ "-" ^ tag, sel_tag, "");
      (modkey ^ "-Shift-" ^ tag, set_tag, "");
      (modkey ^ "-" ^ program, launch, "");
      (modkey ^ "-" ^ termkey, spawn, terminal);
   ]

let events =
   [
   ]

let tagrules = 
   "/MPlayer.*/ -> ~\n" ^
   "/.*/ -> !\n" ^
   "/.*/ -> default\n"

let colrules = 
   "/.*/ -> 50+50"

(* Status *)
let status () =
    "Welcome to OCaml-wmii!"
