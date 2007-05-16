(******************************************************************************)
(* ocaml-wmii                                                                 *)
(*                                                                            *)
(* Copyright 2007                                                             *)
(* Oscar Hellström, oscar at oscarh dot net                                   *)
(* Ulf Eliasson, eliassou at ituniv dot se                                    *)
(*                                                                            *)
(* All rights reserved                                                        *)
(* Redistribution and use in source and binary forms, with or without         *)
(* modification, are permitted provided that the following conditions are     *)
(* met:                                                                       *)
(*                                                                            *)
(*     * Redistributions of source code must retain the above copyright       *)
(*       notice, this list of conditions and the following disclaimer.        *)
(*     * Redistributions in binary form must reproduce the above copyright    *)
(*       notice, this list of conditions and the following disclaimer in the  *)
(*       documentation and/or other materials provided with the distribution. *)
(*     * The names of its contributors may not be used to endorse or promote  *)
(*       products derived from this software without specific prior written   *)
(*       permission.                                                          *)
(*                                                                            *)
(*                                                                            *)
(* THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND    *)
(* ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE      *)
(* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE *)
(* ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE    *)
(* FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL *)
(* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR *)
(* SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER *)
(* CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT         *)
(* LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY  *)
(* OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF     *)
(* SUCH DAMAGE.                                                               *)
(******************************************************************************)
open Wmii

(** User Settings **)
(* Look and feel *)
let font = "-*-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"
let normcolors = {text = "#222222" ; color = "#eeeeee" ; border = "#666666"}
let focuscolors = {text = "#ffffff" ; color = "#335577" ; border = "#447799"}
let urgentcolors = {text = "#222222" ; color = "#FFE47A" ; border = "#447799"}
let backgroundcolors = "#333333"

let dmenu_on_bottom = true

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
let last = "i"
let view_urgent = "g"

let switch_left = "o"
let switch_right = "e"

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
		(modkey ^ "-" ^ last, toggle_last, "");
		(modkey ^ "-" ^ view_urgent, view_urgent_tag, "");
		(modkey ^ "-" ^ switch_left, switch_workspace, "left");
		(modkey ^ "-" ^ switch_right, switch_workspace, "right");
	]

(* Register for events *)
let events =
	[
		("CreateTag", create_tag);
		("DestroyTag", destroy_tag);
		("LeftBarClick", tagbar_click);
		("FocusTag", focus_tag);
		("UnfocusTag", unfocus_tag);
		("Urgent", urgent); 
		("NotUrgentTag", not_urgent_tag);
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
	]
