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
let ac_state_file = "/proc/acpi/ac_adapter/AC/state"
let batt_state_file = "/proc/acpi/battery/BAT0/state"
let batt_info_file = "/proc/acpi/battery/BAT0/info"

let value_strip = Str.regexp "[a-z ]+: +\\([0-9A-Za-z-]+\\)"

let power_state () =
    let inc = open_in ac_state_file in
    let status = input_line inc in
    close_in inc;
    if Str.string_match value_strip status 0 then
        match Str.matched_group 1 status with
        | "on-line" -> "AC"
        | _ -> "Battery"
    else
        "Unknown"

let battery_percent () =
    let inc = open_in batt_state_file in
    let battery_state = Array.create 5 "" in
    for i = 0 to (Array.length battery_state) - 1 do
        battery_state.(i) <- input_line inc
    done;
    close_in inc;
    let inc = open_in batt_info_file in
    let battery_info = Array.create 3 "" in
    for i = 0 to (Array.length battery_info) - 1 do
        battery_info.(i) <- input_line inc
    done;
    close_in inc;
    let b_present = if Str.string_match value_strip battery_state.(0) 0 then
            match Str.matched_group 1 battery_state.(0) with
            | "yes" -> true
            | _ -> false
        else 
            false in
    if b_present then
        let remaning = if Str.string_match value_strip battery_state.(4) 0 then
                float_of_string (Str.matched_group 1 battery_state.(4))
            else 
                -1. in
        let capacity = if Str.string_match value_strip battery_info.(2) 0 then
                float_of_string (Str.matched_group 1 battery_info.(2))
            else
                1. in
        let percent = floor ((remaning /. capacity) *. 100.) in
        (string_of_int (int_of_float percent)) ^ "%"
    else 
        "N/A"
