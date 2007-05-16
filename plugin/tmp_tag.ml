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

(*
 *
 * Put something like this in keys in wmii_config:
 * (modkey ^ "-" ^ b, Tmp_tag.toggle, "");
 * and something like this in events:
 * 
 * ("DestroyClient", Tmp_tag.destroy_client_callback);
 *)

open Wmii

let original_tags = Hashtbl.create 4

let destroy_client_callback args =
	let cid = List.hd args in
	Hashtbl.remove original_tags cid

let tag cid =
	try
		Hashtbl.add original_tags cid (current_tag ());
		let tmp_tag = "+" ^ cid in
		send_to_tag cid tmp_tag;
		view_tag  tmp_tag
	with _ -> ()

let untag_and_view cid tag =
	try
		Hashtbl.remove original_tags cid;
		let tmp_tag = "-" ^ cid in
		send_to_tag cid tmp_tag;
		view_tag tag
	with _ -> ()

let toggle _ =
	try
		let cid = read conn rootfid "/client/sel/ctl" in
		(try 
			let tag = Hashtbl.find original_tags cid in
			untag_and_view cid tag
		with Not_found -> tag cid)
	with _ -> ()
