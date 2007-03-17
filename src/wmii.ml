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

open Printf

(* Types/Globals *)
type color = {text:string; color:string; border:string}

let last_tag = ref None
let second_last_tag = ref None
let urgent_tags = ref []

(* Connection *)
let adrs_exp = Str.regexp "unix!\\(.+\\)"
let wmii_address =
    let adrs = Sys.getenv "WMII_ADDRESS" in
    if Str.string_match adrs_exp adrs 0 then Str.matched_group 1 adrs
    else adrs

let user = Sys.getenv "USER"
let conn = O9pc.connect wmii_address
let rootfid = O9pc.attach conn user "/"

(* Activate/deactivate debug *)
let debug_channel = ref None

let normcolors = ref {text = "#222222" ; color = "#eeeeee" ; border="#666666"}
let focuscolors = ref {text = "#ffffff" ; color = "#335577" ; border = "#447799"}
let urgentcolors = ref {text = "#222222" ; color = "#F5B800" ; border = "#447799"}
let backgroundcolors = ref "#333333"
let font = ref "-*-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"

(* Action menu *)
let (actions : (string, (unit -> unit)) Hashtbl.t) = Hashtbl.create 10

let debug str =
   match !debug_channel with
   | Some channel -> 
      output_string channel str;
      flush channel
   | None -> ()

(* Core functions *)
let write conn rootfid file data =
   let fid, iounit = O9pc.walk_open conn rootfid false file O9pc.oWRITE in
   let len = Int32.of_int (String.length data) in
   ignore(O9pc.write conn fid iounit Int64.zero len data);
   O9pc.clunk conn fid

let read conn rootfid file = 
   let fid, iounit = O9pc.walk_open conn rootfid false file O9pc.oREAD in
   let data = O9pc.read conn fid iounit Int64.zero (Int32.of_int 4096) in
   O9pc.clunk conn fid;
   data

let create conn rootfid file =
    let perm = Int32.shift_left (Int32.of_int 0x2) 6 in
    let index = try String.rindex file '/' with Not_found -> 0 in
    let dir = String.sub file 0 index in
    let file = 
        String.sub file (index + 1) ((String.length file) - (index + 1)) in
    let fid = O9pc.walk conn rootfid false dir in
    let _ = O9pc.create conn fid file perm O9pc.oWRITE in
    O9pc.clunk conn fid

let remove conn rootfid file =
    let fid = O9pc.walk conn rootfid false file in
    O9pc.remove conn fid

let dmenu ?prompt:(prompt="") out_str =
   let cmd = "dmenu" in
   let args = [|
      cmd;
      "-b";
      "-fn"; !font;
      "-nb"; !normcolors.color;
      "-nf"; !normcolors.text;
      "-sb"; !focuscolors.color;
      "-sf"; !focuscolors.text;
   |] in
   let args = match prompt with
      | "" -> args
      |  _ -> Array.append args [|"-p"; prompt|] in
   (* Names with regard to child process. *)
   let in_read, in_write = Unix.pipe () in
   let out_read, out_write = Unix.pipe () in
   match Unix.fork () with
   | 0 ->
      (* Use the pipes as stdin / stdout. *)
      (Unix.dup2 in_read Unix.stdin; Unix.close in_read;
      Unix.dup2 out_write Unix.stdout; Unix.close out_write;
      (* We should not care about these in the child. *)
      Unix.close in_write; Unix.close out_read;
      (* All set, start dmenu *)
      try Unix.execvp cmd args with _ ->
         let msg = "Could not execute dmenu, make sure it is in your path." in
         raise (Failure msg))
   | pid ->
      (* Close the child's fd:s. *)
      (Unix.close out_write; Unix.close in_read;
      ignore (Unix.write in_write out_str 0 (String.length out_str));
      (* EOF in dmenu *)
      Unix.close in_write;
      let buffer = String.create 1024 in
      let len = Unix.read out_read buffer 0 1024 in
      Unix.close out_read;
      (* Clean up child process. *)
      ignore (Unix.waitpid [] pid);
      String.sub buffer 0 len)

let current_tags () =
   let data = read conn rootfid "/tag" in
   let files = O9pc.unpack_files data in
   List.rev (List.fold_left 
   (
      fun name_list stat -> 
         match stat.Fcall.name with
         | "sel" -> []
         | name -> name :: name_list 
   ) [] files)

let client_tags () =
   Util.split_string (read conn rootfid "/client/sel/tags") '+'

let current_tag () =
   try
   read conn rootfid "/tag/sel/ctl"
   with O9pc.Client_error _ -> ""

let quit () =
   write conn rootfid "/ctl" "quit"

let rec do_filtering l filter filtered =
   match l with
   | [] -> filtered
   | head :: tail -> 
      match String.compare head filter with
      | 0 -> do_filtering tail filter filtered 
      | _ -> do_filtering tail filter (head :: filtered)

let rec filter l filter_l =
   match filter_l with
   | [] -> l
   | head :: tail -> 
         let new_l = do_filtering l head [] in
         filter new_l tail

let send_to_tag cid tag =
   write conn rootfid ("/client/" ^ cid ^ "/tags") tag

(* Misc helper functions *)
let color_to_string color =
   color.text ^ " " ^ color.color ^ " " ^ color.border

let list_to_str ?ignore:(ignore = []) ?prefix:(pre = "") str_list =
   let filtered_list = filter str_list ignore in
   List.fold_left
   (
      fun str_list str ->
         let pre_str = pre ^ str in
         match str_list with
         | "" ->  pre_str 
         | _ -> str_list ^ "\n" ^ pre_str
   ) "" filtered_list


let read_dir dir = 
   try 
      let handle = Unix.opendir dir in
      let rec read_file acc =
         try 
            let new_acc = match Unix.readdir handle with
            | ".." -> acc
            | "." -> acc
            | file -> if file.[0] = '.' then acc else file :: acc in
            read_file new_acc
         with End_of_file -> acc in
      let files = read_file [] in
      Unix.closedir handle;
      files
   with Unix.Unix_error (_, "opendir", _) -> []

let programs () =
    let path = Util.split_string (Sys.getenv "PATH") ':' in
    let read acc dir = read_dir dir @ acc in
    List.fold_left read [] path

let program_str =
   let buff = Buffer.create 2048 in
   let progs = List.sort String.compare (programs ()) in
   Buffer.add_string buff (List.hd progs);
   let add program = Buffer.add_string buff ("\n" ^ program) in
   List.iter add (List.tl progs);
   Buffer.contents buff

(* Key bind functions *)
let kill _ =
   write conn rootfid "/client/sel/ctl" "kill"

let spawn cmd =
   ignore(Sys.command (cmd ^ "&"));
   ()

let focus dir = 
   (* 
    * Catching exception due to 
    * http://flyspray.otur.se/?do=details&task_id=12  
    *)
   try write conn rootfid "/tag/sel/ctl" ("select " ^ dir)
   with O9pc.Client_error _ -> ()


let send dir =
   try write conn rootfid "/tag/sel/ctl" ("send sel " ^ dir) 
   with O9pc.Client_error _ -> ()

let mode m =
   try write conn rootfid "/tag/sel/ctl" ("colmode sel " ^ m)
   with O9pc.Client_error _ -> ()

let view_tag ?set_history:(set_history=true)tag = 
	if set_history then
		begin 
			match !second_last_tag with
			| None ->
				last_tag := Some (current_tag ())
			| _ ->
				last_tag := !second_last_tag;
				second_last_tag := None
		end
	else
		begin 
			match !second_last_tag with
			| None -> 
					second_last_tag := !last_tag;
					last_tag := Some (current_tag ())
			| _ ->
					()
		end;
   write conn rootfid "/ctl" ("view " ^ tag)

let view_urgent_tag _ =
	match !urgent_tags with
	| [] -> () 
	| _ ->  view_tag ~set_history:false (List.hd !urgent_tags)

let toggle_last _ =
   match !last_tag with
   | Some t -> view_tag t
   | None -> ()

let sel_tag _ =
   let current = current_tag () in
   let tags = current_tags () in
	let tags_str = list_to_str ~ignore:[current] tags in
   let new_tag = dmenu ~prompt:"view:" tags_str in 
	if current = new_tag then
		()
	else
		view_tag new_tag

let set_tag _ =
   try 
      let cid = read conn rootfid "/client/sel/ctl" in
      let client_tags = client_tags () in
      let tags = current_tags () in
      let regular_tags = list_to_str tags in
      let plus_tags = list_to_str ~ignore:client_tags ~prefix:"+" tags in
      let minus_tags = 
         if List.length client_tags > 1 then
            list_to_str ~prefix:"-" client_tags
         else "" in
      let tags_str =  list_to_str [minus_tags ; plus_tags ; regular_tags] in
      let new_tag = dmenu ~prompt:"set:" tags_str in 
      send_to_tag cid new_tag;
   with _ -> ()

let launch _ =
   match dmenu ~prompt:"run: " program_str with
   | "" -> ()
   | cmd -> spawn cmd

let action_menu _ =
   let action_str = if Hashtbl.length actions > 0 then
      let build_str str action = str ^ "\n" ^ action in
      let action_list = 
         Hashtbl.fold (fun action _ l -> action :: l) actions [] in
      List.fold_left build_str (List.hd action_list) (List.tl action_list)
   else
      "" in
   match dmenu ~prompt:"action: " action_str with
   | ""-> ()
   | action -> 
      try 
         let cb = Hashtbl.find actions action in
         cb ()
      with Not_found -> ()


(* Event functions *)
let create_client cid =
   let current_tag = read conn rootfid "/tag/sel/ctl" in
   write conn rootfid ("/client/" ^ cid ^ "/tags") current_tag

let create_tag args =
   let tag = List.hd args in
   let tag_file = "/lbar/" ^ tag in
   create conn rootfid tag_file;
   write conn rootfid tag_file 
      ((color_to_string !normcolors) ^ tag)

let destroy_tag args =
   let tag = List.hd args in
   try 
      remove conn rootfid ("/lbar/" ^ tag)
   with O9pc.Client_error msg -> 
      debug (sprintf "\nERROR:\nClient_error %s\n\n" msg)

let tagbar_click args =
   match args with
   | [_; tag] -> view_tag tag
   | _ -> ()

let focus_tag args =
   let tag = List.hd args in
   try 
      let tag_file = "/lbar/" ^ tag in
      write conn rootfid tag_file 
         ((color_to_string !focuscolors) ^ tag)
   with O9pc.Client_error _ -> ()

let set_tagbar_color tag color =
   try 
      let tag_file = "/lbar/" ^ tag in
      write conn rootfid tag_file 
         ((color_to_string color) ^ tag)
   with O9pc.Client_error _ -> ()

let focus_tag args =
   let tag = List.hd args in
		set_tagbar_color tag !focuscolors

let unfocus_tag args =
   let tag = List.hd args in
	let colors =
		if List.exists (fun t -> tag = t) !urgent_tags then
			!urgentcolors
		else 
			!normcolors
	in set_tagbar_color tag colors

(* Urgent handling *)
let get_client_tags cid =
	let tags = read conn rootfid ("/client/" ^ cid ^ "/tags") in
	Util.split_string tags '+'


let set_urgent tags =
	let current = current_tag () in
	urgent_tags := List.append !urgent_tags tags;
	List.iter (
		fun tag -> if not (current = tag) then set_tagbar_color tag !urgentcolors
		) tags

let urgent args =
	let client_id = List.hd args in
	let client_tags = get_client_tags client_id in
	set_urgent client_tags

let not_urgent_tag args =
	let tag = List.nth args 1 in
	let current = current_tag () in	
	urgent_tags := List.filter (fun t -> not (t = tag)) !urgent_tags;
	let colors = 
		begin if current = tag then
			!focuscolors
		else
			!normcolors
		end
	in set_tagbar_color tag colors 
