
let msg_count () =
    let cmd = "gajim-remote get_unread_msgs_number 2> /dev/null" in
    let chan = Unix.open_process_in cmd in
    try
        let count = input_line chan in
        ignore (Unix.close_process_in chan);
        count
    with _ -> "-"

let open_pending_event () =
    let cmd = "gajim-remote show_next_pending_event >/dev/null 2>&1 &" in
    ignore (Sys.command cmd)
