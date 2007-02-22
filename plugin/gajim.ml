
let msg_count () =
    let cmd = "gajim-remote get_unread_msgs_number 2> /dev/null" in
    let chan = Unix.open_process_in cmd in
    try
        let count = input_line chan in
        ignore (Unix.close_process_in chan);
        count
    with _ -> "-"
