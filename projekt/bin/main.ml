open Front

let () = Opts.parse_argv ()
let () = Logf.open_log ()
let () = Front.eval ()
let () = Logf.close_log ()
