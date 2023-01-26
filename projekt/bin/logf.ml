let output_log =
  if Option.is_none @@ Sys.getenv_opt "LGS_LOG_ON" then open_out "/dev/null"
  else if Option.is_some @@ Sys.getenv_opt "LGS_LOG_TO_STDIN" then stdout
  else
    let output_file =
      match Sys.getenv_opt "LGS_LOG_OUTPUT_FILE" with
      | Some s -> s
      | None -> "log_out"
    in
    open_out output_file

let logf fmt = Printf.fprintf output_log fmt

let log_backtrace () =
    Printexc.print_backtrace output_log

let close_log () =
  close_out_noerr output_log
