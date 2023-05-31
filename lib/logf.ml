let output_log = ref None
let devnull = open_out "/dev/null"

let open_log () =
  output_log :=
      if Option.is_none @@ Sys.getenv_opt "LGS_LOG_ON" then
          None
      else if Option.is_some @@ Sys.getenv_opt "LGS_LOG_TO_STDIN" then Some(stdout)
      else
        let output_file =
          match Sys.getenv_opt "LGS_LOG_OUTPUT_FILE" with
          | Some s -> s
          | None -> "log_out"
        in
        Some(open_out output_file)

let get_or opt = match opt with None -> devnull | Some chan -> chan

let logf (fmt : ('a, 'b, 'c, 'd, 'e, 'f) format6) =
    Printf.fprintf (get_or !output_log) (fmt^^"%!")

let log_backtrace () =
  match !output_log with
  | Some output_log -> Printexc.print_backtrace output_log
  | None -> ()

let close_log () =
  match !output_log with
  | Some output_log -> close_out_noerr output_log
  | None -> ()
