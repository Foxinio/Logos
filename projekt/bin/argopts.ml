open Getopts

module Make () : sig
  val files : string list ref
  val to_eval : string list ref
  val print_detail : bool ref
  val print_lexer : bool ref
  val parse_argv : unit -> unit
end = struct
  let files = ref []
  let to_eval = ref []
  let print_detail = ref false
  let print_lexer = ref false

  let spec =
    Getopts.spec "[-gle:][--][files]" "Logos iterpreter"
      [
        Getopts.flag 'g'
          (fun () -> print_detail := true)
          "Print extra debug info";
        Getopts.flag 'l'
          (fun () -> print_lexer := true)
          "Print lexer output";
        Getopts.string 'e'
          (fun s () -> to_eval := s :: !to_eval)
          "eval expressions given from command argument";
      ]
      (Getopts.queue files)
      [ Getopts.note "AUTHOR" "Szymon Jędras" ]

  let parse_argv () = Getopts.parse_argv spec ()
end
