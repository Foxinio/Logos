open Getopts

module Make () : sig
  val files : string list ref
  val extra_debug : bool ref
  val parse_argv : unit -> unit
end = struct
  let files = ref []
  let extra_debug = ref false

  let spec =
    Getopts.spec "[-g][--][files]" "Logos iterpreter"
      [
        Getopts.flag 'g'
          (fun () -> extra_debug := true)
          "Print extra debug info";
      ]
      (Getopts.queue files)
      [ Getopts.note "AUTHOR" "Szymon Jędras" ]

  let parse_argv () = Getopts.parse_argv spec ()
end
