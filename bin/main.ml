open! Core

let unit_arg =
  Command.Arg_type.create (fun value ->
      match Unitconv.parse_unit value with
      | Some unit_name -> unit_name
      | None ->
          failwithf
            "Unsupported unit %S. Supported units: %s"
            value
            Unitconv.supported_units
            ())

let command =
  Command.basic
    ~summary:"Convert between mm, cm, m, ft, and in"
    (let%map_open.Command value =
       Command.Param.anon ("value" %: float)
     and from_unit =
       Command.Param.flag
         "from"
         (Command.Param.required unit_arg)
         ~doc:"UNIT Source unit"
     and to_unit =
       Command.Param.flag
         "to"
         (Command.Param.required unit_arg)
         ~doc:"UNIT Target unit" in
     fun () ->
       let result = Unitconv.convert value from_unit to_unit in
       printf "%.6f %s\n" result (Unitconv.unit_to_string to_unit))

let () = Command_unix.run command
