open! Core

let unit_arg =
  Command.Arg_type.create Fn.id

let unit_type_arg =
  Command.Arg_type.create (fun value ->
      match Unitconv.supported_units_for_type value with
      | Some _ -> String.lowercase value
      | None ->
          failwithf
            "Unsupported unit type %S. Supported unit types: %s"
            value
            Unitconv.supported_unit_types
            ())

let command =
  Command.basic
    ~summary:"Convert between units within a selected unit type"
    (let%map_open.Command unit_type =
       Command.Param.anon ("type" %: unit_type_arg)
     and value =
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
       let result, target_unit =
         Unitconv.convert unit_type value ~from_unit ~to_unit
       in
       printf "%.6f %s\n" result target_unit)

let () = Command_unix.run command
