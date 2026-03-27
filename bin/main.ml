open! Core

let unit_arg =
  Command.Arg_type.create Fn.id

let command =
  Command.basic_or_error
    ~summary:"Convert between units within a selected unit type"
    (let%map_open.Command unit_type =
       Command.Param.anon ("type" %: string)
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
       let%map.Or_error result, target_unit =
         Unitconv.convert (String.lowercase unit_type) value ~from_unit ~to_unit
       in
       printf "%.6f %s\n" result target_unit)

let () = Command_unix.run command
