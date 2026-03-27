open! Core

type packed = T : (module Unit_intf.S with type t = 'a) -> packed

let registered_modules =
  [ T (module Length_unit); T (module Mass_unit) ]

let registry =
  lazy
    (let table = String.Table.create () in
     List.iter registered_modules ~f:(fun (T (module Unit)) ->
         Hashtbl.add_exn
           table
           ~key:(String.lowercase Unit.name)
           ~data:(T (module Unit)));
     table)

let find unit_type =
  Hashtbl.find (Lazy.force registry) (String.lowercase unit_type)

let supported_unit_types () =
  Hashtbl.keys (Lazy.force registry) |> List.sort ~compare:String.compare
