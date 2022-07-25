(*
let () =
  let fn = ref "test.ml" in
  if Array.length Sys.argv > 1 then fn := Sys.argv.(1) ;
  Main_proto.main (`File !fn)
 *)

let () =
  let fn = ref "test.py" in
  if Array.length Sys.argv > 1 then fn := Sys.argv.(1) ;
  Main_proto.main_py (`Py_file !fn)
