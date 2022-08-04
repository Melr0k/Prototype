open Main

let testdir = "tests/"

(* * )
let ml_test = testdir ^ "test1.ml"
let () =
  let fn = ref ml_test in
  if Array.length Sys.argv > 1 then fn := Sys.argv.(1) ;
  Main_proto.main (`File !fn)
( * *)

let py_test = testdir ^ "test1.py"
let () =
  let fn = ref py_test in
  if Array.length Sys.argv > 1 then fn := Sys.argv.(1) ;
  Main_py.main (`Py_file !fn)
(* *)
