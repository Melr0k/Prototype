open Main

let testdir = "tests/"

let file_by_lang filename =
  if String.ends_with ~suffix:".py" filename
  then `Python (`File filename)
  else `File filename

let ml_test = testdir ^ "test1.ml"
let () =
  let fn = ref ml_test in
  if Array.length Sys.argv > 1 then fn := Sys.argv.(1) ;
  Main_proto.main (file_by_lang !fn)
