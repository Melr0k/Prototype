let my_ref = ref 5

let inside = !my_ref

let _ = (my_ref := true)

let f1 = fun x -> succ x

let f2 = fun x -> (x + x)

let f3 = fun x -> !x

let f4 = fun (x:Ref) -> x
