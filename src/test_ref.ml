let my_ref = ref 5

let inside = !my_ref

let _ = (my_ref := true)

let f1 = fun x -> succ x

let f2 = fun x -> (x + x)

let f3 = fun x -> !x

let f4 = fun (x:Ref) -> x

let f5 = fun f -> if (f 5) is Int then (f 5) else 2
(* no_se : ((5 -> Int & 'a) -> Int & 'a) & ((5 -> 'b) -> 2 | Int & 'b)
   se    : ((5 -> 'a) -> 2 | 'a) & ((5 -> Int & 'b) -> Int & 'b) &
           ((5 -> Any \ Int) -> 2) *)
