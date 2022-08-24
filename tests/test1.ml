let a = ref 5

let b = fun x -> ref x

let c = !a

let u = a := true

let d = if a is Ref then 5 else "hi"
