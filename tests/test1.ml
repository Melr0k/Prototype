let a = ref 5

let b = fun x -> ref x

let c = !a

let d = a := true

let e = if a is Ref then 1 else "ha"

let err = !a := false

let aa = ref a

let dederef = !(!aa)

let f = fun x -> !x

let g = fun x -> if !x is Int then 2 else "he"

let h = fun x -> if !x is Ref then 3 else "hi"

let i = fun x -> !(f x)

let j = if a is Any then 4 else "ho"

let k = if !a is Any then 5 else "hu"

let l = if !a is Ref then !a else "han"
