[<AutoOpen>]
module Common

let thunk v _ = v
let thunk1 f x _ = f x
let thunk2 f x y = f x y
