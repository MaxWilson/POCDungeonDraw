[<AutoOpen>]
module Common

let thunk v _ = v
let thunk1 f x _ = f x
let thunk2 f x y = f x y
let notImpl msg = failwith $"Not implemented! Talk to Max if you want this feature. {msg}"

let inline trace label x =
#if DEBUG
    printfn $"Trace/{label}: {x}"
#endif
    x