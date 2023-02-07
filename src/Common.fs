[<AutoOpen>]
module Common

let thunk v _ = v
let thunk1 f x _ = f x
let thunk2 f x y = f x y
let notImpl msg = failwith $"Not implemented! Talk to Max if you want this feature. {msg}"
let shouldntHappen data =
    System.Console.WriteLine(data :> obj)
    failwith $"This shouldn't happen. There must be a bug. {data}"
let inline breakHere() = System.Diagnostics.Debugger.Break()
let tuple2 x y = x,y
let flip f x y = f y x
let rand = System.Random()
let chooseRandom (lst: _ list) =
    lst[rand.Next lst.Length]
module String =
    let join (sep:string) (input: string seq) = System.String.Join(sep, input)
module List =
    let mapiOneBased f lst = lst |> List.mapi (fun ix -> f (ix + 1))
    let create v = [v]

let inline trace label x =
#if DEBUG
    printfn $"Trace/{label}: {x}"
#endif
    x