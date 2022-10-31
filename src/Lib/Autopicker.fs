module Lib.Autopicker

type ComposedChoice<'acc, 'intermediateState, 'r> = ('intermediateState -> 'r) -> 'acc -> 'r
module Choice =
    
    type Param = { probabilityInPercent: int; key: string list }
        with
        static member create prob = { probabilityInPercent = prob; key = [] }
        static member appendKey newValue old = { old with key = old.key @ [newValue] }

type Compose() =
    member _.from label (suboptions: ComposedChoice<_,_,_> list) : ComposedChoice<_,_,_> = fun k acc ->
        let acc = acc |> Choice.Param.appendKey label
        chooseRandom suboptions k acc
    member _.a v : ComposedChoice<_,_,_> = fun k acc -> k [v]
    member _.oneOf label options : ComposedChoice<_,_,_> = fun k acc -> 
        let chooseOne (choices : _ list) k acc =
            [choices |> chooseRandom] |> k
        chooseOne options k (acc |> Choice.Param.appendKey label)
    member _.ctor2 ctor (choice1: ComposedChoice<_,_,_>) (choice2 : ComposedChoice<_,_,_>) : ComposedChoice<_,_,_> = fun k acc ->
        choice1 (Option.map (fun arg1 -> choice2 (function arg2 -> ctor(arg1, arg2)) acc) >> k) acc
    member _.ctor choice ctor : ComposedChoice<_,_,_> = fun k -> choice (Option.map ctor >> k)
let compose = Compose()
