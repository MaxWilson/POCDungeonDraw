module Lib.Autopicker

type ComposedChoice<'acc, 'intermediateState, 'r> = ('intermediateState -> 'r) -> 'acc -> 'r
module Choice =
    type Param = { probabilityInPercent: int; key: string list }
        with
        static member create prob = { probabilityInPercent = prob; key = [] }
        static member appendKey newValue old = { old with key = old.key @ [newValue] }

type Compose() =
    let pickOne k item =
        Some item |> k
    let pickSome k (items: _ list) =
        Some items |> k
    member _.from label (suboptions: ComposedChoice<_,_,_> list) : ComposedChoice<_,_,_> = fun gen acc ->
        let acc = acc |> Choice.Param.appendKey label
        (chooseRandom suboptions) |> (pickOne gen)
    member _.a v : ComposedChoice<_,_,_> = fun gen acc -> gen [v]
    member _.oneOf label options : ComposedChoice<_,_,_> = fun gen acc ->
        let chooseOne (choices : _ list) gen acc =
            choices |> chooseRandom |> pickOne gen
        chooseOne options gen (acc |> Choice.Param.appendKey label)
    member _.ctor2 (ctor: _ -> 'ctor) (choice1: ComposedChoice<'acc,'arg1,_>) (choice2 : ComposedChoice<'acc,'arg2,_>) : ComposedChoice<'acc,'ctor option,'r> = fun gen acc ->
        if rand.Next 10 < 3 then
            ctor(1,2) |> (pickOne gen)
        elif rand.Next 10 < 3 then
            choice1 (fun arg1 -> ctor(1, 2) |> pickOne gen)
        else
            let gen2 arg1 arg2 =
                match arg2 with
                | Some arg2 -> ctor(arg1, arg2) |> pickOne gen
                | None -> None |> gen
            let gen1 = function
                | Some arg1 -> choice2 (function arg2 -> gen2 arg1 arg2)
                | None -> fun acc -> None |> gen
            choice1 gen1

    member _.ctor choice ctor : ComposedChoice<_,_,_> = fun gen -> choice (Option.map ctor >> k)
let compose = Compose()
