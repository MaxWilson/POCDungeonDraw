module Lib.Autopicker

// create -> acc -> domainType option
type ComposedChoice<'acc, 'intermediateState, 'domainType> = ('intermediateState -> 'domainType option) -> 'acc -> 'domainType option

module Choice =
    type Param = { probabilityInPercent: int; key: string list }
        with
        static member create prob = { probabilityInPercent = prob; key = [] }
        static member appendKey newValue old = { old with key = old.key @ [newValue] }

type Compose() =
    let pickOne create item : _ option =
        create item
    let pickSome create (items: _ list) =
        items |> List.map (Option.bind create)
    member _.from label (suboptions: ComposedChoice<_,_,_> list) : ComposedChoice<_,_,_> = fun create acc ->
        let acc = acc |> Choice.Param.appendKey label
        (chooseRandom suboptions) |> Some |> (pickOne create)
    member _.a v : ComposedChoice<_,_,_> = fun create acc -> pickOne create v
    member _.oneOf label options : ComposedChoice<_,_,_> = fun create acc ->
        let chooseOne (choices : _ list) create acc =
            choices |> chooseRandom |> pickOne create
        chooseOne options create (acc |> Choice.Param.appendKey label)
    member _.ctor2 (ctor: _ -> 'ctor) (choice1: ComposedChoice<'acc,'arg1,_>) (choice2 : ComposedChoice<'acc,'arg2,_>) : ComposedChoice<'acc,'ctor,'r> = fun create acc ->
        if rand.Next 10 < 3 then
            ctor(1,2) |> (pickOne create)
        elif rand.Next 10 < 3 then
            let choicex = choice1
            let choice1 = notImpl()
            choice1 (Option.bind (fun arg1 -> ctor(1, 2) |> pickOne create))
        else
            let create2 arg1 arg2 =
                match arg2 with
                | Some arg2 -> ctor(arg1, arg2) |> pickOne create
                | None -> None
            let create1 = function
                | Some arg1 -> choice2 (function arg2 -> create2 arg1 arg2) acc
                | None -> None
            choice1 create1

    member _.ctor choice ctor : ComposedChoice<_,_,_> = fun create -> choice (Option.map ctor >> k)
let compose = Compose()
