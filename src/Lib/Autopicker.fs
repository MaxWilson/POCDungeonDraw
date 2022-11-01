module Lib.Autopicker

// in order of actual runtime order the values go acc => intermediate => domainType
// therefore the type is create -> acc -> domainType option
// and the runtime order of the stages is choice => create.
// There are options all along the pipeline because choosing could fail to resolve at any stage due to e.g. lack of user input, or because the user picked a different option.
// Only when everything in the pipeline returns a definite Some [results, which could be empty] is the final choice definitely made.
type ComposedChoice<'acc, 'intermediateState, 'domainType> = ('intermediateState option -> 'domainType option) -> 'acc -> 'domainType option

module Choice =
    type Param = { probabilityInPercent: int; key: string list }
        with
        static member create prob = { probabilityInPercent = prob; key = [] }
        static member appendKey newValue old = { old with key = old.key @ [newValue] }

type Compose() =
    let pickOne create item : _ option =
        create (Some item)
    let pickSome create (items: _ list) =
        items |> List.map (Some >> create)
    member _.from label (suboptions: ComposedChoice<_,_,_> list) : ComposedChoice<_,_,_> = fun create acc ->
        let acc = acc |> Choice.Param.appendKey label
        (chooseRandom suboptions) |> (pickOne create)
    member _.a v : ComposedChoice<_,_,_> = fun create acc -> pickOne create v
    member _.oneOf label options : ComposedChoice<_,_,_> = fun create acc ->
        let chooseOne (choices : _ list) create acc =
            choices |> chooseRandom |> pickOne create
        chooseOne options create (acc |> Choice.Param.appendKey label)
    member _.ctor2 (ctor: _ -> 'constructedType) (choice1: ComposedChoice<'acc,'arg1,_>) (choice2 : ComposedChoice<'acc,'arg2,_>) : ComposedChoice<'acc,'constructedType,'domainType> = fun create acc ->
        choice1 (Option.bind (fun arg1 -> ctor(1, 2) |> pickOne create)) acc

    member _.ctor choice ctor : ComposedChoice<_,_,_> = fun create -> choice (Option.bind (ctor >> create))
let choose = Compose()
