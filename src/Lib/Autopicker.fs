﻿module Lib.Autopicker

// in order of actual runtime order the values go acc => intermediate => domainType
// therefore the type is yield' -> acc -> domainType option
// and the runtime order of the stages is choice => yield'.
// There are options all along the pipeline because choosing could fail to resolve at any stage due to e.g. lack of user input, or because the user picked a different option.
// Only when everything in the pipeline returns a definite Some [results, which could be empty] is the final choice definitely made.
type ComposedChoice<'acc, 'intermediateState, 'domainType> = ('intermediateState option -> 'domainType option) -> 'acc -> 'domainType option

module Choice =
    type Param = { probabilityInPercent: int; key: string list }
        with
        static member yield' prob = { probabilityInPercent = prob; key = [] }
        static member appendKey newValue old = { old with key = old.key @ [newValue] }

type Compose() =
    let pickOne yield' item : _ option =
        yield' (Some item)
    let pickSome yield' (items: _ list) =
        items |> List.map (Some >> yield')

    member _.a v : ComposedChoice<_,_,_> = fun yield' acc -> pickOne yield' v
    member _.oneOf label options : ComposedChoice<_,_,_> = fun yield' acc ->
        let acc = acc |> Choice.Param.appendKey label
        let chooseOne (choices : _ list) yield' acc =
            let rec recur = function
            | choice::rest ->
                match choice id acc with
                | Some v -> pickOne yield' v
                | None -> recur rest
            | [] -> None
            choices |> recur
        chooseOne options yield' (acc |> Choice.Param.appendKey label)
    member _.someOf label (options: ComposedChoice<_,_,_> list) : ComposedChoice<_,_,_> = fun yield' acc ->
        let acc = acc |> Choice.Param.appendKey label
        let mutable abort = false
        let chosen = [
            for choice in options do
                match choice id acc with
                | Some v -> yield v
                | None -> abort <- true
            ]
        if abort then None else Some (pickSome yield' chosen)

    member _.ctor ctor choice: ComposedChoice<'acc,'arg1,'domainType> = fun yield' -> choice (Option.bind (ctor >> pickOne yield'))
    member _.ctor2 (ctor: _ -> 'constructedType) (choice1: ComposedChoice<'acc,'arg1,_>) (choice2 : ComposedChoice<'acc,'arg2,_>) : ComposedChoice<'acc,'constructedType,'domainType> = fun yield' acc ->
        choice1 (Option.bind (fun arg1 -> choice2 (Option.bind(fun arg2 -> ctor(arg1, arg2) |> pickOne yield')) acc)) acc

    member _.randomly (suboptions: ComposedChoice<_,_,_> list) : ComposedChoice<_,_,_> = fun yield' acc ->
        chooseRandom suboptions yield' acc
    member _.some (suboptions: ComposedChoice<_,_,_> list) = fun yield' acc ->
        let mutable allSuccess = true
        let lst =
            [   for ix, choice in suboptions |> List.mapiOneBased tuple2 do
                    let acc = acc |> Choice.Param.appendKey (ix.ToString())
                    match choice id acc with
                    | Some v -> yield v
                    | None -> allSuccess <- false
                ]
        if allSuccess then pickSome yield' lst |> Some else None

let choose = Compose()
