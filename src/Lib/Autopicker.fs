module Lib.Autopicker

// in order of actual runtime order the values go acc => intermediate => domainType
// therefore the type is yield' -> acc -> domainType option
// and the runtime order of the stages is choice => yield'.
// There are options all along the pipeline because choosing could fail to resolve at any stage due to e.g. lack of user input, or because the user picked a different option.
// Only when everything in the pipeline returns a definite Some [results, which could be empty] is the final choice definitely made.
type ComposedChoice<'acc, 'intermediateState, 'domainType> = ('intermediateState option -> 'domainType option) -> 'acc -> 'domainType option

module Choice =
    type Param = { probabilityInPercent: int; key: string list }
        with
        static member create prob = { probabilityInPercent = prob; key = [] }
        static member appendKey newValue old = { old with key = old.key @ [newValue] }

type Compose() =
    let pickOne yield' item : _ option =
        yield' (Some item)
    let pickSome yield' (items: _ list) =
        items |> List.map (Some >> yield')

    // choose a value directly (or don't and fail, if the user doesn't select it)
    member _.a v : ComposedChoice<_,_,_> = fun yield' acc -> pickOne yield' v
    // choose directly among values, not among choices
    member this.oneValue label (options: _ list) : ComposedChoice<_,_,_>  = fun yield' acc ->
        let acc = acc |> Choice.Param.appendKey label
        let chooseOne (choices : _ list) yield' acc =
            let rec recur = function
            | value::rest ->
                match pickOne yield' value with
                | Some v -> Some v
                | None -> recur rest
            | [] -> None
            choices |> recur
        chooseOne options yield' (acc |> Choice.Param.appendKey label)
        //this.oneOf label (options |> List.map this.a)
    // choose among choices
    member _.oneOf label (options: ComposedChoice<_,_,_> list) : ComposedChoice<_,_,_> = fun yield' acc ->
        let acc = acc |> Choice.Param.appendKey label
        let chooseOne (choices : ComposedChoice<_,_,_> list) : ComposedChoice<_,_,_> = fun yield' acc ->
            let rec recur = function
            | choice::rest ->
                let yield0 = function
                    | None -> None
                    | Some intermediate -> (yield' (Some intermediate))
                match choice yield0 acc with
                | Some v -> Some v
                | None -> recur rest
            | [] -> None
            choices |> recur
        chooseOne options yield' (acc |> Choice.Param.appendKey label)
    // change a choice yielding a single value into a choice yielding zero or more values (suitable for aggregation)
    member _.uplift (choice : ComposedChoice<_,_,'domainType>) : ComposedChoice<_,_,'domainType list> = fun yield' acc ->
        match choice id acc with
        | Some v -> (yield' (Some [v]))
        | None -> None
    // a choice that returns the sum of everything its subchoices return
    member _.aggregate (options: ComposedChoice<_,_,_> list) : ComposedChoice<_,_,'domainType list> = fun yield' acc ->
        let mutable allSucceed = true
        let chosen = [
            for choice in options do
                match choice yield' acc with
                | Some v -> yield! v
                | None -> allSucceed <- false
            ]
        if allSucceed then chosen |> Some
        else None

    member _.ctor ctor choice: ComposedChoice<'acc,'arg1,'domainType> = fun yield' acc ->
        let yield0 = function
            | None -> None
            | Some intermediate -> (yield' (Some (ctor intermediate)))
        choice yield0 acc
    member _.ctor2 (ctor: _ -> 'constructedType) (choice1: ComposedChoice<'acc,'arg1,_>) (choice2 : ComposedChoice<'acc,'arg2,_>) : ComposedChoice<'acc,'constructedType,'domainType> = fun yield' acc ->
        choice1 (Option.bind (fun arg1 -> choice2 (Option.bind(fun arg2 -> ctor(arg1, arg2) |> pickOne yield')) acc)) acc

let choose = Compose()
