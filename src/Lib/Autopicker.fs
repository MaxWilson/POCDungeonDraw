module Lib.Autopicker

module Choice =
    type 't Param = { recognizer: string -> bool; key: string }
        with
        member this.recognized() = this.recognizer this.key
        static member create (chosenOptions: string list) = { key = ""; recognizer = fun key -> chosenOptions |> List.exists ((=) key) }
        static member create prob = { key = ""; recognizer = fun _ -> rand.Next 100 <= prob }
        member old.appendKey newValue = { old with key = if old.key.Length = 0 then newValue else old.key + "-" + newValue }

// in order of actual runtime order the values go acc => intermediate => domainType
// therefore the type is yield' -> acc -> domainType option
// and the runtime order of the stages is choice => yield'.
// There are options all along the pipeline because choosing could fail to resolve at any stage due to e.g. lack of user input, or because the user picked a different option.
// Only when everything in the pipeline returns a definite Some [results, which could be empty] is the final choice definitely made.
type ComposedChoice<'acc, 'intermediateState, 'domainType> = ('intermediateState option -> 'domainType option) -> 'acc Choice.Param -> 'domainType option

type Compose() =
    let pickOne yield' (acc: _ Choice.Param) item : _ option =
        if acc.recognized() then
            yield' (Some item)
        else None
    let pickSome yield' (items: _ list) =
        items |> List.map (Some >> yield')

    // choose a value directly (or don't and fail, if the user doesn't select it)
    member _.a v : ComposedChoice<_,_,_> = fun yield' acc -> pickOne yield' (acc.appendKey (v.ToString())) v
    // choose directly among values, not among choices
    member this.oneValue label (options: _ list) : ComposedChoice<_,_,_>  = fun yield' acc ->
        let acc = acc.appendKey label
        let chooseOne (choices : _ list) yield' =
            let rec recur = function
            | value::rest ->
                let acc = acc.appendKey (value.ToString())
                match pickOne yield' acc value with
                | Some v -> Some v
                | None -> recur rest
            | [] -> None
            choices |> recur
        chooseOne options yield'
        //this.oneOf label (options |> List.map this.a)
    // choose among choices
    member _.oneOf label (options: ComposedChoice<_,_,_> list) : ComposedChoice<_,_,_> = fun yield' acc ->
        let acc = acc.appendKey label
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
        chooseOne options yield' acc
    // change a choice yielding a single value into a choice yielding zero or more values (suitable for aggregation) or failure
    member _.mandatory (choice : ComposedChoice<_,_,'domainType>) : ComposedChoice<_,_,'domainType list> = fun yield' acc ->
        match choice id acc with
        | Some v -> (yield' (Some [v]))
        | None -> None
    // change a choice yielding a single value into a choice yielding zero or more values (suitable for aggregation)
    member _.optional (choice : ComposedChoice<_,_,'domainType>) : ComposedChoice<_,_,'domainType list> = fun yield' acc ->
        match choice id acc with
        | Some v -> (yield' (Some [v]))
        | None -> Some []
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
        choice1 (Option.bind (fun arg1 -> choice2 (Option.bind(fun arg2 -> ctor(arg1, arg2) |> pickOne yield' acc)) acc)) acc

let choose = Compose()
