module Lib.Autopicker

module Choice =
    type SelectionState = { isSelected: bool; key: string }
    type Selection =
        | Grant of description: string
        | Option of description: string * state: SelectionState
        | Submenu of description: string * state: SelectionState * items: Selection list
        | Decorator of description: string * inner: Selection
    type Param = { recognizer: string -> bool; key: string }
        with
        member this.recognized() = this.recognizer this.key
        member this.recognize f : 'result option = if this.recognized() then f() else None
        static member create (chosenOptions: string list) = { key = ""; recognizer = fun key -> chosenOptions |> List.exists (fun k -> k.StartsWith key) }
        static member create prob = { key = ""; recognizer = fun _ -> rand.Next 100 <= prob }
        member old.appendKey newValue = match newValue with None -> old | Some newValue -> { old with key = if old.key.Length = 0 then newValue else old.key + "-" + newValue }
        member p.state = { isSelected = p.recognized(); key = p.key }
    type Choice<'domainType> = // it's okay for childtype to go unused, e.g. by Grant, but it constrains the allowed children
        abstract member getMenus: Param -> Selection
        abstract member getValues: Param -> 'domainType option
    type Grant<'domainType>(key:string option, value) =
        interface Choice<'domainType> with
            member this.getMenus param = Selection.Grant (value.ToString())
            member this.getValues param = Some value
    type Allow<'domainType>(key:string option, value) =
        let key = key |> function None -> value.ToString() |> Some | _ -> key
        interface Choice<'domainType> with
            member this.getMenus param = Selection.Option(value.ToString(), param.state)
            member this.getValues param = if (param.appendKey key).recognized() then Some value else None
    type OneTransform<'childType, 'domainType>(key:string option, child: Choice<'childType>, adapter: 'childType option -> 'domainType option) =
        interface Choice<'domainType> with
            member this.getMenus param =
                let param = param.appendKey key
                child.getMenus param
            member this.getValues param =
                let param = param.appendKey key
                if param.recognized() then
                    child.getValues param |> adapter
                else None
    type ChoiceCtor2<'child1Type, 'child2Type, 'domainType>(label, adapter, key:string option, child1: Choice<'child1Type>, child2: Choice<'child2Type>) =
        interface Choice<'domainType> with
            member this.getMenus param =
                let param = param.appendKey key
                if param.recognized() then // don't prompt for dependent choices unless they're relevant/needed
                    Submenu(label, param.state, [child1.getMenus param; child2.getMenus param])
                else Option(label, param.state)
            member this.getValues param =
                let param = param.appendKey key
                if param.recognized() then
                    match child1.getValues param, child2.getValues param with
                    | Some v1, Some v2 -> adapter(v1, v2) |> Some
                    | _ -> None
                else None
    type ChoiceCtor<'childType, 'domainType>(label, adapter, key:string option, children: Choice<'childType> list, adapt) =
        interface Choice<'domainType> with
            member this.getMenus param =
                let param = param.appendKey key
                if param.recognized() then // don't prompt for dependent choices unless they're relevant/needed
                    Submenu(label, param.state, children |> List.map (fun c -> c.getMenus param))
                else Option(label, param.state)
            member this.getValues param =
                let param = param.appendKey key
                if param.recognized() then
                    let rec recur = function
                        | [] -> None
                        | (h: 'childType Choice)::t ->
                            match h.getValues param with
                            | Some v -> adapt v |> Some
                            | None ->
                                recur t
                    recur children
                else None
    type SomeChoices<'childType, 'domainType>(key:string option, children: Choice<'childType> list, adapter: 'childType option -> 'domainType list option) =
        interface Choice<'domainType list> with
            member this.getMenus param =
                children |> List.collect (fun child -> child.getMenus(param.appendKey key))
            member this.getValues param : 'domainType list option =
                if param.recognized() then
                    let param = param.appendKey key
                    let rec recur soFar = function
                        | [] -> soFar
                        | (h: 'childType Choice)::t ->
                            match h.getValues param |> adapter with
                            | Some vs -> recur (soFar@vs) t
                            | None -> recur soFar t
                    recur [] children |> Some
                else None
    type ConditionalChoice<'childType, 'domainType when 'childType: equality>(key: string option, child: 'childType Choice, childAdapter, conditionals: ('childType * 'domainType list Choice) list) =
        interface Choice<'domainType list> with
            member this.getMenus param =
                if param.recognized() then
                    let param = param.appendKey key
                    child.getMenus param @ (notImpl())
                else []
            member this.getValues param =
                let param = param.appendKey key
                if param.recognized() then
                    match child.getValues param with
                    | None -> None
                    | Some v ->
                        [   yield childAdapter v
                            for v1, child in conditionals do
                                if v1 = v then
                                    match child.getValues param with
                                    | Some v -> yield! v
                                    | None -> ()
                            ] |> Some
                else None
    type ChooseUntil<'childType, 'domainType>(key:string option, children: Choice<'childType> list, adapter: 'childType option -> 'domainType list option, validator) =
        interface Choice<'domainType list> with
            member this.getMenus param =
                children |> List.collect (fun child -> child.getMenus(param.appendKey key))
            member this.getValues param : 'domainType list option =
                if param.recognized() then
                    let param = param.appendKey key
                    let rec recur soFar = function
                        | [] -> soFar
                        | (h: 'childType Choice)::t ->
                            match h.getValues param |> adapter with
                            | Some vs when validator(soFar@vs) -> recur (soFar@vs) t
                            | _ -> recur soFar t
                    recur [] children |> Some
                else None
    type ChooseAtLeastUntil2D<'childType, 'domainType>(key:string option, children: Choice<'childType> list list, adapter: 'childType option -> 'domainType option, validator) =
        interface Choice<'domainType list> with
            member this.getMenus param =
                children |> List.collect (List.collect (fun child -> child.getMenus(param.appendKey key)))
            member this.getValues param : 'domainType list option =
                if param.recognized() then
                    let param = param.appendKey key
                    let mutable isValid = true
                    let mutable results = []
                    let rec recur soFar = function
                        | [] -> soFar
                        | (h: 'childType Choice)::t ->
                            match h.getValues param |> adapter with
                            | Some vs ->
                                if validator(results, soFar@[vs]) |> not then isValid <- false
                                recur (soFar@[vs]) t
                            | _ -> recur soFar t
                    for children in children do
                        results <- recur [] children::results
                    if isValid then
                        Some (results |> List.rev |> List.collect id)
                    else None
                else None

open Choice

type Compose() =
    // choose a value directly (or don't and fail, if the user doesn't select it)
    member _.grant v : _ Choice = Grant(None, v)
    member _.a v : _ Choice = Allow(None, v)
    // labelled overload of choose.a
    member _.a(label, v) : _ Choice = Allow(Some label, v)
    // choose directly among values, not among choices
    member this.oneValue (options: _ list) : _ Choice = ChoiceCtor(None, options |> List.map (fun v -> Allow(None, v)), id)
    // choose directly among values, not among choices
    member this.oneValue (adapt, options: _ list) : _ Choice = ChoiceCtor(None, options |> List.map (fun v -> Allow(None, v)), adapt)
    // choose directly among values, not among choices
    member this.oneValueWith (label, options: _ list) : _ Choice = ChoiceCtor(Some label, options |> List.map (fun v -> Allow(None, v)), id)
    // choose directly among values, not among choices
    member this.oneValueWith (label, adapter, options: _ list) : _ Choice = ChoiceCtor(Some label, options |> List.map (fun v -> Allow(None, v)), adapter)
    // choose among choices
    member _.oneOf (options: _ Choice list) : _ Choice = ChoiceCtor(None, options, id)
    // choose among choices
    member _.oneOfWith (label:string) (options: _ Choice list) : _ Choice = ChoiceCtor(Some label, options, id)
    // change a choice yielding a single value into a choice yielding zero or more values (suitable for aggregation) or failure
    member _.mandatory (choice : _ Choice) : _ Choice =
        OneTransform(None, choice,
            function
            | Some v -> (Some [v])
            | None -> None)
        :> _ Choice
    // change a choice yielding a single value into a choice yielding zero or more values (suitable for aggregation)
    member _.optional (choice : _ Choice) : _ Choice =
        OneTransform(None, choice, function
            | Some v -> (Some [v])
            | None -> Some []
            )
        :> _ Choice
    // change a choice yielding a single value into a choice yielding zero or more values (suitable for aggregation)
    member _.optionalWith (label:string) (choice : _ Choice) : _ Choice =
        OneTransform(Some label, choice, function
            | Some v -> (Some [v])
            | None -> Some []
            )
        :> _ Choice
    // a choice that returns the sum of everything its subchoices return
    member _.aggregate (options: _ Choice list) : _ Choice =
        SomeChoices(None, options, id)
    // a choice that returns the sum of everything its subchoices return
    member _.aggregateWith label (options: _ Choice list) : _ Choice =
        SomeChoices(Some label, options, id)
    member _.ctor ctor choice: _ Choice =
        ChoiceCtor(None, choice, ctor)
    member _.ctorWith (label:string) ctor choice: _ Choice =
        ChoiceCtor(Some label, choice, ctor)
    member _.ctor2 (ctor: _ -> 'constructedType) choice1 choice2 : _ Choice =
        ChoiceCtor2(None, choice1, choice2, ctor)
    member _.ctor2With label (ctor: _ -> 'constructedType) choice1 choice2 : _ Choice =
        ChoiceCtor2(Some label, choice1, choice2, ctor)
    member _.conditional expression childAdapter switches : _ Choice = ConditionalChoice(None, expression, childAdapter, switches)
    member _.until validator choices : _ Choice = ChooseUntil(None, choices, id, validator)
    member this.upToBudget (budget:int) costFunc choices : _ Choice =
        ChooseUntil(None, choices |> List.map this.optional, id, fun choices -> (choices |> List.sumBy costFunc) <= budget)
    member this.upToBudget2 (budget:int) costFunc listChoices choices : _ Choice =
        ChooseUntil(None, listChoices @ (choices |> List.map this.optional), id, fun choices -> (choices |> List.sumBy costFunc) <= budget)
    member _.disadvantagesWithPrioritizedBudget costFunc (budgetsAndChoices: (int * _ Choice list) list) : _ Choice =
        let choices = budgetsAndChoices |> List.map snd
        let budgets = budgetsAndChoices |> List.map fst
        let jointCost (priorChoices: _ list, choices) =
            let budget = budgets |> List.take (priorChoices.Length + 1) |> List.sum
            let priorCost = (priorChoices |> List.sumBy (List.sumBy costFunc))
            let cost = (choices |> List.sumBy costFunc)
            -(priorCost + cost) >= -budget // We want to ask the user to keep choosing at least until the disadvantage budget requirement is satisfied.
        ChooseAtLeastUntil2D(None, choices, id, jointCost)

let choose = Compose()
