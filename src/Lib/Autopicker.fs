module Lib.Autopicker

module Choice =
    type MenuItem = { text: string; key: string; isCurrentlySelected: bool; submenu: Menu option }
    and Menu = {
        header: string
        items: MenuItem list
        }
        with
        static member placeholder() = { header = "placeholder"; items = [] }
        static member placeholder (submenus: Menu list) = Menu.placeholder()
    type Param = { recognizer: string -> bool; key: string }
        with
        member this.recognized() = this.recognizer this.key
        member this.recognize f : 'result option = if this.recognized() then f() else None
        static member create (chosenOptions: string list) = { key = ""; recognizer = fun key -> chosenOptions |> List.exists (fun k -> k.StartsWith key) }
        static member create prob = { key = ""; recognizer = fun _ -> rand.Next 100 <= prob }
        member old.appendKey newValue = match newValue with None -> old | Some newValue -> { old with key = if old.key.Length = 0 then newValue else old.key + "-" + newValue }
    type Choice<'domainType> = // it's okay for childtype to go unused, e.g. by Grant, but it constrains the allowed children
        abstract member getMenus: Param -> Menu
        abstract member getValues: Param -> 'domainType option
    type Grant<'domainType>(key:string option, value) =
        interface Choice<'domainType> with
            member this.getMenus param = Menu.placeholder()
            member this.getValues param = Some value
    type Allow<'domainType>(key:string option, value) =
        interface Choice<'domainType> with
            member this.getMenus param = Menu.placeholder()
            member this.getValues param = if (param.appendKey (key |> function None -> value.ToString() |> Some | _ -> key)).recognized() then Some value else None
    type OneTransform<'childType, 'domainType>(key:string option, child: Choice<'childType>, adapter: 'childType option -> 'domainType option) =
        interface Choice<'domainType> with
            member this.getMenus param = Menu.placeholder()
            member this.getValues param =
                if param.recognized() then
                    let param = param.appendKey key
                    child.getValues param |> adapter
                else None
    type ChoiceCtor2<'child1Type, 'child2Type, 'domainType>(key:string option, child1: Choice<'child1Type>, child2: Choice<'child2Type>, adapter) =
        interface Choice<'domainType> with
            member this.getMenus param = Menu.placeholder()
            member this.getValues param =
                if param.recognized() then
                    let param = param.appendKey key
                    match child1.getValues param, child2.getValues param with
                    | Some v1, Some v2 -> adapter(v1, v2) |> Some
                    | _ -> None
                else None
    type ChoiceCtor<'childType, 'domainType>(key:string option, children: Choice<'childType> list, adapt) =
        interface Choice<'domainType> with
            member this.getMenus param = Menu.placeholder()
            member this.getValues param =
                if param.recognized() then
                    let param = param.appendKey key
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
            member this.getMenus param = Menu.placeholder()
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

open Choice
type ComposedChoice<'domainType> = Choice<'domainType>

type Compose() =
    let pickOne yield' (acc: Choice.Param) item : _ option * Choice.Menu =
        (if acc.recognized() then
            yield' (Some item)
        else None), Choice.Menu.placeholder()
    let pickSome yield' (items: _ list) =
        (items |> List.map (Some >> yield')), Choice.Menu.placeholder()

    // choose a value directly (or don't and fail, if the user doesn't select it)
    member _.grant v : ComposedChoice<_> = Grant(None, v)
    member _.a v : ComposedChoice<_> = Allow(None, v)
    // labelled overload of choose.a
    member _.a(label, v) = Allow(Some label, v)
    // choose directly among values, not among choices
    member this.oneValue (options: _ list) = ChoiceCtor(None, options |> List.map (fun v -> Allow(None, v)), id)
    // choose directly among values, not among choices
    member this.oneValue (adapt, options: _ list) = ChoiceCtor(None, options |> List.map (fun v -> Allow(None, v)), adapt)
    // choose directly among values, not among choices
    member this.oneValueWith (label, options: _ list) = ChoiceCtor(Some label, options |> List.map (fun v -> Allow(None, v)), id)
    // choose directly among values, not among choices
    member this.oneValueWith (label, adapter, options: _ list) = ChoiceCtor(Some label, options |> List.map (fun v -> Allow(None, v)), adapter)
    // choose among choices
    member _.oneOf (options: ComposedChoice<_> list) = ChoiceCtor(None, options, id)
    // choose among choices
    member _.oneOfWith (label:string) (options: ComposedChoice<_> list) = ChoiceCtor(Some label, options, id)
    // change a choice yielding a single value into a choice yielding zero or more values (suitable for aggregation) or failure
    member _.mandatory (choice : ComposedChoice<'domainType>) =
        OneTransform(None, choice,
            function
            | Some v -> (Some [v])
            | None -> None)
        :> _ Choice
    // change a choice yielding a single value into a choice yielding zero or more values (suitable for aggregation)
    member _.optional (choice : ComposedChoice<'domainType>) : ComposedChoice<'domainType list> =
        OneTransform(None, choice, function
            | Some v -> (Some [v])
            | None -> Some []
            )
        :> _ Choice
    // change a choice yielding a single value into a choice yielding zero or more values (suitable for aggregation)
    member _.optionalWith (label:string) (choice : ComposedChoice<'domainType>) : ComposedChoice<'domainType list> =
        OneTransform(Some label, choice, function
            | Some v -> (Some [v])
            | None -> Some []
            )
        :> _ Choice
    // a choice that returns the sum of everything its subchoices return
    member _.aggregate (options: ComposedChoice<_> list) : ComposedChoice<'domainType list> =
        SomeChoices(None, options, id)
    member _.ctor ctor choice: ComposedChoice<'domainType> =
        ChoiceCtor(None, choice, ctor)
    member _.ctorWith (label:string) ctor choice: ComposedChoice<'domainType> =
        ChoiceCtor(Some label, choice, ctor)
    member _.ctor2 (ctor: _ -> 'constructedType) choice1 choice2 =
        ChoiceCtor2(None, choice1, choice2, ctor)
    member _.ctor2With label (ctor: _ -> 'constructedType) choice1 choice2 =
        ChoiceCtor2(Some label, choice1, choice2, ctor)

let choose = Compose()
