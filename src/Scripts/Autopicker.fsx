#I __SOURCE_DIRECTORY__
#I ".."
#load "Common.fs"
#load "Lib\Autopicker.fs"
open Lib.Autopicker
open Lib.Autopicker.Choice
#load "DungeonFantasy\Chargen.fs"
open DungeonFantasy.Chargen

let sometimes (choice: _ Choice) = choice.getValues (Param.create 25)
let trace (acc:Param) =
    let trace key =
        let v = acc.recognizer key
        if key <> "" then
            printfn $"{key}: {v}"
        v
    { acc with recognizer = trace }
let pick (indices: string list) (choice: _ Choice) = choice.getValues(Param.create indices |> trace)
let menu (indices: string list) (choice: _ Choice) =
    let rec prettyprint indentLevel (menuItems:MenuItem list) =
        for item in menuItems do
            let indent = String.replicate indentLevel " "
            let selectionBullet = if item.isCurrentlySelected then "*" else " "
            match item.submenu with
            | Some sub ->
                printfn $"{indent}{selectionBullet}{item.text}:"
                prettyprint (indentLevel+1) sub.items
            | None -> printfn $"{indent}{selectionBullet}{item.text}"
    choice.getMenus(Param.create indices |> trace)
    |> prettyprint 0
weaponMaster id |> menu [
    ]

let traits() =
    choose.aggregate [
        choose.mandatory (choose.oneValueWith("Profession", Profession, Enumerate.Professions))
        choose.optional (choose.oneValueWith("Adv", Advantage, [DangerSense; PeripheralVision; HeroicArcher; Magery 6]))
        choose.aggregate [
            for a in Enumerate.PrimaryAbilities do
                (choose.optional (choose.a (Increase(a,1))))
            for adv in [DangerSense; PeripheralVision; HeroicArcher] do
                (choose.optional (choose.a (Advantage adv)))
            choose.optionalWith "WeaponMaster" (weaponMaster Advantage)
            choose.optionalWith "Magery" (choose.oneOf [
                for m in 0..6 do
                    yield (choose.a (m.ToString(), Advantage (Magery m)))
                ])
            ]
        ]

sometimes (traits())
traits() |> pick [
    "Profession-Swashbuckler"
    "WeaponMaster-Single-Rapier"
    "Magery-0"
    ]
traits() |> menu [
    "Profession-Swashbuckler"
    "WeaponMaster-Single-Rapier"
    "Magery-0"
    ]


printfn "~~~~~~~~~~~~~~"

traits() |> pick [
    "Profession-Swashbuckler"
    "Increase ST"
    "Increase DX"
    "Magery-5"
    "WeaponMaster-Two-Weapon1-Rapier"
    "WeaponMaster-Two-Weapon2-MainGauche"
    ]

traits() |> pick ["WeaponMaster-Single-Rapier"]

module QueryStore =
    type QueryStore<'t when 't:comparison> =
        { keyed: Map<string, 't>; values: Set<'t> }
        with
        member this.check(v) =
            this.values.Contains v
        member this.check(key, predicate) =
            match this.keyed |> Map.tryFind key with
            | Some v -> predicate v
            | None -> false
    let create (values, f) =
        let keyed = [
            for v in values do
                match f v with
                | Some (key:string) -> key, v
                | None -> ()
            ]
        { keyed = keyed |> Map.ofList; values = values |> Set.ofSeq }

let extractWeaponMasterOnly = function WeaponMaster _ -> nameof(WeaponMaster) |> Some | _ -> None
let weaponMasterOf weapon = function
    | WeaponMaster focus ->
        match focus, weapon with
        | All, _ -> true
        | Swords, (Rapier | Longsword) -> true
        | WeaponOfChoice(w), _
        | TwoWeapon(w,_), _
        | TwoWeapon(_, w),_ when w = weapon -> true
        | _ -> false
    | _ -> false

let q args = QueryStore.create(args, extractWeaponMasterOnly).check(nameof(WeaponMaster), weaponMasterOf Rapier)
q [WeaponMaster(TwoWeapon(Rapier,Shield)); DangerSense] = true
q [WeaponMaster(TwoWeapon(Shield,Rapier)); DangerSense] = true
q [WeaponMaster(All); DangerSense] = true
q [WeaponMaster(Swords); DangerSense] = true
q [WeaponMaster(WeaponOfChoice Shield); DangerSense] = false
q [DangerSense] = false
q [] = false
q [WeaponMaster(WeaponOfChoice Rapier)] = true

type AverageOps<'T when 'T: (static member (+): 'T * 'T -> 'T)
    and  'T: (static member DivideByInt : 'T*int -> 'T)
    and  'T: (static member Zero : 'T)> = 'T
let inline average<'T when AverageOps<'T>>(xs: 'T array) =
    let mutable sum = 'T.Zero
    for x in xs do
        sum <- sum + x
    'T.DivideByInt(sum, xs.Length)
average [|2.2;3.3;4.5;9.9|]
average [|1.;5.;7.|]

// demonstrate new F# 7.0 features: easier SRTPs. Could be useful to us maybe.
type MyClass<'t when 't: (member Length: int) and 't: (member plus: int -> 't)> = 't
type Int(x:int) =
    member _.plus rhs = x + rhs |> Int
    member _.Length = x
let inline foo<'T when MyClass<'T>>(lst:'T list) =
    lst |> List.sumBy (fun item -> item.Length)
let inline bar<'T when MyClass<'T>>(lst:'T list) =
    lst |> List.map(fun i -> (i.plus 7).Length)
[2;3;4] |> List.map Int |> bar
