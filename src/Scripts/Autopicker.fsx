#I __SOURCE_DIRECTORY__
#I ".."
#load "Common.fs"
#load "Lib\Autopicker.fs"
open Lib.Autopicker
open Lib.Autopicker.Choice
#load "DungeonFantasy\Chargen.fs"
open DungeonFantasy.Chargen

let one x = Grant(x) :> Choice<_>
let maybe x = Allow x :> Choice<_>
let x = maybe "abc"
let vals (x:Choice<_>) = x.getValues(Param.create 50)
vals x
let y = OneChoice<_,_>.create([maybe "XYZ"; x; one "xyz"])
let mk x = OneChoice<_,_>.create x
let some x = SomeChoices<_,_>(None, x, Some)
mk [maybe "XYZ"; maybe "123"] |> vals
some [maybe "XYZ"; maybe "123"] |> vals
let z = SomeChoices(None,[maybe "XYZ"; maybe "123"],fun v -> Some [v])
vals z
vals y
let weapons = OneChoice.create(Enumerate.Weapons |> List.map maybe)
let single = OneTransform(None, weapons, WeaponOfChoice >> Some)
let focii = OneChoice(None, [maybe All; maybe Swords; OneTransform(None, weapons, WeaponOfChoice >> Some)], WeaponMaster >> Some)
let wms = [for _ in 1..200 -> vals focii]
wms |> List.filter (function Some(WeaponMaster(WeaponOfChoice _)) -> true | _ -> false)
vals single
vals (OneChoice<_,_>.create)
x.ToString()

let sometimes choice = choice id (Choice.Param<_>.create 25)
let trace (acc:Choice.Param<_>) =
    let trace key =
        let v = acc.recognizer key
        if key <> "" then
            printfn $"{key}: {v}"
        v
    { acc with recognizer = trace }
let pick (indices: string list) choice = choice id (Choice.Param<_>.create indices |> trace)
sometimes (traits())
traits() |> pick [
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
