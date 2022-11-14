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

weaponMaster id |> menu [
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
