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

// POC for lookup by flats or partial flags
let traits1 = [Advantage(WeaponMaster(TwoWeapon(Rapier, Shield))); Advantage(DangerSense); Increase(ST)]

let inline recur (prefix:string) (newEntry:string) f x =
    prefix::(f x (prefix+newEntry))
type Flattener() =
    member this.flatten x = fun prefix ->
        let inline recur x name = prefix::(this.flatten x (prefix+name))
        match x with
        | Advantage x -> nameof(Advantage) |> recur
        | Increase x -> nameof(Increase) |> recur
        | x -> [x.ToString()]
    member this.flatten x = fun prefix ->
        let recur x name = prefix::(this.flatten x (prefix+name))
        match x with
        | WeaponMaster x -> nameof(WeaponMaster) |> recur
        | x -> [x.ToString()]
    member this.flatten (x:WeaponMasterFocus) = fun prefix ->
        let recur2 x1 x2 name = prefix::(this.flatten x1 (prefix+name) @ this.flatten x2 (prefix+name))
        let recur name = prefix::(this.flatten x (prefix+name))
        match x with
        | TwoWeapon(w1, w2) -> nameof(TwoWeapon) |> recur2
        | WeaponOfChoice x -> nameof(WeaponOfChoice)::(this.flatten x)
        | x -> [x.ToString()]
    member this.flatten (x:Weapon) = [x.ToString()]
    member this.flatten (x:Ability) = [x.ToString()]
let flatten = Flattener()
traits1 |> List.map (flatten.flatten "")
