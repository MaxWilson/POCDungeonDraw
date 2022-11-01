#I __SOURCE_DIRECTORY__
#I ".."
#load "Common.fs"
#load "Lib\Autopicker.fs"
open Lib.Autopicker
open Lib.Autopicker.Choice
#load "DungeonFantasy\Chargen.fs"
open DungeonFantasy.Chargen

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