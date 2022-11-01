#I __SOURCE_DIRECTORY__
#I ".."
#load "Common.fs"
#load "Lib\Autopicker.fs"
open Lib.Autopicker
open Lib.Autopicker.Choice
#load "DungeonFantasy\Chargen.fs"
open DungeonFantasy.Chargen

let weaponMaster() : ComposedChoice<_,_,_> =
    choose.ctor WeaponMaster (
        choose.oneOf "Focus" [
            choose.a All
            choose.a Swords
            choose.ctor WeaponOfChoice (choose.oneValue "Weapon" Enumerate.Weapons)
            choose.ctor2 TwoWeapon (choose.oneValue "Weapon" Enumerate.Weapons) (choose.oneValue "Weapon2" Enumerate.Weapons)
            ])


let traits() =
    choose.aggregate [
        choose.mandatory (choose.ctor Profession (choose.oneValue "Profession" Enumerate.Professions))
        choose.optional (choose.ctor Advantage (weaponMaster()))
        choose.optional (choose.ctor Advantage (choose.oneValue "Adv" [DangerSense; PeripheralVision; HeroicArcher; Magery 6]))
        ]

let sometimes choice = choice id (Choice.Param<_>.create 25)
let trace (acc:Choice.Param<_>) =
    let trace key =
        let v = acc.recognizer key
        printfn $"{key}: {v}"
        v
    { acc with recognizer = trace }
let pick (indices: string list) choice = choice id (Choice.Param<_>.create indices |> trace)
sometimes (traits())
traits() |> pick [
    "Profession-Swashbuckler"
    "Focus-Weapon-Rapier"
    "Adv-Magery 6"
    ]
traits() |> pick [
    "Profession-Swashbuckler"
    "Adv-Magery 6"
    ]
