﻿module DungeonFantasy.Chargen

open Lib.Autopicker

type Weapon = Rapier | Longsword | Shield | MainGauche | Bow
type WeaponMasterFocus = All | Swords | TwoWeapon of Weapon * Weapon | WeaponOfChoice of Weapon
type Race = Catfolk | Elf | Dwarf | Gnome | HalfOgre | HalfOrc | Halfling
type Profession = Swashbuckler | MartialArtist | Wizard
type Ability = ST | DX | IQ | HT | Speed | MV | HP | FP
type Advantage = WeaponMaster of WeaponMasterFocus | DangerSense | PeripheralVision | HeroicArcher | Magery of int
type Severity = Terrible = 6 | Bad = 9 | Average = 12 | Mild = 15
type Duty = Companions | Nature | Needy
type Disadvantage = BadTemper of Severity | Greed of Severity | SenseOfDuty of Duty
type Trait = Race of Race | Profession of Profession | Increase of Ability | Advantage of Advantage | Disadvantage of Disadvantage

type Enumerate =
    static member Races = [Catfolk ; Elf ; Dwarf ; Gnome ; HalfOgre ; HalfOrc ; Halfling]
    static member Professions = [Swashbuckler; MartialArtist; Wizard]
    static member PrimaryAbilities = [ST; DX; IQ; HT]
    static member DerivedAbilities = [HP; FP; Speed; MV]
    static member Weapons = [Rapier ; Longsword ; Shield ; MainGauche ; Bow]
    static member ToString v =
        match v with
        | HalfOgre -> "Half-ogre"
        | HalfOrc -> "Half-orc"
        | v -> v.ToString()
    static member ToString v =
        match v with
        | MartialArtist -> "Martial Artist"
        | v -> v.ToString()


let weaponMaster() : ComposedChoice<_,_,_> =
    choose.ctor WeaponMaster (
        choose.randomly [
            choose.a All
            choose.a Swords
            choose.ctor WeaponOfChoice (choose.oneValue "Weapon" Enumerate.Weapons)
            choose.ctor2 TwoWeapon (choose.oneValue "Weapon" Enumerate.Weapons) (choose.oneValue "Weapon2" Enumerate.Weapons)
            ])


let traits() =
    choose.aggregate [
        choose.uplift (choose.ctor Profession (choose.oneValue "Profession" Enumerate.Professions))
        choose.ctor Advantage (weaponMaster())
        choose.ctor Advantage (choose.oneValue [DangerSense; PeripheralVision; HeroicArcher; Magery 6])
        ]

let wm0() = choose.ctor Advantage (weaponMaster())
let sometimes choice = choice id (Choice.Param.create 25)
wm0() |> sometimes
let v1 = (choose.ctor WeaponOfChoice (choose.oneValue "Weapon" Enumerate.Weapons)) id (Choice.Param.create 25)
let x0 = (choose.oneValue "Profession" Enumerate.Professions)
let x1() = choose.ctor Advantage (weaponMaster())
let y0 = choose.aggregate [choose.uplift x0; choose.uplift (choose.ctor Advantage (weaponMaster()))] |> sometimes
let x: ComposedChoice<_,Trait,Profession> = choose.ctor Profession (choose.oneValue "Profession" Enumerate.Professions)
let v = x id (Choice.Param.create 25)
let y: ComposedChoice<_,_,Trait list> = x |> choose.uplift