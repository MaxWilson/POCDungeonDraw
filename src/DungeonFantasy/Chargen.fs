module DungeonFantasy.Chargen

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
        choose.oneOf [
            choose.a All
            choose.a Swords
            choose.ctorWith "Single" WeaponOfChoice (choose.oneValue Enumerate.Weapons)
            choose.ctor2With "Two" TwoWeapon ("Weapon1", choose.oneValue Enumerate.Weapons) ("Weapon2", choose.oneValue Enumerate.Weapons)
            ])


let traits() =
    choose.aggregate [
        choose.mandatory (choose.ctorWith "Profession" Profession (choose.oneValue Enumerate.Professions))
        choose.optional (choose.ctorWith "Adv" Advantage (choose.oneValue [DangerSense; PeripheralVision; HeroicArcher; Magery 6]))
        choose.aggregate [
            for a in Enumerate.PrimaryAbilities do
                (choose.optional (choose.a (Increase a)))
            for adv in [DangerSense; PeripheralVision; HeroicArcher] do
                (choose.optional (choose.a (Advantage adv)))
            choose.optionalWith "WeaponMaster" (choose.ctor Advantage (weaponMaster()))
            choose.optionalWith "Magery" (choose.oneOf [
                for m in 0..6 do
                    yield (choose.a (m.ToString(), Advantage (Magery m)))
                ])
            ]
        ]