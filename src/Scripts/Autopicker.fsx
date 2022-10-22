// First we want a way to define and use generic APIs. I'm not sure that the SRTP-based approach is superior to the inheritance-based approach frankly.
module Autopicker

#I __SOURCE_DIRECTORY__
#I ".."
#load "Common.fs"

module POC1 =
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
    type Character = {
        race: Race option
        profession: Profession
        abilityMods: Map<Ability, int>
        advantages: Advantage list
        disadvantages: Disadvantage list
        }
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
    Enumerate.Races |> List.map Enumerate.ToString
    Enumerate.Professions |> List.map Enumerate.ToString
    let rand = System.Random()
    let chooseRandom (lst: _ list) =
        lst[rand.Next lst.Length]
    let chooseRandomRepeatedly n (lst: _ list) =
        [for _ in 1..n -> chooseRandom lst]
    let sample =
        let profession = chooseRandom Enumerate.Professions
        {
            race = chooseRandom ([None] @ (Enumerate.Races |> List.map Some))
            profession = profession
            abilityMods = chooseRandomRepeatedly (rand.Next 8) Enumerate.PrimaryAbilities
                            @ chooseRandomRepeatedly (rand.Next 8) Enumerate.DerivedAbilities
                          |> List.countBy id
                          |> Map.ofList
            advantages = []
            disadvantages = []
        }
    type Priors = string list list
    type 't Choice = Priors -> ('t list -> 't) -> 't
    type Chooser (priorChoices: Priors) =
        member choose.item item = fun priors -> item
        member choose.from (lst: 't list) : 't Choice = fun priors choiceType -> choiceType lst
        member choose.among (choices: _ Choice list) = chooseRandom choices
    let advantageTree (choose: Chooser) =
        let focii =
            [
            All |> choose.item
            Swords |> choose.item
            TwoWeapon (choose.from Enumerate.Weapons, choose.from Enumerate.Weapons)
            WeaponOfChoice (choose.from Enumerate.Weapons)
            ]
            |> choose.choices
        [
            WeaponMaster (choose.among focii) ; DangerSense ; PeripheralVision ; HeroicArcher ; Magery (chooseRandom [3..6])
            ] |> choose.among
    advantageTree
    advantageTree (Chooser(
        [
            ["WeaponMaster";"TwoWeapon";"Rapier"]
            ["WeaponMaster";"TwoWeapon";"MainGauche"]
            ["DangerSense"]
            ]))

type 'Choice MenuItem =
    | Simple of 'Choice list
type 'Choice Choices = 'Choice MenuItem list
type API<'Choice, 'Render> =
    abstract member rootChoices: 'Choice Choices
    abstract member expand: 'Choice -> 'Choice Choices

module API =
    let inline (|HasOne|) (a:^T) : 'R =
      (^T : (member one : ^R) a)
    let inline (|HasZero|) (a:^T) : 'R =
      (^T : (member zero : ^R) a)
    let inline (|HasNormalize|) (a:^T) : ^R -> ^R -> ^R =
      fun (b:^R) (c:^R) -> (^T : (member normalize : (^R * ^R -> ^R)) a)(b,c)
open API

let inline norm3 (HasOne one & HasZero zero & HasNormalize normalize) v =
    let ten = List.init 10 (fun _ -> one) |> List.fold (+) zero
    normalize ten v
let api = {| zero = 0; normalize = (fun (ref, v) -> v/ref); one = 1 |}
norm3 ({| normalize = (fun (ref,v) -> v/ref); zero = 0; one = 1 |}) 1400

type choose =
    static member atMostOneOf lst = notImpl()
    static member exactlyOneOf lst = notImpl()
    static member optional choice = notImpl()
    static member mandatory menu = notImpl()
    static member among lst: _ Choices = notImpl()
let (==>) x y = notImpl() // dependent choices
type DFRPGChoice = { choice: string; cost: int; dependentChoices: DFRPGChoice Choices }
// menu is a list of choices, which may or may not have cost, constraints or dependent choices.
let initialMenu : DFRPGChoice Choices  = choose.among [
    choose.optional "Race" ==> choose.atMostOneOf ["Elf", 20; "Dwarf", 20; "Half-ogre", 20; "Catfolk", 40]
    choose.mandatory <| choose.exactlyOneOf ["Swashbuckler"; "Knight"; "Martial Artist"]
    "Elf" ==> choose.exactlyOneOf ["Moon"; "Sun"; "Drow"]
    "Weapon Master", 20 ==> choose.exactlyOneOf ["All", 45; "Swords", 35; "Weapon of Choice", 20; "Two-weapon", 25]
    "Swashbuckler" ==> choose.among [
        "Weapons package" ==> choose.exactlyOneOf [
            "Sword" ==> choose.exactlyOneOf ["Rapier", 20; "Broadsword", 20; "Shortsword", 20]
            "Sword and shield"
            "Two-handed"
            ]
        ]
    ]
