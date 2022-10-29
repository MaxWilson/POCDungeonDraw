#I __SOURCE_DIRECTORY__
#I ".."
#load "Common.fs"
#load "Lib\Autopicker.fs"
#load "DungeonFantasy\Chargen.fs"

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

    type CharacterTrait = Level of int | Race of Race | WeaponMaster of WeaponMasterFocus
    type ChoiceParam = { probabilityInPercent: int; key: string list }
        with
        static member create prob = { probabilityInPercent = prob; key = [] }
        static member appendKey newValue old = { old with key = old.key @ [newValue] }
    let maybe choice k param = (if rand.Next 100 <= param.probabilityInPercent then choice k param else k [])
    let generate v k param = (if rand.Next 100 <= param.probabilityInPercent then [v] else []) |> k
    let chooseSome options =
        [
            for o in options do
                yield (o |> generate |> maybe)
            ]
    let chooseOne (choices : _ list) k acc =
        [choices |> chooseRandom] |> k
    let makeRandomChoice choices = chooseRandom choices
    type MenuItem<'a, 'r> = 'a -> 'r
    let sometimes (choices: MenuItem<_,_> list) k accum =
        [
            for choice in choices do
                yield! (choice k accum)
            ]
    let levels = chooseOne [
        for i in 1..10 do
            yield (Level i)
        ]
    let races = chooseSome [
        for r in Enumerate.Races do
            yield (Race r)
        ]
    let choices1 =
        [levels] @ races
    sometimes choices1 id (ChoiceParam.create 25)
    let mapCtor ctor = function // should probably be called mapCtor or something
        | [v] -> [ctor v]
        | _ -> []
    let bindChoice f = function // should probably be called bindChoice or something instead
        | [v] -> f v
        | _ -> []
    let chooseWeapon acc k = chooseOne Enumerate.Weapons acc k
    sometimes [chooseWeapon] id ()
    type ComposedChoice<'acc, 'intermediateState, 'r> = ('intermediateState -> 'r) -> 'acc -> 'r
    type Compose() =
        member _.from label (suboptions: ComposedChoice<_,_,_> list) : ComposedChoice<_,_,_> = fun k acc ->
            let acc = acc |> ChoiceParam.appendKey label
            chooseRandom suboptions k acc
        member _.a v : ComposedChoice<_,_,_> = fun k acc -> k [v]
        member _.oneOf label options : ComposedChoice<_,_,_> = fun k acc -> chooseOne options k (acc |> ChoiceParam.appendKey label)
        member _.ctor2 ctor (choice1: ComposedChoice<_,_,_>) (choice2 : ComposedChoice<_,_,_>) : ComposedChoice<_,_,_> = fun k acc ->
            choice1 (bindChoice (fun arg1 -> choice2 (function [arg2] -> [ctor(arg1, arg2)] | _ -> []) acc) >> k) acc
        member _.ctor choice ctor : ComposedChoice<_,_,_> = fun k -> choice (mapCtor ctor >> k)
    let compose = Compose()
    let chooseWeaponMasterFocus (k: _ -> 'r) =
        let x: ComposedChoice<_,_,'r> = compose.ctor (compose.oneOf "WeaponFocus" Enumerate.Weapons) WeaponOfChoice
        compose.from "WeaponMasterFocus" [
            compose.a All
            compose.a Swords
            compose.ctor (compose.oneOf "WeaponFocus" Enumerate.Weapons) WeaponOfChoice
            //compose.ctor2 TwoWeapon (chooseOne Enumerate.Weapons k) (chooseOne Enumerate.Weapons k)
            fun k acc ->
                let chooseWeapon k = chooseOne Enumerate.Weapons k acc
                let combine ctor choice k =
                    choice ((bindChoice (fun arg1 -> choice (mapCtor (fun arg2 -> ctor(arg1, arg2))))) >> k)
                //let choice = combine TwoWeapon chooseWeapon k
                let choice2 = chooseWeapon (bindChoice (fun arg1 -> chooseWeapon (function [arg2] -> [TwoWeapon(arg1, arg2)] | _ -> [])) >> k)
                choice2
            ]
            k
    sometimes [(chooseWeaponMasterFocus: ComposedChoice<_,_,_>)] (mapCtor WeaponMaster) (ChoiceParam.create 25)
    type Menu<'input, 'acc, 'r> = (ChoiceParam -> ('input -> 'acc) -> 'r) list
    let inline chooseWeaponMaster acc k = chooseWeaponMasterFocus (mapCtor (WeaponMaster >> k)) acc
    sometimes [chooseWeaponMaster] (ChoiceParam.create 25) id
    sometimes choices1 id (ChoiceParam.create 25)
    let x = [chooseWeaponMaster]
    let y = choices1
    sometimes (choices1@choices1) id (ChoiceParam.create 25)
    let choices2 =
        choices1 @ [maybe (compose.ctor chooseWeaponMasterFocus WeaponMaster)]
    sometimes choices2 id (ChoiceParam.create 50)


    let weapon = chooseOne Enumerate.Weapons
    let singleWeapon = chooseCtor1 WeaponOfChoice weapon
    let dualWeapon = chooseCtor2 TwoWeapon (chooseDistinct2 weapon weapon)
    let focus = chooseSome []
    let weaponMaster = chooseCtor1
    let choices2 =
        [levels] @ races @ weaponMaster
    sometimes choices1 () id

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
    type Chooser () =
        member choose.item item : _ Choice = fun priors choiceType -> choiceType [item]
        member choose.item2 ctor (arg1: _ Choice) (arg2: _ Choice) : _ Choice = fun priors choiceType -> choiceType [ctor(choose.among arg1 priors, choose.among arg2 priors)]
        member choose.from (lst: 't list) : 't Choice = fun priors choiceType -> choiceType lst
        member choose.among (choices: _ Choice) priors = choices priors chooseRandom
    let choose = Chooser()
    let advantageTree (choose: Chooser) =
        let focii =
            [
            All |> choose.item
            Swords |> choose.item
            (TwoWeapon, choose.from Enumerate.Weapons, choose.from Enumerate.Weapons) ||> choose.item2
            WeaponOfChoice (choose.from Enumerate.Weapons)
            ]
            |> choose.choices
        [
            WeaponMaster (choose.among focii) ; DangerSense ; PeripheralVision ; HeroicArcher ; Magery (chooseRandom [3..6])
            ] |> choose.among []
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
