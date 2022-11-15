module DungeonFantasy.Chargen

open Lib.Autopicker
open Choice

type EquipmentId = string
type Skill =
    | Broadsword | Rapier | Saber | Shortsword | Smallsword | MainGauche | Knife | Whip
    | Bow | Crossbow | Sling
    | Brawling | Boxing | Judo | Karate
    | Cloak | Shield | ShieldBuckler
    | ThrownWeaponKnife | Throwing

    | Acrobatics
    | Carousing
    | Climbing
    | Connoisseur of string
    | FastDraw of Skill
    | FastTalk
    | FirstAid
    | Gambling
    | Gesture
    | Hiking
    | Jumping
    | Intimidation
    | Lasso
    | SavoirFaire
    | Scrounging
    | Seamanship
    | Search
    | SexAppeal
    | Stealth
    | Streetwise

type Weapon = | Broadsword | Rapier | Saber | Shortsword | Smallsword | Longsword | Shield | MainGauche | Bow
type WeaponMasterFocus = All | Swords | TwoWeapon of Weapon * Weapon | WeaponOfChoice of Weapon
type Race = Catfolk | Elf | Dwarf | Gnome | HalfOgre | HalfOrc | Halfling
type Profession = Swashbuckler | MartialArtist | Wizard

type Ability = ST | DX | IQ | HT | SpeedTimesFour | MV | HP | FP // use SpeedTimesFour to allow easy 0.25 increments without needing floats
type Attractiveness = Attractive | Beautiful | VeryBeautiful | Unattractive | Ugly | Hideous
type Code = Gentlemans | Outlaws
type Compulsion = Carousing | Spending
type Duty = AdventuringCompanions | Nature | Needy
type Frequency = | Hourly | EveryHalfHour | EveryTenMinutes
type Obsession = BestSwordsman
type Severity = | Terrible | Bad | Average | Mild
type Vow = UseOnlyWeaponOfChoice | NeverRefuseChallengeToCombat | ChallengeEverySwordsmanToCombat | NeverWearArmor

type AdvantageOrDisadvantage =
    | Ambidexterity
    | Attractive of Attractiveness
    | ArmorFamiliarity of int
    | Charisma of int
    | CombatReflexes
    | DangerSense
    | Daredevil
    | EnhancedBlock of int * Skill
    | EnhancedDodge of int
    | EnhancedParry of int * Skill
    | EnhancedTimeSense
    | EveryOnesACritical
    | ExtraAttack of int
    | GreatVoid
    | HeroicArcher
    | Luck of Frequency
    | Magery of int
    | PerfectBalance
    | PeripheralVision
    | RapierWit
    | Serendipity of int
    | SignatureGear of EquipmentId
    | SpringingAttack
    | StrikingST of int
    | TrademarkMove of string
    | WeaponBond of EquipmentId // specific weapon id
    | WeaponMaster of WeaponMasterFocus

    | BadTemper of Severity
    | CodeOfHonor of Code
    | Compulsive of Compulsion * Severity
    | Chummy
    | Greed of Severity
    | Gregarious
    | Impulsiveness of Severity
    | Jealousy
    | Lecherousness of Severity
    | Obsession of Obsession * Severity
    | OneEye
    | OverConfidence of Severity
    | SenseOfDuty of Duty
    | ShortAttentionSpan of Severity
    | Trickster of Severity
    | Wounded
    | Vow of Vow

type Trait = Race of Race | Profession of Profession | Increase of Ability * int | Advantage of AdvantageOrDisadvantage | Disadvantage of AdvantageOrDisadvantage | Skill of Skill * int

type Enumerate =
    static member Races = [Catfolk ; Elf ; Dwarf ; Gnome ; HalfOgre ; HalfOrc ; Halfling]
    static member Professions = [Swashbuckler; MartialArtist; Wizard]
    static member PrimaryAbilities = [ST; DX; IQ; HT]
    static member DerivedAbilities = [HP; FP; SpeedTimesFour; MV]
    static member Weapons = [Rapier ; Longsword ; Shield ; MainGauche ; Bow]
    static member WeaponSkills = [Skill.Rapier; Skill.Broadsword; Skill.Bow]
    static member Severity = [Terrible; Bad; Average; Mild]
    static member ShieldSkills = [Skill.Shield; ShieldBuckler; Cloak]
    static member ToString v =
        match v with
        | HalfOgre -> "Half-ogre"
        | HalfOrc -> "Half-orc"
        | v -> v.ToString()
    static member ToString v =
        match v with
        | MartialArtist -> "Martial Artist"
        | v -> v.ToString()

let weaponMaster adapter : _ Choice =
    choose.ctor (WeaponMaster >> adapter) [
        choose.a All
        choose.a Swords
        choose.oneValueWith ("Single", WeaponOfChoice, Enumerate.Weapons)
        choose.ctor2With "Two" TwoWeapon (choose.oneValueWith("Weapon1", Enumerate.Weapons)) (choose.oneValueWith("Weapon2", Enumerate.Weapons))
        ]

let traitCost = notImpl

module Templates =
    let race() =
        choose.conditional (choose.oneValueWith("Race", Race, Enumerate.Races)) id [
            Race(Catfolk), choose.aggregate []
            Race(Elf), choose.aggregate []
            Race(HalfOgre), choose.aggregate []
            ]
    let swashbuckler adapter =
        choose.aggregate [
            choose.grant [
                            Increase(ST,1); Increase(DX,5); Increase(HT, 3);
                            for adv in [CombatReflexes; Luck Hourly] do
                                Advantage adv
                ]
            choose.mandatory (weaponMaster Advantage)
            choose.mandatory (choose.oneValueWith("EnhancedParry", (fun x -> Advantage(EnhancedParry(1,x))), [Skill.Rapier; Skill.Broadsword]))
            // need to somehow choose a weapon bond from starting equipment
            // choose 60 points of advantages
            // choose -15 and -35 points of disadvantages
            choose.upToBudget2 60 traitCost [race()] [
                let ability ab n = Increase(ab, n)
                choose.oneValue(ability ST, [1..6])
                choose.oneValue(ability DX, [1..3])
                choose.oneValue(ability SpeedTimesFour, [1..12])
                choose.oneValue(ability MV, [1..3])
                let one adv = choose.a (Advantage adv)
                let upTo n adv = choose.oneValue(adv >> Advantage, [1..n])
                let upToSkill n skills adv = choose.ctor2 (adv >> Advantage) (choose.oneValue [1..3]) (choose.oneValue skills)
                one Ambidexterity
                upTo 5 ArmorFamiliarity
                upTo 5 Charisma
                one Daredevil
                upToSkill 3 Enumerate.ShieldSkills EnhancedBlock
                choose.ctor2 (EnhancedParry >> Advantage) (choose.oneValue [1..3]) (choose.oneValue [Skill.Rapier; Skill.Broadsword])
                one EnhancedTimeSense
                one EveryOnesACritical
                upTo 2 ExtraAttack
                choose.oneValue (Advantage << Luck, [EveryHalfHour; EveryTenMinutes])
                one GreatVoid
                one PerfectBalance
                one RapierWit
                upTo 4 Serendipity
                // todo: signature gear
                one SpringingAttack
                upTo 2 StrikingST
                // todo: trademark move
                // todo: weapon bond
                // weapon master upgrade: todo, link to already-granted weaponMaster
                choose.ctor (WeaponMaster >> Advantage) [
                    choose.a All
                    choose.a Swords
                    choose.ctor2With "Two" TwoWeapon (choose.oneValueWith("Weapon1", Enumerate.Weapons)) (choose.oneValueWith("Weapon2", Enumerate.Weapons))
                    ]
                ]
            choose.disadvantagesWithPrioritizedBudget traitCost [
                let one dadv = choose.a (Disadvantage dadv)
                let mental dadv = choose.oneValue(dadv >> Disadvantage, Enumerate.Severity)
                let mental1 broad specific = mental (fun severity -> broad(specific, severity))
                -15, [  choose.oneValue(Disadvantage << CodeOfHonor, [Gentlemans; Outlaws])
                        mental1 Obsession BestSwordsman
                        choose.oneValue (Disadvantage << Vow, [
                            UseOnlyWeaponOfChoice
                            NeverRefuseChallengeToCombat
                            ChallengeEverySwordsmanToCombat
                            NeverWearArmor
                            ])
                        ]
                -35, [  choose.oneValue(Disadvantage, [Chummy; Gregarious])
                        mental1 Compulsive Carousing
                        mental1 Compulsive Spending
                        mental Greed
                        mental Impulsiveness
                        one Jealousy
                        mental Lecherousness
                        one OneEye
                        mental OverConfidence
                        one (SenseOfDuty AdventuringCompanions)
                        mental ShortAttentionSpan
                        mental Trickster
                        one Wounded
                        ]
                ]
            choose.oneOf [
                let swashweapons = [Skill.Broadsword; Skill.Rapier; Skill.Saber; Skill.Shortsword; Skill.Smallsword]
                let skill bonus skill = Skill(skill, bonus)
                choose.aggregate  [choose.mandatory (choose.oneValue (skill +4, swashweapons)); choose.grant [Skill(Skill.MainGauche, +1)]]
                choose.optionalWith "Sword!" (choose.oneValue (skill +5, swashweapons))
                choose.aggregateWith "Sword and dagger" [choose.mandatory (choose.oneValue (skill +4, swashweapons)); choose.grant [Skill(Skill.MainGauche, +1)]]
                choose.aggregateWith "Sword and dagger B" [choose.mandatory (choose.oneValue (skill +3, swashweapons)); choose.grant [Skill(Skill.MainGauche, +2)]]
                choose.aggregateWith "Sword and shield" [choose.mandatory (choose.oneValue (skill +4, swashweapons)); choose.mandatory(choose.oneValue(skill +1, [Skill.Cloak; Skill.ShieldBuckler]))]
                choose.aggregateWith "Sword and shield B" [choose.mandatory (choose.oneValue (skill +3, swashweapons)); choose.mandatory(choose.oneValue(skill +2, [Skill.Cloak; Skill.ShieldBuckler]))]
                ]
            ]
    let professions() =
        choose.conditional (choose.oneValueWith("Profession", Profession, Enumerate.Professions)) id [
            Profession(Swashbuckler), swashbuckler id
            ]
    let allTemplates() =
        choose.aggregate [
            race()
            professions()
            ]
