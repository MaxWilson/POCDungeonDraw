// First we want a way to define and use generic APIs. I'm not sure that the SRTP-based approach is superior to the inheritance-based approach frankly.
module Autopicker

#I __SOURCE_DIRECTORY__
#I ".."
#load "Common.fs"

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
