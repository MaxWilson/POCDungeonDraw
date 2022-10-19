// First we want a way to define and use generic APIs. I'm not sure that the SRTP-based approach is superior to the inheritance-based approach frankly.
module Autopicker

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