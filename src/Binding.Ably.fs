module Binding.Ably

  type IMessage =
    abstract name: string
    abstract data: string

  type IChannel =
    abstract publish: string * string -> unit
    abstract subscribe: string * (IMessage -> unit) -> unit

  type IChannels =
    abstract ``get``: string -> IChannel

  type IRealTime =
    abstract channels: IChannels

  type IAbly =
    abstract Realtime: (string) -> IRealTime

  let Ably: IAbly = importAll "Ably"