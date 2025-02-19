﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Diagnostics
open Prime

[<RequireQualifiedAccess>]
module Core =

    let mutable private LastTimeStamp = Stopwatch.GetTimestamp ()
    let private LastTimeStampLock = obj ()

    /// Get a time stamp at the highest-available resolution.
    /// Thread-safe.
    let getTimeStamp () =
        Stopwatch.GetTimestamp ()

    /// Get a unique time stamp, spinning until the time stamp advances if need be.
    /// Thread-safe.
    let getTimeStampUnique () =
        lock LastTimeStampLock $ fun () ->
            let mutable nextStamp = getTimeStamp ()
            while nextStamp = LastTimeStamp do nextStamp <- getTimeStamp ()
            LastTimeStamp <- nextStamp
            nextStamp

[<AutoOpen>]
module CoreOperators =

    /// Sequences two functions like Haskell ($).
    /// Same as the ($) operator found in Prime, but placed here to expose it directly from Nu.
    let inline ($) f g = f g

    /// Test for object equality.
    /// Same as the (===) operator found in Prime, but placed here to expose it directly from Nu.
    let inline (===) (a : obj) (b : obj) = objEq a b

    /// Test for object inequality.
    /// Same as the (=/=) operator found in Prime, but placed here to expose it directly from Nu.
    let inline (=/=) (a : obj) (b : obj) = objNeq a b

    /// TODO: P1: remove this after updating Prime.
    /// Convert an value to an value of the given type using symbolic conversion.
    /// Thread-safe.
    let objToObj (ty : Type) (value : obj) =
        match value with
        | null -> null
        | _ ->
            let ty2 = value.GetType ()
            if not (ty.IsAssignableFrom ty2) then
                let converter = SymbolicConverter ty
                let converter2 = SymbolicConverter ty2
                let symbol = converter2.ConvertTo (value, typeof<Symbol>)
                converter.ConvertFrom symbol
            else value