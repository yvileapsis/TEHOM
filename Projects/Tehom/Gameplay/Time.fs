namespace Tehom

open Nu

module TehomTime =
    // Time is a 360-based clock and a day counter.
    // Day is composed of 12 hours, each separated into 30 minutes
    // Each hour is named astrologically.
    // Point is to avoid AM/PM/Military stuff and also distance the world.

    type Hours =
        | Aries
        | Taurus
        | Gemini
        | Cancer
        | Leo
        | Virgo
        | Libra
        | Scorpio
        | Sagittarius
        | Capricorn
        | Aquarius
        | Pisces

    type Minutes =
        | Begin = 0
        | End = 30

    type Time = Time of uint
    with
        static member empty = Time 0u
        static member advance add (Time time) = Time (time + add)
        static member getMinutes (Time time) = time
        static member getMinute (Time time) = time % 30u
        static member getHour (Time time) =
            match (time / 30u) % 12u with
            | 0u -> Aries
            | 1u -> Taurus
            | 2u -> Gemini
            | 3u -> Cancer
            | 4u -> Leo
            | 5u -> Virgo
            | 6u -> Libra
            | 7u -> Scorpio
            | 8u -> Sagittarius
            | 9u -> Capricorn
            | 10u -> Aquarius
            | _ -> Pisces
        static member getDay (Time time) = time / 30u / 12u

type TehomTime = TehomTime.Time