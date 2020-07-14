#lang scribble/manual

@(require scribble/eval
          (for-label racket/base
                     racket/contract
                     tzinfo))

@(define the-eval (make-base-eval))
@(the-eval '(require tzinfo))

@title{TZInfo}
@author{@(author+email "Jon Zeppieri" "zeppieri@gmail.com")}

@defmodule[tzinfo]

@section{Introduction}

The @tt{tzinfo} library provides an interface for querying the IANA time zone database
(also known as the Olson database).

UNIX systems usually come with a compiled version of the IANA database (typically in
@tt{/usr/share/zoneinfo}). @tt{tzinfo} will use the system's database if available.
However, if the @tt{tzdata} package is installed, that will be used instead. Since
Windows systems do not come with a zoneinfo database, Windows users must install
@tt{tzdata} to use @tt{tzinfo}.

@section{Querying the Database}

@defproc[(all-tzids) (listof string?)]{
Returns a list containing all of the time zone IDs in the database.
}

@defproc[(tzid-exists? [tzid string?]) boolean?]{
Returns @racket[#t] if the given ID is in the database, @racket[#f] otherwise.
        
@examples[#:eval the-eval
(tzid-exists? "Europe/London")
(tzid-exists? "Fillory/Whitespire")
]
}

@defproc[(utc-seconds->tzoffset [tzid string?]
                                [seconds real?])
         tzoffset?]{
For a given time zone ID and number of seconds since the UNIX epoch (1970-01-01 00:00:00 UTC),
returns a @racket[tzoffset?] describing the offset from UTC in effect at that moment of time
in the given time zone. Raises @racket[exn:fail:tzinfo:zone-not-found] if the given time zone
ID is not in the database.

@examples[#:eval the-eval
(utc-seconds->tzoffset "America/New_York" 0)
(utc-seconds->tzoffset "Fillory/Whitespire" 0)
]
}

@defproc[(local-seconds->tzoffset [tzid string?]
                                  [seconds real?])
         (or/c tzoffset? tzgap? tzoverlap?)]{
For a given time zone ID and number of seconds since 1970-01-01 00:00:00
@italic{in the given time zone}, returns one of:

@itemize[
@item{
a @tt{tzoffset} struct, describing the offset from UTC in effect at the given time in the given time zone;
}
@item{
a @tt{tzgap} struct if the given local time falls into a gap between different offsets (as, for example,
when an hour is skipped at the start of daylight saving time in most parts of the United States); or
}
@item{
a @tt{tzoverlap} struct if the given local time falls in a period when two different offsets might be
in effect (as, for example, at the end of daylight saving time, when an hour is repeated).
}
]

Raises @racket[exn:fail:tzinfo:zone-not-found] if the given time zone ID is not in the database.

@examples[#:eval the-eval
(local-seconds->tzoffset "America/New_York" 1409606993)
(local-seconds->tzoffset "America/New_York" 1394330400)
(local-seconds->tzoffset "America/New_York" 1414890000)
]
}

@defproc[(tzid->country-codes [tzid string?]) (listof string?)]{
Returns a list of ISO 3166 alpha-2 country codes in which the given time zone is used.

@examples[#:eval the-eval
(tzid->country-codes "Europe/Moscow")
(tzid->country-codes "Antarctica/Troll")
(tzid->country-codes "Africa/Kinshasa")
]
}

@defproc[(country-code->tzids [cc string?]) (listof string?)]{
Returns a list of time zone IDs that are used in the country identified by the given
ISO 3166 alpha-2 country code.

@examples[#:eval the-eval
(country-code->tzids "US")
(country-code->tzids "IT")
]
}

@defproc[(system-tzid) (or/c string? #f)]{
Returns the ID of the current system time zone, if it can be determined, @racket[#f] otherwise.
}

@defstruct*[exn:fail:tzinfo:zone-not-found ()]{
An exception that is raised by query functions when the given time zone ID does not
exist in the tzinfo database.
}

@section{Offsets, Gaps, and Overlaps}

@defstruct*[tzoffset ([utc-seconds exact-integer?]
                      [dst? boolean?]
                      [abbreviation string?])
                     #:transparent]{
A structure type representing a time zone-specific offset from UTC.
@tt{utc-seconds} contains the different from UTC
in seconds. @tt{dst?} is true just in case the offset represents an offset in effect during
daylight saving time. @tt{abbreviation} is, e.g., "EST" for "Eastern Standard Time," "BST" for
"British Summer Time," etc.
}

@defstruct*[tzgap ([starts-at exact-integer?]
                   [offset-before tzoffset?]
                   [offset-after tzoffset?])
                  #:transparent]{
A structure type representing a time zone-specific gap between two offsets from UTC. Gaps occur
at the start of daylight saving time.
@tt{starts-at} is the start of the gap, represented as a number of seconds since the UNIX epoch.
@tt{offset-before} is the @tt{tzoffset} in effect before the gap.
@tt{offset-after} is the @tt{tzoffset} in effect after the gap.
}

@defstruct*[tzoverlap ([offset-before tzoffset?]
                       [offset-after tzoffset?])
                      #:transparent]{
A structure type representing a time-zone specific overlap of two different offsets. Overlaps
occur at the end of daylight saving time.
@tt{offset-before} is the earlier offset. E.g., when going from daylight saving time to standard time,
@tt{offset-before} is the daylight saving time offset.
@tt{offset-after} is the later offset.
}

@section{Data Sources}

@tt{tzinfo} allows for a pluggable data sources. At present, the only supported source
is based on @tt{zoneinfo} files, which are a compiled form of the IANA database, widely-
used on UNIX systems.

@defparam[current-tzinfo-source tzinfo-source tzinfo-source? #:value #f]{
A parameter containing the current @tt{tzinfo} data source.
}

@defproc[(set-default-tzinfo-source-constructor! [ctor (-> tzinfo-source?)]) void?]{
Sets the constructor function that will be applied to build the default tzinfo source.
To use a custom source, you must call this function before using any of the querying functions.
}

@section{Data Source Generics}

@defmodule[tzinfo/source]

@defthing[gen:tzinfo-source any/c]{
A generic interface for defining custom tzinfo data sources. It defines the following
methods:

@itemize[
@item{@racket[tzinfo->all-tzids]}
@item{@racket[tzinfo-has-tzid?]}
@item{@racket[tzinfo-tzid->country-codes]}
@item{@racket[tzinfo-country-code->tzids]}
@item{@racket[seconds->tzoffset/utc]}
@item{@racket[seconds->tzoffset/local]}
@item{@racket[detect-system-tzid]}
]
}

@defproc[(tzinfo-source? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a tzinfo source, @racket[#f] otherwise.
}

@defproc[(tzinfo->all-tzids [src tzinfo-source?]) (listof string?)]{
Returns the full list of time zone IDs contained in the given tzinfo source.
}

@defproc[(tzinfo-has-tzid? [src tzinfo-source?]
                           [tzid string?])
         boolean?]{
@racket[#t] if the tzinfo source contains the given time zone ID, @racket[#f] otherwise.
}

@defproc[(tzinfo-tzid->country-codes [src tzinfo-source?]
                                     [tzid string?])
         (listof string?)]{
Returns the list of ISO 3166 alpha-2 country codes in which the given time zone is used, accoring
to the tzinfo source data.
}

@defproc[(tzinfo-country-code->tzids [src tzinfo-source?]
                                     [cc string?])
         (listof string?)]{
Returns the list of time zone IDs that are used in the given country (identified by an
ISO 3166 alpha-2 country code), according to the tzinfo data source.
}

@defproc[(seconds->tzoffset/utc [src tzinfo-source?]
                                [tzid string?]
                                [seconds real?])
         tzoffset?]{
Returns the @tt{tzoffset} in use at the given UTC time in the given time zone, according to
the tzinfo source.
}

@defproc[(seconds->tzoffset/local [src tzinfo-source?]
                                  [tzid string?]
                                  [seconds real?])
         (or/c tzoffset? tzgap? tzoverlap?)]{
Returns a @tt{tzoffset}, @tt{tzgap}, or @tt{tzoveralap}, depending on what offset is in effect
at the given local time in the given time zone, according to the tzinfo source.
}

@defproc[(detect-system-tzid) (or/c string? #f)]{
Returns the time zone ID currently in use by the operating system, if it can be detected,
@racket[#f] otherwise.
}
