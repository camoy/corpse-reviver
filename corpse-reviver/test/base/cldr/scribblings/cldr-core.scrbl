#lang scribble/manual

@(require scribble/eval
          (for-label racket/base
                     racket/runtime-path
                     cldr/core
                     cldr/likely-subtags
                     json))
@(define the-eval (make-base-eval))
@(the-eval '(require cldr/core
                     cldr/likely-subtags))

@title{CLDR Core}
@author[@author+email["Jon Zeppieri" "zeppieri@gmail.com"]]

@margin-note{
@deftech{CLDR} is the @link["http://cldr.unicode.org/"]{Common Locale Data Repository}, a
database of localization information published by the Unicode Consortium.
}

The CLDR Core library is a Racket interface to the
@link["https://github.com/unicode-cldr/cldr-core"]{cldr-core} JSON distribution published by
the Unicode Consortium. 

This package provides functions for:
@itemize[
  @item{enumerating the set of locales supported by CLDR and testing whether a given locale is supported;}
  @item{accessing CLDR JSON data; and }
  @item{canonicalizing locale strings and mapping them to the set of supported CLDR locales.}
]

@defmodule[cldr/core]


@section{Locale Support}

@defproc[(all-locales) (listof string?)]{Returns the list of locales supported by CLDR}

@defproc[(modern-locales) (listof string?)]{Returns the list of modern locales supported by CLDR.}

@defproc[(locale? [loc string?]) boolean?]{
Returns @racket[#t] if @racket[loc] is in @racket[(all-locales)], @racket[#f] otherwise.

@examples[#:eval the-eval
(locale? "fr")
(locale? "yi")
]}

@defproc[(modern-locale? [loc string?]) boolean?]{
Returns @racket[#t] if @racket[loc] is in @racket[(moderns-locales)], @racket[#f] otherwise.

@examples[#:eval the-eval
(modern-locale? "fr")
(modern-locale? "yi")
]}

@defproc[(available-locales) jsexpr?]{
Returns the raw data from the @tt{availableLocales.json} data file.
}


@section{Accessing CLDR JSON Data}

@defproc[(cldr-ref [data jsexpr?]
                   [key cldr-key/c]
                   [fail any/c (λ ()
                                 (raise (exn:fail:contract ....)))])
         any/c]{
Looks up the mapping of @racket[key] in @racket[data]. If there is no such mapping,
then @racket[fail] determines the result:

@itemize[
  @item{If @racket[fail] is a procedure, it is called
       (through a tail call) with no arguments to produce the result.}
  @item{Otherwise, @racket[fail] is returned as the result.}
]}

@defproc[(cldr-ref* [data jsexpr?]
                    [#:fail fail any/c (λ ()
                                         (raise (exn:fail:contract ....)))]
                    [key cldr-key/c]
                    ...)
         any/c]{
Like @racket[cldr-ref], except that any number of keys may be provided. The keys are tried in order,
and the value of the first key to be found is returned. If none of the given keys are mapped in
@racket[data], then @racket[fail] is used to determine the result, just as in @racket[cldr-ref].
}

@defproc[(cldr-json [path-to-zipfile path?]
                    [package-name string?]
                    [path-within-zipfile path?]
                    [common-prefix cldr-key/c])
         jsexpr?]{
A low-level procedure for accessing raw CLDR data from @tt{.zip} files.
This function is useful for implementing packages within the @tech{cldr}
collection but is generally @emph{not} useful for users of those packages.

In order to keep download sizes reasonable (since the CLDR data set is very large),
packages in the @tt{cldr} collection keep their data in a @tt{.zip} file named
for the package in question. For example, the @racket[cldr-core] package contains a
data file named @tt{cldr-core.zip}. This file is a compressed archive of the
@link["https://github.com/unicode-cldr/cldr-core"]{official distribution}.

The @racket[cldr-json] procedure takes:

@itemize[
  @item{the path to this zipfile (typically defined within the package using
        @racket[define-runtime-path]);}
  @item{the name of the package (which doubles as the name of the zipfile,
        without the @tt{.zip} extension);}
  @item{the path within the zipfile to the desired @tt{.json} file; and}
  @item{a key used to prune the returned JSON data.}
]}

@defproc[(raise-locale-not-found [locale string?]
                                 [package-name string?])
         any/c]{
Raises @racket[exn:cldr:locale-not-found], indicating that @racket[locale] is not available
in the CLDR data set for the package named by @racket[package-name].

This function is useful for authors of packages within the @tech{cldr} collection.
}

@defstruct*[(exn:cldr exn:fail) () #:transparent]{
A struct type that serves as the base type for all CLDR errors.
}

@defstruct*[(exn:cldr:locale-not-found exn:cldr) ([locale string?] [pkg string?]) #:transparent]{
Raised by functions that are given locale strings that cannot be mapped to CLDR locales.
}

@defthing[cldr-key/c flat-contract?]{
A contract for lookup keys used by @racket[cldr-ref] and @racket[cldr-ref*]. The contract is defined as:

@racketblock[
(or/c symbol? string? integer?
      (listof (or/c symbol? string? integer?)))
]}

@defthing[cldr-main/c chaperone-contract?]{
A contract for functions that return raw data from the @tt{main} section of the CLDR dataset.
Data in the @tt{main} section is per-locale. The contract is defined as:

@racketblock[
(-> string? jsexpr?)
]

The string argument is a locale name.
}

@defthing[cldr/supplemental-c chaperone-contract?]{
A contract for functions that return raw data from the @tt{supplemental} section of the CLDR dataset.
Data in the @tt{supplemental} section is @emph{not} distributed in a per-locale manner. This contract
is defined as:

@racketblock[
(-> jsexpr?)
]}


@section{Canonicalizing Locale Strings}

@defmodule[cldr/likely-subtags]

This module provides a high-level interface to the data in @tt{likelySubtags.json}, described
in the
@link["http://www.unicode.org/reports/tr35/tr35-39/tr35.html#Likely_Subtags"]{CLDR specification}.

@defproc[(locale->available-cldr-locale [locale string?]
                                        [available? (-> string? boolean?)])
         string?]{
Returns the best available CLDR locale string corresponding to the given CLDR string.
Availability is determined by the given @racket[available?] predicate.

@examples[#:eval the-eval
(locale->available-cldr-locale "gd" locale?)
(locale->available-cldr-locale "gd" modern-locale?)
]}

@defproc[(locale->cldr-locale [locale string?])
         cldr-locale?]{
Returns the @racket[cldr-locale] that best matches the given @racket[locale] string.

@examples[#:eval the-eval
(locale->cldr-locale "en")
(locale->cldr-locale "ar")
(locale->cldr-locale "zh")
]}

@defproc[(locale->cldr-language [locale string?]) string?]{
Returns the CLDR language code that best matches the given locale string.
Equivalent to @racket[(cldr-locale-lang (locale->cldr-locale locale))].
}

@defproc[(locale->cldr-region [locale string?]) string?]{
Returns the CLDR region code that best matches the given locale string.
Equivalent to @racket[(cldr-locale-region (locale->cldr-locale locale))].
}

@defproc[(locale->cldr-script [locale string?]) string?]{
Returns the CLDR script code that best matches the given locale string.
Equivalent to @racket[(cldr-locale-script (locale->cldr-locale locale))].
}


@defstruct*[cldr-locale ([lang string?] [script string?] [region string?]) #:transparent]

