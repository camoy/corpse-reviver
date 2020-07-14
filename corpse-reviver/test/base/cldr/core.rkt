#lang racket/base

(require racket/contract/base
         racket/runtime-path
         racket/set
         json
         "file.rkt")

(define cldr-main/c         (-> string? jsexpr?))
(define cldr-supplemental/c (-> jsexpr?))
(define cldr-key/c          (or/c symbol? string? integer?
                                  (listof (or/c symbol? string? integer?))))

(provide/contract
 [cldr-json         (-> path? string? path? cldr-key/c jsexpr?)]
 [cldr-ref          (->* [jsexpr? cldr-key/c] [any/c] any/c)]
 [available-locales (-> (listof string?))]
 [modern-locales    (-> (listof string?))]
 [all-locales       (-> (listof string?))]
 [modern-locale?    (-> string? boolean?)]
 [locale?           (-> string? boolean?)]
 [raise-locale-not-found (-> string? string? any/c)]
 [cldr-ref*         (->i ([json jsexpr?]
                          #:fail [failure-result any/c])
                         #:rest [keys (listof cldr-key/c)]
                         [result any/c])])

(provide (struct-out exn:cldr)
         (struct-out exn:cldr:locale-not-found)
         cldr-main/c
         cldr-supplemental/c)

(struct exn:cldr exn:fail () #:transparent)
(struct exn:cldr:locale-not-found exn:cldr (locale pkg) #:transparent)

(define (raise-locale-not-found loc pkg)
  (raise (exn:cldr:locale-not-found
          (format "locale \"~a\" is not in package: ~a" loc pkg)
          (current-continuation-marks)
          loc
          pkg)))

(define (available-locales)
  (cldr-json ZIP-PATH PKG "availableLocales.json" 'availableLocales))

(define (modern-locales)
  (cldr-ref (available-locales) 'modern))

(define (all-locales)
  (cldr-ref (available-locales) 'full))

(define (modern-locale? loc)
  (set-member? (modern-locales) loc))

(define (locale? loc)
  (set-member? (all-locales) loc))

(define-syntax-rule (get filename prefix)
  (λ ()
    (cldr-json ZIP-PATH
               PKG
               (build-path "supplemental" filename)
               (cons 'supplemental prefix))))

(define-syntax-rule (defsection name filename path)
  (begin
    (define name (get filename path))
    (provide/contract [name cldr-supplemental/c])))

(defsection aliases                  "aliases.json"                '(metadata alias))
(defsection calendar-data            "calendarData.json"           '(calendarData))
(defsection calendar-preference-data "calendarPreferenceData.json" '(calendarPreferenceData))
(defsection character-fallbacks      "characterFallbacks.json"     '(characters character-fallback))
(defsection code-mappings            "codeMappings.json"           '(codeMappings))
(defsection currency-data            "currencyData.json"           '(currencyData))
(defsection gender                   "gender.json"                 '(gender))
(defsection language-data            "languageData.json"           '(languageData))
(defsection language-matching        "languageMatching.json"       '(languageMatching))
(defsection likely-subtags           "likelySubtags.json"          '(likelySubtags))
(defsection measurement-data         "measurementData.json"        '(measurementData))
(defsection meta-zones               "metaZones.json"              '(metaZones))
(defsection numbering-systems        "numberingSystems.json"       '(numberingSystems))
(defsection ordinals                 "ordinals.json"               '(plurals-type-ordinal))
(defsection parent-locales           "parentLocales.json"          '(parentLocales parentLocale))
(defsection plurals                  "plurals.json"                '(plurals-type-cardinal))
(defsection postal-code-data         "postalCodeData.json"         '(postalCodeData))
(defsection primary-zones            "primaryZones.json"           '(primaryZones))
(defsection references               "references.json"             '(references))
(defsection telephone-code-data      "telephoneCodeData.json"      '(telephoneCodeData))
(defsection territory-containment    "territoryContainment.json"   '(territoryContainment))
(defsection territory-info           "territoryInfo.json"          '(territoryInfo))
(defsection time-data                "timeData.json"               '(timeData))
(defsection week-data                "weekData.json"               '(weekData))
(defsection windows-zones            "windowsZones.json"           '(windowsZones mapTimezones))


(define (modern-locale-set)
  (list->set (modern-locales)))

(define (all-locale-set)
  (list->set (all-locales)))

(define PKG "cldr-core")
(define-runtime-path ZIP-PATH "data/cldr-core.zip")
