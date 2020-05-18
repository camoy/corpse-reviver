#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
	 ffi/winapi)

(provide subresources)

(define _HKEY (_cpointer/null 'HKEY))
(define _LONG _long)
(define _DWORD _int32)
(define _REGSAM _DWORD)
(define _BOOL (make-ctype _int (lambda (v) (if v 1 0)) (lambda (v) (not (zero? v)))))

(define KEY_QUERY_VALUE #x1)
(define KEY_ENUMERATE_SUB_KEYS #x8)

(define ERROR_SUCCESS 0)

(define (const-hkey v)
  (cast (bitwise-ior v (arithmetic-shift -1 32)) _intptr _HKEY))

(define HKEY_CLASSES_ROOT   (const-hkey #x80000000))
(define HKEY_CURRENT_USER   (const-hkey #x80000001))
(define HKEY_LOCAL_MACHINE  (const-hkey #x80000002))
(define HKEY_USERS          (const-hkey #x80000003))
(define HKEY_CURRENT_CONFIG (const-hkey #x80000005))

(define advapi-dll (and (eq? (system-type) 'windows)
                     (ffi-lib "Advapi32.dll")))

(define-ffi-definer define-advapi advapi-dll
  #:default-make-fail make-not-available)

(define-advapi RegOpenKeyExW
  (_fun #:abi winapi
        _HKEY _string/utf-16 _DWORD _REGSAM (hkey : (_ptr o _HKEY))
        -> (r : _LONG)
        -> (and (= r ERROR_SUCCESS) hkey)))

(define-advapi RegCloseKey (_fun #:abi winapi _HKEY -> _LONG))

(define-advapi RegEnumKeyExW
  (_fun #:abi winapi
        _HKEY
        _DWORD
        (name : (_bytes o 256))
        (name-len : (_ptr io _DWORD))
        (_pointer = #f)
        (_pointer = #f)
        (_pointer = #f)
        (_pointer = #f)
        -> (r : _LONG)
        -> (and (= r ERROR_SUCCESS)
                (let*-values
                    ([(cnv) (bytes-open-converter "platform-UTF-16" "platform-UTF-8")]
                     [(bs n _) (bytes-convert cnv name 0 (* 2 name-len))]
                     [(str) (bytes->string/utf-8 bs)])
                  (bytes-close-converter cnv)
                  str))))

(define (section->hkey section)
  (case section
    [("HKEY_CLASSES_ROOT")   HKEY_CLASSES_ROOT]
    [("HKEY_CURRENT_CONFIG") HKEY_CURRENT_CONFIG]
    [("HKEY_CURRENT_USER")   HKEY_CURRENT_USER]
    [("HKEY_LOCAL_MACHINE")  HKEY_LOCAL_MACHINE]
    [("HKEY_USERS")          HKEY_USERS]
    [else (error "bad section")]))


(define (subresources section entry)
  (define hkey (section->hkey section))
  (define sub-hkey (RegOpenKeyExW hkey entry 0 KEY_ENUMERATE_SUB_KEYS))
  (define result
    (for*/list ([i (in-naturals)]
                [key (in-value (RegEnumKeyExW sub-hkey i 256))]
                #:break (not key))
      key))
  (RegCloseKey sub-hkey)
  result)
