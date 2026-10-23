\ option.f - option<T>: a present value (some) or its absence (none).
\ Returning option<T> instead of a -1 / value+flag sentinel forces every
\ caller to handle the absent case through MATCH at check time.
\ Consumers must `require src/lib/adt/option.f` before returning or matching one.
\ STORAGE CLASS. None: a type declaration only.

ENUM option<a>
  VARIANT none ;VARIANT
  VARIANT some FIELD value a ;VARIANT
;ENUM
