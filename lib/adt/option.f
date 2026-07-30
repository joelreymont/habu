\ option.f - option<T>: a present value (some) or its absence (none).
\ Returning option<T> instead of a -1 / value+flag sentinel forces every
\ caller to handle the absent case through MATCH at check time.
\ Consumers must `require lib/adt/option.f` before returning or matching one.

ENUM option 1
  VARIANT none ;VARIANT
  VARIANT some FIELD value a ;VARIANT
;ENUM
