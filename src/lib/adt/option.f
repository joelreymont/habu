\ option.f - option<T>: a present value (some) or its absence (none).
\ Returning option<T> instead of a -1 / value+flag sentinel makes every caller
\ handle the absent case through MATCH at check time.
\ STORAGE CLASS. None: a type declaration only.

ENUM option<a>
  VARIANT none ;VARIANT
  VARIANT some FIELD value a ;VARIANT
;ENUM
