\ result.f - result<ok,err>: a success value (ok) or an error value (err).
\ Returning result<T,E> instead of a value+rc sentinel forces every caller to
\ MATCH both arms at check time — dropping the error no longer type-checks.
\ Consumers must `require lib/adt/result.f` before returning or matching one.

ENUM result 2
  VARIANT ok  FIELD value a ;VARIANT
  VARIANT err FIELD error b ;VARIANT
;ENUM
