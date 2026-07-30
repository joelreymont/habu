\ result.f - result<ok,err>: a success value (ok) or an error value (err).
\ Returning result<T,E> instead of a value+rc sentinel forces every caller to
\ MATCH both arms at check time — dropping the error no longer type-checks.
\ Consumers must `require lib/adt/result.f` before returning or matching one.

SUMTYPE result 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
