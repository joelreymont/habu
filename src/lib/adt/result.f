\ result.f - result<ok,err>: a success value (ok) or an error value (err).
\ Returning result<T,E> instead of a value+rc sentinel makes every caller MATCH
\ both arms at check time.
\ STORAGE CLASS. None: a type declaration only.

ENUM result<a,b>
  VARIANT ok  FIELD value a ;VARIANT
  VARIANT err FIELD error b ;VARIANT
;ENUM
