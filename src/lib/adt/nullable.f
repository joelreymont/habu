\ nullable.f - nullable<a>: an address of an `a` (some) or no address (none).
\ The value is one cell: the address for some, zero for none, so zero-filled
\ storage reads as none. MATCH is the only way to the address.
\ STORAGE CLASS. None: a type declaration only.

ENUM nullable<a> POLICY niche-null
  VARIANT none ;VARIANT
  VARIANT some FIELD value ptr a ;VARIANT
;ENUM
