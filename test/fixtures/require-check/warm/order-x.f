\ order-x.f - calls m.f's word before its own require of m.f, which
\ order-e.f's require grants it.
include order-e.f
: MAIN ( -- ) RQC-WARM-M:SEVEN drop ;
require m.f
