\ inc-main.f - calls m.f's word, which inc.f's require grants it.
require sib.f
include inc.f
: MAIN ( -- ) RQC-WARM-M:SEVEN drop ;
