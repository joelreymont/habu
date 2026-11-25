\ prov-bad.f - calls that word too, though only prov-good.f's require
\ loaded prov-sib.f.
require test/fixtures/require-check/prov-good.f
package RQC-PROV-BAD
public
: SEVEN ( -- n ) RQC-PROV-WORD:SEVEN ;
;package
