\ prov-good.f - calls a word of the file prov-sib.f includes, and requires
\ prov-sib.f.
require test/fixtures/require-check/prov-sib.f
package RQC-PROV-GOOD
public
: SEVEN ( -- n ) RQC-PROV-WORD:SEVEN ;
;package
