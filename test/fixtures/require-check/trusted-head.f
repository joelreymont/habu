\ trusted-head.f - a trusted definer calls word.f's word, which only
\ sibling.f's require loaded, before its does>.
require test/fixtures/require-check/sibling.f
package RQC-TRUSTED-HEAD
public
TRUSTED: MADE ( n -- ) RQC-WORD:SEVEN + create , does> ( -- n ) @ ;
;package
