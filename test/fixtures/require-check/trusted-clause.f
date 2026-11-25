\ trusted-clause.f - a trusted definer's clause calls word.f's word, which only
\ sibling.f's require loaded.
require test/fixtures/require-check/sibling.f
package RQC-TRUSTED-CLAUSE
public
TRUSTED: MADE ( n -- ) create , does> ( -- n ) @ RQC-WORD:SEVEN + ;
;package
