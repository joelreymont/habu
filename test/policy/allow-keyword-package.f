\ allow-keyword-package.f - a package with a public word spelled like a dispatch
\ row cannot be admitted: under the seal tier 0 would read the name as the row,
\ not the word.
require src/lib/vocabulary.f

package PKW
public
: DUP ( n n -- n ) drop ;
;package

: RUN ( -- ) s" PKW" VOCABULARY:ALLOW-PACKAGE ;
RUN
