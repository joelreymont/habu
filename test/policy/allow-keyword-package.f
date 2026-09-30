\ allow-keyword-package.f - a package with a public word spelled like a dispatch
\ row cannot be admitted: under the seal tier 0 reads the name as the row and
\ tier 1 as the word.
require lib/policy.f

package PKW
public
: DUP ( n n -- n ) drop ;
;package

: RUN ( -- ) s" PKW" POLICY:ALLOW ;
RUN
