\ A public word spelled like admitted design syntax is still a dispatch row;
\ admitting it would give the same token two meanings across tiers.
require src/lib/vocabulary.f

package PSYNTAX
public
: package ( -- n ) 5 ;
;package

: RUN ( -- ) s" PSYNTAX" VOCABULARY:ALLOW-PACKAGE ;
RUN
