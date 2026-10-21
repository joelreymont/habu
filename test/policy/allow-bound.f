\ allow-bound.f - a package whose wordlist id is past the admission bitmap.
\ PBIG is minted after PROT-WID-MAX other wordlists, so its wid has no bit.
\ With an argument it admits PDEP, seals and loads the design; with none it
\ tries to admit PBIG itself.
require src/lib/vocabulary.f
require test/policy/dep.f
using VOCABULARY

: MANY ( -- ) PROT-WID-MAX 0 do wordlist drop loop ;
MANY

package PBIG
public
: FAR ( -- n ) 5 ;
;package

: RUN ( -- )
   SCRIPT-ARGC 0= if s" PBIG" ALLOW-PACKAGE exit then
   s" PDEP" ALLOW-PACKAGE RESTRICT 0 SCRIPT-ARGV$ included ;
RUN
;using
