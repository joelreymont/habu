\ allow-bound.f - a package whose wordlist id is past the admission bitmap.
\ PBIG is minted after PROT-WID-MAX other wordlists, so its wid has no bit.
\ With an argument it admits PDEP, seals and loads the design; with none it
\ tries to admit PBIG itself.
require lib/policy.f
require test/policy/dep.f

: MANY ( -- ) PROT-WID-MAX 0 do wordlist drop loop ;
MANY

package PBIG
public
: FAR ( -- n ) 5 ;
;package

: RUN ( -- )
   SCRIPT-ARGC 0= if s" PBIG" POLICY:ALLOW exit then
   s" PDEP" POLICY:ALLOW POLICY:SEAL 0 SCRIPT-ARGV$ included ;
RUN
