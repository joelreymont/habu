\ dep.f - the package the policy harnesses admit (lib/policy-test.f).
\ STEP is public and marked internal, the shape of an engine-owned word an
\ admitted package happens to hold: checked source may not call it.

package PDEP
private
: HIDDEN ( -- n ) 7 ;
TRUSTED: MARK-LAST ( -- ) ndict@ 1- int-mark ;
public
: ANSWER ( -- n ) 42 ;
: ZERO ( -- n ) 0 ;
: BIG? ( n -- bool ) 10 > ;
: SHOW ( n -- ) . ;
: NAME-LEN ( ptr u8 n -- n ) nip ;
: BOOM ( -- ) -30001 throw ;
: STEP ( -- n ) 1 ;
MARK-LAST
;package
