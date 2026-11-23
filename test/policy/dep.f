\ dep.f - the package the policy harnesses admit (src/lib/vocabulary-test.f).

package PDEP
private
: HIDDEN ( -- n ) 7 ;
public
: ANSWER ( -- n ) 42 ;
: ZERO ( -- n ) 0 ;
: BIG? ( n -- bool ) 10 > ;
: SHOW ( n -- ) . ;
: NAME-LEN ( str -- n ) str> nip ;
: SHOW-NAME ( str -- ) type ;
: BOOM ( -- ) -30001 throw ;
;package
