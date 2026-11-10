\ seal-second.f - the sealed program's second file: its package gains two words,
\ an export, a does> child, a variable and a private word, which a link of this
\ file's object replays, then is sealed.

require test/fixtures/file-objects/seal-first.f

: MK ( n -- ) create , does> ( -- n ) @ ;
package FILE-OBJECTS-SEAL
public
: B ( -- ) s" B ran" type cr ;
EXPORT FILE-OBJECTS-SEAL-OTHER:Z
7 MK K
variable V
private
: P ( -- ) s" P ran" type cr ;
public
: C ( -- ) P ;
seal-package
;package
