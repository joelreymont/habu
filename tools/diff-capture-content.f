\ diff-capture-content.f - fail-closed bulk-content provider contract.

require lib/errors.f
require tools/diff-capture-metadata.f

package DIFF-CONTENT
private

PTR-VARIABLE META-A
variable META-U
PTR-VARIABLE META-PATH-A
variable META-PATH-U
variable PROVIDER-BOUND
variable PROVIDER-ACTIVE

: REQUIRE-ACTIVE ( -- )
   PROVIDER-ACTIVE @ 0= if E-DIFF-CAPTURE throw then ;

defer CONTENT-PROVIDER ( -- )

: MISSING-PROVIDER ( -- )
   E-DIFF-CAPTURE throw ;

: RESET-PROVIDER ( -- )
   [: MISSING-PROVIDER ;] is CONTENT-PROVIDER
   false PROVIDER-BOUND !
   false PROVIDER-ACTIVE ! ;

RESET-PROVIDER

public

: CONFIGURE ( ptr u8 n ptr u8 n -- )
   {: meta:ptr metau:n path:ptr pathu:n :}
   meta META-A ! metau META-U !
   path META-PATH-A ! pathu META-PATH-U ! ;

: CONTENT-PROVIDER! ( [ -- ] -- )
   PROVIDER-ACTIVE @ if E-DIFF-CAPTURE throw then
   is CONTENT-PROVIDER
   true PROVIDER-BOUND ! ;

: RESET ( -- )
   RESET-PROVIDER ;

: PROVIDE ( -- )
   PROVIDER-BOUND @ 0= if E-DIFF-CAPTURE throw then
   true PROVIDER-ACTIVE !
   [: CONTENT-PROVIDER ;] catch {: code:n :}
   false PROVIDER-ACTIVE !
   code 0<> if code throw then
   DIFF-META:COMPLETE? 0= if E-DIFF-CAPTURE throw then ;

: CONTENT-METADATA$ ( -- ptr u8 n )
   REQUIRE-ACTIVE
   META-A @ META-U @ ;

: CONTENT-METADATA-PATH$ ( -- ptr u8 n )
   REQUIRE-ACTIVE
   META-PATH-A @ META-PATH-U @ ;

: CONTENT-ROW-COUNT ( -- n )
   REQUIRE-ACTIVE
   DIFF-META:COUNT ;

: CONTENT-ROW! ( n n bool ptr u8 n n bool ptr u8 n -- )
   REQUIRE-ACTIVE
   DIFF-META:CONTENT! ;

;package
