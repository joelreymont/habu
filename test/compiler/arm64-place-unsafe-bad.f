\ arm64-place-unsafe-bad.f - a link rooted at PL-MAKE, whose own body calls the
\ kernel's `create`, is refused; test/compiler/arm64-place.f runs it and checks
\ the refusal.

require src/lib/error.f
require src/lib/string.f
require src/lib/le.f
require src/lib/fs.f
require src/lib/ffi-abi.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/abi.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/habu/interpreter.f
require src/arch/arm64/asm.f
require src/habu/address-carrier.f
require src/arch/arm64/link.f
require src/arch/arm64/icode.f
require src/compiler/native/dict.f
require src/arch/arm64/link-kernel.f
require src/habu/link-band.f

package LINKER
private
package ARM64-PLACE-UNSAFE

4096 constant SRC-CAP
SRC-CAP BUFFER: SRC                  \ the fixture's text
variable SRC-U
variable RECS0                       \ shadow records filed before the load


: LOAD-FIXTURE ( -- )
   s" test/compiler/arm64-place-fixture.f" SRC SRC-CAP FS:READ-ALL SRC-U !
   SRC SRC-U @ 0 SRC-U @ INTERPRETER:EVAL-FRAMED-RANGE ;


\ The last shadow row filed since the open whose record is named a u, or -1.
: ROW-NAMED ( ptr u8 n -- n )
   {: a:ptr u:n :}
   -1
   NSHADOW:RECORDS RECS0 @ ?do
      i NSHADOW:RECORD@ XREF-REC XREF-NAME$ a u STR= if drop i then
   loop ;


public

: RUN ( -- )
   INTERPRETER:INSTALL-CREATE
   NABI:BINDING NSHADOW:OPEN-NATIVE
   NSHADOW:RECORDS RECS0 !
   LOAD-FIXTURE
   s" ARM64-PLACE-UNSAFE-ARMED" type cr
   ROUTINES
   s" PL-MAKE" ROW-NAMED NSHADOW:RECORD@ XREF-REC XREF-START ROOT
   s" arm64-place-unsafe: PL-MAKE was not refused" 1 die ;

;package

ARM64-PLACE-UNSAFE:RUN

;package
