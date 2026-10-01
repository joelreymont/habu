\ publish.f - commit one sealed native emission to its pending dictionary record.
\ Every refusal precedes the code window; the commit phase only copies accepted
\ bytes, records relocation sites, and publishes the record. The emission is
\ read through NEMIT alone, in bytes, so nothing here decodes an instruction.
\
\ THE CODE REGION IS THE RUNNING ENGINE'S, so it takes only the instructions of
\ the machine NABI:BINDING names: an emission any other backend sealed is
\ refused first. A shadow target's routine for the same definition is filed
\ with the record (src/compiler/native/shadow.f), past the window, because the
\ shadow refused whatever it could before publication began.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/native/abi.f
require src/compiler/native/dict.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/habu/code-span.f

package NPUB

private

$4000 constant CODE-RESERVE
TYPED-VARIABLE UNIT-OBSERVER [ n n n -- ]
variable UNIT-ARMED

TRUSTED: CODE-WINDOW ( ptr u8 n n -- )
   code-publish ;

TRUSTED: RELOC-EXTERNAL ( n -- )
   callmap-set ;

TRUSTED: RELOC-ADDR ( n -- )
   addrmap-set ;

TRUSTED: PUBLISH-REC ( n n n -- )
   xref-retarget ;

TRUSTED: MIN-IN-REC ( n n -- )
   min-in-mark ;

TRUSTED: DOES-RECORD ( n n -- )
   does-record ;

TRUSTED: APPEND-PENDING ( n -- )
   ndict-append ;

: CODE-CEILING ( -- n )
   dbase@ REGION + CODE-RESERVE - ;

: ROOM-CK ( n -- n ) {: size:n :}
   cp@ {: fn:n :}
   fn size + CODE-CEILING > if E-NPUB-ROOM throw then
   fn ;

: PLACE-CK ( n -- ) {: fn:n :}
   NEMIT:PLACED? 0= if exit then
   NEMIT:PLACEMENT fn <> if E-NPUB-PLACE throw then ;

: EXTERNAL? ( n -- bool ) {: t:n :}
   t dbase@ < if true exit then
   t dbase@ REGION + >= ;

\ A tail branch out of the region cannot be relocated by the call map. The
\ emission lies inside the region, so a branch to outside it leaves the emission.
: TAIL-RELOC-CK ( -- )
   NEMIT:CALL-SITES 0 ?do
      i NEMIT:CALL-KIND@ NEMIT:TAIL = if
         i NEMIT:CALL-TARGET@ EXTERNAL? if E-NPUB-RELOC throw then
      then
   loop ;

: RELOC-CALLS ( n -- ) {: fn:n :}
   NEMIT:CALL-SITES 0 ?do
      i NEMIT:CALL-KIND@ NEMIT:CALL = if
         i NEMIT:CALL-TARGET@ EXTERNAL? if
            fn i NEMIT:CALL-SITE@ + RELOC-EXTERNAL
         then
      then
   loop ;

: RELOC-ADDRS ( n -- ) {: fn:n :}
   NEMIT:ADDR-SITES 0 ?do
      fn i NEMIT:ADDR-SITE@ + RELOC-ADDR
   loop ;

\ Legacy records omit the trailing return. An emission with none explicitly
\ records its whole span, so a reader never borrows the next record's bytes.
: RECORDED-LEN ( n -- n ) {: size:n :}
   NEMIT:RET-BYTES {: ret:n :}
   ret 0= if size CODE-SPAN:EXACT exit then
   size ret - ;

: VALIDATE-EMISSION ( n -- n ) {: size:n :}
   size ROOM-CK {: fn:n :}
   fn PLACE-CK
   TAIL-RELOC-CK
   fn ;

: COMMIT ( n n n -- ) {: idx:n fn:n size:n :}
   NEMIT:BYTES fn size CODE-WINDOW
   fn RELOC-CALLS
   fn RELOC-ADDRS
   fn  size RECORDED-LEN  idx PUBLISH-REC ;

: PENDING-IDX ( -- n )
   ndict@ ;

: PENDING-CK ( n -- ) {: idx:n :}
   idx PENDING-IDX <> if E-NPUB-PENDING throw then
   idx XREF-REC XREF-WORDLIST {: wid:n :}
   wid XREF-RETIRED-WL = if E-NPUB-PENDING throw then
   wid XREF-NAMESPACE-WL = if E-NPUB-PENDING throw then
   idx XREF-REC XREF-FLAGS {: f:n :}
   f DNAME-INT and 0<> if E-NPUB-PENDING throw then
   f DNAME-IMM and 0<> if E-NPUB-PENDING throw then ;

: TARGET-CK ( -- )
   NEMIT:ARCH  NABI:BINDING CBIND:TARGET@ CTARGET:ARCH@  CTARGET-ARCH:EQ
   0= if E-NPUB-TARGET throw then ;

: PENDING-PROVE ( -- n n n )
   TARGET-CK
   NEMIT:SIZE {: size:n :}
   PENDING-IDX {: idx:n :}
   idx PENDING-CK
   size VALIDATE-EMISSION {: fn:n :}
   idx fn size ;

: UNIT-NOTIFY ( n n n -- )
   UNIT-ARMED @ if UNIT-OBSERVER @ execute else drop drop drop then ;

\ Publishing the checker's one-shot minimum-input latch is engine authority.
\ Keep the boundary at the native publisher that consumes it for this record.
TRUSTED: PENDING-FACTS ( n -- ) {: idx:n :}
   CHECKER-OWNER:WIDE-PUBLISH
   CHECKER-OWNER:MIN-IN {: mi:n :}
   mi 0<> if idx mi MIN-IN-REC then ;

5 constant DOES-SUFFIX-BYTES

: PAD-INSTRUCTION ( n -- n )
   3 + -4 and ;

: DOES-NAME-PAD ( n -- n )
   XREF-REC XREF-NAME$ nip DOES-SUFFIX-BYTES + PAD-INSTRUCTION ;

: DOES-PROVE ( n -- n n n n ) {: fun:n :}
   PENDING-PROVE {: idx:n fn:n size:n :}
   idx 1+ DICT-CAP >= if E-NPUB-PENDING throw then
   idx DOES-NAME-PAD {: pad:n :}
   fn size + pad + CODE-CEILING > if E-NPUB-ROOM throw then
   fun NEMIT:FUNCTION-OFFSET@ {: off:n :}
   off 0 <= if E-NPUB-OFFSET throw then
   idx fn size off ;

public

: PUBLISH-PENDING ( -- )
   PENDING-PROVE {: idx:n fn:n size:n :}
   idx fn size UNIT-NOTIFY
   idx fn size COMMIT
   idx NSHADOW:PUBLISH
   idx APPEND-PENDING
   idx PENDING-FACTS ;

: PUBLISH-PENDING-DOES ( n -- ) {: fun:n :}
   fun DOES-PROVE {: idx:n fn:n size:n off:n :}
   idx fn size UNIT-NOTIFY
   idx fn size COMMIT
   idx NSHADOW:PUBLISH
   fn off +  size off - RECORDED-LEN  DOES-RECORD
   idx APPEND-PENDING
   idx XREF-REC XREF-NAME$ true CHECKER-OWNER:DOES-FINISH
   idx PENDING-FACTS
   idx 1+ APPEND-PENDING
   CHECKER-OWNER:DOES-COMMIT ;

: NEXT-SLOT ( -- n )
   cp@ ;

: IN-REGION? ( n -- bool )
   EXTERNAL? 0= ;

private
get-current prot-wid-add

public

: WITH-UNIT ( [ n n n -- ] [ -- ] -- ) {: observer q :}
   UNIT-ARMED @ if E-NPUB-PENDING throw then
   observer UNIT-OBSERVER !
   1 UNIT-ARMED !
   q catch {: rc:n :}
   0 UNIT-ARMED !
   rc 0<> if rc throw then ;
get-current prot-wid-add

;package
