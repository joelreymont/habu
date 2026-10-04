\ publish.f - commit one sealed native emission to its pending dictionary record.
\ Every refusal precedes the code window; the commit phase only copies accepted
\ bytes, records relocation sites, and publishes the record. The emission is
\ read through its owned artifact, in bytes; nothing here decodes instructions.
\
\ THE CODE REGION IS THE RUNNING ENGINE'S, so it takes only the instructions of
\ the machine NABI:BINDING names: an emission any other backend sealed is
\ refused first. A shadow target's routine for the same definition is filed
\ with the record (src/compiler/native/shadow.f), past the window, because the
\ shadow refused whatever it could before publication began.

require lib/prelude.f
require lib/errors.f
require src/core/does-clause.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/native/abi.f
require src/compiler/native/dict.f
require src/compiler/session/emission.f
require src/compiler/native/shadow.f
require src/habu/code-span.f

package NPUB

private

$4000 constant CODE-RESERVE
TYPED-VARIABLE UNIT-OBSERVER [ NART:emission n n n -- ]
variable UNIT-ARMED

\ Inside NPUB the publication primitives bind this package's private rows
\ (src/habu/prims.f), so their callers here are checked. Anywhere else a
\ checked caller is refused by name: code-publish, xref-retarget and
\ does-record are undefined there, and the global rows of callmap-set and
\ addrmap-set are trusted-only.
: CODE-WINDOW ( ptr u8 n n -- )
   code-publish ;

: RELOC-EXTERNAL ( n -- )
   callmap-set ;

: RELOC-ADDR ( n -- )
   addrmap-set ;

: PUBLISH-REC ( n n n -- )
   xref-retarget ;

\ min-in-mark and ndict-append are seed records the engine marks internal
\ (ENGINE-PRIMS:GLOBAL-INT-WID), and only a TRUSTED: body compiles a call to one.
TRUSTED: MIN-IN-REC ( n n -- )
   min-in-mark ;

: DOES-RECORD ( n n -- )
   does-record ;

TRUSTED: APPEND-PENDING ( n -- )
   ndict-append ;

: CODE-CEILING ( -- n )
   dbase@ REGION + CODE-RESERVE - ;

: ROOM-CK ( n -- n ) {: size:n :}
   cp@ {: fn:n :}
   fn size + CODE-CEILING > if E-NPUB-ROOM throw then
   fn ;

: PLACE-CK ( NART:emission n -- )
   {: e:NART:emission fn:n :}
   e NART:PLACED? 0= if exit then
   e NART:PLACEMENT fn <> if E-NPUB-PLACE throw then ;

: EXTERNAL? ( n -- bool ) {: t:n :}
   t dbase@ < if true exit then
   t dbase@ REGION + >= ;

\ A tail branch out of the region cannot be relocated by the call map. The
\ emission lies inside the region, so a branch to outside it leaves the emission.
: TAIL-RELOC-CK ( NART:emission -- )
   {: e:NART:emission :}
   e NART:CALL-SITES 0 ?do
      e i NART:CALL-KIND@ NEMIT:TAIL = if
         e i NART:CALL-TARGET@ EXTERNAL? if E-NPUB-RELOC throw then
      then
   loop ;

: RELOC-CALLS ( NART:emission n -- )
   {: e:NART:emission fn:n :}
   e NART:CALL-SITES 0 ?do
      e i NART:CALL-KIND@ NEMIT:CALL = if
         e i NART:CALL-TARGET@ EXTERNAL? if
            fn e i NART:CALL-SITE@ + RELOC-EXTERNAL
         then
      then
   loop ;

: RELOC-ADDRS ( NART:emission n -- )
   {: e:NART:emission fn:n :}
   e NART:ADDR-SITES 0 ?do
      fn e i NART:ADDR-SITE@ + RELOC-ADDR
   loop ;

\ Legacy records omit the trailing return. An emission with none explicitly
\ records its whole span, so a reader never borrows the next record's bytes.
: RECORDED-LEN ( NART:emission n -- n )
   {: e:NART:emission size:n :}
   e NART:RET-BYTES {: ret:n :}
   ret 0= if size CODE-SPAN:EXACT exit then
   size ret - ;

: VALIDATE-EMISSION ( NART:emission n -- n )
   {: e:NART:emission size:n :}
   size ROOM-CK {: fn:n :}
   e fn PLACE-CK
   e TAIL-RELOC-CK
   fn ;

: COMMIT ( NART:emission n n n -- )
   {: e:NART:emission idx:n fn:n size:n :}
   e NART:BYTES fn size CODE-WINDOW
   e fn RELOC-CALLS
   e fn RELOC-ADDRS
   fn e size RECORDED-LEN idx PUBLISH-REC ;

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

: TARGET-CK ( NART:emission -- )
   NART:ARCH NABI:BINDING CBIND:TARGET@ CTARGET:ARCH@ CTARGET-ARCH:EQ
   0= if E-NPUB-TARGET throw then ;

: PENDING-PROVE ( NART:emission -- n n n )
   {: e:NART:emission :}
   e TARGET-CK
   e NART:SIZE {: size:n :}
   PENDING-IDX {: idx:n :}
   idx PENDING-CK
   e size VALIDATE-EMISSION {: fn:n :}
   idx fn size ;

: UNIT-NOTIFY ( NART:emission n n n -- )
   {: e:NART:emission idx:n fn:n size:n :}
   UNIT-ARMED @ if e idx fn size UNIT-OBSERVER @ execute then ;

\ Publishing the checker's one-shot minimum-input latch is engine authority.
\ Keep the boundary at the native publisher that consumes it for this record.
TRUSTED: PENDING-FACTS ( n -- ) {: idx:n :}
   CHECKER-OWNER:WIDE-PUBLISH
   CHECKER-OWNER:MIN-IN {: mi:n :}
   mi 0<> if idx mi MIN-IN-REC then ;

: PAD-INSTRUCTION ( n -- n )
   3 + -4 and ;

: DOES-NAME-PAD ( n -- n )
   XREF-REC XREF-NAME$ nip DOES-CLAUSE:SUFFIX$ nip + PAD-INSTRUCTION ;

: DOES-PROVE ( NART:emission n -- n n n n )
   {: e:NART:emission fun:n :}
   e PENDING-PROVE {: idx:n fn:n size:n :}
   idx 1+ DICT-CAP >= if E-NPUB-PENDING throw then
   idx DOES-NAME-PAD {: pad:n :}
   fn size + pad + CODE-CEILING > if E-NPUB-ROOM throw then
   e fun NART:FUNCTION-OFFSET@ {: off:n :}
   off 0 <= if E-NPUB-OFFSET throw then
   idx fn size off ;

public

: PUBLISH-PENDING ( NART:emission -- )
   {: e:NART:emission :}
   e PENDING-PROVE {: idx:n fn:n size:n :}
   e idx fn size UNIT-NOTIFY
   e idx fn size COMMIT
   idx NSHADOW:PUBLISH
   idx APPEND-PENDING
   idx PENDING-FACTS ;

: PUBLISH-PENDING-DOES ( NART:emission n -- )
   {: e:NART:emission fun:n :}
   e fun DOES-PROVE {: idx:n fn:n size:n off:n :}
   e idx fn size UNIT-NOTIFY
   e idx fn size COMMIT
   idx NSHADOW:PUBLISH
   fn off + e size off - RECORDED-LEN DOES-RECORD
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

: WITH-UNIT ( [ NART:emission n n n -- ] [ -- ] -- )
   {: observer q :}
   UNIT-ARMED @ if E-NPUB-PENDING throw then
   observer UNIT-OBSERVER !
   1 UNIT-ARMED !
   q catch {: rc:n :}
   0 UNIT-ARMED !
   rc 0<> if rc throw then ;
get-current prot-wid-add

;package
