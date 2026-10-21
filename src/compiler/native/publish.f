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

require src/lib/prelude.f
require src/lib/errors.f
require src/core/does-clause.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/native/abi.f
require src/compiler/native/dict.f
require src/compiler/session/emission.f
require src/compiler/native/shadow.f
require src/compiler/native/host-publish.f
require src/habu/code-span.f

package NPUB

private

$4000 constant CODE-RESERVE
TYPED-VARIABLE UNIT-OBSERVER [ NART:emission n n n -- ]
TYPED-VARIABLE UNIT-PRIOR-OBSERVER [ NART:emission n n n -- ]
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
\ (ENGINE-PRIMS:GLOBAL-INT-WID); package NPUB's own rows (src/habu/prims.f)
\ admit the calls here.
: MIN-IN-REC ( n n -- )
   min-in-mark ;

: WIDE-REC ( -- )
   wide-mark ;

: DOES-RECORD ( n n -- )
   does-record ;

: APPEND-PENDING ( n -- )
   ndict-append ;

\ A prepared package unit (tools/native-unit-object.f): the build host has
\ relocated its code and dictionary rows and names each relocation site as a
\ unit-relative byte offset. The publication itself is this owner's alone.
: PUBLISH-UNIT ( ptr u8 n ptr u8 n -- )
   native-unit-publish ;

: IMPORT-UNIT ( ptr u8 n ptr u8 n ptr n n ptr n n -- )
   {: code:ptr codeu:n recs:ptr recn:n calls:ptr callu:n addrs:ptr addru:n :}
   \ Without code there are no relocation sites. Namespace rows still publish;
   \ a file with only DATA effects has no dictionary span either.
   codeu 0= if
      callu 0<> addru 0<> or if E-NPUB-RELOC throw then
      recn 0= if exit then
   then
   cp@ {: base:n :}
   code codeu recs recn PUBLISH-UNIT
   callu 0 ?do calls i cells + @ base + RELOC-EXTERNAL loop
   addru 0 ?do addrs i cells + @ base + RELOC-ADDR loop ;

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

\ The ARM call map needs only calls out of REGION. A stripped x86 image packs
\ reachable bodies at new addresses, so every recorded E8 call or E9 tail
\ branch must move with it, including calls between REGION words.
: RELOC-CALLS ( NART:emission n -- )
   {: e:NART:emission fn:n :}
   e NART:ARCH CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ {: x64:bool :}
   e NART:CALL-SITES 0 ?do
      e i NART:CALL-KIND@ {: kind:n :}
      x64 if
         kind NEMIT:CALL = kind NEMIT:TAIL = or
      else
         kind NEMIT:CALL =
         e i NART:CALL-TARGET@ EXTERNAL? and
      then if
         fn e i NART:CALL-SITE@ + RELOC-EXTERNAL
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

: PLACE ( NART:emission n n -- ) {: e:NART:emission fn:n size:n :}
   e NART:BYTES fn size CODE-WINDOW
   e fn RELOC-CALLS
   e fn RELOC-ADDRS ;

: COMMIT ( NART:emission n n n -- )
   {: e:NART:emission idx:n fn:n size:n :}
   e fn size PLACE
   fn e size RECORDED-LEN idx PUBLISH-REC ;

TYPED-VARIABLE HELD-E NART:emission
variable HELD-IDX
variable HELD-FN
variable HELD-SIZE
variable HELD-DEFER                     \ a defer body's dispatch cell, or 0

\ A defer record's span ends at the trailer just past its code slot.
2 cells constant TRAILER-BYTES
create TRAILER TRAILER-BYTES allot

: TRAILER-PLACE ( n n -- ) {: cell:n slot:n :}
   DEFER-MAGIC TRAILER !
   cell TRAILER CELL + !
   TRAILER slot TRAILER-BYTES CODE-WINDOW ;

\ A defer body's record ends at its trailer, which names the dispatch cell the
\ body jumps through (layout.f DEFER-MAGIC), so the span is the slot and not the
\ emission's return-trimmed length.
: COMMIT-DEFER ( NART:emission n n n n -- )
   {: e:NART:emission idx:n fn:n size:n cell:n :}
   e fn size PLACE
   cp@ {: slot:n :}
   fn slot fn - CODE-SPAN:EXACT idx PUBLISH-REC
   cell slot TRAILER-PLACE ;

: UNIT-NOTIFY ( NART:emission n n n -- )
   {: e:NART:emission idx:n fn:n size:n :}
   UNIT-ARMED @ if e idx fn size UNIT-OBSERVER @ execute then ;

: COMMIT-HELD ( -- )
   HELD-DEFER @ 0<> if
      HELD-E @ HELD-IDX @ HELD-FN @ HELD-SIZE @ HELD-DEFER @ COMMIT-DEFER exit
   then
   HELD-E @ HELD-IDX @ HELD-FN @ HELD-SIZE @ COMMIT ;

\ The unit observer is the last callback; checker writes follow the commit.
: NOTIFY-HELD ( -- )
   HELD-E @ HELD-IDX @ HELD-FN @ HELD-SIZE @ UNIT-NOTIFY
   0 CHECKER-OWNER:WRITE-WINDOW ;

: COMMIT-OWNED-BODY ( -- )
   [: NOTIFY-HELD ;] catch {: notify-rc:n :}
   notify-rc 0<> if NHOST:ABANDON-TAKEN notify-rc throw then
   [: COMMIT-HELD ;] catch {: rc:n :}
   rc 0<> if NHOST:ABANDON-TAKEN rc throw then
   NHOST:PUBLISH-TAKEN ;

: COMMIT-OWNED ( NART:emission n n n -- )
   {: e:NART:emission idx:n fn:n size:n :}
   e fn NHOST:TAKE
   e HELD-E !  idx HELD-IDX !  fn HELD-FN !  size HELD-SIZE !
   [: COMMIT-OWNED-BODY ;] [: 0 HELD-FN !  0 HELD-DEFER ! ;] finally ;

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

\ Publishing the checker's one-shot minimum-input latch is engine authority.
\ Keep the boundary at the native publisher that consumes it for this record.
: PENDING-FACTS ( n -- ) {: idx:n :}
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

\ Commit one pending body; callers decide whether to consume checker facts.
: PUBLISH-CODE ( NART:emission -- n )
   {: e:NART:emission :}
   e PENDING-PROVE {: idx:n fn:n size:n :}
   e fn NSHADOW:SOURCE-FUNS
   e idx fn size COMMIT-OWNED
   idx NSHADOW:PUBLISH
   idx APPEND-PENDING
   idx ;

: HOOKED? ( -- bool )
   data-base HOOK-CELL + @ 0<> ;

public

: PUBLISH-PENDING ( NART:emission -- )
   PUBLISH-CODE PENDING-FACTS ;

: PUBLISH-RAW ( NART:emission -- )
   PUBLISH-CODE drop ;

: PUBLISH-PENDING-DEFER ( NART:emission n -- )
   {: e:NART:emission cell:n :}
   e PENDING-PROVE {: idx:n fn:n size:n :}
   fn size + TRAILER-BYTES + CODE-CEILING > if E-NPUB-ROOM throw then
   e fn NSHADOW:SOURCE-FUNS
   cell HELD-DEFER !
   e idx fn size COMMIT-OWNED
   idx NSHADOW:PUBLISH
   idx APPEND-PENDING
   HOOKED? if idx PENDING-FACTS then ;

: PUBLISH-PENDING-DOES ( NART:emission n -- )
   {: e:NART:emission fun:n :}
   e fun DOES-PROVE {: idx:n fn:n size:n off:n :}
   e fn NSHADOW:SOURCE-FUNS
   e idx fn size COMMIT-OWNED
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

\ A prepared definition reuses the interpreter's pending row.  Its name and
\ wordlist were already written when the source definition opened.  All
\ references and implementation facts have been prepared before this call.
: IMPORT-PENDING ( ptr u8 n n ptr n n ptr n n bool n -- n )
   {: code:ptr size:n recorded:n calls:ptr calln:n
      addrs:ptr addrn:n wide:bool minin:n :}
   PENDING-IDX {: idx:n :}
   idx PENDING-CK
   size 0 <= size 3 and 0<> or if E-NPUB-RELOC throw then
   recorded CODE-SPAN:VALID? 0= if E-NPUB-RELOC throw then
   recorded CODE-SPAN:BYTES size <> if E-NPUB-RELOC throw then
   minin 0 < minin 255 > or if E-NPUB-RELOC throw then
   calln 0 < addrn 0 < or if E-NPUB-RELOC throw then
   calln 0 ?do
      calls i cells + @ {: site:n :}
      site 0 < site 3 and 0<> or
      site size 4 - > or if E-NPUB-RELOC throw then
   loop
   addrn 0 ?do
      addrs i cells + @ {: site:n :}
      site 0 < site 3 and 0<> or
      site size 4 - > or if E-NPUB-RELOC throw then
   loop
   size ROOM-CK {: fn:n :}
   code fn size CODE-WINDOW
   calln 0 ?do fn calls i cells + @ + RELOC-EXTERNAL loop
   addrn 0 ?do fn addrs i cells + @ + RELOC-ADDR loop
   fn recorded idx PUBLISH-REC
   idx APPEND-PENDING
   wide if WIDE-REC then
   minin 0<> if idx minin MIN-IN-REC then
   NHOST:PUBLISH-TAKEN
   idx ;

private
get-current prot-wid-add

public

: IMPORT-FILE-UNIT ( ptr u8 n ptr u8 n ptr n n ptr n n -- )
   IMPORT-UNIT ;

: WITH-UNIT ( [ NART:emission n n n -- ] [ -- ] -- )
   {: observer q :}
   UNIT-ARMED @ if E-NPUB-PENDING throw then
   UNIT-OBSERVER @ UNIT-PRIOR-OBSERVER !
   observer UNIT-OBSERVER !
   1 UNIT-ARMED !
   q catch {: rc:n :}
   0 UNIT-ARMED !
   UNIT-PRIOR-OBSERVER @ UNIT-OBSERVER !
   rc 0<> if rc throw then ;
get-current prot-wid-add

;package
