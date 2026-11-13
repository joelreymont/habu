\ publish.f - commit one sealed native emission to its pending dictionary record.
\ Every refusal precedes the code window; the commit only copies accepted bytes,
\ records relocation sites and publishes the record. The code region is the
\ running engine's: an emission sealed for any machine but NABI:BINDING's is
\ refused first. A shadow target's routine is filed with the record past the
\ window (src/compiler/native/shadow.f).

require src/lib/prelude.f
require src/lib/error.f
require src/core/does-clause.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/native/abi.f
require src/compiler/native/dict.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/compiler/native/host-publish.f
require src/habu/code-span.f

package NPUB

private

$4000 constant CODE-RESERVE

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

: APPEND-PENDING ( n -- )
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

\ The ARM call map needs only calls out of REGION. A stripped x86 image packs
\ reachable bodies at new addresses, so every recorded E8 call or E9 tail
\ branch must move with it, including calls between REGION words.
: RELOC-CALLS ( n -- ) {: fn:n :}
   NEMIT:ARCH CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ {: x64:bool :}
   NEMIT:CALL-SITES 0 ?do
      i NEMIT:CALL-KIND@ {: kind:n :}
      x64 if
         kind NEMIT:CALL = kind NEMIT:TAIL = or
      else
         kind NEMIT:CALL =
         i NEMIT:CALL-TARGET@ EXTERNAL? and
      then if
         fn i NEMIT:CALL-SITE@ + RELOC-EXTERNAL
      then
   loop ;

: RELOC-ADDRS ( n -- ) {: fn:n :}
   NEMIT:ADDR-SITES 0 ?do
      fn i NEMIT:ADDR-SITE@ + RELOC-ADDR
   loop ;

: VALIDATE-EMISSION ( n -- n ) {: size:n :}
   size ROOM-CK {: fn:n :}
   fn PLACE-CK
   TAIL-RELOC-CK
   fn ;

: PLACE ( n n -- ) {: fn:n size:n :}
   NEMIT:BYTES fn size CODE-WINDOW
   fn RELOC-CALLS
   fn RELOC-ADDRS ;

: COMMIT ( n n n -- ) {: idx:n fn:n size:n :}
   fn size PLACE
   fn size NEMIT:RET-BYTES CODE-SPAN:RECORDED idx PUBLISH-REC ;

\ One routine's commit, held where COMMIT-OWNED's quotations read it. Compiler
\ working storage, as NHOST's TAKEN-* are: an image carries none of it.
0 CWORK-VARIABLE HELD-IDX n
0 CWORK-VARIABLE HELD-FN n
0 CWORK-VARIABLE HELD-SIZE n
0 CWORK-VARIABLE HELD-DEFER n           \ a defer body's dispatch cell, or 0

2 cells constant TRAILER-BYTES
create TRAILER TRAILER-BYTES allot

: TRAILER-PLACE ( n n -- ) {: cell:n slot:n :}
   DEFER-MAGIC TRAILER !
   cell TRAILER CELL + !
   TRAILER slot TRAILER-BYTES CODE-WINDOW ;

\ A defer body's record ends at its trailer, which names the dispatch cell the
\ body jumps through (layout.f DEFER-MAGIC), so the span is the slot and not the
\ emission's return-trimmed length.
: COMMIT-DEFER ( n n n n -- ) {: idx:n fn:n size:n cell:n :}
   fn size PLACE
   cp@ {: slot:n :}
   fn slot fn - CODE-SPAN:EXACT idx PUBLISH-REC
   cell slot TRAILER-PLACE ;

: COMMIT-HELD ( -- )
   HELD-DEFER @ 0<> if
      HELD-IDX @ HELD-FN @ HELD-SIZE @ HELD-DEFER @ COMMIT-DEFER exit
   then
   HELD-IDX @ HELD-FN @ HELD-SIZE @ COMMIT ;

\ A take TAKE refuses is its own to abandon; one it made is abandoned here.
: COMMIT-OWNED-BODY ( -- )
   0 CHECKER-OWNER:WRITE-WINDOW
   HELD-FN @ NHOST:TAKE
   [: COMMIT-HELD ;] catch {: rc:n :}
   rc 0<> if NHOST:ABANDON-TAKEN rc throw then
   NHOST:PUBLISH-TAKEN ;

\ The held routine is cleared on every exit, so a refused commit leaves nothing
\ the next one could read.
: COMMIT-OWNED ( n n n n -- ) {: idx:n fn:n size:n cell:n :}
   idx HELD-IDX !  fn HELD-FN !  size HELD-SIZE !  cell HELD-DEFER !
   [: COMMIT-OWNED-BODY ;]
   [: 0 HELD-IDX !  0 HELD-FN !  0 HELD-SIZE !  0 HELD-DEFER ! ;] finally ;

: PENDING-IDX ( -- n )
   ndict@ ;

: PENDING-CK ( n -- ) {: idx:n :}
   idx PENDING-IDX <> if E-NPUB-PENDING throw then
   idx XREF-REC XREF-WORDLIST {: wid:n :}
   wid 0 < if E-NPUB-PENDING throw then           \ a package or retired row
   idx XREF-REC XREF-FLAGS {: f:n :}
   f DNAME-INT and 0<> if E-NPUB-PENDING throw then
   f DNAME-IMM and 0<> if E-NPUB-PENDING throw then ;

: TARGET-CK ( -- )
   NEMIT:ARCH NABI:BINDING CBIND:TARGET@ CTARGET:ARCH@ CTARGET-ARCH:EQ
   0= if E-NPUB-TARGET throw then ;

: PENDING-PROVE ( -- n n n )
   TARGET-CK
   NEMIT:SIZE {: size:n :}
   PENDING-IDX {: idx:n :}
   idx PENDING-CK
   size VALIDATE-EMISSION {: fn:n :}
   idx fn size ;

\ Publishing the checker's one-shot minimum-input latch is engine authority,
\ kept at the native publisher that consumes it for this record.
: PENDING-FACTS ( n -- ) {: idx:n :}
   CHECKER-OWNER:WIDE-PUBLISH
   CHECKER-OWNER:MIN-IN {: mi:n :}
   mi 0<> if idx mi MIN-IN-REC then ;

: PAD-INSTRUCTION ( n -- n )
   3 + -4 and ;

: DOES-NAME-PAD ( n -- n )
   XREF-REC XREF-NAME$ nip DOES-CLAUSE:SUFFIX$ nip + PAD-INSTRUCTION ;

: DOES-PROVE ( n -- n n n n ) {: fun:n :}
   PENDING-PROVE {: idx:n fn:n size:n :}
   idx 1+ DICT-CAP >= if E-NPUB-PENDING throw then
   idx DOES-NAME-PAD {: pad:n :}
   fn size + pad + CODE-CEILING > if E-NPUB-ROOM throw then
   fun NEMIT:FUNCTION-OFFSET@ {: off:n :}
   off 0 <= if E-NPUB-OFFSET throw then
   idx fn size off ;

\ Callers decide whether to consume checker facts.
: PUBLISH-CODE ( -- n )
   PENDING-PROVE {: idx:n fn:n size:n :}
   fn NSHADOW:SOURCE-FUNS
   idx fn size 0 COMMIT-OWNED
   idx NSHADOW:PUBLISH
   idx APPEND-PENDING
   idx ;

public

\ Each publishes the emission NEMIT holds sealed, so it runs before the
\ backend's RETIRE clears it.
: PUBLISH-PENDING ( -- )
   PUBLISH-CODE PENDING-FACTS ;

: PUBLISH-RAW ( -- )
   PUBLISH-CODE drop ;

: PUBLISH-PENDING-DEFER ( n -- ) {: cell:n :}
   PENDING-PROVE {: idx:n fn:n size:n :}
   fn size + TRAILER-BYTES + CODE-CEILING > if E-NPUB-ROOM throw then
   fn NSHADOW:SOURCE-FUNS
   idx fn size cell COMMIT-OWNED
   idx NSHADOW:PUBLISH
   idx APPEND-PENDING
   idx PENDING-FACTS ;

: PUBLISH-PENDING-DOES ( n -- ) {: fun:n :}
   fun DOES-PROVE {: idx:n fn:n size:n off:n :}
   fn NSHADOW:SOURCE-FUNS
   idx fn size 0 COMMIT-OWNED
   idx NSHADOW:PUBLISH
   fn off + size off - NEMIT:RET-BYTES CODE-SPAN:RECORDED does-record
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

\ Place a prepared body after the pending row's checks; `pad` is room the
\ caller writes past the body.
: IMPORT-CODE ( ptr u8 n n ptr n n ptr n n n -- n n )
   {: code:ptr size:n recorded:n calls:ptr calln:n
      addrs:ptr addrn:n pad:n :}
   PENDING-IDX {: idx:n :}
   idx PENDING-CK
   size 0 <= size 3 and 0<> or if E-NPUB-RELOC throw then
   recorded CODE-SPAN:VALID? 0= if E-NPUB-RELOC throw then
   recorded CODE-SPAN:BYTES size <> if E-NPUB-RELOC throw then
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
   size pad + ROOM-CK {: fn:n :}
   code fn size CODE-WINDOW
   calln 0 ?do fn calls i cells + @ + RELOC-EXTERNAL loop
   addrn 0 ?do fn addrs i cells + @ + RELOC-ADDR loop
   fn recorded idx PUBLISH-REC
   idx fn ;

: IMPORT-MARKS ( n bool n -- ) {: idx:n wide:bool minin:n :}
   wide if WIDE-REC then
   minin 0<> if idx minin MIN-IN-REC then ;

: MIN-IN-CK ( n -- ) {: minin:n :}
   minin 0 < minin 255 > or if E-NPUB-RELOC throw then ;

public

\ A prepared definition reuses the interpreter's pending row, whose name and
\ wordlist the source definition wrote. All references and implementation
\ facts are prepared before this call.
: IMPORT-PENDING ( ptr u8 n n ptr n n ptr n n bool n -- n )
   {: code:ptr size:n recorded:n calls:ptr calln:n
      addrs:ptr addrn:n wide:bool minin:n :}
   minin MIN-IN-CK
   code size recorded calls calln addrs addrn 0 IMPORT-CODE drop {: idx:n :}
   idx APPEND-PENDING
   idx wide minin IMPORT-MARKS
   NHOST:PUBLISH-TAKEN
   idx ;

\ A prepared `does>` definer: its `;does` clause starts `off` bytes into the
\ body and publishes as the next record, as PUBLISH-PENDING-DOES lays it out.
\ The checker slice installed beside it carries the clause's checker facts.
: IMPORT-PENDING-DOES ( ptr u8 n n ptr n n ptr n n bool n n -- n )
   {: code:ptr size:n recorded:n calls:ptr calln:n
      addrs:ptr addrn:n wide:bool minin:n off:n :}
   minin MIN-IN-CK
   off 0 <= off size >= or off 3 and 0<> or if E-NPUB-RELOC throw then
   PENDING-IDX {: idx:n :}
   idx 1+ DICT-CAP >= if E-NPUB-PENDING throw then
   code size recorded calls calln addrs addrn idx DOES-NAME-PAD IMPORT-CODE
      {: at:n fn:n :}
   fn off + recorded off - does-record
   at APPEND-PENDING
   at wide minin IMPORT-MARKS
   at 1+ APPEND-PENDING
   NHOST:PUBLISH-TAKEN
   at ;

\ A prepared defer body: its record spans the whole body; the trailer naming `cell` follows at
\ fn + size, the code pointer where COMMIT-DEFER lays it: an arm64 code slot is one instruction word.
: IMPORT-PENDING-DEFER ( ptr u8 n ptr n n ptr n n bool n n -- n )
   {: code:ptr size:n calls:ptr calln:n
      addrs:ptr addrn:n wide:bool minin:n cell:n :}
   minin MIN-IN-CK
   code size  size CODE-SPAN:EXACT  calls calln addrs addrn TRAILER-BYTES IMPORT-CODE
      {: idx:n fn:n :}
   cell fn size + TRAILER-PLACE
   idx APPEND-PENDING
   idx wide minin IMPORT-MARKS
   NHOST:PUBLISH-TAKEN
   idx ;

\ A record for code already in place, a kernel primitive's: the pending row
\ takes its entry and recorded span, and nothing is placed.
: IMPORT-ENTRY ( n n -- n )
   {: entry:n recorded:n :}
   PENDING-IDX {: idx:n :}
   idx PENDING-CK
   recorded CODE-SPAN:VALID? 0= if E-NPUB-RELOC throw then
   entry recorded idx PUBLISH-REC
   idx APPEND-PENDING
   idx ;

seal-package
;package
