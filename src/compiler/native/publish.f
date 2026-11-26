\ publish.f - commit one sealed native emission to its pending dictionary record.
\ Every rejection precedes the code window; the commit only copies accepted bytes
\ and publishes the record. The code region is the running engine's: an emission
\ sealed for any machine but NABI:BINDING's is rejected first. A shadow target's
\ routine is filed with the record past the window
\ (src/compiler/native/shadow.f).

require src/lib/prelude.f
require src/lib/error.f
require src/core/does-clause.f
require src/core/checker.f
require src/core/check-hook.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/native/abi.f
require src/compiler/native/checker-owner.f
require src/compiler/native/dict.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/habu/code-span.f
require src/compiler/workspace.f

package NPUB

private

EXPORT CHECK:DOES-PUBLISHED
EXPORT CODE-RECLAIM:TRUNCATE
EXPORT CHECK:CHECKER-NATIVE-DOES-COMMIT

: CODE-WINDOW ( str n -- ) {: s:str dst:n :}
   s STR:DATA dst s STR:LEN code-publish ;

: PUBLISH-REC ( n n n -- )
   xref-retarget ;

: WIDE-REC ( -- )
   wide-mark ;

: APPEND-PENDING ( n -- )
   ndict-append ;

\ Where cp stood at the pending definition's open: its long name's copy is
\ all the open placed.
: OPEN-CP ( ^n -- n ) {: rec:^ :}
   rec XREF-EXT? 0= if cp@ exit then
   rec XREF-NAME-SLOT XREF-CELL@ ;

\ Rejects size bytes of code the ceiling has no room for after the pending
\ definition's name, measured from its open; a rejection leaves cp there.
: CODE-ROOM ( n -- ) {: size:n :}
   ndict@ XREF-REC {: rec:^ :}
   rec XREF-NAME$ {: name:str :}
   cp@ {: cp:n :}
   rec OPEN-CP TRUNCATE
   name STR:DATA name STR:LEN  name STR:DATA name STR:LEN  size CODE-SPACE:ROOM
   cp TRUNCATE ;

\ The bytes size bytes of code and the u bytes written after them take: each
\ write starts on a code slot.
: TRAILED-SIZE ( n n -- n ) {: size:n u:n :}
   size CODE-SPACE:SLOTS u CODE-SPACE:SLOTS + ;

\ The code slot size bytes go to.
: ROOM-CK ( n -- n )
   CODE-ROOM cp@ ;

: PLACE-CK ( n -- ) {: fn:n :}
   NEMIT:PLACED? 0= if exit then
   NEMIT:PLACEMENT fn <> if E-NPUB-PLACE throw then ;

: VALIDATE-EMISSION ( n -- n ) {: size:n :}
   size ROOM-CK {: fn:n :}
   fn PLACE-CK
   fn ;

: PLACE ( n n -- ) {: fn:n size:n :}
   NEMIT:BYTES size >str fn CODE-WINDOW ;

: COMMIT ( n n n -- ) {: idx:n fn:n size:n :}
   fn size PLACE
   fn size NEMIT:RET-BYTES CODE-SPAN:RECORDED idx PUBLISH-REC ;

\ One routine's commit, held where COMMIT-OWNED's quotations read it. Compiler
\ working storage: an image carries none of it.
0 CWORK-VARIABLE HELD-IDX n
0 CWORK-VARIABLE HELD-FN n
0 CWORK-VARIABLE HELD-SIZE n
0 CWORK-VARIABLE HELD-DEFER n           \ a defer body's dispatch cell, or 0
0 CWORK-VARIABLE HELD-STEPS ^u8         \ a copy word's steps trailer
0 CWORK-VARIABLE HELD-STEPS-U n         \ its bytes, or 0

2 cells constant TRAILER-BYTES
create TRAILER TRAILER-BYTES allot

: TRAILER-PLACE ( n n -- ) {: cell:n slot:n :}
   DEFER-MAGIC TRAILER !
   cell TRAILER CELL + !
   TRAILER TRAILER-BYTES >str slot CODE-WINDOW ;

\ A record a trailer follows ends at it, so the span is the slot and not the
\ emission's return-trimmed length.
: TRAILED ( n n -- n ) {: idx:n fn:n :}
   cp@ {: slot:n :}
   fn slot fn - CODE-SPAN:EXACT idx PUBLISH-REC
   slot ;

\ A defer's trailer names the dispatch cell its body jumps through (layout.f
\ DEFER-MAGIC).
: DEFER-TRAILER ( n n n -- ) {: idx:n fn:n cell:n :}
   cell  idx fn TRAILED  TRAILER-PLACE ;

\ A copy word's trailer holds its steps.
: STEPS-TRAILER ( n n str -- ) {: idx:n fn:n s:str :}
   s  idx fn TRAILED  CODE-WINDOW
   idx XREF-REC XREF:MARK-COPY ;

: COMMIT-DEFER ( n n n n -- ) {: idx:n fn:n size:n cell:n :}
   fn size PLACE
   idx fn cell DEFER-TRAILER ;

: COMMIT-COPY ( n n n str -- ) {: idx:n fn:n size:n s:str :}
   fn size PLACE
   idx fn s STEPS-TRAILER ;

: COMMIT-HELD ( -- )
   HELD-STEPS-U @ 0<> if
      HELD-IDX @ HELD-FN @ HELD-SIZE @  HELD-STEPS @ HELD-STEPS-U @ >str
      COMMIT-COPY exit
   then
   HELD-DEFER @ 0<> if
      HELD-IDX @ HELD-FN @ HELD-SIZE @ HELD-DEFER @ COMMIT-DEFER exit
   then
   HELD-IDX @ HELD-FN @ HELD-SIZE @ COMMIT ;

: COMMIT-OWNED-BODY ( -- )
   0 CHECKER-OWNER:WRITE-WINDOW
   COMMIT-HELD ;

\ The held routine is cleared on every exit, so a rejected commit leaves nothing
\ the next one could read.
: COMMIT-OWNED ( n n n n str -- ) {: idx:n fn:n size:n cell:n s:str :}
   idx HELD-IDX !  fn HELD-FN !  size HELD-SIZE !
   cell HELD-DEFER !  s STR:DATA HELD-STEPS !  s STR:LEN HELD-STEPS-U !
   [: COMMIT-OWNED-BODY ;]
   [: 0 HELD-IDX !  0 HELD-FN !  0 HELD-SIZE !  0 HELD-DEFER !
      0 HELD-STEPS-U ! ;] finally ;

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

: DOES-NAME-PAD ( n -- n )
   XREF-REC XREF-NAME$ STR:LEN
   DOES-CLAUSE:SUFFIX$ STR:LEN + CODE-SPACE:SLOTS ;

\ The definer's `does>` rejected a clause with no record left.
: DOES-PROVE ( n -- n n n n ) {: fun:n :}
   PENDING-PROVE {: idx:n fn:n size:n :}
   idx DOES-NAME-PAD {: pad:n :}
   size CODE-SPACE:SLOTS pad + CODE-ROOM
   fun NEMIT:FUNCTION-OFFSET@ {: off:n :}
   off 0 <= if E-NPUB-OFFSET throw then
   idx fn size off ;

\ Callers decide whether to consume checker facts.
: PUBLISH-CODE ( -- n )
   PENDING-PROVE {: idx:n fn:n size:n :}
   fn NSHADOW:SOURCE-FUNS
   idx fn size 0 NULL-PTR 0 >str COMMIT-OWNED
   idx NSHADOW:PUBLISH
   idx APPEND-PENDING
   idx ;

EXPORT CHECK:REC-WIDE-PUBLISH

public

\ Each publishes the emission NEMIT holds sealed, so it runs before the
\ backend's RETIRE clears it.
: PUBLISH-PENDING ( -- )
   PUBLISH-CODE drop  REC-WIDE-PUBLISH ;

: PUBLISH-RAW ( -- )
   PUBLISH-CODE drop ;

: PUBLISH-PENDING-DEFER ( n -- ) {: cell:n :}
   PENDING-PROVE {: idx:n fn:n size:n :}
   size TRAILER-BYTES TRAILED-SIZE CODE-ROOM
   fn NSHADOW:SOURCE-FUNS
   idx fn size cell NULL-PTR 0 >str COMMIT-OWNED
   idx NSHADOW:PUBLISH
   idx APPEND-PENDING
   REC-WIDE-PUBLISH ;

\ A copy word with its steps' trailer, whole instructions.
: PUBLISH-PENDING-COPY ( str -- ) {: s:str :}
   PENDING-PROVE {: idx:n fn:n size:n :}
   size s STR:LEN TRAILED-SIZE CODE-ROOM
   fn NSHADOW:SOURCE-FUNS
   idx fn size 0 s COMMIT-OWNED
   idx NSHADOW:PUBLISH
   idx APPEND-PENDING
   REC-WIDE-PUBLISH ;

: PUBLISH-PENDING-DOES ( n -- ) {: fun:n :}
   fun DOES-PROVE {: idx:n fn:n size:n off:n :}
   fn NSHADOW:SOURCE-FUNS
   idx fn size 0 NULL-PTR 0 >str COMMIT-OWNED
   idx NSHADOW:PUBLISH
   fn off + size off - NEMIT:RET-BYTES CODE-SPAN:RECORDED does-record
   idx APPEND-PENDING
   idx XREF-REC XREF-NAME$ true DOES-PUBLISHED
   REC-WIDE-PUBLISH
   idx 1+ APPEND-PENDING
   CHECKER-NATIVE-DOES-COMMIT ;

: NEXT-SLOT ( -- n )
   cp@ ;

: IN-REGION? ( n -- bool ) {: t:n :}
   t dbase@ < if false exit then
   t dbase@ REGION + < ;

private

: HOST-X64? ( -- bool )
   NABI:BINDING CBIND:TARGET@ CTARGET:ARCH@
   CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ ;

: IMPORT-ALIGNED? ( n -- bool )
   HOST-X64? if drop true else 3 and 0= then ;

\ Place a prepared body after the pending row's checks; `pad` is room the
\ caller writes past the body.
: IMPORT-CODE ( str n n -- n n )
   {: code:str recorded:n pad:n :}
   code STR:LEN {: size:n :}
   PENDING-IDX {: idx:n :}
   idx PENDING-CK
   size 0 <= size IMPORT-ALIGNED? 0= or if E-NPUB-RELOC throw then
   recorded CODE-SPAN:VALID? 0= if E-NPUB-RELOC throw then
   recorded CODE-SPAN:BYTES size <> if E-NPUB-RELOC throw then
   size pad TRAILED-SIZE ROOM-CK {: fn:n :}
   code fn CODE-WINDOW
   fn recorded idx PUBLISH-REC
   idx fn ;

: IMPORT-MARKS ( bool -- )
   if WIDE-REC then ;

public

\ A prepared definition reuses the interpreter's pending row, whose name and
\ wordlist the source definition wrote. All references are prepared before
\ this call.
: IMPORT-PENDING ( str n bool -- n )
   {: code:str recorded:n wide:bool :}
   code recorded 0 IMPORT-CODE drop {: idx:n :}
   idx APPEND-PENDING
   wide IMPORT-MARKS
   idx ;

\ A prepared `does>` definer: its `;does` clause starts `off` bytes into the
\ body and publishes as the next record, as PUBLISH-PENDING-DOES lays it out.
\ The checker slice installed beside it carries the clause's checker facts.
\ PART-FRESH? links a part only when its records fit, so the clause has its record.
: IMPORT-PENDING-DOES ( str n bool n -- n )
   {: code:str recorded:n wide:bool off:n :}
   off 0 <= off code STR:LEN >= or off IMPORT-ALIGNED? 0= or
   if E-NPUB-RELOC throw then
   PENDING-IDX {: idx:n :}
   code recorded idx DOES-NAME-PAD IMPORT-CODE
      {: at:n fn:n :}
   fn off + recorded off - does-record
   at APPEND-PENDING
   wide IMPORT-MARKS
   at 1+ APPEND-PENDING
   at ;

\ The trailer follows the host's aligned code slot, as for a fresh defer.
: IMPORT-PENDING-DEFER ( str bool n -- n )
   {: code:str wide:bool cell:n :}
   code  code STR:LEN CODE-SPAN:EXACT  TRAILER-BYTES IMPORT-CODE
      {: idx:n fn:n :}
   idx fn cell DEFER-TRAILER
   idx APPEND-PENDING
   wide IMPORT-MARKS
   idx ;

\ The steps' trailer follows the host's code slot, as for a fresh copy word.
: IMPORT-PENDING-COPY ( str bool str -- n )
   {: code:str wide:bool s:str :}
   code  code STR:LEN CODE-SPAN:EXACT  s STR:LEN IMPORT-CODE {: idx:n fn:n :}
   idx fn s STEPS-TRAILER
   idx APPEND-PENDING
   wide IMPORT-MARKS
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
