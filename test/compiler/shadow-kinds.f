\ shadow-kinds.f - each kind of definition, loaded by the typed loop at tier 0
\ under a native arm64 shadow: its shadow routine against T0's host routine.

require src/lib/error.f
require src/lib/string.f
require src/lib/le.f
require src/lib/fs.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/abi.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/habu/interpreter.f
require src/lib/ffi-callback.f

package SHADOW-KINDS
public
variable PROBE
private

\ A compiled definer's `create` calls through CREATEP-CELL, which on arm64 holds
\ the kernel's `create` until the typed loop's definer is installed there.
\ Generated text reaches the typed loop through the evaluate cells.
INTERPRETER:INSTALL-CREATE
INTERPRETER:INSTALL-EVALUATE

variable ND0                         \ the first record the fixture can make
variable ND1                         \ and one past its last
variable RECS0                       \ shadow records filed before the load
variable FAILS
65536 constant SRC-CAP
SRC-CAP BUFFER: SRC                  \ the fixture's text
variable SRC-U
variable SEC-FIRST                   \ the section being read
variable SEC-END

\ How a word of a shadow routine may differ from the host's: not at all, in a
\ call row's `bl`/`b` field (the shadow's is 0, the host's the placed
\ distance), or in a created word's patch slot (the host's `ret` became `b`).
0 constant PLAIN
1 constant CALL-ROW
2 constant PATCH-ROW

$FC000000 constant OP6               \ an arm64 branch's opcode bits
$14000000 constant B-OP              \ `b` imm26

CAST: CODE>BYTES ( n -- ptr u8 )

: .N ( n -- ) {: n:n :}
   n 0< if [char] - emit  0 n - recurse exit then
   n 10 >= if n 10 / recurse then
   n 10 mod $30 + emit ;

: T0? ( -- bool )
   data-base NCOMP-DISPATCH:COMP-CELL + @ 0<> ;

: TIER-CK ( -- )
   T0? 0= if
      s" shadow-kinds: no tier-0 compiler is installed (COMP-CELL 0)" 1 die
   then ;

\ Whether a section line starts at byte at.
: MARK? ( n -- bool ) {: at:n :}
   at 4 + SRC-U @ > if false exit then
   at 0= if true else SRC at 1- + c@ 10 = then
   SRC at + 4 s" \ ==" STR= and ;

\ The first section line at or past from, or the end of the text.
: NEXT-MARK ( n -- n ) {: from:n :}
   SRC-U @ from ?do i MARK? if i unloop exit then loop
   SRC-U @ ;

: LINE-END ( n -- n ) {: from:n :}
   SRC-U @ from ?do SRC i + c@ 10 = if i unloop exit then loop
   SRC-U @ ;

: .SECTION ( n -- ) {: at:n :}
   at MARK? 0= if s" requires" type exit then
   SRC at 5 + +  at LINE-END at 5 + -  type ;

: FAIL-HEAD ( ptr u8 n -- )
   s" FAIL " type type s" : " type
   1 FAILS +! ;

\ One section, read by the typed loop's framed entry: the one
\ SOURCE-ROOT:USE-INTERPRETER binds for a loaded file's bytes, here given one
\ section's range of them so that a kind's refusal ends only its section.
: READ-SECTION ( n n -- ) {: first:n end:n :}
   first SEC-FIRST !  end SEC-END !
   NSHADOW:RECORDS {: rows:n :}
   [: SRC SRC-U @ SEC-FIRST @ SEC-END @ INTERPRETER:EVAL-FRAMED-RANGE ;] catch {: rc:n :}
   rc 0<> if
      s" FAIL " type first .SECTION s" : threw " type rc .N
      s"  after filing " type NSHADOW:RECORDS rows - .N s"  shadow rows" type cr
      1 FAILS +!
   then ;

: LOAD-FIXTURE ( -- )
   s" test/compiler/shadow-kinds-fixture.f" SRC SRC-CAP FS:READ-ALL SRC-U !
   0 begin
      dup SRC-U @ <
   while
      dup 1+ NEXT-MARK {: first:n end:n :}
      first end READ-SECTION
      end
   repeat
   drop ;

: .HEX ( n -- ) {: v:n :}
   s" $" type
   8 0 ?do
      s" 0123456789ABCDEF" drop  v 28 i 4 * - rshift $F and +  c@ emit
   loop ;

: SKIP? ( n -- bool )
   XREF-REC XREF-WORDLIST {: wid:n :}
   wid XREF:PACKAGE-WL?  wid XREF:RETIRED-WL? or ;

\ The last shadow row filed since the open that names record idx, or -1: a
\ section that refused leaves the rows of what it made before the refusal, and
\ the record index they name is made again.
: SHADOW-ROW ( n -- n ) {: idx:n :}
   -1
   NSHADOW:RECORDS RECS0 @ ?do
      i NSHADOW:RECORD@ idx = if drop i then
   loop ;

\ The rows naming a record no live record of the fixture is published under.
: STALE ( -- n )
   0
   NSHADOW:RECORDS RECS0 @ ?do
      i NSHADOW:RECORD@ SHADOW-ROW i <> if 1+ then
   loop ;

\ A record of the fixture made before idx whose routine starts where idx's
\ does and has a shadow row, or -1: idx is then an alias (EXPORT), which
\ files no shadow row of its own.
: SHARER ( n -- n ) {: idx:n :}
   -1
   idx ND0 @ ?do
      i XREF-REC XREF-START  idx XREF-REC XREF-START =
      i SHADOW-ROW 0< 0= and if drop i then
   loop ;

\ What kind of word emission e holds at offset at.
: ROW-AT ( n n -- n ) {: e:n at:n :}
   PLAIN
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ at = if
         drop  e i NSHADOW:PATCH-CALL? if PATCH-ROW else CALL-ROW then  leave
      then
   loop ;

: SAME? ( n n n -- bool ) {: h:n s:n row:n :}
   row PLAIN = if h s = exit then
   row CALL-ROW = if h OP6 and  s OP6 and = exit then
   h OP6 and B-OP = ;

\ The first record-relative offset among n bytes where the host routine at
\ start and emission e entered at ent differ, or -1.
: FIRST-DIFF ( n n n n -- n ) {: start:n e:n ent:n n:n :}
   -1
   n 4 / 0 ?do
      start i 4 * + CODE>BYTES LE:U32@
      e NSHADOW:BYTES ent + i 4 * + LE:U32@
      e ent i 4 * + ROW-AT
      SAME? 0= if drop i 4 * leave then
   loop ;

: SIZES ( n n -- ) {: h:n s:n :}
   s" size host " type h .N  s"  shadow " type s .N  s" ; " type ;

: DIFF-LINE ( n n n n -- ) {: start:n e:n ent:n off:n :}
   s" word at " type off .N
   s" host " type  start off + CODE>BYTES LE:U32@ .HEX
   s"  shadow " type  e NSHADOW:BYTES ent + off + LE:U32@ .HEX
   e ent off + ROW-AT CALL-ROW = if s"  (call row)" type then ;

: CHECK ( n -- ) {: idx:n :}
   idx XREF-REC {: rec:ptr :}
   rec XREF-START {: start:n :}
   rec XREF-CODE-BYTES {: hlen:n :}
   idx SHADOW-ROW {: r:n :}
   r 0< if
      idx SHARER {: o:n :}
      o 0< 0= if
         s" ok " type rec XREF-NAME$ type
         s"  (shares " type o XREF-REC XREF-NAME$ type s" )" type cr exit
      then
      rec XREF-NAME$ FAIL-HEAD s" no shadow record; host " type hlen .N s"  bytes" type
      cr exit
   then
   r NSHADOW:EMISSION@ {: e:n :}
   r NSHADOW:ENTRY@ {: ent:n :}
   e NSHADOW:SIZE ent - {: slen:n :}
   slen hlen =  slen hlen e NSHADOW:RET-BYTES + = or {: sized:bool :}
   start e ent  slen hlen min  FIRST-DIFF {: off:n :}
   sized off 0< and if s" ok " type rec XREF-NAME$ type cr exit then
   rec XREF-NAME$ FAIL-HEAD
   sized 0= if hlen slen SIZES then
   off 0< 0= if start e ent off DIFF-LINE then
   cr ;

: REPORT ( -- )
   ND1 @ ND0 @ ?do
      i SKIP? 0= if i CHECK then
   loop
   s" stale shadow rows: " type STALE .N cr ;

: FINISH ( -- )
   FAILS @ 0<> if s" shadow-kinds: a kind failed" 1 die then ;

public

: RUN ( -- )
   TIER-CK
   NABI:BINDING NSHADOW:OPEN-NATIVE
   NSHADOW:RECORDS RECS0 !
   ndict@ ND0 !
   LOAD-FIXTURE
   ndict@ ND1 !
   REPORT
   NSHADOW:CLOSE
   FINISH ;

;package

SHADOW-KINDS:RUN
