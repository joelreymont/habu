\ x86-64-link-records.f - src/habu/link-x64.f lays a captured window out for an
\ x86-64 image and links it: the kernel's records and the window's at their final
\ addresses, every routine copied from the capture's shadow to a code slot with
\ each of its sites resolved, wids rebased from the host's window to the image's
\ numbering, the protected-wid bitmap, the name index the writer builds, and the
\ image's xt for each declared code cell.
\
\ WHAT IS DRIVEN. A small window compiled at tier 1 with an x86-64 shadow open,
\ as test/aot-shadow-capture.f compiles its own: a package, a leaf, a caller, a
\ `does>` definer, a word whose name is past sixteen bytes, calls to kernel
\ bodies, a code literal naming a window word, a quotation, a string literal in
\ the window's DATA, a tail call, two declared code cells (a window word's xt
\ and a kernel body's), two clauses, a `variable`, a `constant` and a created
\ word that the Habu loop reads (src/habu/interpret.f), whose bodies NCOMP
\ compiles with the shadow open, and the package's public wordlist protected.
\ Then the real capture and the shadow reader, the x86-64 kernel's rows emitted
\ into a stream by test/x86-64-boot-harness.f, and X64LINK:LAYOUT over both, as
\ the image writer runs it before the stream links.
\
\ WHAT IS ASKED. Which record lies at each image index; that each code record
\ enters its own routine's bytes on a code slot and a does> companion enters at
\ its clause; a long name's bytes in the code band; a kernel body's record and
\ its min-in; each wid against the host's own, rebased; the bitmap bit; one
\ index slot per record. And every site, read back from the laid bytes and never
\ from the linker's placement: the instruction at the site's place in its
\ routine is decoded, and where it lands is held against the record laid for
\ the word it names, the kernel's own entry label for a kernel body, the
\ capture's bytes of the routine it lands on, the offset of the string the host
\ holds or of the cell a definer made, and the function offset the capture
\ carries. A variable's and a created word's routine ends in the slot
\ does-patch aims, `jmp rel32` with displacement 0 and `ret`; a constant's
\ does not.
\
\ THE DOES-PATCH ROW RUNS THE LINKED WORDS. Each of these images lays the
\ window out over its own kernel stream, stages it as hb-x64-link-index does,
\ puts the region at rest and names MADE, the created word, in LASTC-CELL.
\ hb-x64-link-does runs MADE and SEVEN, the constant, aims MADE at one clause
\ and then another, which clears its DKIND, restores the bare body twice and
\ finds every band closed; it exits 0, and hb-x64-link-does-negative, the same
\ expecting a wrong first answer, exits 21. hb-x64-link-does-checker arms
\ stand-in checkers and the check hook, so each clause's signature registers
\ as MADE's raw effect and the checker's tail sets its min-in; it exits 0.
\ hb-x64-link-does-slot-armed aims SEVEN, whose routine has no slot, and exits
\ 83 before the row writes. The peer runs them.
\
\ THE INDEX IS READ BY THE KERNEL. The stream the kernel's rows open becomes the
\ image hb-x64-link-index: the writer's records at the region, its code band,
\ the record count, and its index at INDEX-VA, which HIDXP-CELL names. There the
\ kernel's xref-search-wl, whose probe is FIND-HELPER, finds every record by its
\ own name and wordlist, finds a name folded, and misses a name in another
\ wordlist and an absent one. The image exits 0 when every lookup held; it runs
\ on an x86-64 host.
\
\ THE INDEX IS MEASURED DETERMINISTIC. A child built with one record, one
\ wordlist and one DATA cell more ahead of the window lays out the same bytes:
\ records, linked code band, bitmap and index, by digest.
\
\ WHAT IS REFUSED. A capture the layout cannot place or link, by name, each in a
\ child because a refusal ends the process before the layout returns: a call to
\ a window word with no x86-64 routine (a tier-0 word), a call and a code cell
\ naming a word of the engine's own prefix no kernel body carries, and five
\ forged rows: a site naming the package row, the same in a `does>` definer's
\ routine, which the refusal names by the definer, a code cell whose xt row is
\ gone, a code cell whose xt row names the package row, and a protected-wid row
\ outside the window. Four the capture refuses
\ before any layout, because the shadow keys every routine, target and code cell
\ by the record row the capture ships: a live private word whose routine no
\ shipped record carries, a shadow call to a private word the capture strips
\ and one to a retired word, and a code cell holding a quotation's entry.
\
\ LOAD ORDER. The x86-64 side first, then the ARM64 code layer the capture
\ needs: src/arch/arm64/icode.f defines CODE, LBL and ASM-LEN as globals, and a
\ `using X64CODE` opened after them refuses (src/habu/link-x64.f "LOAD ORDER").
\
\ Registered as `SUITE x86-64-link-records`. Run standalone from the repository
\ root: bin/hb --load test/x86-64-link-records.f
\ A child: bin/hb --load test/x86-64-link-records.f -- MODE, MODE one of shift
\ stray strip callee retired wid unresolved quot cellname pkgsite doessite
\ xtless pkgcell

package X64LT
public
ndict@ here  variable PRE-R  variable PRE-D  PRE-D !  PRE-R !
variable PUB-WID                     \ the window package's public wordlist, host's id
;package

require lib/string.f

package X64LT
public
: MODE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ a u STR= exit then
   false ;
\ The determinism child puts a record, a wordlist and a DATA cell ahead of it all.
: SHIFT$ ( -- ptr u8 n )
   s" shift" MODE? if s" variable SHIFT-CELL wordlist drop" exit then
   s" " ;
\ What a refusal child adds to the window.
: EXTRA$ ( -- ptr u8 n )
   s" stray" MODE? if s" 0 set-tier : BARE ( -- n ) 7 ; 1 set-tier : WRAP ( -- n ) BARE 1+ ;" exit then
   s" strip" MODE? if s" private : HELPER ( n -- n ) 3 * ; public : USER ( n -- n ) HELPER 1+ ;" exit then
   s" callee" MODE? if s" private 0 set-tier : LOW ( -- n ) 7 ; public 1 set-tier : HIGH ( -- n ) LOW 1+ ;" exit then
   s" retired" MODE? if s" 0 set-tier : GONE ( -- n ) 7 ; 1 set-tier : KEEP ( -- n ) GONE 1+ ; undefine GONE" exit then
   s" unresolved" MODE? if s" : SAME ( ptr u8 n ptr u8 n -- bool ) STR= ;" exit then
   s" quot" MODE? if s" : Q ( -- [ n -- n ] ) [: 1 + ;] ;  Q align here 0 , xt!" exit then
   s" cellname" MODE? if s" ' STR= align here 0 , xt!" exit then
   s" " ;
SHIFT$ evaluate
;package

require test/x86-64-boot-harness.f
require lib/le.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-shadow.f
require src/compiler/native/string.f
require src/compiler/native/x64ir.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/passes.f
require src/habu/link-x64.f
require src/core/sha256.f
require lib/process.f
require lib/process-argv.f
require src/habu/interpret.f

\ A binding is a multi-cell value, which only a compiled body may hold.
package X64LT
public
: OPEN-X64 ( -- ) X64ABI:BINDING NSHADOW:OPEN ;
;package
X64LT:OPEN-X64
AOT-ARM:WINDOW-OPEN
NSTR:WINDOW-OPEN

package X64LT-WIN
public
get-current X64LT:PUB-WID !

1 set-tier
: LEAF ( n n -- n ) + ;
: CALLER ( n -- n ) dup LEAF 1 + ;
: TAILER ( n -- n ) 2 * CALLER ;
: CONST ( n -- ) create , does> ( -- n ) @ ;
: SPELLED-PAST-SIXTEEN ( -- n ) 7 ;
: SHOW ( n -- ) . ;
: TICK ( -- [ n n -- n ] ) ['] LEAF ;
: QUOT ( -- [ n -- n ] ) [: 1 + ;] ;
: GREET ( -- ptr u8 n ) s" linked" ;
: BUMP ( n -- n ) 1 + ;
: BUMP2 ( n -- n ) 2 + ;
X64LT:EXTRA$ evaluate
s" variable CELLV 7 constant SEVEN create MADE 2 cells allot" OUTER:INTERPRET
0 set-tier

' LEAF align here 0 , xt!
' negate align here 0 , xt!

get-current prot-wid-add
;package

AOT-ARM:WINDOW-CLOSE

package X64LT
using AOT-BUF

\ ---- the window, as the capture numbers it ----------------------------------
: WIDX ( ptr u8 n -- n ) XREF-FIND-INDEX AOT-ARM:R0 @ - ;
: W-LEAF ( -- n ) s" X64LT-WIN:LEAF" WIDX ;
: W-CALLER ( -- n ) s" X64LT-WIN:CALLER" WIDX ;
: W-CONST ( -- n ) s" X64LT-WIN:CONST" WIDX ;
: W-LONG ( -- n ) s" X64LT-WIN:SPELLED-PAST-SIXTEEN" WIDX ;
: W-CELLV ( -- n ) s" X64LT-WIN:CELLV" WIDX ;
: W-SEVEN ( -- n ) s" X64LT-WIN:SEVEN" WIDX ;
: W-MADE ( -- n ) s" X64LT-WIN:MADE" WIDX ;
: W-BUMP ( -- n ) s" X64LT-WIN:BUMP" WIDX ;
: W-BUMP2 ( -- n ) s" X64LT-WIN:BUMP2" WIDX ;

\ ---- the layout --------------------------------------------------------------
: IMG ( n -- n ) X64LINK:PRIMS + ;
: RF@ ( n n -- n ) {: k:n f:n :} X64LINK:DICT$ drop k DREC * + f + LE:U64@ ;
: NAME$ ( n -- ptr u8 n ) X64LINK:REC-NAME$ ;
: BAND ( n -- ptr u8 ) {: va:n :} X64LINK:CODE$ drop va X64LINK:CODE-VA - + ;
: SH@ ( n n -- n ) {: r:n f:n :}
   AOT-SHADOW:REC-BUF@ r AOT-SHADOW:REC-ROW * + f + LE:U32@ ;

\ The shadow row of window record w.
: ROW-OF ( n -- n ) {: w:n :}
   -1
   AOT-SHADOW:REC-N @ 0 ?do i 0 SH@ w = if drop i leave then loop ;

: PRIM-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   ENGINE-PRIMS:COUNT 0 ?do i ENGINE-PRIMS:NAME$ a u STR= if drop i leave then loop ;

: LONG-PRIM ( -- n )
   -1
   ENGINE-PRIMS:COUNT 0 ?do i ENGINE-PRIMS:NAME-LEN DNAME-INL > if drop i leave then loop ;

: BIT? ( n -- bool ) {: w:n :}
   X64LINK:BITS$ drop w 3 rshift + c@ w 7 and rshift 1 and 0<> ;

: SLOTS-USED ( -- n )
   0
   HIDX-SLOTS 0 ?do X64LINK:INDEX$ drop i 4 * + LE:U32@ 0<> if 1+ then loop ;

\ ---- the capture and the kernel ------------------------------------------------
: CAPTURE ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   AOT-CAPTURE:SHADOW-CAPTURE ;

: KERNEL ( -- ) X64HARNESS:INIT  false X64HARNESS:BOOT-OPEN, ;

\ ---- the cases -----------------------------------------------------------------
: RECORDS-CASE ( -- )
   s" the image dictionary is the kernel's records, then the window's in capture order" T-LABEL
   AOT-REC-N @  AOT-ARM:R1 @ AOT-ARM:R0 @ -  T=
   s" dup" PRIM-OF NAME$ s" dup" T$=
   0 IMG NAME$ s" X64LT-WIN" T$=
   0 IMG X64KERNEL:REC-WID RF@ DICT-WL:NAMESPACE T=
   W-LEAF IMG NAME$ s" LEAF" T$=
   W-CALLER IMG NAME$ s" CALLER" T$=
   W-CONST IMG NAME$ s" CONST" T$=
   W-CONST 1+ IMG NAME$ s" CONST;does" T$= ;

: ROUTINE-CASE ( -- )
   s" a code record enters its own routine, copied from the shadow to a code slot and filled with int3" T-LABEL
   W-LEAF ROW-OF {: r:n :}
   W-LEAF IMG 0 RF@ {: va:n :}
   va X64LINK:CODE-VA -  X64KERNEL:CODE-SLOT mod 0 T=
   va BAND r 8 SH@  AOT-SHADOW:CODE-BUF@ r 4 SH@ + r 8 SH@  T$=
   va r 8 SH@ + BAND c@ $CC T=
   W-LEAF IMG 8 RF@  r 8 SH@ CODE-SPAN:EXACT  T=
   W-CALLER IMG 0 RF@ va <> TTRUE ;

: DOES-CASE ( -- )
   s" a does> companion enters its definer's routine at the clause, spanning the rest of it" T-LABEL
   W-CONST ROW-OF {: r:n :}
   r 1+ 12 SH@ {: entry:n :}
   entry 0 > TTRUE
   W-CONST 1+ IMG 0 RF@  W-CONST IMG 0 RF@ entry +  T=
   W-CONST 1+ IMG 8 RF@  r 8 SH@ entry - CODE-SPAN:EXACT  T= ;

: LONG-NAME-CASE ( -- )
   s" a name past sixteen bytes lies in the code band ahead of the routines, its record holding its address" T-LABEL
   W-LONG IMG 16 RF@ DNAME-EXT and 0<> TTRUE
   W-LONG IMG X64KERNEL:REC-NAME RF@ {: at:n :}
   at X64LINK:CODE-VA >=  at W-LEAF IMG 0 RF@ < and TTRUE
   at BAND 20 s" SPELLED-PAST-SIXTEEN" T$=
   LONG-PRIM {: p:n :}
   p 0 >= TTRUE
   p 16 RF@ DNAME-EXT and 0<> TTRUE
   p NAME$ p ENGINE-PRIMS:NAME$ T$= ;

: PRIM-CASE ( -- )
   s" a kernel body's record enters the text, spans its body exactly, and states the primitive's min-in" T-LABEL
   s" dup" PRIM-OF {: p:n :}
   p 0 RF@ {: at:n :}
   at X64LINK:TEXT-VA >=  at X64LINK:TEXT-VA X64CODE:ASM-LEN + < and TTRUE
   p 8 RF@ CODE-SPAN:FULL? TTRUE
   p 16 RF@ DNAME-MIN-IN-MASK and 52 rshift 1 T=
   p 16 RF@ DNAME-LEN-MASK and 3 T=
   p X64KERNEL:REC-WID RF@ 0 T= ;

: WID-CASE ( -- )
   s" every wid is the host's, rebased from the window's first wordlist to T0" T-LABEL
   PUB-WID @ AOT-ARM:W0 @ - X64LINK:T0 + {: w:n :}
   W-LEAF IMG X64KERNEL:REC-WID RF@ w T=
   W-CONST 1+ IMG X64KERNEL:REC-WID RF@ w T=
   0 IMG 0 RF@ w T= ;

: BITMAP-CASE ( -- )
   s" the window's protected wordlist is the bit of its rebased wid, and the only one" T-LABEL
   AOT-PWIN-N @ 1 T=
   W-LEAF IMG X64KERNEL:REC-WID RF@ BIT? TTRUE
   0
   PROT-WID-MAX 0 ?do i BIT? if 1+ then loop
   1 T= ;

: INDEX-CASE ( -- )
   s" the writer's index holds one slot for each record" T-LABEL
   SLOTS-USED X64LINK:RECORDS T= ;

\ ---- the sites, read back from the laid bytes ----------------------------------
\ Nothing here asks the linker where it put a routine: a site's address comes from
\ its routine's laid record and the capture's own rows, and where its instruction
\ lands comes from decoding the image's bytes there.
: W-TICK ( -- n ) s" X64LT-WIN:TICK" WIDX ;
: W-QUOT ( -- n ) s" X64LT-WIN:QUOT" WIDX ;
: W-GREET ( -- n ) s" X64LT-WIN:GREET" WIDX ;

: SITE@ ( n n -- n ) {: s:n f:n :}
   AOT-SHADOW:SITE-BUF@ s AOT-SHADOW:SITE-ROW * + f + LE:U32@ ;
: XT@ ( n n -- n ) {: x:n f:n :}
   AOT-SHADOW:XT-BUF@ x AOT-SHADOW:XT-ROW * + f + LE:U32@ ;
: CELL-META ( n -- n ) {: c:n :}
   AOT-WINDOW:XTOFF-BUF@ c AOT-WINDOW:XTOFF-ROW * + 4 + LE:U32@ ;
: POOL$ ( n -- ptr u8 n ) {: off:n :} AOT-NAMES-BUF@ off + {: e:ptr :} e 1+ e c@ ;

: W-TAILER ( -- n ) s" X64LT-WIN:TAILER" WIDX ;
: ENTRY ( n -- n ) 0 RF@ ;
\ A kernel body's entry: its first label, by the kernel's own name lookup.
: KENTRY ( ptr u8 n -- n ) X64KERNEL:ENTRY-LABEL X64CODE:LABEL-AT X64LINK:TEXT-VA + ;

\ The shadow row whose emission holds shadow code byte n: the first, so a does>
\ definer's and not its companion's.
: HOLDER ( n -- n ) {: at:n :}
   -1
   AOT-SHADOW:REC-N @ 0 ?do
      at i 4 SH@ >=  at i 4 SH@ i 8 SH@ + < and if drop i leave then
   loop ;

\ Where row r's emission starts in the image, from its record's laid entry.
: START-VA ( n -- n ) {: r:n :} r 0 SH@ IMG ENTRY  r 12 SH@ - ;

\ Where site s lies in the image.
: SITE-VA ( n -- n ) {: s:n :}
   s 0 SITE@ HOLDER {: r:n :}
   r START-VA  s 0 SITE@ r 4 SH@ - + ;

\ The first site of `kind` in window word w's routine.
: SITE-IN ( n n -- n ) {: w:n kind:n :}
   w ROW-OF {: r:n :}
   -1
   AOT-SHADOW:SITE-N @ 0 ?do
      i 0 SITE@ {: at:n :}
      at r 4 SH@ >=  at r 4 SH@ r 8 SH@ + < and  i 4 SITE@ kind = and if drop i leave then
   loop ;

: BYTE@ ( n -- n ) BAND c@ ;
\ E8 or E9 cd: the displacement counts from the instruction's end, five bytes on.
: LANDS ( n -- n ) {: va:n :} va 1+ BAND LE:S32@  va 5 + + ;
\ mov r64, imm64: REX.W with B free, B8 + r, then the eight immediate bytes.
: MOVABS? ( n -- bool ) {: va:n :}
   va BYTE@ $FE and $48 =  va 1+ BYTE@ $F8 and $B8 = and ;
: IMM@ ( n -- n ) 2 + BAND LE:U64@ ;
\ What the capture left in a MOVABS site's immediate.
: CAPTURED-IMM ( n -- n ) {: s:n :} AOT-SHADOW:CODE-BUF@ s 0 SITE@ + 2 + LE:U64@ ;

\ The bytes at va are LEAF's routine as the capture compiled it: it has no site.
: ON-LEAF ( n -- ) {: va:n :}
   W-LEAF ROW-OF {: r:n :}
   va BAND r 8 SH@  AOT-SHADOW:CODE-BUF@ r 4 SH@ + r 8 SH@  T$= ;

: CALL-CASE ( -- )
   s" a call to a window word lands on that word's routine, by its record and by the capture's bytes" T-LABEL
   W-CALLER AOT-SHADOW:CALL SITE-IN {: s:n :}
   s 0 >= TTRUE
   s SITE-VA {: va:n :}
   va BYTE@ $E8 T=
   va LANDS W-LEAF IMG ENTRY T=
   va LANDS ON-LEAF ;

: TAIL-CASE ( -- )
   s" a tail call to a window word branches to that word's entry" T-LABEL
   W-TAILER AOT-SHADOW:TAIL SITE-IN {: s:n :}
   s 0 >= TTRUE
   s SITE-VA {: va:n :}
   va BYTE@ $E9 T=
   va LANDS W-CALLER IMG ENTRY T= ;

: CODE-CASE ( -- )
   s" a code literal naming a window word holds that word's entry" T-LABEL
   W-TICK AOT-SHADOW:CODE SITE-IN {: s:n :}
   s 0 >= TTRUE
   s SITE-VA {: va:n :}
   va MOVABS? TTRUE
   va IMM@ W-LEAF IMG ENTRY T=
   va IMM@ ON-LEAF ;

\ Every site naming a word of the engine's own prefix, `create` and `,` in the
\ definer with the does-patch its clause needs, and `.`, is a call that lands on
\ the entry of the kernel body that name registers.
: NAMED-OK? ( n -- bool ) {: s:n :}
   s 8 SITE@ SITE-TARGET-MASK and POOL$ KENTRY {: want:n :}
   s SITE-VA {: va:n :}
   va BYTE@ $E8 =  va LANDS want = and ;

: KERNEL-CASE ( -- )
   s" every site naming a word of the engine's own prefix reaches the kernel body of that name" T-LABEL
   0 0
   AOT-SHADOW:SITE-N @ 0 ?do
      i 8 SITE@ SITE-NAME-TAG and 0<> if
         swap 1+ swap
         i NAMED-OK? 0= if 1+ then
      then
   loop
   0 T=
   4 >= TTRUE ;

: FUN-CASE ( -- )
   s" a quotation's code literal holds the address of its function inside its own routine" T-LABEL
   W-QUOT AOT-SHADOW:FUN SITE-IN {: s:n :}
   s 0 >= TTRUE
   s SITE-VA {: va:n :}
   va MOVABS? TTRUE
   s CAPTURED-IMM {: off:n :}
   off 0 > TTRUE
   va IMM@  W-QUOT ROW-OF START-VA off +  T= ;

: CLAUSE-CASE ( -- )
   s" a definer's clause literal holds its does> companion's entry" T-LABEL
   W-CONST AOT-SHADOW:FUN SITE-IN {: s:n :}
   s 0 >= TTRUE
   s SITE-VA {: va:n :}
   va MOVABS? TTRUE
   va IMM@ W-CONST 1+ IMG ENTRY T=
   va IMM@  W-CONST ROW-OF START-VA s CAPTURED-IMM +  T= ;

\ The window's DATA lands at DATA-AT, as far into it as the bytes lie into the
\ host's window, and a cell keeps the alignment it was captured with.
: IMAGE-DATA ( ptr u8 -- n )
   data-base BYTE-VIEW - DATA-VA VA>N + AOT-ARM:D0 @ -  X64LINK:DATA-AT + ;
: IMAGE-VA ( ptr u8 -- n ) IMAGE-DATA X64LAYOUT:DATA-VA VA>N + ;

: DATA-CASE ( -- )
   s" a DATA literal holds the image address of the bytes it named in the host's window" T-LABEL
   W-GREET AOT-SHADOW:DATA SITE-IN {: s:n :}
   s 0 >= TTRUE
   s SITE-VA {: va:n :}
   va MOVABS? TTRUE
   X64LT-WIN:GREET {: a:ptr u:n :}
   a u s" linked" T$=
   va IMM@ a IMAGE-VA T=
   X64LINK:DATA-AT X64LINK:HEAP-FLOOR >= TTRUE
   X64LINK:DATA-AT AOT-ARM:D0 @ - 7 and 0 T= ;

: CELL-CASE ( -- )
   s" a declared code cell holds the image xt of its window word, or of its kernel body by name" T-LABEL
   -1
   AOT-SHADOW:XT-N @ 0 ?do i 4 XT@ W-LEAF = if drop i 0 XT@ leave then loop {: c:n :}
   c 0 >= TTRUE
   c X64LINK:CELL-XT W-LEAF IMG ENTRY T=
   c X64LINK:CELL-XT ON-LEAF
   -1
   AOT-WINDOW:XTOFF-N @ 0 ?do
      i CELL-META {: m:n :}
      m AOT-WINDOW:XTOFF-KIND-MASK and AOT-WINDOW:XTOFF-NAME-TAG = if
         m AOT-WINDOW:XTOFF-VALUE-MASK and 1- POOL$ s" negate" STR= if drop i leave then
      then
   loop {: n:n :}
   n 0 >= TTRUE
   n X64LINK:CELL-XT s" negate" KENTRY T= ;

\ ---- the definers, linked --------------------------------------------------------
\ Window word w's routine ends in the slot does-patch aims: `jmp rel32` with
\ displacement 0, then `ret`.
: SLOT? ( n -- bool ) {: w:n :}
   w ROW-OF {: r:n :}
   r START-VA r 8 SH@ + 6 - {: at:n :}
   at BYTE@ $E9 =  at 1+ BAND LE:S32@ 0= and  at 5 + BYTE@ $C3 = and ;

\ MADE is the last record, the one the checker's tail sets the min-in of
\ (kernel-x64.f WIDE-PUBLISH,).
: DEFINER-CASE ( -- )
   s" a definer's word enters its own routine, a variable's and a created word's ending in the slot" T-LABEL
   W-CELLV ROW-OF 0 >= TTRUE
   W-MADE ROW-OF 0 >= TTRUE
   W-SEVEN ROW-OF {: r:n :}
   r 0 >= TTRUE
   W-SEVEN IMG ENTRY BAND r 8 SH@  AOT-SHADOW:CODE-BUF@ r 4 SH@ + r 8 SH@  T$=
   W-CELLV SLOT? TTRUE
   W-MADE SLOT? TTRUE
   W-SEVEN SLOT? 0= TTRUE
   W-MADE IMG 1+ X64LINK:RECORDS T= ;

\ Window word w's DATA literal holds the image address of the cell at a.
: CELL-SITE ( n ptr u8 -- ) {: w:n a:ptr :}
   w AOT-SHADOW:DATA SITE-IN {: s:n :}
   s 0 >= TTRUE
   s SITE-VA {: va:n :}
   va MOVABS? TTRUE
   va IMM@ a IMAGE-VA T= ;

: DEFINER-DATA-CASE ( -- )
   s" a variable's and a created word's DATA literal holds the image address of the cell it made" T-LABEL
   W-CELLV X64LT-WIN:CELLV BYTE-VIEW CELL-SITE
   W-MADE X64LT-WIN:MADE BYTE-VIEW CELL-SITE ;

\ ---- the index, read by the kernel ---------------------------------------------
\ The stream KERNEL opened becomes an image: the writer's records at the region
\ and its code band DICT-SIZE past them, where the long names lie; the count,
\ through ndict!; then the writer's index at INDEX-VA, which HIDXP-CELL names
\ from there on.
: STAGE ( ptr u8 n n -- ) {: a:ptr u:n off:n :}
   u 0 ?do  a i + LE:U64@ off i + X64HARNESS:REGION!,  CELL +loop ;

: STAGE-INDEX ( -- )
   X64LINK:INDEX$ {: a:ptr u:n :}
   u 0 ?do
      a i + LE:U64@ {: v:n :}
      v 0<> if v X64LINK:INDEX-OFF i + X64HARNESS:CELL!, then
   CELL +loop
   X64LINK:INDEX-VA HIDXP-CELL X64HARNESS:CELL!, ;

: XREF, ( ptr u8 n n -- ) {: a:ptr u:n w:n :}
   a u X64HARNESS:PUSH-TEXT,  w X64HARNESS:PUSH,
   s" xref-search-wl" X64HARNESS:CALL-ROW, ;

\ Or into the cell under it how far the lookup of record k by its own name and
\ wordlist lands from record k: nothing when it finds it.
: LOOKUP, ( n -- ) {: k:n :}
   k NAME$ k X64KERNEL:REC-WID RF@ XREF,
   k DREC * X64HARNESS:PUSH-REGION,
   s" -" X64HARNESS:CALL-ROW,  s" or" X64HARNESS:CALL-ROW, ;

: STAGE-RECORDS ( -- )
   X64LINK:DICT$ 0 STAGE
   X64LINK:CODE$ DICT-SIZE STAGE
   X64LINK:RECORDS X64HARNESS:PUSH,  s" ndict!" X64HARNESS:CALL-ROW, ;

: INDEX-IMAGE ( -- )
   STAGE-RECORDS
   STAGE-INDEX
   0 X64HARNESS:PUSH,
   X64LINK:RECORDS 0 ?do i LOOKUP, loop
   0 X64HARNESS:EXPECT-POP,
   W-LEAF IMG X64KERNEL:REC-WID RF@ {: w:n :}
   s" leaf" w XREF,  W-LEAF IMG X64HARNESS:EXPECT-ROW,
   s" const;DOES" w XREF,  W-CONST 1+ IMG X64HARNESS:EXPECT-ROW,
   s" DUP" 0 XREF,  s" dup" PRIM-OF X64HARNESS:EXPECT-ROW,
   s" LEAF" 0 XREF,  0 X64HARNESS:EXPECT-POP,
   s" UNDEFINED-HERE" w XREF,  0 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   s" hb-x64-link-index" TMP-PATH X64HARNESS:BOOT-CLOSE, ;

\ ---- the definers in images ------------------------------------------------------
\ Each image lays the window out over its own kernel stream, whose labels die
\ when it links, and stages it as INDEX-IMAGE does. With the region at rest a
\ row writes through its own windows and the routines run read-execute.
\ LASTC-CELL names MADE, the created word does-patch rewrites.
: DOES-OPEN, ( bool -- )
   X64HARNESS:BOOT-OPEN,
   X64LINK:LAYOUT
   STAGE-RECORDS
   X64HARNESS:REST,
   W-MADE IMG DREC * LASTC-CELL X64HARNESS:REGION-ADDR!, ;

\ Run window word w's routine through the kernel's execute.
: CALL-WORD, ( n -- ) IMG ENTRY X64HARNESS:PUSH,  s" execute" X64HARNESS:CALL-ROW, ;

\ does-patch to a clause's entry, 0 for the bare body, with no signature.
: CLAUSE, ( n -- )
   X64HARNESS:PUSH,  0 X64HARNESS:PUSH,  0 X64HARNESS:PUSH,
   s" does-patch" X64HARNESS:CALL-ROW, ;

\ The same with the clause's signature `-- n`.
: CLAUSE-SIG, ( n -- )
   X64HARNESS:PUSH,  s" -- n" X64HARNESS:PUSH-TEXT,
   s" does-patch" X64HARNESS:CALL-ROW, ;

: MADE-AT ( -- n ) X64LT-WIN:MADE BYTE-VIEW IMAGE-DATA ;

\ Check MADE's flags are as laid with the bits of mask clear and those of set
\ set.
: MADE-FLAGS, ( n n -- ) {: mask:n set:n :}
   W-MADE IMG X64KERNEL:REC-FLAGS RF@  mask invert and  set or
   W-MADE IMG X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD, ;

\ MADE runs bare, then each clause does-patch aims it at, then bare again; the
\ first clause clears its DKIND. A second bare patch finds the displacement 0
\ and writes nothing.
: DOES-IMAGE ( bool ptr u8 n -- ) {: negative:bool path:ptr pathu:n :}
   negative DOES-OPEN,
   W-MADE CALL-WORD,  MADE-AT X64HARNESS:EXPECT-POP-DATA,
   W-SEVEN CALL-WORD,  7 X64HARNESS:EXPECT-POP,
   W-BUMP IMG ENTRY CLAUSE,
   W-MADE CALL-WORD,  MADE-AT 1+ X64HARNESS:EXPECT-POP-DATA,
   DKIND:MASK 0 MADE-FLAGS,
   W-BUMP2 IMG ENTRY CLAUSE,
   W-MADE CALL-WORD,  MADE-AT 2 + X64HARNESS:EXPECT-POP-DATA,
   0 CLAUSE,
   W-MADE CALL-WORD,  MADE-AT X64HARNESS:EXPECT-POP-DATA,
   0 CLAUSE,
   W-MADE CALL-WORD,  MADE-AT X64HARNESS:EXPECT-POP-DATA,
   X64HARNESS:PUSH-BANDS,  0 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu TMP-PATH X64HARNESS:BOOT-CLOSE, ;

\ The stand-in checkers' records, at the heap floor, and the scratch cells
\ their operations write. The active one's trust-raw keeps the lengths of the
\ name and the signature it is given; each operation adds its count.
DATA-START constant STAND-IN
STAND-IN $400 + constant OTHER-STAND-IN
0 constant SIG-U-AT
CELL constant NAME-U-AT
2 CELL * constant COUNT-AT
3 CELL * constant SINK-AT
1 constant RAW-COUNT                   \ the active checker's trust-raw
16 constant OTHER-COUNT                \ the target's, when it is another
256 constant TAIL-COUNT                \ the active one's rec-wide-publish
3 constant MIN-IN-ANSWER               \ ...and what its rec-min-in@ answers

: STAND-INS, ( -- )
   [: SIG-U-AT X64HARNESS:POP-SCRATCH,  SINK-AT X64HARNESS:POP-SCRATCH,
      NAME-U-AT X64HARNESS:POP-SCRATCH,  SINK-AT X64HARNESS:POP-SCRATCH,
      RAW-COUNT COUNT-AT X64HARNESS:ADD-SCRATCH, ;] X64HARNESS:ROUTINE,
   STAND-IN NCOMP-DISPATCH:DECL-RAW-OFF + X64HARNESS:LABEL-CELL!,
   [: 4 0 ?do SINK-AT X64HARNESS:POP-SCRATCH, loop
      OTHER-COUNT COUNT-AT X64HARNESS:ADD-SCRATCH, ;] X64HARNESS:ROUTINE,
   OTHER-STAND-IN NCOMP-DISPATCH:DECL-RAW-OFF + X64HARNESS:LABEL-CELL!,
   [: TAIL-COUNT COUNT-AT X64HARNESS:ADD-SCRATCH, ;] X64HARNESS:ROUTINE,
   STAND-IN NCOMP-DISPATCH:DECL-REC-WIDE-PUBLISH-OFF + X64HARNESS:LABEL-CELL!,
   [: MIN-IN-ANSWER X64HARNESS:PUSH, ;] X64HARNESS:ROUTINE,
   STAND-IN NCOMP-DISPATCH:DECL-REC-MIN-IN-OFF + X64HARNESS:LABEL-CELL!,
   STAND-IN NCOMP-DISPATCH:DECL-CELL X64HARNESS:DATA-ADDR!,
   STAND-IN NCOMP-DISPATCH:TARGET-DECL-CELL X64HARNESS:DATA-ADDR!,
   1 HOOK-CELL X64HARNESS:CELL!, ;

\ With a checker active and the check hook armed, a clause's signature
\ registers as MADE's raw effect under the name its body capture starts with,
\ through the active checker and, when the target is another, the target's
\ too. The checker's tail then runs and its min-in lands in MADE's flags,
\ whose DKIND and DNAME-WIDE are clear; CRSIG clears.
: CHECKER-IMAGE ( -- )
   false DOES-OPEN,
   STAND-INS,
   s" MADE create " {: a:ptr u:n :}
   a u BODYBUF-OFF X64HARNESS:TEXT!,  u BODYLEN-CELL X64HARNESS:CELL!,
   W-BUMP IMG ENTRY CLAUSE-SIG,
   W-MADE CALL-WORD,  MADE-AT 1+ X64HARNESS:EXPECT-POP-DATA,
   4 NAME-U-AT X64HARNESS:EXPECT-SCRATCH,
   4 SIG-U-AT X64HARNESS:EXPECT-SCRATCH,
   RAW-COUNT TAIL-COUNT + COUNT-AT X64HARNESS:EXPECT-SCRATCH,
   DKIND:MASK DNAME-WIDE or DNAME-MIN-IN-MASK or  MIN-IN-ANSWER 52 lshift  MADE-FLAGS,
   0 CRSIG-U-CELL X64HARNESS:EXPECT-CELL,
   OTHER-STAND-IN NCOMP-DISPATCH:TARGET-DECL-CELL X64HARNESS:DATA-ADDR!,
   W-BUMP2 IMG ENTRY CLAUSE-SIG,
   RAW-COUNT TAIL-COUNT + 2 *  OTHER-COUNT +  COUNT-AT X64HARNESS:EXPECT-SCRATCH,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   s" hb-x64-link-does-checker" TMP-PATH X64HARNESS:BOOT-CLOSE, ;

\ SEVEN's routine has no slot: does-patch exits 83 before it writes.
: SLOT-IMAGE ( -- )
   false DOES-OPEN,
   W-SEVEN IMG DREC * LASTC-CELL X64HARNESS:REGION-ADDR!,
   W-BUMP IMG ENTRY CLAUSE,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   s" hb-x64-link-does-slot-armed" TMP-PATH X64HARNESS:BOOT-CLOSE, ;

\ ---- the children --------------------------------------------------------------
create SHA-CTX SHA256-CTX-BYTES allot
create DIGEST 32 allot
create HEX 64 allot
create LINE 96 allot    variable LINE-U

: FEED ( ptr u8 n -- ) {: a:ptr u:n :} SHA-CTX a u SHA256-FEED ;
: HEX-DIGIT ( n -- n ) dup 10 < if $30 + exit then $57 + ;

\ What the layout wrote, as one digest in hex.
: LAYOUT-HEX ( -- ptr u8 n )
   SHA-CTX SHA256-BEGIN
   X64LINK:DICT$ FEED  X64LINK:CODE$ FEED  X64LINK:BITS$ FEED  X64LINK:INDEX$ FEED
   SHA-CTX DIGEST SHA256-END
   32 0 ?do
      DIGEST i + c@ {: b:n :}
      b 4 rshift HEX-DIGIT HEX i 2 * + c!
      b $F and HEX-DIGIT HEX i 2 * + 1+ c!
   loop
   HEX 64 ;

: DIGEST$ ( -- ptr u8 n ) s" x64-link: digest " ;

$8000 constant CAP
240000 constant CHILD-MS
create OUT CAP allot    variable OUT-U
create ERR CAP allot    variable ERR-U
create EMPTY 1 allot                 \ zero-length stdin
variable RC

: RUN-CHILD ( ptr u8 n -- ) {: m:ptr mu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/x86-64-link-records.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   m mu >LEN PROC-ARGV+
   s" bin/hb" >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  CHILD-MS >MS
   RUN-ARGV-STDIN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: SHOW-CHILD ( -- )
   s" x86-64-link-records: child rc=" type RC @ . cr
   s" x86-64-link-records: child stdout:" type cr OUT OUT-U @ type cr
   s" x86-64-link-records: child stderr:" type cr ERR ERR-U @ type cr ;

: SHIFT-CASE ( -- )
   s" a host with a record, a wordlist and a DATA cell more ahead of the window lays out the same bytes" T-LABEL
   DIGEST$ {: p:ptr pu:n :}
   p LINE pu BYTE-COPY
   LAYOUT-HEX {: h:ptr hu:n :}
   h LINE pu + hu BYTE-COPY
   pu hu + LINE-U !
   s" shift" RUN-CHILD
   OUT OUT-U @ LINE LINE-U @ CONTAINS? {: same:bool :}
   RC @ 0<> same 0= or if SHOW-CHILD then
   RC @ 0 T=
   same TTRUE ;

\ The child exits the layout's refusal status, naming `w` on stdout, where the
\ capture's refusals name what they refuse, and dying with `m`.
: REFUSED ( ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n w:ptr wu:n m:ptr mu:n :}
   mode modeu RUN-CHILD
   ERR ERR-U @ m mu CONTAINS?  OUT OUT-U @ w wu CONTAINS? and {: said:bool :}
   RC @ 74 <> said 0= or if SHOW-CHILD then
   RC @ 74 T=
   said TTRUE ;

: STRAY-CASE ( -- )
   s" a call to a window word with no x86-64 routine, a tier-0 word, is refused by the callee's name" T-LABEL
   s" stray" s" window record BARE has no x86-64 routine"
   s" x64link: a code record the capture's shadow carries no routine for" REFUSED ;

: STRIP-CASE ( -- )
   s" a capture that strips a live private word no shipped record carries refuses its routine by name" T-LABEL
   s" strip" s" routine of HELPER is live"
   s" aot-capture: a live shadow routine no shipped record carries" REFUSED ;

: CALLEE-CASE ( -- )
   s" a shadow call to a window word the capture strips is refused by the callee's name" T-LABEL
   s" callee" s" names window record LOW"
   s" aot-capture: a shadow site names a record the capture strips" REFUSED ;

\ GONE is a tier-0 word, with no routine of its own: a tier-1 GONE is live through
\ KEEP, and the capture refuses its routine first, at its own row, as
\ test/aot-shadow-capture.f's retired child shows.
: RETIRED-CASE ( -- )
   s" a shadow call to a retired window word is refused by the callee's name" T-LABEL
   s" retired" s" names window record GONE"
   s" aot-capture: a shadow site names a record the capture strips" REFUSED ;

: WID-FORGED-CASE ( -- )
   s" a protected-wid row outside the capture window is refused by its row" T-LABEL
   s" wid" s" protected-wid row 0"
   s" x64link: a protected wid outside the window or at PROT-WID-MAX" REFUSED ;

: UNRESOLVED-CASE ( -- )
   s" a call to a word of the engine's own prefix that no kernel body carries is refused by its name" T-LABEL
   s" unresolved" s" names STR=, which no x86-64 kernel body carries"
   s" x64link: a shadow site names a word the x86-64 kernel does not carry" REFUSED ;

: QUOT-CASE ( -- )
   s" a declared code cell holding a quotation's entry is refused by the cell" T-LABEL
   s" quot" s" which is window code no shipped record enters"
   s" aot-capture: a shadowed code cell targets code no shipped record enters" REFUSED ;

: CELLNAME-CASE ( -- )
   s" a code cell holding the xt of a prefix word no kernel body carries is refused by its name" T-LABEL
   s" cellname" s" holds STR=, which no x86-64 kernel body carries"
   s" x64link: a code cell names a word the x86-64 kernel does not carry" REFUSED ;

: PKGSITE-CASE ( -- )
   s" a site forged to name the package row, which has no routine, is refused by that row" T-LABEL
   s" pkgsite" s" X64LT-WIN, which has no x86-64 routine"
   s" x64link: a shadow site names a record with no x86-64 routine" REFUSED ;

: DOESSITE-CASE ( -- )
   s" a site forged to name the package row in a does> definer's routine is refused by the definer" T-LABEL
   s" doessite" s" the shadow routine of CONST at code byte"
   s" x64link: a shadow site names a record with no x86-64 routine" REFUSED ;

: PKGCELL-CASE ( -- )
   s" a code cell whose xt row names the package row, which has no routine, is refused by that row" T-LABEL
   s" pkgcell" s" X64LT-WIN, which has no x86-64 routine"
   s" x64link: a code cell names a record with no x86-64 routine" REFUSED ;

: XTLESS-CASE ( -- )
   s" a code cell the capture's xt rows do not key is refused by its row" T-LABEL
   s" xtless" s" holds window code no shipped record enters"
   s" x64link: a code cell targets code no shipped record enters" REFUSED ;

\ The pkgsite and doessite children point a site, CALLER's call or the first
\ call in CONST's routine, at row 0, the package.
: FORGE-SITE ( n -- ) {: s:n :}
   SITE-REC-TAG AOT-SHADOW:SITE-BUF@ s AOT-SHADOW:SITE-ROW * + 8 + LE:U32! ;

\ The pkgcell child points the one xt row, LEAF's cell, at row 0, the package.
: FORGE-XT ( -- ) 0 AOT-SHADOW:XT-BUF@ 4 + LE:U32! ;

\ The wid child moves the window's one protected row to its span.
: FORGE-WID ( -- ) AOT-WID-SPAN @ AOT-PWIN-BUF@ LE:U32! ;

\ The children that end in a refusal, before the layout returns.
: REFUSAL? ( -- bool )
   s" stray" MODE?  s" strip" MODE? or  s" callee" MODE? or  s" retired" MODE? or
   s" wid" MODE? or  s" unresolved" MODE? or  s" quot" MODE? or  s" xtless" MODE? or
   s" cellname" MODE? or  s" pkgsite" MODE? or  s" doessite" MODE? or
   s" pkgcell" MODE? or ;

public

: RUN ( -- )
   CAPTURE
   NSHADOW:CLOSE
   s" wid" MODE? if FORGE-WID then
   s" xtless" MODE? if 0 AOT-SHADOW:XT-N ! then
   s" pkgsite" MODE? if W-CALLER AOT-SHADOW:CALL SITE-IN FORGE-SITE then
   s" doessite" MODE? if W-CONST AOT-SHADOW:CALL SITE-IN FORGE-SITE then
   s" pkgcell" MODE? if FORGE-XT then
   KERNEL
   X64LINK:LAYOUT
   REFUSAL? if s" x86-64-link-records: laid out" type cr exit then
   s" shift" MODE? if DIGEST$ type LAYOUT-HEX type cr exit then
   T-RESET
   RECORDS-CASE
   ROUTINE-CASE
   DOES-CASE
   LONG-NAME-CASE
   PRIM-CASE
   WID-CASE
   BITMAP-CASE
   INDEX-CASE
   CALL-CASE
   TAIL-CASE
   CODE-CASE
   KERNEL-CASE
   FUN-CASE
   CLAUSE-CASE
   DATA-CASE
   CELL-CASE
   DEFINER-CASE
   DEFINER-DATA-CASE
   INDEX-IMAGE
   SHIFT-CASE
   STRAY-CASE
   STRIP-CASE
   CALLEE-CASE
   RETIRED-CASE
   WID-FORGED-CASE
   UNRESOLVED-CASE
   QUOT-CASE
   CELLNAME-CASE
   PKGSITE-CASE
   DOESSITE-CASE
   XTLESS-CASE
   PKGCELL-CASE
   false s" hb-x64-link-does" DOES-IMAGE
   true s" hb-x64-link-does-negative" DOES-IMAGE
   CHECKER-IMAGE
   SLOT-IMAGE
   s" x86-64-link-records: prims=" type X64LINK:PRIMS .
   s" records=" type X64LINK:RECORDS .
   s" code=" type X64LINK:CODE$ nip . cr
   T-REPORT ;

;using
;package

X64LT:RUN
