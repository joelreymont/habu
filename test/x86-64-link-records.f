\ x86-64-link-records.f - src/habu/link-x64.f lays a captured x86-64 window out
\ and links it: records at their final addresses, routines copied from the
\ shadow to code slots with every site resolved, wids rebased to the image's
\ numbering, the protected-wid bitmap, the name index and each declared code
\ cell's xt.

package X64LT
public
ndict@ here BYTE-VIEW NULL-PTR BYTE-VIEW - variable PRE-R  variable PRE-D  PRE-D !  PRE-R !
variable PUB-WID                     \ the window package's public wordlist, host's id
variable PRIVATE-WID
;package

require src/lib/string.f

package X64LT
public
\ Registered as `SUITE x86-64-link-records`. A child: bin/hb --load
\ test/x86-64-link-records.f -- MODE, MODE one of shift stray linked callee
\ retired wid unresolved cellname pkgsite doessite xtless pkgcell
: MODE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ a u STR= exit then
   false ;
variable IMAGE-MODE
1024 constant IMAGE-CAP
create IMAGE-PATH IMAGE-CAP allot
variable IMAGE-PATH-U
: IMAGE-PATH$ ( -- ptr u8 n ) IMAGE-PATH IMAGE-PATH-U @ ;
: SAVE-IMAGE ( -- )
   s" image" MODE?  s" stripimage" MODE? or 0= if exit then
   SCRIPT-ARGC 2 <> if s" x86-64-link-records: image path required" 64 die then
   1 SCRIPT-ARGV$ {: path:ptr size:n :}
   size IMAGE-CAP > if s" x86-64-link-records: image path too long" 64 die then
   path IMAGE-PATH size BYTE-COPY
   size IMAGE-PATH-U !
   1 IMAGE-MODE ! ;
\ The determinism child puts a record, a wordlist and a DATA cell ahead of it all.
: SHIFT$ ( -- ptr u8 n )
   s" shift" MODE? if s" variable SHIFT-CELL wordlist drop" exit then
   s" " ;
: EXTRA$ ( -- ptr u8 n )
   s" stripimage" MODE? if
      s" private : HELPER ( n -- n ) 3 * ; public : USER ( n -- n ) HELPER 1+ ; : IMAGE-ENTRY ( -- ) 6 USER 19 = if 42 . else 99 . then ; : IMAGE-ENTRY! ( -- ) ['] IMAGE-ENTRY data-base APP-ENTRY:XT-CELL + xt! ; IMAGE-ENTRY!"
      exit
   then
   s" image" MODE? if
      s" : IMAGE-ENTRY ( -- ) 42 . ; : IMAGE-ENTRY! ( -- ) ['] IMAGE-ENTRY data-base APP-ENTRY:XT-CELL + xt! ; IMAGE-ENTRY!"
      exit
   then
   s" stray" MODE? if s" 0 set-tier : BARE ( -- n ) 7 ; 1 set-tier : WRAP ( -- n ) BARE 1+ ;" exit then
   s" linked" MODE? if
      s" private get-current X64LT:PRIVATE-WID ! : HELPER ( n -- n ) 3 * ; public : USER ( n -- n ) HELPER 1+ ; private : SHARED ( -- n ) 7 ; public EXPORT SHARED : Q ( -- [ n -- n ] ) [: 1 + ;] ;  Q align here 0 , xt!"
      exit
   then
   s" callee" MODE? if s" private 0 set-tier : LOW ( -- n ) 7 ; public 1 set-tier : HIGH ( -- n ) LOW 1+ ;" exit then
   s" retired" MODE? if s" 0 set-tier : GONE ( -- n ) 7 ; 1 set-tier : KEEP ( -- n ) GONE 1+ ; undefine GONE" exit then
   s" unresolved" MODE? if s" : SAME ( ptr u8 n ptr u8 n -- bool ) STR= ;" exit then
   s" cellname" MODE? if s" ' STR= align here 0 , xt!" exit then
   s" " ;
SHIFT$ evaluate
;package

\ tools/native-emit-x64.f, the image modes' writer, sizes the shared image
\ buffer under `using X64CODE`, so it loads before the ARM64 code layer that
\ sets the capture budget.
require tools/native-emit-x64.f
require test/x86-64-boot-harness.f
require src/lib/fs-mutate.f
require src/lib/le.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/address-cells.f
require src/habu/aot-capture.f
require src/habu/aot-shadow.f
require src/compiler/native/string.f
require src/compiler/native/x64ir.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/passes.f
require src/habu/link-x64.f
require src/core/sha256.f
require src/lib/process-argv.f
require test/pcap-result.f

X64LT:SAVE-IMAGE

\ A binding is a multi-cell value, which only a compiled body may hold.
package X64LT
public
: OPEN-X64 ( -- ) X64ABI:BINDING NSHADOW:OPEN ;
;package
\ The window is compiled as test/aot-shadow-capture.f compiles its own; the
\ kernel's rows come from test/x86-64-boot-harness.f.
X64LT:OPEN-X64
DATA-START ADDRESS-CELLS:KEEP-BELOW
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
X64LT:EXTRA$ evaluate
0 set-tier

' LEAF align here 0 , xt!
' negate align here 0 , xt!

get-current prot-wid-add
;package

AOT-ARM:WINDOW-CLOSE

package X64LT
using AOT-BUF

: WIDX ( ptr u8 n -- n ) XREF-FIND-INDEX AOT-ARM:R0 @ - ;
: W-LEAF ( -- n ) s" X64LT-WIN:LEAF" WIDX ;
: W-CALLER ( -- n ) s" X64LT-WIN:CALLER" WIDX ;
: W-CONST ( -- n ) s" X64LT-WIN:CONST" WIDX ;
: W-LONG ( -- n ) s" X64LT-WIN:SPELLED-PAST-SIXTEEN" WIDX ;
: CREC ( n -- ptr u8 ) {: k:n :} AOT-REC-BUF@ AOT-REC-MAX 48 * + k AOT-CREC-ROW * + ;
: SHIPPED ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   AOT-REC-N @ 0 ?do
      AOT-NAMES-BUF@ i CREC 8 + LE:U32@ + {: e:ptr :}
      e 1+ e c@ a u STR= if drop i leave then
   loop ;
: W-HELPER ( -- n ) s" HELPER" SHIPPED ;
: W-USER ( -- n ) s" USER" SHIPPED ;

: IMG ( n -- n ) X64LINK:PRIMS + ;
: RF@ ( n n -- n ) {: k:n f:n :} X64LINK:DICT$ drop k DREC * + f + LE:U64@ ;
: NAME$ ( n -- ptr u8 n ) X64LINK:REC-NAME$ ;
: BAND ( n -- ptr u8 ) {: va:n :} X64LINK:CODE$ drop va X64LINK:CODE-VA - + ;
: SH@ ( n n -- n ) {: r:n f:n :}
   AOT-SHADOW:REC-BUF@ r AOT-SHADOW:REC-ROW * + f + LE:U32@ ;

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

: CAPTURE ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:TARGET-CAPTURE ;

: KERNEL ( -- ) X64HARNESS:INIT  false X64HARNESS:BOOT-OPEN, ;

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

\ A site's address comes from its routine's laid record and the capture's rows,
\ and where it lands from decoding the image's bytes there, never from the
\ linker's placement.
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
: KENTRY ( ptr u8 n -- n ) X64KERNEL:ENTRY-LABEL X64CODE:LABEL-AT X64LINK:TEXT-VA + ;

\ The first shadow row holding shadow code byte n: a does> definer's, not its
\ companion's.
: HOLDER ( n -- n ) {: at:n :}
   -1
   AOT-SHADOW:REC-N @ 0 ?do
      at i 4 SH@ >=  at i 4 SH@ i 8 SH@ + < and if drop i leave then
   loop ;

: START-VA ( n -- n ) {: r:n :} r 0 SH@ IMG ENTRY  r 12 SH@ - ;

: SITE-VA ( n -- n ) {: s:n :}
   s 0 SITE@ HOLDER {: r:n :}
   r START-VA  s 0 SITE@ r 4 SH@ - + ;

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
: CAPTURED-IMM ( n -- n ) {: s:n :} AOT-SHADOW:CODE-BUF@ s 0 SITE@ + 2 + LE:U64@ ;

\ LEAF has no site, so its laid bytes equal the capture's.
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

\ The window's DATA lands at DATA-AT, as far into it as the string lies into the
\ host's window, and a cell keeps the alignment it was captured with.
: DATA-CASE ( -- )
   s" a DATA literal holds the image address of the bytes it named in the host's window" T-LABEL
   W-GREET AOT-SHADOW:DATA SITE-IN {: s:n :}
   s 0 >= TTRUE
   s SITE-VA {: va:n :}
   va MOVABS? TTRUE
   X64LT-WIN:GREET {: a:ptr u:n :}
   a u s" linked" T$=
   a data-base BYTE-VIEW - DATA-VA VA>N + AOT-ARM:D0 @ - {: off:n :}
   X64LAYOUT:DATA-VA VA>N X64LINK:DATA-AT + {: base:n :}
   va IMM@ base off + T=
   X64LINK:DATA-AT X64LINK:INDEX-END >= TTRUE
   X64LINK:HEAP-FLOOR X64LINK:DATA-AT >= TTRUE
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

\ A linked image must retain declarations for a later AOT capture of its own
\ REGION. A patched instruction with no SITES row cannot contribute a closure;
\ a resolved xt whose cell lost its declaration cannot be recaptured safely.
: HAS-IMAGE-SITE? ( ptr u8 n n -- bool ) {: data:ptr off:n kind:n :}
   data SNAP-RELOC:SITE-N-CELL + LE:U64@ {: count:n :}
   count SNAP-RELOC:SITE-CAP > if 0 0<> exit then
   count 0 ?do
      data SNAP-RELOC:SITE-ROWS-OFF + i SNAP-RELOC:SITE-ROW-BYTES * + {: row:ptr :}
      row LE:U32@ off =  row SNAP-RELOC:SITE-KIND-OFF + c@ kind = and if
         0 0= unloop exit
      then
   loop
   0 0<> ;

: HAS-IMAGE-CELL? ( ptr u8 n -- bool ) {: data:ptr off:n :}
   data SNAP-RELOC:XTCELL-N-CELL + LE:U64@ {: count:n :}
   count ADDRESS-CELLS:BOOT-CAP > if 0 0<> exit then
   count 0 ?do
      data ADDRESS-CELLS:BOOT-OFF + i cells + LE:U64@
         SNAP-RELOC:XTCELL-OFF-MASK and off = if 0 0= unloop exit then
   loop
   0 0<> ;

: REGISTRY-CASE ( -- )
   s" linked call, address and xt declarations survive in image DATA" T-LABEL
   X64LINK:DATA$ {: data:ptr size:n :}
   size ADDRESS-CELLS:BOOT-OFF > TTRUE
   data SNAP-RELOC:SITE-N-CELL + LE:U64@ 0 > TTRUE
   data SNAP-RELOC:XTCELL-N-CELL + ADDRESS-CELLS:MAGIC-FIELD + LE:U64@
      ADDRESS-CELLS:MAGIC T=
   W-CALLER AOT-SHADOW:CALL SITE-IN SITE-VA X64LINK:REGION-VA -
      SNAP-RELOC:SITE-CALL data -rot HAS-IMAGE-SITE? TTRUE
   W-TICK AOT-SHADOW:CODE SITE-IN SITE-VA X64LINK:REGION-VA -
      SNAP-RELOC:SITE-ADDR data -rot HAS-IMAGE-SITE? TTRUE
   data APP-ENTRY:XT-CELL HAS-IMAGE-CELL? TTRUE ;

: NAMES-CASE ( -- )
   s" a sidecar span names the placed x64 routine rather than the host blob" T-LABEL
   W-LEAF ROW-OF X64LINK:NAMES-SPAN {: off:n raw:n :}
   off W-LEAF IMG ENTRY X64LINK:CODE-VA - T=
   raw W-LEAF IMG 8 RF@ T= ;

\ The code band lies DICT-SIZE past the records and holds the long names. The
\ index image exits 0 when every lookup held; it runs on an x86-64 host.
\ xref-search-wl folds case and keys by wordlist: DUP finds dup, LEAF in
\ wordlist 0 misses.
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

\ Ors into the cell under it the distance from record k to where the lookup of
\ its own name and wordlist lands: zero when it finds it.
: LOOKUP, ( n -- ) {: k:n :}
   k NAME$ k X64KERNEL:REC-WID RF@ XREF,
   k DREC * X64HARNESS:PUSH-REGION,
   s" -" X64HARNESS:CALL-ROW,  s" or" X64HARNESS:CALL-ROW, ;

using X64ASM
using X64CODE
using X64RT
: CALL-USER, ( n -- ) {: at:n :}
   RAX ENGINE-GPR:X64-DBASE >R64 at X64LINK:REGION-VA - MEM-OFF ASM-SINK ENC-LEA
   RAX ASM-SINK ENC-CALL-REG ;
;using
;using
;using

: INDEX-IMAGE ( -- )
   X64LINK:DICT$ 0 STAGE
   X64LINK:CODE$ DICT-SIZE STAGE
   X64LINK:RECORDS X64HARNESS:PUSH,  s" ndict!" X64HARNESS:CALL-ROW,
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

: STRIP-IMAGE ( -- )
   X64LINK:DICT$ 0 STAGE
   X64LINK:CODE$ DICT-SIZE STAGE
   X64LINK:RECORDS X64HARNESS:PUSH, s" ndict!" X64HARNESS:CALL-ROW,
   STAGE-INDEX
   PRIVATE-WID @ AOT-ARM:W0 @ - X64LINK:T0 + {: private-wid:n :}
   PUB-WID @ AOT-ARM:W0 @ - X64LINK:T0 + {: public-wid:n :}
   s" HELPER" private-wid XREF, W-HELPER IMG X64HARNESS:EXPECT-ROW,
   s" HELPER" public-wid XREF, 0 X64HARNESS:EXPECT-POP,
   7 X64HARNESS:PUSH,
   W-USER IMG ENTRY CALL-USER,
   22 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   s" hb-x64-link-private" TMP-PATH X64HARNESS:BOOT-CLOSE, ;

create SHA-CTX SHA256-CTX-BYTES allot
create DIGEST 32 allot
create HEX 64 allot
create LINE 96 allot    variable LINE-U

: FEED ( ptr u8 n -- ) {: a:ptr u:n :} SHA-CTX a u SHA256-FEED ;
: HEX-DIGIT ( n -- n ) dup 10 < if $30 + exit then $57 + ;

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
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

: RUN-STRIP-IMAGE ( -- )
   HB-TARGET-LINUX-X86-64? 0= if exit then
   PROC-ARGV-RESET
   s" hb-x64-link-private" TMP-PATH >LEN PROC-ARGV+
   s" hb-x64-link-private" TMP-PATH >LEN
      EMPTY 0 >LEN OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS
      RUN-ARGV-STDIN-CAPTURE
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U !
   RC @ 0 T= ;

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

\ A refusal ends the process before the layout returns, so each refusal runs in
\ a child. The child exits 74 naming `w` on stdout, as the capture's refusals
\ name what they refuse, and dies with `m`. The capture refuses calls to tier-0
\ and retired words before any layout: a reached shadow target must have a
\ native routine.
: REFUSED ( ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n w:ptr wu:n m:ptr mu:n :}
   mode modeu RUN-CHILD
   ERR ERR-U @ m mu CONTAINS?  OUT OUT-U @ w wu CONTAINS? and {: said:bool :}
   RC @ 74 <> said 0= or if SHOW-CHILD then
   RC @ 74 T=
   said TTRUE ;

: STRAY-CASE ( -- )
   s" a call to a tier-0 word with no x86-64 routine is refused by the callee's name" T-LABEL
   s" stray" s" names window record BARE"
   s" aot-capture: a shadow site names a record the capture strips" REFUSED ;

\ The linked child's window holds the strip, export and quotation subjects.
: LINKED-CASE ( -- )
   s" linked" RUN-CHILD
   OUT OUT-U @ s" x86-64-link-records: linked private" CONTAINS? {: strip:bool :}
   OUT OUT-U @ s" x86-64-link-records: quotation cell linked" CONTAINS? {: quot:bool :}
   RC @ 0<> strip 0= or quot 0= or if SHOW-CHILD then
   s" a window with a live private helper, a public EXPORT alias of a live private body and a quotation code cell lays out" T-LABEL
   RC @ 0 T=
   s" a live private helper ships with its public caller and links with its private WID" T-LABEL
   strip TTRUE
   s" a declared code cell holds its anonymous quotation entry in linked image DATA" T-LABEL
   quot TTRUE
   RUN-STRIP-IMAGE ;

: STRIP-CHILD ( -- )
   T-RESET
   W-HELPER 0 >= TTRUE
   W-USER 0 >= TTRUE
   W-HELPER CREC 16 + LE:U32@ PRIVATE-WID @ AOT-ARM:W0 @ - 1+ T=
   W-HELPER IMG X64KERNEL:REC-WID RF@
      PRIVATE-WID @ AOT-ARM:W0 @ - X64LINK:T0 + T=
   W-USER IMG X64KERNEL:REC-WID RF@
      PUB-WID @ AOT-ARM:W0 @ - X64LINK:T0 + T=
   W-USER AOT-SHADOW:CALL SITE-IN {: s:n :}
   s 0 >= TTRUE
   s 8 SITE@ W-HELPER SITE-REC-TAG or T=
   STRIP-IMAGE
   T-REPORT
   s" x86-64-link-records: linked private" type cr ;

: CALLEE-CASE ( -- )
   s" a shadow call without a native routine is refused by the callee's name" T-LABEL
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

: QUOT-LINKED ( -- )
   -1
   AOT-SHADOW:XT-N @ 0 ?do
      i 4 XT@ SITE-SHADOW-TAG and SITE-SHADOW-TAG = if drop i leave then
   loop {: x:n :}
   x 0 < if s" x86-64-link-records: quotation cell lost its shadow row" 74 die then
   x 4 XT@ SITE-TARGET-MASK and {: r:n :}
   r 0 SH@ AOT-SHADOW:ANON-REC and 0= if
      s" x86-64-link-records: quotation cell points to a named row" 74 die
   then
   r X64LINK:NAMES-SPAN drop X64LINK:CODE-VA + {: entry:n :}
   x 0 XT@ {: c:n :}
   c X64LINK:CELL-XT entry <> if
      s" x86-64-link-records: quotation cell points to another routine" 74 die
   then
   AOT-WINDOW:XTOFF-BUF@ c AOT-WINDOW:XTOFF-ROW * + LE:U32@ {: loc:n :}
   loc AOT-WINDOW:XTOFF-WINDOW-TAG and 0= if
      s" x86-64-link-records: quotation cell left its window" 74 die
   then
   X64LINK:DATA$ drop X64LINK:DATA-AT + loc AOT-WINDOW:XTOFF-LOC-MASK and +
      LE:U64@ entry <> if
      s" x86-64-link-records: image DATA lost the quotation entry" 74 die
   then
   s" x86-64-link-records: quotation cell linked" type cr ;

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

\ Row 0 is the package, which has no x86-64 routine; pkgsite forges CALLER's
\ call to it, doessite the first call in CONST's routine.
: FORGE-SITE ( n -- ) {: s:n :}
   SITE-REC-TAG AOT-SHADOW:SITE-BUF@ s AOT-SHADOW:SITE-ROW * + 8 + LE:U32! ;

\ The pkgcell child points the one xt row, LEAF's cell, at row 0, the package.
: FORGE-XT ( -- ) 0 AOT-SHADOW:XT-BUF@ 4 + LE:U32! ;

\ The wid child moves the window's one protected row to its span.
: FORGE-WID ( -- ) AOT-WID-SPAN @ AOT-PWIN-BUF@ LE:U32! ;

: REFUSAL? ( -- bool )
   s" stray" MODE?  s" callee" MODE? or  s" retired" MODE? or
   s" wid" MODE? or  s" unresolved" MODE? or  s" xtless" MODE? or
   s" cellname" MODE? or  s" pkgsite" MODE? or  s" doessite" MODE? or
   s" pkgcell" MODE? or ;

public

: IMAGE-CASE ( -- )
   AOT-FILE:OWN dup NATIVE-LAYOUT:CURRENT IMAGE-PATH$ NATIVE-EMIT:WRITE
   AOT-OWNED:CLOSE
   IMAGE-PATH$ FS:CHMOD-X ;

: RUN ( -- )
   CAPTURE
   NSHADOW:CLOSE
   s" wid" MODE? if FORGE-WID then
   s" xtless" MODE? if 0 AOT-SHADOW:XT-N ! then
   s" pkgsite" MODE? if W-CALLER AOT-SHADOW:CALL SITE-IN FORGE-SITE then
   s" doessite" MODE? if W-CONST AOT-SHADOW:CALL SITE-IN FORGE-SITE then
   s" pkgcell" MODE? if FORGE-XT then
   IMAGE-MODE @ 0<> if IMAGE-CASE exit then
   KERNEL
   X64LINK:LAYOUT
   s" linked" MODE? if QUOT-LINKED STRIP-CHILD exit then
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
   REGISTRY-CASE
   NAMES-CASE
   INDEX-IMAGE
   SHIFT-CASE
   STRAY-CASE
   LINKED-CASE
   CALLEE-CASE
   RETIRED-CASE
   WID-FORGED-CASE
   UNRESOLVED-CASE
   CELLNAME-CASE
   PKGSITE-CASE
   DOESSITE-CASE
   XTLESS-CASE
   PKGCELL-CASE
   s" x86-64-link-records: prims=" type X64LINK:PRIMS .
   s" records=" type X64LINK:RECORDS .
   s" code=" type X64LINK:CODE$ nip . cr
   T-REPORT ;

;using
;package

X64LT:RUN
