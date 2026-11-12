\ kernel-object.f - the tree's arm64 start-up and kernel, written as a kernel
\ object a build's link adopts.
\ Only a kernel build compiles this file, so the kernel and the layout it reads
\ come from the tree being built.
\
\ The object: the bytes HBKERNEL, the SHA-256 of every byte after it, then the
\ sections in this order. Every number is a u64, low byte first; a name is its
\ length, then its bytes padded with zeros to a multiple of 8.
\   code        its length in bytes, then the bytes padded as a name's
\   labels      n, then each label's word position, -1 while unbound
\   references  n, then each one's site, label and kind
\   data spans  n, then each span's first and end word position
\   fixups      n, then each pending one's site, label and kind
\   primitives  n, then each one's first label, last label, wordlist and name
\   scope sites n, then the label of scope-kind?'s chain for each kind from 2
\   symbols     n, then each one's name and value
\   presets     n, then each header cell's offset and the value the link writes
\   checker rows their length in bytes, then n, then each row's kind,
\               trusted-only, fixed-ABI, stow kind, name, package name, atom
\               count and atoms

require src/lib/le.f
require src/lib/string.f
require src/core/bytes.f
require src/core/sha256.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/wl.f
require src/habu/prims.f
require src/habu/primitive-registry.f
require src/arch/arm64/icode.f
require src/arch/arm64/boot.f
using CRASH
using ENGINE-EMIT

package KERNEL-OBJECT
private

DYNAMIC-BUFFER OUT u8                \ the object being written
variable OUT-U
variable COUNT-AT                    \ the open section's count, as an offset in OUT
variable COUNT-N
8 TYPED-BUFFER TAIL n                \ the labels START, and ENGINE-START, take, in their order
variable K0                          \ the kernel's first word position
SHA256-CTX-BYTES BUFFER: SHA-CTX


: U64+ ( n -- )
   OUT-U @ 8 + OUT-RESERVE
   0 OUT OUT-U @ + LE:U64!
   8 OUT-U +! ;

: SPAN+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 7 + -8 and {: end:n :}
   OUT-U @ end + OUT-RESERVE
   a  0 OUT OUT-U @ +  u BYTE-COPY
   end u ?do 0  0 OUT OUT-U @ + i + c! loop
   end OUT-U +! ;

: NAME+ ( ptr u8 n -- ) dup U64+ SPAN+ ;


: SECTION ( -- ) OUT-U @ COUNT-AT !  0 COUNT-N !  0 U64+ ;
: ENTRY ( -- ) 1 COUNT-N +! ;
: ;SECTION ( -- ) COUNT-N @  0 OUT COUNT-AT @ + LE:U64! ;


: TAIL@ ( n -- label ) TAIL A64ICODE:LABEL@ ;

: TEXT-VA ( -- n ) IMAGE-VA CODE-OFF + ;


\ The start-up at code offset 0, then the kernel. The tail's labels stay unbound
\ for the link to bind.
: EMIT ( bool -- )
   {: engine:bool :}
   ENGINE-EMIT:OPEN
   engine if ENGINE-EMIT:LINK-SCOPE-ENTRIES then
   engine if 8 else 4 then 0 ?do A64ICODE:LBL i TAIL A64ICODE:LABEL! loop
   engine if
      TEXT-VA  0 TAIL@ 1 TAIL@ 2 TAIL@ 3 TAIL@ 4 TAIL@ 5 TAIL@ 6 TAIL@ 7 TAIL@
      A64BOOT:ENGINE-START,
   else
      TEXT-VA  0 TAIL@ 1 TAIL@ 2 TAIL@ 3 TAIL@  A64BOOT:START,
   then
   A64ICODE:ASM-CP @ K0 !
   ENGINE-EMIT:KERNEL ;


: CODE+ ( -- ) A64ICODE:ASM-LEN dup U64+  A64ICODE:CODE swap SPAN+ ;

: LABELS+ ( -- )
   A64ICODE:NLBL @ dup U64+
   0 ?do i A64ICODE:LBLP @ U64+ loop ;

: REFS+ ( -- )
   A64ICODE:REFS dup U64+
   0 ?do
      i A64ICODE:REF-SITE U64+  i A64ICODE:REF-LABEL LABEL>N U64+  i A64ICODE:REF-KIND U64+
   loop ;

: DATA-SPANS+ ( -- )
   A64ICODE:DATA-SPANS dup U64+
   0 ?do i A64ICODE:DATA-SPAN-FIRST U64+  i A64ICODE:DATA-SPAN-END U64+ loop ;

: FIXUPS+ ( -- )
   SECTION
   A64ICODE:NLBL @ 0 ?do
      i A64ICODE:FXH @
      begin dup 0 >= while
         dup cells A64ICODE:FXS + @ U64+  i U64+  dup cells A64ICODE:FXK + @ U64+  ENTRY
         cells A64ICODE:FXN + @
      repeat drop
   loop
   ;SECTION ;

\ Refuses a pending chain on a label no emitter placed, which links as a branch
\ or address to its own site.
: ?PLACED ( bool -- )
   if 8 else 4 then {: tails:n :}
   A64ICODE:NLBL @ 0 ?do
      i A64ICODE:FXH @ 0 >= if
         false  tails 0 ?do  j i TAIL@ LABEL>N = or  loop
         0= if
            SB-RESET
            s" kernel-object: a reference to a label nothing places label=" SB-APPEND
            i A64ICODE:IDEC$ SB-APPEND
            s"  site=" SB-APPEND
            i A64ICODE:FXH @ cells A64ICODE:FXS + @ A64ICODE:IDEC$ SB-APPEND
            SB$ A64ICODE:ICODE-EXIT-RC die
         then
      then
   loop ;

: PRIMS+ ( -- )
   ENGINE-PRIMS:COUNT dup U64+
   0 ?do
      i ENGINE-PRIMS:FIRST-LABEL LABEL>N U64+
      i ENGINE-PRIMS:LAST-LABEL LABEL>N U64+
      i ENGINE-PRIMS:HELPER-WID U64+
      i ENGINE-PRIMS:NAME$ NAME+
   loop ;

\ A stripped program's kernel reads no chain.
: SCOPES+ ( bool -- )
   if
      7 U64+  9 2 ?do i ENGINE-EMIT:SCOPE-ENTRY-SITE LABEL>N U64+ loop
   else
      0 U64+
   then ;


: SYM+ ( ptr u8 n n -- )
   {: a:ptr u:n v:n :}
   a u NAME+  v U64+  ENTRY ;

: LSYM+ ( ptr u8 n label -- ) LABEL>N SYM+ ;

: COMMON-SYMBOLS ( -- )
   s" K0" K0 @ SYM+
   s" IMAGE" 0 TAIL@ LSYM+
   s" IMAGE-END" 1 TAIL@ LSYM+
   s" LIST" 2 TAIL@ LSYM+
   s" LIST-END" 3 TAIL@ LSYM+
   s" UNCAUGHT" LUNCAUGHT A64ICODE:LABEL@ LSYM+
   s" SIGH" LSIGH A64ICODE:LABEL@ LSYM+
   s" NCOUNT" LNCOUNT A64ICODE:LABEL@ LSYM+
   s" DICT" LDICT A64ICODE:LABEL@ LSYM+
   s" TEXT-VA" TEXT-VA SYM+
   s" DATA-VA" DATA-VA VA>N SYM+
   s" DATA-START" DATA-START SYM+
   s" DATA-SIZE" DATA-SIZE SYM+
   s" DP-CEILING-OFF" DATA-SIZE PROF-CNT-BYTES - SYM+
   s" DP-CELL" DP-CELL SYM+
   s" DP-FLOOR-CELL" BOOT:DP-FLOOR-CELL SYM+
   s" DP-CEILING-CELL" BOOT:DP-CEILING-CELL SYM+
   s" HEAP-START-CELL" BOOT:HEAP-START-CELL SYM+
   s" FD-PTR-CELL" SIGNAL-ABI:FD-PTR-CELL SYM+
   s" FD-CELL" SIGNAL-ABI:FD-CELL SYM+
   s" RBASE-CELL" RBASE-CELL SYM+
   s" UNCGH-CELL" UNCGH-CELL SYM+
   s" STUB-CELL" SIGNAL-ABI:STUB-CELL SYM+
   s" CODE-END-CELL" CODE-END-CELL SYM+ ;

: ENGINE-SYMBOLS ( -- )
   s" RECS" 4 TAIL@ LSYM+
   s" RECS-END" 5 TAIL@ LSYM+
   s" RLIST" 6 TAIL@ LSYM+
   s" RLIST-END" 7 TAIL@ LSYM+
   s" EVALREC" LEVALREC A64ICODE:LABEL@ LSYM+
   s" FLOORREC" LFLOORREC A64ICODE:LABEL@ LSYM+
   s" EVALREC-CELL" EVALREC-CELL SYM+
   s" FLOORREC-CELL" FLOORREC-CELL SYM+
   s" SEAL-NDICT-CELL" SEAL-NDICT-CELL SYM+
   s" WIDN-CELL" WIDN-CELL SYM+
   s" TYPES-WL-CELL" BOOT:TYPES-WL-CELL SYM+ ;

: SYMBOLS+ ( bool -- )
   {: engine:bool :}
   SECTION
   s" ENGINE" engine if 1 else 0 then SYM+
   COMMON-SYMBOLS
   engine if ENGINE-SYMBOLS then
   engine if ENGINE-MAIN:XT-CELL else APP-ENTRY:XT-CELL then  s" ENTRY-CELL" rot SYM+
   ;SECTION ;


: PRESET+ ( n n -- )
   {: v:n off:n :}
   off U64+  v U64+  ENTRY ;

\ A bound label's byte offset in the code; labels hold word positions.
: CODE-OFFSET ( label -- n ) LABEL>N A64ICODE:LBLP @ 4 * ;

: PRESETS+ ( bool -- )
   {: engine:bool :}
   SECTION
   0 BOOT:GLOBAL-WL-CELL PRESET+
   0 BOOT:IMAGE-BASE-CELL PRESET+
   WL:NAMESPACE BOOT:NAMESPACE-WL-CELL PRESET+
   engine if
      0 CUR-CELL PRESET+
      0 HIDXP-CELL PRESET+
      0 HIDX:CLAIMS PRESET+
      0 LASTC-CELL PRESET+
      0 PKG-REC-CELL PRESET+
      0 ENCLOSING:REC PRESET+
      0 POLICY-NDICT-CELL PRESET+
      LDICT A64ICODE:LABEL@ CODE-OFFSET PRIM-TABLE-CELL PRESET+
   then
   ;SECTION ;


: FLAG+ ( bool -- ) if 1 else 0 then U64+ ;

: CHECK-ROW+ ( n -- )
   {: row:n :}
   row PRIM-SPEC:KIND@ U64+
   row PRIM-SPEC:TRUSTED-ONLY? FLAG+
   row PRIM-SPEC:FIXED-ABI? FLAG+
   row PRIM-SPEC:C2-STOW-KIND U64+
   row PRIM-SPEC:NAME$ NAME+
   row PRIM-SPEC:PKG$ NAME+
   row PRIM-SPEC:CODE-LEN@ dup U64+
   0 ?do row i PRIM-SPEC:CODE@ U64+ loop ;

\ The table in its own order: an overloaded name's rows replay in it.
: CHECK-ROWS+ ( -- )
   OUT-U @ {: at:n :}
   0 U64+
   PRIM-SPEC:COUNT dup U64+
   0 ?do i CHECK-ROW+ loop
   OUT-U @ at - 8 -  0 OUT at + LE:U64! ;


: SUM ( -- )
   SHA-CTX  0 OUT 40 +  OUT-U @ 40 -  0 OUT 8 +  SHA256-IN ;

: WRITE-OUT ( bool [ ptr u8 n -- ] -- )
   {: engine:bool sink :}
   engine EMIT  engine ?PLACED
   0 OUT-U !
   s" HBKERNEL" SPAN+  4 0 ?do 0 U64+ loop
   CODE+ LABELS+ REFS+ DATA-SPANS+ FIXUPS+ PRIMS+
   engine SCOPES+  engine SYMBOLS+  engine PRESETS+  CHECK-ROWS+
   SUM
   0 OUT OUT-U @ sink execute ;

: RELEASE ( -- ) OUT-RELEASE A64ICODE:ASM-RELEASE ;

public

\ The object's bytes go once to the sink, valid only during the call.
: WRITE ( bool [ ptr u8 n -- ] -- ) ['] WRITE-OUT ['] RELEASE finally ;

;package
;using
;using
