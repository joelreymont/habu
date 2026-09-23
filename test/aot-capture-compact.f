\ Capture graph and relocation checks with independently chosen coordinates.
require lib/test.f
require lib/process-argv.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

package CGT-USER
public
: die ( n -- n ) 1+ ;
: throw ( n -- n ) 1+ ;
;package

\ Tier 1 ends this definition at the engine throw; there is no fallback die.
1 set-tier
ndict@ here AOT-CAPTURE:PRELUDE-MARK
AOT-ARM:WINDOW-OPEN
NSTR:WINDOW-OPEN
package CGT-WINDOW
: DEAD ( -- n ) 17 ;
: CALLEE ( n -- n ) 1+ ;
public
: LIVE ( n -- n ) CALLEE ;
: FATAL ( -- ) E-A-EMPTY throw ;
private
: AFTER-FATAL ( -- n ) 173 ;
public
variable ABCX-LONG-METADATA
variable ABCT-LONG-METADATA
variable ABCP-LONG-METADATA
variable ABCX-LONG-RETIRED
undefine ABCX-LONG-RETIRED
;package
AOT-ARM:WINDOW-CLOSE

package AOT-CAPTURE
using AOT-BUF

: CGT-FIND ( ptr u8 n -- n ) {: name:ptr size:n :}
   MAP-N 0 ?do
      i MAP-NAME$ name size CORE-STR= if i unloop exit then
   loop
   s" capture-compact: fixture record missing" 74 die ;

: CGT-REAL ( -- )
   s" a real capture drops a dead private body and retains its live sibling" T-LABEL
   AOT-ARM:WINDOW$ CAPTURE
   s" DEAD" CGT-FIND dup MAP-NAMED 0 T= MAP-START -1 T=
   s" CALLEE" CGT-FIND dup MAP-NAMED 0 T= MAP-START 0 >= TTRUE
   s" LIVE" CGT-FIND dup MAP-NAMED 1 T= MAP-START 0 >= TTRUE
   s" AFTER-FATAL" CGT-FIND dup MAP-NAMED 0 T= MAP-START -1 T=
   AOT-BLOB-LEN @ AOT-ARM:B1 @ AOT-ARM:B0 @ - < TTRUE
   CODE-WINDOW {: first:n end:n size:n :}
   first AOT-ARM:B0 @ T= end AOT-ARM:B1 @ T= size AOT-BLOB-LEN @ T= ;

: CGT-ROW ( n n n -- ) {: k:n start:n size:n :}
   k ACAP-REC-DST {: row:ptr :}
   48 0 ?do 0 row i + c! loop
   start row AOT-N-C!
   size CODE-SPAN:EXACT row 8 + AOT-N-C! ;

: CGT-SETUP ( -- )
   ACAP-RESET
   6 AOT-REC-N ! 6 ACAP-REC-ALL ! 24 AOT-BLOB-LEN !
   6 ACAP-GSITE-RESERVE
   6 0 ?do
      0 i ACAP-GSITE !
      $D65F03C0 AOT-BLOB-BUF@ i 4 * + AOT-P32!
   loop
   0 397 4 CGT-ROW
   398 0 ACAP-REC-DST 8 + AOT-N-C!
   -1 0 ACAP-REC-DST 40 + AOT-N-C!      \ namespace fields are not code
   1 0 4 CGT-ROW
   2 4 4 CGT-ROW
   3 8 8 CGT-ROW                     \ unreachable body to be removed
   4 16 4 CGT-ROW
   5 16 4 CGT-ROW                    \ same-entry alias
   ACAP-GRAPH-INDEX 0 ACAP-GWORK-N ! ;

: CGT-INDEX ( -- )
   s" code owners exclude namespace rows and retain aliases" T-LABEL
   0 ACAP-GOWNER @ 2 T=
   1 ACAP-GOWNER @ 3 T=
   2 ACAP-GOWNER @ 4 T=
   3 ACAP-GOWNER @ 4 T=
   4 ACAP-GOWNER @ 5 T=
   5 ACAP-GOWNER @ 0 T=
   3 ACAP-GRAPH-START 8 T=
   3 ACAP-GRAPH-END 16 T= ;

: CGT-CALL ( -- )
   s" a boundary call keeps both continuation and target, removing only dead code" T-LABEL
   $94000004 AOT-BLOB-BUF@ AOT-P32!
   1 ACAP-GRAPH-MARK-REC ACAP-GRAPH-SWEEP
   -1 ACAP-GRAPH-READY !
   1 ACAP-GRAPH-LIVE? TTRUE 2 ACAP-GRAPH-LIVE? TTRUE
   3 ACAP-GRAPH-LIVE? TFALSE
   4 ACAP-GRAPH-LIVE? TTRUE 5 ACAP-GRAPH-LIVE? TFALSE
   ACAP-GRAPH-BUILD-MAP
   ACAP-GNEWLEN @ 16 T=
   0 ACAP-GRAPH-MAP@ 0 T=
   4 ACAP-GRAPH-MAP@ 4 T=
   8 ACAP-GRAPH-MAP@ -1 T=
   12 ACAP-GRAPH-MAP@ -1 T=
   16 ACAP-GRAPH-MAP@ 8 T=
   20 ACAP-GRAPH-MAP@ 12 T=
   ACAP-GRAPH-COPY-BLOB ACAP-GRAPH-PATCH
   0 ACAP-GRAPH-W32@ $94000002 T=
   4 ACAP-GRAPH-W32@ $D65F03C0 T=
   8 ACAP-GRAPH-W32@ $D65F03C0 T= ;

: CGT-JUMP ( -- )
   s" a tail jump has no return continuation" T-LABEL
   $14000004 AOT-BLOB-BUF@ AOT-P32!
   1 ACAP-GRAPH-MARK-REC ACAP-GRAPH-SWEEP
   -1 ACAP-GRAPH-READY !
   2 ACAP-GRAPH-LIVE? TFALSE
   4 ACAP-GRAPH-LIVE? TTRUE 5 ACAP-GRAPH-LIVE? TFALSE ;

: CGT-EXTERNAL ( ptr u8 n n bool -- )
   {: name:ptr size:n scope:n continues:bool :}
   CGT-SETUP
   $94000000 AOT-BLOB-BUF@ AOT-P32!       \ canonical external BL, target in its row
   0 name size scope ACAP-ADD-SITE
   ACAP-GRAPH-INDEX
   1 ACAP-GRAPH-MARK-REC ACAP-GRAPH-SWEEP
   -1 ACAP-GRAPH-READY !
   continues if 2 ACAP-GRAPH-LIVE? TTRUE else 2 ACAP-GRAPH-LIVE? TFALSE then
   4 ACAP-GRAPH-LIVE? TFALSE ;

: CGT-EXTERNALS ( -- )
   s" the fatal primitive has no continuation" T-LABEL
   s" die" 0 false CGT-EXTERNAL
   s" the engine throw has no continuation, including for code zero" T-LABEL
   s" throw" 0 false CGT-EXTERNAL
   s" an ordinary external call keeps its continuation" T-LABEL
   s" emit" 0 true CGT-EXTERNAL
   s" a qualified user word called die can return" T-LABEL
   s" CGT-USER:die" WID-QUAL true CGT-EXTERNAL
   41 CGT-USER:die 42 T=
   s" a qualified user word called throw can return" T-LABEL
   s" CGT-USER:throw" WID-QUAL true CGT-EXTERNAL
   41 CGT-USER:throw 42 T= ;

\ A source definition may replace the global spelling too. This isolated test
\ runs last; already compiled diagnostics still call the original primitive.
TRUSTED: CGT-SHADOW-PRIMITIVES ( -- )
   s" undefine die : die ( ptr u8 n n -- ) 2drop drop ;" evaluate
   s" undefine throw : throw ( n -- ) drop ;" evaluate ;

public
: CGT-SHADOW ( -- )
   s" the global spelling alone does not establish a primitive" T-LABEL
   CGT-SHADOW-PRIMITIVES
   s" die" 0 true CGT-EXTERNAL
   s" throw" 0 true CGT-EXTERNAL
   ACAP-RESET ;
private

: CGT-PC-REL ( n n -- ) {: before:n after:n :}
   CGT-SETUP
   before AOT-BLOB-BUF@ AOT-P32!
   1 ACAP-GRAPH-MARK-REC ACAP-GRAPH-SWEEP
   -1 ACAP-GRAPH-READY !
   4 ACAP-GRAPH-LIVE? TTRUE 3 ACAP-GRAPH-LIVE? TFALSE
   ACAP-GRAPH-BUILD-MAP ACAP-GRAPH-COPY-BLOB ACAP-GRAPH-PATCH
   0 ACAP-GRAPH-W32@ after T= ;

: CGT-PC-RELS ( -- )
   s" conditional branches and ADR retain and relocate cross-record targets" T-LABEL
   $54000080 $54000040 CGT-PC-REL    \ b.eq +16 -> +8
   $B4000089 $B4000049 CGT-PC-REL    \ cbz x9,+16 -> +8
   $B5000089 $B5000049 CGT-PC-REL    \ cbnz x9,+16 -> +8
   $B6080089 $B6080049 CGT-PC-REL    \ tbz x9,#33,+16 -> +8
   $B7080089 $B7080049 CGT-PC-REL    \ tbnz x9,#33,+16 -> +8
   $10000089 $10000049 CGT-PC-REL    \ adr x9,+16 -> +8
   $30000089 $30000049 CGT-PC-REL ;  \ adr x9,+17 -> +9 (byte offset retained)

: CGT-PC-BACK ( n n -- ) {: before:n after:n :}
   CGT-SETUP
   before AOT-BLOB-BUF@ 16 + AOT-P32!
   4 ACAP-GRAPH-MARK-REC ACAP-GRAPH-SWEEP
   -1 ACAP-GRAPH-READY !
   1 ACAP-GRAPH-LIVE? TTRUE 3 ACAP-GRAPH-LIVE? TFALSE
   ACAP-GRAPH-BUILD-MAP ACAP-GRAPH-COPY-BLOB ACAP-GRAPH-PATCH
   4 ACAP-GRAPH-W32@ after T= ;

: CGT-PC-BACKS ( -- )
   s" backward displacements retain their sign and byte offset" T-LABEL
   $54FFFF80 $54FFFFE0 CGT-PC-BACK   \ b.eq -16 -> -4
   $B4FFFF89 $B4FFFFE9 CGT-PC-BACK   \ cbz x9,-16 -> -4
   $B60FFF89 $B60FFFE9 CGT-PC-BACK   \ tbz x9,#33,-16 -> -4
   $30FFFF89 $30FFFFE9 CGT-PC-BACK ; \ adr x9,-15 -> -3

: CGT-REFUSE ( -- )
   CGT-SETUP
   0 SCRIPT-ARGV$ s" adrp" CORE-STR= if $90000009 else $58000089 then
   AOT-BLOB-BUF@ AOT-P32!
   1 ACAP-GRAPH-MARK-REC ACAP-GRAPH-SWEEP ;

: CGT-NAME-MOVE ( -- )
   s" a moved code-cell target cannot give an unrelated dead body a name" T-LABEL
   s" CGT-WINDOW" DICT-WL:NAMESPACE XREF-FIND-WL AOT-RLEN
   3 ACAP-REC-DST 40 + AOT-N-C!
   0 3 ACAP-NAMED-BIT !
   3 ACAP-NAMED? TFALSE
   \ Callee offset 16 moved to 8. The dead body's OLD offset is also 8;
   \ recomputing the keep rule in mixed coordinates would resurrect it.
   1 AOT-WINDOW:XTOFF-N !
   9 AOT-WINDOW:XTOFF-BUF@ 4 + AOT-P32!
   3 ACAP-NAMED? TTRUE
   0 AOT-REC-N ! 0 AOT-SPAN:N !
   3 ACAP-COMPACT-ONE
   AOT-REC-N @ 0 T= AOT-SPAN:N @ 0 T= ;

: CGT-GAP ( -- )
   s" a call in an unowned gap retains its callee and continuation" T-LABEL
   -1 1 ACAP-REC-DST 40 + AOT-N-C!
   $94000004 AOT-BLOB-BUF@ AOT-P32!
   ACAP-GRAPH-INDEX
   ACAP-GRAPH-SWEEP-GAPS ACAP-GRAPH-SWEEP
   -1 ACAP-GRAPH-READY !
   2 ACAP-GRAPH-LIVE? TTRUE
   3 ACAP-GRAPH-LIVE? TFALSE
   4 ACAP-GRAPH-LIVE? TTRUE ;

: CGT-LITERAL ( -- )
   s" a code literal follows the compacted target exactly once" T-LABEL
   36 AOT-BLOB-LEN ! 9 ACAP-GSITE-RESERVE
   9 0 ?do
      0 i ACAP-GSITE !
      $D65F03C0 AOT-BLOB-BUF@ i 4 * + AOT-P32!
   loop
   1 0 20 CGT-ROW 2 20 4 CGT-ROW 3 24 8 CGT-ROW
   4 32 4 CGT-ROW 5 32 4 CGT-ROW
   $D2800009 AOT-BLOB-BUF@ AOT-P32!
   $F2A00009 AOT-BLOB-BUF@ 4 + AOT-P32!
   $F2C00009 AOT-BLOB-BUF@ 8 + AOT-P32!
   $F2E00009 AOT-BLOB-BUF@ 12 + AOT-P32!
   AOT-BLOB-BUF@ 32 SNAP-RELOC:SET-CHAIN
   2 0 ACAP-GSITE!
   1 AOT-CSITE-N ! 0 AOT-DSITE-BUF@ AOT-P32!
   ACAP-GRAPH-INDEX
   1 ACAP-GRAPH-MARK-REC ACAP-GRAPH-SWEEP
   -1 ACAP-GRAPH-READY !
   4 ACAP-GRAPH-LIVE? TTRUE 2 ACAP-GRAPH-LIVE? TFALSE
   ACAP-GRAPH-BUILD-MAP ACAP-GRAPH-COPY-BLOB ACAP-GRAPH-PATCH
   ACAP-GRAPH-REMAP-DSITES
   AOT-BLOB-LEN @ 24 T=
   AOT-BLOB-BUF@ SNAP-RELOC:CHAINV 20 T=
   AOT-CSITE-N @ 1 T= AOT-DSITE-N @ 0 T= ;

: CGT-METADATA ( -- )
   s" defer metadata is data even when its cells look like instructions" T-LABEL
   CGT-SETUP
   2 AOT-REC-N ! 2 ACAP-REC-ALL !
   DEFER-MAGIC AOT-BLOB-BUF@ 4 + AOT-N-C!
   $58434241 AOT-BLOB-BUF@ 12 + AOT-P32!
   $54434241 AOT-BLOB-BUF@ 16 + AOT-P32!
   1 AOT-DSITE-N !
   12 AOT-DSITE-CELL or AOT-DSITE-BUF@ AOT-P32!
   ACAP-GRAPH-INDEX
   1 ACAP-GRAPH-MARK-REC ACAP-GRAPH-SWEEP
   ACAP-GRAPH-SWEEP-GAPS ACAP-GRAPH-SWEEP
   ACAP-GRAPH-BUILD-MAP ACAP-GRAPH-COPY-BLOB ACAP-GRAPH-PATCH
   4 ACAP-GSITE@ 3 T= 8 ACAP-GSITE@ 3 T=
   12 ACAP-GRAPH-W32@ $58434241 T=
   16 ACAP-GRAPH-W32@ $54434241 T= ;

: CGT-STRING ( -- )
   s" bootstrap inline strings remain bytes through both call scan and compaction" T-LABEL
   CGT-SETUP
   28 AOT-BLOB-LEN ! 2 AOT-REC-N ! 2 ACAP-REC-ALL !
   1 0 28 CGT-ROW
   $14000004 AOT-BLOB-BUF@ AOT-P32!       \ b +16
   $14003FFF AOT-BLOB-BUF@ 4 + AOT-P32!   \ string bytes resembling an external B
   $58000000 AOT-BLOB-BUF@ 8 + AOT-P32!   \ and an LDR literal
   $70797420 AOT-BLOB-BUF@ 12 + AOT-P32!  \ and an ADR (the bytes " typ")
   $10FFFFA9 AOT-BLOB-BUF@ 16 + AOT-P32!  \ adr x9,-12 (first string byte)
   $F8008669 AOT-BLOB-BUF@ 20 + AOT-P32!  \ str x9,[x19],#8
   $D65F03C0 AOT-BLOB-BUF@ 24 + AOT-P32!
   ACAP-GRAPH-INDEX ACAP-SCAN-CALLS
   AOT-SITE-N @ 0 T=
   1 ACAP-GRAPH-MARK-REC ACAP-GRAPH-SWEEP
   ACAP-GRAPH-BUILD-MAP ACAP-GRAPH-COPY-BLOB ACAP-GRAPH-PATCH
   4 ACAP-GRAPH-W32@ $14003FFF T=
   8 ACAP-GRAPH-W32@ $58000000 T=
   12 ACAP-GRAPH-W32@ $70797420 T=
   16 ACAP-GRAPH-W32@ $10FFFFA9 T= ;

: CGT-DIAGNOSTIC ( -- )
   s" bootstrap error messages remain bytes, with address and length checked" T-LABEL
   CGT-SETUP
   32 AOT-BLOB-LEN ! 2 AOT-REC-N ! 2 ACAP-REC-ALL !
   1 0 32 CGT-ROW
   $14000004 AOT-BLOB-BUF@ AOT-P32!       \ b +16
   $14003FFF AOT-BLOB-BUF@ 4 + AOT-P32!
   $58000000 AOT-BLOB-BUF@ 8 + AOT-P32!
   $706D6F63 AOT-BLOB-BUF@ 12 + AOT-P32!  \ "comp" resembles ADR
   $D2800040 AOT-BLOB-BUF@ 16 + AOT-P32!  \ movz x0,2
   $10FFFF81 AOT-BLOB-BUF@ 20 + AOT-P32!  \ adr x1,-16
   $D2800182 AOT-BLOB-BUF@ 24 + AOT-P32!  \ movz x2,12
   $D65F03C0 AOT-BLOB-BUF@ 28 + AOT-P32!
   0 16 ACAP-GRAPH-DIAG? TTRUE
   $D2800102 AOT-BLOB-BUF@ 24 + AOT-P32!  \ wrong length
   0 16 ACAP-GRAPH-DIAG? TFALSE
   $D2800182 AOT-BLOB-BUF@ 24 + AOT-P32!
   $10FFFF82 AOT-BLOB-BUF@ 20 + AOT-P32!  \ wrong address register
   0 16 ACAP-GRAPH-DIAG? TFALSE
   $10FFFF81 AOT-BLOB-BUF@ 20 + AOT-P32!
   ACAP-GRAPH-INDEX ACAP-SCAN-CALLS
   AOT-SITE-N @ 0 T=
   1 ACAP-GRAPH-MARK-REC ACAP-GRAPH-SWEEP
   ACAP-GRAPH-BUILD-MAP ACAP-GRAPH-COPY-BLOB ACAP-GRAPH-PATCH
   4 ACAP-GRAPH-W32@ $14003FFF T=
   8 ACAP-GRAPH-W32@ $58000000 T=
   12 ACAP-GRAPH-W32@ $706D6F63 T= ;

: CGT-MAIN ( -- )
   T-RESET
   SCRIPT-ARGC 0 > if CGT-REFUSE exit then
   CGT-REAL
   CGT-SETUP CGT-INDEX CGT-CALL CGT-NAME-MOVE
   CGT-SETUP CGT-INDEX CGT-JUMP
   CGT-EXTERNALS
   CGT-PC-RELS
   CGT-PC-BACKS
   CGT-SETUP CGT-GAP
   CGT-SETUP CGT-LITERAL
   CGT-METADATA
   CGT-STRING
   CGT-DIAGNOSTIC
   ACAP-RESET ;

CGT-MAIN
;using
;package

\ Evaluate the global replacement outside the authenticated package context.
AOT-CAPTURE:CGT-SHADOW
T-REPORT
