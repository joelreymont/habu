\ Build-only DATA extent checks; runtime images need only layout.f.
require src/habu/boot-cells.f
require src/habu/layout.f
require src/habu/address-cells.f
require src/habu/prof-abi.f
require src/habu/native-observer-cells.f
require src/habu/native-body-abi.f
require src/habu/native-host-cells.f

\ Every claim on DATA states its extent. CLAIMS-ASSERT refuses an overlapping
\ pair at engine build time, naming both, when an engine builder
\ (src/habu/habu1.f, src/habu/kernel-x64.f) loads this file. Without the map a
\ TASK:+USER row overran the AOT window: one $40 row crashed at teardown.
\
\ The map holds every claim with a declared extent: everything from $3A00 up,
\ src/habu/address-cells.f LOCK-CELL and INDEX-CELL, and the profiler band, the top PROF-CNT-BYTES of DATA as src/habu/prof-abi.f
\ PROF-BAND-AT places it for the global DATA-SIZE. An x86-64 build's
\ X64LAYOUT:DATA-SIZE differs on a macOS host, so kernel-x64.f places the band
\ again through BAND-ASSERT.
\
\ Deliberate aliases are one row: VVAL-STACK of VSMAX cells, inside which
\ DEF-TKA-CELL and DEF-TKL-CELL live at $250/$258 because they are live only while the virtual stack is empty (the
\ note at CMM-CELL). Separate rows would assert a conflict the engine takes on
\ purpose.
package DATA-CLAIMS
public

$100 constant MSG-CAP
2 constant ROW-CELLS

create MSG-BUF MSG-CAP allot
variable MSG-U

: MSG-RESET ( -- )
   0 MSG-U ! ;

: MSG+ ( ptr u8 n -- ) {: a u :}
   0 begin dup u < while
      MSG-U @ MSG-CAP < if
         dup a + c@ MSG-BUF MSG-U @ + c!
         MSG-U @ 1+ MSG-U !
      then
      1+
   repeat drop ;

: MSG$ ( -- ptr u8 n )
   MSG-BUF MSG-U @ ;

\ Each claim is one line, `<offset> <length> CLAIM <display name>`. The row is
\ two contiguous cells in TAB; the name goes to its own blob, one counted string
\ per row in row order, because its bytes cannot sit between the rows. CLAIM
\ reads the name with parse-name, which allots nothing at HERE, so TAB grows
\ with `,` alone.
$1000 constant NAMES-CAP
create NAMES NAMES-CAP allot
variable NAMES-U

: NAME-C+ ( n -- ) {: b :}
   NAMES-U @ NAMES-CAP < 0= if
      s" layout: DATA-CLAIMS names exceed NAMES-CAP" 76 die
   then
   b NAMES NAMES-U @ + c!
   NAMES-U @ 1+ NAMES-U ! ;

: CLAIM ( n n -- ) {: off len :}
   parse-name {: a u :}
   u 0 = if s" layout: DATA-CLAIMS row has no name" 76 die then
   u $FF > if s" layout: DATA-CLAIMS name exceeds its count byte" 76 die then
   u NAME-C+
   0 begin dup u < while dup a + c@ NAME-C+ 1+ repeat drop
   off , len , ;
parses: CLAIM 1

create TAB
   DP-CELL                            1 cells                 CLAIM DP-CELL
   HND-CELL                           1 cells                 CLAIM HND-CELL
   LOCN-CELL                          1 cells                 CLAIM LOCN-CELL
   CMBK-CELL                          1 cells                 CLAIM CMBK-CELL
   CMFRD-CELL                         1 cells                 CLAIM CMFRD-CELL
   ADDRESS-CELLS:LOCK-CELL            1 cells                 CLAIM ADDRESS-CELLS-LOCK
   BODYLEN-CELL                       1 cells                 CLAIM BODYLEN-CELL
   RBASE-CELL                         1 cells                 CLAIM RBASE-CELL
   LOOPSP-CELL                        1 cells                 CLAIM LOOPSP-CELL
   STACK-ABI:BASE-CELL                1 cells                 CLAIM STACK-ABI-BASE-CELL
   SSCR-CELL                          1 cells                 CLAIM SSCR-CELL
   GTOD-SCRATCH                       2 cells                 CLAIM GTOD-SCRATCH
   VSP-CELL                           1 cells                 CLAIM VSP-CELL
   CREATEP-CELL                       1 cells                 CLAIM CREATEP-CELL
   QPATCH-CELL                        1 cells                 CLAIM QPATCH-CELL
   VVAL-OFF                           VSMAX cells             CLAIM VVAL-STACK
   NCOMP-DISPATCH:COMP-CELL           1 cells                 CLAIM NCOMP-COMP-CELL
   NCOMP-DISPATCH:FIXED-COMP-CELL     1 cells                 CLAIM NCOMP-FIXED-COMP-CELL
   NCOMP-DISPATCH:DECL-CELL           1 cells                 CLAIM NCOMP-DECL-CELL
   NCOMP-DISPATCH:TARGET-DECL-CELL    1 cells                 CLAIM NCOMP-TARGET-DECL-CELL
   NCOMP-DISPATCH:TIER-CELL           1 cells                 CLAIM NCOMP-TIER-CELL
   NCOMP-DISPATCH:DEF-TIER-CELL       3 cells                 CLAIM NCOMP-DEF-TIER
   NCOMP-DISPATCH:RECOVER-CELL        1 cells                 CLAIM NCOMP-RECOVER-CELL
   LASTC-CELL                         1 cells                 CLAIM LASTC-CELL
   RSP-CELL                           1 cells                 CLAIM RSP-CELL
   EXITH-CELL                         1 cells                 CLAIM EXITH-CELL
   LVD-CELL                           1 cells                 CLAIM LVD-CELL
   GENIO-ABI:OUT-CELL                 GENIO-ABI:END GENIO-ABI:OUT-CELL -  CLAIM GENIO-ABI
   AOT-CELLS:SPAN-TABLE-CELL          3 cells                 CLAIM AOT-SPAN
   SIGNAL-ABI:STUB-CELL               3 cells                 CLAIM SIGNAL-ABI
   COMMA-RUN-CELL                     1 cells                 CLAIM COMMA-RUN-CELL
   COMMA-END-CELL                     1 cells                 CLAIM COMMA-END-CELL
   COMMA-BASE-CELL                    1 cells                 CLAIM COMMA-BASE-CELL
   FRAME-CELL                         1 cells                 CLAIM FRAME-CELL
   QFRAME-CELL                        1 cells                 CLAIM QFRAME-CELL
   BODY-TEXT:BASE-CELL                4 cells                 CLAIM BODY-TEXT
   BODY-ITEMS:BASE-CELL               4 cells                 CLAIM BODY-ITEMS
   CMM-CELL                           1 cells                 CLAIM CMM-CELL
   DOESB-CELL                         1 cells                 CLAIM DOESB-CELL
   TRUSTED-CELL                       1 cells                 CLAIM TRUSTED-CELL
   SRCLOC:PATH-CELL                   1 cells                 CLAIM SRCLOC-PATH
   SRCLOC:PATHLEN-CELL                1 cells                 CLAIM SRCLOC-PATHLEN
   SRCLOC:INB-CELL                    1 cells                 CLAIM SRCLOC-INB
   SRCLOC:OWNED-CELL                  1 cells                 CLAIM SRCLOC-OWNED
   SRCLOC:OWNEDLEN-CELL               1 cells                 CLAIM SRCLOC-OWNEDLEN
   SRCLOC:SOURCE-CELL                 1 cells                 CLAIM SRCLOC-SOURCE
   SRCLOC:READS-CELL                  1 cells                 CLAIM SRCLOC-READS
   TOKBODY-CELL                       1 cells                 CLAIM TOKBODY-CELL
   TOKLOCN-CELL                       1 cells                 CLAIM TOKLOCN-CELL
   TOKCP-CELL                         1 cells                 CLAIM TOKCP-CELL
   TOKVSP-CELL                        1 cells                 CLAIM TOKVSP-CELL
   PENDTKA-CELL                       1 cells                 CLAIM PENDTKA-CELL
   NCOMP-DISPATCH:FIXED-SHADOW-CELL   1 cells                 CLAIM NCOMP-FIXED-SHADOW-CELL
   NCOMP-DISPATCH:DOES-SHADOW-CELL    1 cells                 CLAIM NCOMP-DOES-SHADOW-CELL
   TASK-CHAIN-CELL                    1 cells                 CLAIM TASK-CHAIN-CELL
   PKGRESYNC-CELL                     1 cells                 CLAIM PKGRESYNC-CELL
   HIDX:CLAIMS                        1 cells                 CLAIM HIDX-CLAIMS
   PROT:WINDOW                        1 cells                 CLAIM PROT-WINDOW
   PROT:WLO                           1 cells                 CLAIM PROT-WLO
   PROT:RLO                           1 cells                 CLAIM PROT-RLO
   ENGINE-HOOK-OFF                    ENGINE-HOOK-LEN         CLAIM ENGINE-HOOK
   SUSP-HOOK-OFF                      SUSP-HOOK-LEN           CLAIM SUSP-HOOK
   REFUSAL-ABI:CODE-CELL              1 cells                 CLAIM REFUSAL-ABI:CODE-CELL
   EXIT-HOOK-CELL                     1 cells                 CLAIM EXIT-HOOK-CELL
   FLOORREC-CELL                      1 cells                 CLAIM FLOORREC-CELL
   FAULT-MARK-CELL                    1 cells                 CLAIM FAULT-MARK-CELL
   DBASE-CELL                         1 cells                 CLAIM DBASE-CELL
   MAIN-STACK-TOP-CELL                1 cells                 CLAIM MAIN-STACK-TOP-CELL
   MAIN-THREAD-CELL                   1 cells                 CLAIM MAIN-THREAD-CELL
   CLOSED-FREE-CELL                   1 cells                 CLAIM CLOSED-FREE-CELL
   CODE-END-CELL                      1 cells                 CLAIM CODE-END-CELL
   PRIM-TABLE-CELL                    1 cells                 CLAIM PRIM-TABLE-CELL
   TEXT-SIZE-CELL                     1 cells                 CLAIM TEXT-SIZE-CELL
   DPHW-CELL                          1 cells                 CLAIM DPHW-CELL
   NDICT-HW-CELL                      1 cells                 CLAIM NDICT-HW-CELL
   DATA-FLOOR-CELL                    1 cells                 CLAIM DATA-FLOOR-CELL
   NATIVE-OBS-CELLS:PUBLISHED         1 cells                 CLAIM NATIVE-OBS-CELLS:PUBLISHED
   NATIVE-OBS-CELLS:HOST-INVALIDATE   1 cells                 CLAIM NATIVE-OBS-CELLS:HOST-INVALIDATE
   NATIVE-HOST-CELLS:SCALAR-PRIM      1 cells                 CLAIM NATIVE-HOST-CELLS:SCALAR-PRIM
   DEF-OCC:PTR-CELL                   1 cells                 CLAIM DEF-OCC:PTR-CELL
   LOCNAMES                           LOC-RECS LOC-REC *      CLAIM LOCNAMES
   CAPDEPTH-CELL                      1 cells                 CLAIM CAPDEPTH-CELL
   TOKDEPTH-CELL                      1 cells                 CLAIM TOKDEPTH-CELL
   ARGC-CELL                          1 cells                 CLAIM ARGC-CELL
   ARGV-CELL                          1 cells                 CLAIM ARGV-CELL
   ENVP-CELL                          1 cells                 CLAIM ENVP-CELL
   PROGRAM-FILE-CELL                  1 cells                 CLAIM PROGRAM-FILE-CELL
   PEND-CELL                          1 cells                 CLAIM PEND-CELL
   TKA-CELL                           1 cells                 CLAIM TKA-CELL
   TKL-CELL                           1 cells                 CLAIM TKL-CELL
   INP-CELL                           1 cells                 CLAIM INP-CELL
   INE-CELL                           1 cells                 CLAIM INE-CELL
   BPTRAMP-CELL                       1 cells                 CLAIM BPTRAMP-CELL
   ADDRESS-CELLS:INDEX-CELL           1 cells                 CLAIM ADDRESS-CELLS-INDEX
   BPTAB-OFF                          EVALD-CELL BPTAB-OFF -  CLAIM BPTAB
   EVALD-CELL                         1 cells                 CLAIM EVALD-CELL
   EVALERR-CELL                       1 cells                 CLAIM EVALERR-CELL
   BPWBASE-CELL                       1 cells                 CLAIM BPWBASE-CELL
   BPWN-CELL                          1 cells                 CLAIM BPWN-CELL
   SNAP-CELL                          1 cells                 CLAIM SNAP-CELL
   ENGINE-MAIN:XT-CELL                1 cells                 CLAIM ENGINE-MAIN-XT-CELL
   NCOMP-DISPATCH:CALLBACK-STUB-CELL  1 cells                 CLAIM NCOMP-DISPATCH:CALLBACK-STUB-CELL
   NCOMP-DISPATCH:CALLBACK-ENTRY-CELL  1 cells                 CLAIM NCOMP-DISPATCH:CALLBACK-ENTRY-CELL
   NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:ORIGIN-HOOK cells +  1 cells                 CLAIM NATIVE-BODY-ABI:ORIGIN-HOOK
   NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:ORIGIN-TAKE cells +  1 cells                 CLAIM NATIVE-BODY-ABI:ORIGIN-TAKE
   NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:PENDING-TAKE cells +  1 cells                 CLAIM NATIVE-BODY-ABI:PENDING-TAKE
   NCOMP-DISPATCH:BODY-HOOK-CELL      1 cells                 CLAIM NCOMP-DISPATCH:BODY-HOOK-CELL
   ENGINE-MAIN:REPORT-CELL            1 cells                 CLAIM ENGINE-MAIN-REPORT-CELL
   PROVIDED-XT:EVALUATE-CELL          1 cells                 CLAIM PROVIDED-XT-EVALUATE-CELL
   PROVIDED-XT:EVALUATE-RANGE-CELL    1 cells                 CLAIM PROVIDED-XT-EVALUATE-RANGE-CELL
   PROVIDED-XT:DEFINE-CELL            1 cells                 CLAIM PROVIDED-XT-DEFINE-CELL
   PROVIDED-XT:APPEND-CELL            1 cells                 CLAIM PROVIDED-XT-APPEND-CELL
   PROVIDED-XT:POSTPONE-CELL          1 cells                 CLAIM PROVIDED-XT-POSTPONE-CELL
   PROVIDED-XT:CHILD-CELL             1 cells                 CLAIM PROVIDED-XT-CHILD-CELL
   PROVIDED-XT:STORAGE-LINE-CELL      1 cells                 CLAIM PROVIDED-XT-STORAGE-LINE-CELL
   PROVIDED-XT:LITERAL-CELL           1 cells                 CLAIM PROVIDED-XT-LITERAL-CELL
   PROVIDED-XT:TYPE-OP-CELL           1 cells                 CLAIM PROVIDED-XT-TYPE-OP-CELL
   POLICY-ABI:KEYWORD-CELL            1 cells                 CLAIM POLICY-KEYWORD-CELL
   ENGINE-MAIN:ARENA-CELL             1 cells                 CLAIM ENGINE-MAIN-ARENA-CELL
   HALT-UNWIND-CELL                   1 cells                 CLAIM HALT-UNWIND-CELL
   $3A00                              $288                    CLAIM FFI-BUFFERS
   TASK-TCB-CELL                      1 cells                 CLAIM TASK-TCB-CELL
   TASKS-LIVE-CELL                    1 cells                 CLAIM TASKS-LIVE-CELL
   HIDXP-CELL                         1 cells                 CLAIM HIDXP-CELL
   EVALREC-CELL                       1 cells                 CLAIM EVALREC-CELL
   BOOT:USER-END                      1 cells                 CLAIM BOOT-USER-END
   PROT-REG-OFF                       PROT-REG-LEN            CLAIM PROT-REG
   CB-XDS                             CB-BAND-BYTES           CLAIM CALLBACK-BAND
   CB-TLS-GET                         2 cells                 CLAIM CALLBACK-TLS
   APP-ENTRY:XT-CELL                  1 cells                 CLAIM APP-ENTRY-XT-CELL
   AOT-CELLS:T0-CELL                  1 cells                 CLAIM AOT-WINDOW-T0
   REGION-ALIAS:DELTA-CELL            1 cells                 CLAIM REGION-ALIAS-DELTA
   AOT-CELLS:B0-CELL                  1 cells                 CLAIM AOT-WINDOW-B0
   EVAL-TOP-CELL                      PROT:RHI EVAL-TOP-CELL -  CLAIM EVAL-POINTER-BAND
   PROT:RHI                           1 cells                 CLAIM PROT-RHI
   PROT:CF                            1 cells                 CLAIM PROT-CF
   AOT-CELLS:SIG-POOL-CELL            1 cells                 CLAIM AOT-SIG-POOL
   AOT-CELLS:SIG-LEN-CELL             1 cells                 CLAIM AOT-SIG-LEN
   BOOT:HEAP-START-CELL               1 cells                 CLAIM BOOT-HEAP-START
   BOOT:DP-FLOOR-CELL                 1 cells                 CLAIM BOOT-DP-FLOOR
   BOOT:DP-CEILING-CELL               1 cells                 CLAIM BOOT-DP-CEILING
   BOOT:GLOBAL-WL-CELL                1 cells                 CLAIM BOOT-GLOBAL-WL
   BOOT:NAMESPACE-WL-CELL             1 cells                 CLAIM BOOT-NAMESPACE-WL
   BOOT:TYPES-WL-CELL                 1 cells                 CLAIM BOOT-TYPES-WL
   BOOT:IMAGE-BASE-CELL               1 cells                 CLAIM BOOT-IMAGE-BASE
   STACK-ABI:CAP-CELL                 1 cells                 CLAIM STACK-ABI-CAP
   STACK-ABI:REPL-BASE-CELL           1 cells                 CLAIM STACK-ABI-REPL-BASE
   STACK-ABI:REPL-CAP-CELL            1 cells                 CLAIM STACK-ABI-REPL-CAP
   STACK-ABI:RETURN-BASE-CELL         1 cells                 CLAIM STACK-ABI-RETURN-BASE
   STACK-ABI:LOOP-BASE-CELL           1 cells                 CLAIM STACK-ABI-LOOP-BASE
   TXN-STATE-OFF                      TXN-STATE-LEN           CLAIM TXN-STATE
   USER-BAND:START                    USER-BAND:END USER-BAND:START -  CLAIM USER-BAND
   MEM-ABI:START                      MEM-ABI:BYTES           CLAIM MEM-ABI
   FS-MUT-ABI:START                   FS-MUT-ABI:BYTES        CLAIM FS-MUT-ABI
   FS-ABI:START                       FS-ABI:BYTES            CLAIM FS-ABI
   FMT-ABI:START                      FMT-ABI:BYTES           CLAIM FMT-ABI
   STRING-ABI:START                   STRING-ABI:BYTES        CLAIM STRING-ABI
   PD-TABLE-OFF                       PD-TABLE-END PD-TABLE-OFF -  CLAIM PD-TABLE
   USE-BAND-OFF                       USE-BAND-END USE-BAND-OFF -  CLAIM USE-BAND
   SNAP-RELOC:CALLMAP-OFF             SNAP-RELOC:CALLMAP-BYTES  CLAIM SNAP-CALLMAP
   SNAP-RELOC:ADDRMAP-OFF             SNAP-RELOC:ADDRMAP-BYTES  CLAIM SNAP-ADDRMAP
   SNAP-RELOC:XTCELL-N-CELL           SNAP-RELOC:XTCELL-END SNAP-RELOC:XTCELL-N-CELL -  CLAIM SNAP-XTCELL
   REPLAY-SCOPE:LATCH                 REPLAY-SCOPE:END REPLAY-SCOPE:LATCH -  CLAIM REPLAY-SCOPE
   ENCLOSING:PUB                      ENCLOSING:END ENCLOSING:PUB -  CLAIM ENCLOSING
   REPLAY-SCOPE:ENCLOSING             ENCLOSING:END ENCLOSING:PUB -  CLAIM REPLAY-SCOPE:ENCLOSING
   POLICY-NDICT-CELL                  1 cells                 CLAIM POLICY-NDICT-CELL
   POLICY-BITS-OFF                    PROT-BITS-BYTES         CLAIM POLICY-BITS
   POLICY-REQUIRE-CELL                1 cells                 CLAIM POLICY-REQUIRE-CELL
   CLAUSE-CELL                        1 cells                 CLAIM CLAUSE-CELL
   PACKAGE-PATH:OFF                   PACKAGE-PATH:CAP        CLAIM PACKAGE-PATH
   PKG-OUTSIDE-CELL                   1 cells                 CLAIM PKG-OUTSIDE-CELL
   SECTION-CELL                       1 cells                 CLAIM SECTION-CELL
   \ The profiler band stays the last row, where BAND-ASSERT places it again.
   0 DATA-SIZE PROF-ABI:PROF-BAND-AT  PROF-CNT-BYTES          CLAIM PROF-BAND
   0 ,  0 ,

: ROW-OFF ( n -- n )
   ROW-CELLS * cells TAB + @ ;

: ROW-LEN ( n -- n )
   ROW-CELLS * cells 1 cells + TAB + @ ;

: NAME-AT ( n -- ptr u8 n ) {: ix :}
   NAMES 0 begin dup ix < while
      swap dup c@ 1+ + swap 1+
   repeat drop dup 1+ swap c@ ;

: COUNT-ROWS ( -- n )
   0 begin dup ROW-LEN 0 <> while 1+ repeat ;

\ Half-open [off, off+len) intersection. A zero-length row cannot exist: the
\ walk stops on one, so a claim declared with no extent ends the table early
\ instead of being silently skipped - which is why every row states `1 cells`
\ rather than nothing.
: OVERLAP? ( n n -- bool ) {: a b :}
   a ROW-OFF b ROW-OFF b ROW-LEN + < 
   b ROW-OFF a ROW-OFF a ROW-LEN + < and ;

: CLAIMS-DIE ( n n -- ) {: a b :}
   MSG-RESET
   s" layout: DATA-CLAIMS overlap: " MSG+
   a NAME-AT MSG+
   s"  and " MSG+
   b NAME-AT MSG+
   MSG$ 76 die ;

: CLAIMS-ASSERT ( -- )
   COUNT-ROWS {: n :}
   0 begin dup n < while
      dup 1+ begin dup n < while
         2dup OVERLAP? if 2dup CLAIMS-DIE then
         1+
      repeat drop
      1+
   repeat drop ;

COUNT-ROWS 1- constant PROF-ROW

\ The last row is the band PROF-BAND-AT placed for the global DATA-SIZE; a row
\ added after it would be the one BAND-ASSERT moves instead.
: PROF-ROW-ASSERT ( -- )
   PROF-ROW ROW-OFF 0 DATA-SIZE PROF-ABI:PROF-BAND-AT <> if
      s" layout: DATA-CLAIMS last row is not PROF-BAND" 76 die
   then ;
PROF-ROW-ASSERT

\ Place the profiler band for a target whose DATA is n bytes and check the
\ table again, for a build whose target's DATA-SIZE is not the global one.
: BAND-ASSERT ( n -- )
   {: size :}
   0 size PROF-ABI:PROF-BAND-AT  PROF-ROW ROW-CELLS * cells TAB + !
   CLAIMS-ASSERT ;

CLAIMS-ASSERT
;package
