\ Build-only DATA extent checks; runtime images need only layout.f.
require src/habu/layout.f
require src/habu/regalloc-abi.f
require src/habu/address-cells.f
require src/habu/prof-abi.f
require src/habu/native-observer-cells.f
require src/habu/native-observer-extension.f
require src/habu/native-body-abi.f
require src/habu/native-host-cells.f

\ --- DATA claim map and the layout-time overlap assertion ----------------------
\ WHY IT EXISTS. src/lib/task.f handed out TASK:+USER rows from $41C8 bounded by
\ TXN-STATE-OFF, because the comments in this file said that run was free. It was
\ not: APP-ENTRY:XT-CELL, the AOT capture window, the evaluator-pointer band,
\ PROT, AOT-SIG, BOOT-LAYOUT and src/habu/stack-abi.f all sit inside it. Five
\ shipped libraries fitted into the 472 bytes before the first of them by luck,
\ and a sixth would have overwritten the AOT window with no diagnostic at all -
\ measured, before the bound was corrected: one $40 row past the mark crashed
\ at teardown with a register dump.
\
\ A COMMENT COULD NOT CATCH THAT AND DID NOT. This table can. Every claim on the
\ DATA region states its own extent, and CLAIMS-ASSERT refuses an overlapping
\ pair at ENGINE BUILD TIME, naming both. BANDS-ASSERT, at the end of this file,
\ runs a second check over the same table: every protected band must BE a
\ declared claim, start and length both, which is the check that would have
\ caught the transaction guarding $3000 of a band whose cells end after $300.
\ Both run when an engine builder (src/habu/habu1.f, src/habu/kernel-x64.f)
\ loads this file.
\
\ WHAT IS IN IT: every claim whose extent is DECLARED - a band with a length
\ constant, or a single cell. That is the whole map from $3A00 up, where every
\ library band lives, and all DATA-BANDS rows; the register allocator's cells
\ and tables, which src/habu/regalloc-abi.f declares for this file to read;
\ the address-cell registry's mutex and index cells (src/habu/address-cells.f
\ LOCK-CELL and INDEX-CELL); and the profiler band, the top PROF-CNT-BYTES of
\ DATA, placed as src/habu/prof-abi.f PROF-BAND-AT places it for the global
\ DATA-SIZE, the target's in a native build. An x86-64 build reads its target's
\ only as X64LAYOUT:DATA-SIZE, which differs from the global one on a macOS
\ host, so src/habu/kernel-x64.f places the band again with it through
\ BAND-ASSERT. ONE CLAIM IS OUT: the $1A0 seal fixture poke cell, which no
\ constant names.
\
\ DELIBERATE ALIASES ARE ONE ROW, NOT TWO. The friend arena is one row, not the
\ eighteen cells inside it. VVAL-STACK is one row of VSMAX cells: DEF-TKA-CELL
\ and DEF-TKL-CELL live at $250/$258 inside it and survive there because their
\ liveness is confined to the definition name token, when the virtual stack is
\ empty - the note at CMM-CELL above records that trade. Giving them their own
\ rows would assert a conflict the engine takes on purpose.
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

\ Append the row at byte offset off, len bytes long, named by the next token.
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
   LOCF-CELL                          1 cells                 CLAIM LOCF-CELL
   FRIEND-ARENA                       FRIEND-ARENA-LEN        CLAIM FRIEND-ARENA
   CMBK-CELL                          1 cells                 CLAIM CMBK-CELL
   CMTAG-CELL                         1 cells                 CLAIM CMTAG-CELL
   CMPADS-CELL                        1 cells                 CLAIM CMPADS-CELL
   CMFRD-CELL                         1 cells                 CLAIM CMFRD-CELL
   CMFR-OFF                           CMFR-MAX cells          CLAIM CMFR-STACK
   ADDRESS-CELLS:LOCK-CELL            1 cells                 CLAIM ADDRESS-CELLS-LOCK
   CMFAM-CELL                         1 cells                 CLAIM CMFAM-CELL
   BODYLEN-CELL                       1 cells                 CLAIM BODYLEN-CELL
   RBASE-CELL                         1 cells                 CLAIM RBASE-CELL
   LOOPSP-CELL                        1 cells                 CLAIM LOOPSP-CELL
   STACK-ABI:BASE-CELL                1 cells                 CLAIM STACK-ABI-BASE-CELL
   SSCR-CELL                          1 cells                 CLAIM SSCR-CELL
   GTOD-SCRATCH                       2 cells                 CLAIM GTOD-SCRATCH
   DOESP-CELL                         1 cells                 CLAIM DOESP-CELL
   VSP-CELL                           1 cells                 CLAIM VSP-CELL
   REGALLOC-ABI:VRFREE-CELL           1 cells                 CLAIM VRFREE-CELL
   VTAG-OFF                           VSMAX                   CLAIM VTAG-STACK
   CREATEP-CELL                       1 cells                 CLAIM CREATEP-CELL
   QPATCH-CELL                        1 cells                 CLAIM QPATCH-CELL
   QENT-CELL                          1 cells                 CLAIM QENT-CELL
   QXH-CELL                           1 cells                 CLAIM QXH-CELL
   VVAL-OFF                           VSMAX cells             CLAIM VVAL-STACK
   NCOMP-DISPATCH:XT-CELL             1 cells                 CLAIM NCOMP-XT-CELL
   NCOMP-DISPATCH:DECL-CELL           1 cells                 CLAIM NCOMP-DECL-CELL
   NCOMP-DISPATCH:TARGET-DECL-CELL    1 cells                 CLAIM NCOMP-TARGET-DECL-CELL
   NCOMP-DISPATCH:TIER-CELL           1 cells                 CLAIM NCOMP-TIER-CELL
   JIT-SNAP:SP-CELL                   1 cells                 CLAIM JIT-SNAP-SP-CELL
   NCOMP-DISPATCH:DEF-TIER-CELL       3 cells                 CLAIM NCOMP-DEF-TIER
   NCOMP-DISPATCH:RECOVER-CELL        1 cells                 CLAIM NCOMP-RECOVER-CELL
   LASTC-CELL                         1 cells                 CLAIM LASTC-CELL
   RSP-CELL                           1 cells                 CLAIM RSP-CELL
   EXITH-CELL                         1 cells                 CLAIM EXITH-CELL
   LVD-CELL                           1 cells                 CLAIM LVD-CELL
   LVH-OFF                            LV-LEVELS cells         CLAIM LVH-LEVELS
   GENIO-ABI:OUT-CELL                 GENIO-ABI:END GENIO-ABI:OUT-CELL -  CLAIM GENIO-ABI
   AOT-CELLS:SPAN-TABLE-CELL          3 cells                 CLAIM AOT-SPAN
   SIGNAL-ABI:STUB-CELL               3 cells                 CLAIM SIGNAL-ABI
   LVF-OFF                            LV-LEVELS cells         CLAIM LVF-LEVELS
   LVQ-OFF                            LV-LEVELS cells         CLAIM LVQ-LEVELS
   FRAME-CELL                         1 cells                 CLAIM FRAME-CELL
   QFRAME-CELL                        1 cells                 CLAIM QFRAME-CELL
   BODYBUF-OFF                        BODYBUF-CAP 2 +         CLAIM BODYBUF
   RPKG-CUR                           RPKG-REC RPKG-CUR - 1 cells +  CLAIM RPKG-SNAPSHOT
   CMM-CELL                           1 cells                 CLAIM CMM-CELL
   DOESB-CELL                         1 cells                 CLAIM DOESB-CELL
   TRUSTED-CELL                       1 cells                 CLAIM TRUSTED-CELL
   SRCLOC:PATH-CELL                   1 cells                 CLAIM SRCLOC-PATH
   SRCLOC:PATHLEN-CELL                1 cells                 CLAIM SRCLOC-PATHLEN
   SRCLOC:INB-CELL                    1 cells                 CLAIM SRCLOC-INB
   SRCLOC:OWNED-CELL                  1 cells                 CLAIM SRCLOC-OWNED
   SRCLOC:OWNEDLEN-CELL               1 cells                 CLAIM SRCLOC-OWNEDLEN
   TOKBODY-CELL                       1 cells                 CLAIM TOKBODY-CELL
   TOKLOCN-CELL                       1 cells                 CLAIM TOKLOCN-CELL
   TOKCP-CELL                         1 cells                 CLAIM TOKCP-CELL
   TOKVSP-CELL                        1 cells                 CLAIM TOKVSP-CELL
   TOKVRF-CELL                        1 cells                 CLAIM TOKVRF-CELL
   TOKFRF-CELL                        1 cells                 CLAIM TOKFRF-CELL
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
   JIT-QUOT:SP-CELL                   1 cells                 CLAIM JIT-QUOT-SP-CELL
   JIT-QUOT:STK-OFF                   JIT-QUOT:END JIT-QUOT:STK-OFF -  CLAIM JIT-QUOT-FRAMES
   FLOORREC-CELL                      1 cells                 CLAIM FLOORREC-CELL
   CLOSED-FREE-CELL                   1 cells                 CLAIM CLOSED-FREE-CELL
   CODE-END-CELL                      1 cells                 CLAIM CODE-END-CELL
   TEXT-SIZE-CELL                     1 cells                 CLAIM TEXT-SIZE-CELL
   DPHW-CELL                          1 cells                 CLAIM DPHW-CELL
   DATA-FLOOR-CELL                    1 cells                 CLAIM DATA-FLOOR-CELL
   NATIVE-OBS-CELLS:PUBLISHED         1 cells                 CLAIM NATIVE-OBS-CELLS:PUBLISHED
   NATIVE-OBS-CELLS:INVALIDATE        1 cells                 CLAIM NATIVE-OBS-CELLS:INVALIDATE
   NATIVE-OBS-CELLS:HOST-INVALIDATE   1 cells                 CLAIM NATIVE-OBS-CELLS:HOST-INVALIDATE
   NATIVE-OBS-EXT:CAPTURE-INVALIDATE  1 cells                 CLAIM NATIVE-OBS-EXT:CAPTURE-INVALIDATE
   NATIVE-HOST-CELLS:SELECT           1 cells                 CLAIM NATIVE-HOST-CELLS:SELECT
   NATIVE-HOST-CELLS:SCALAR-PRIM      1 cells                 CLAIM NATIVE-HOST-CELLS:SCALAR-PRIM
   NATIVE-HOST-CELLS:FACTS            1 cells                 CLAIM NATIVE-HOST-CELLS:FACTS
   NATIVE-HOST-CELLS:DIVISION-PRIM    1 cells                 CLAIM NATIVE-HOST-CELLS:DIVISION-PRIM
   PEND-OPAQUE-CELL                   1 cells                 CLAIM PEND-OPAQUE-CELL
   DEF-OCC:PTR-CELL                   1 cells                 CLAIM DEF-OCC:PTR-CELL
   LOCNAMES                           LOC-RECS LOC-REC *      CLAIM LOCNAMES
   REGALLOC-ABI:VRTAB-OFF             REGALLOC-ABI:VRTAB-BYTES  CLAIM VRTAB
   REGALLOC-ABI:VRITAB-OFF            REGALLOC-ABI:VRTAB-BYTES  CLAIM VRITAB
   REPLH-CELL                         1 cells                 CLAIM REPLH-CELL
   RSAVCP-CELL                        1 cells                 CLAIM RSAVCP-CELL
   RSAVND-CELL                        1 cells                 CLAIM RSAVND-CELL
   RSAVDP-CELL                        1 cells                 CLAIM RSAVDP-CELL
   RSAVSP-CELL                        1 cells                 CLAIM RSAVSP-CELL
   RRECP-CELL                         1 cells                 CLAIM RRECP-CELL
   RSAVROWS-CELL                      1 cells                 CLAIM RSAVROWS-CELL
   ARGC-CELL                          1 cells                 CLAIM ARGC-CELL
   ARGV-CELL                          1 cells                 CLAIM ARGV-CELL
   ENVP-CELL                          1 cells                 CLAIM ENVP-CELL
   PEND-CELL                          1 cells                 CLAIM PEND-CELL
   TKA-CELL                           1 cells                 CLAIM TKA-CELL
   TKL-CELL                           1 cells                 CLAIM TKL-CELL
   INP-CELL                           1 cells                 CLAIM INP-CELL
   INE-CELL                           1 cells                 CLAIM INE-CELL
   REGALLOC-ABI:FRFREE-CELL           1 cells                 CLAIM FRFREE-CELL
   FRCLM-CELL                         1 cells                 CLAIM FRCLM-CELL
   BPA-CELL                           1 cells                 CLAIM BPA-CELL
   ADDRESS-CELLS:INDEX-CELL           1 cells                 CLAIM ADDRESS-CELLS-INDEX
   BPTAB-OFF                          EVALD-CELL BPTAB-OFF -  CLAIM BPTAB
   EVALD-CELL                         1 cells                 CLAIM EVALD-CELL
   EVALERR-CELL                       1 cells                 CLAIM EVALERR-CELL
   LMAINP-CELL                        1 cells                 CLAIM LMAINP-CELL
   BPWBASE-CELL                       1 cells                 CLAIM BPWBASE-CELL
   BPWN-CELL                          1 cells                 CLAIM BPWN-CELL
   SNAP-CELL                          1 cells                 CLAIM SNAP-CELL
   NULL-PTR-CELL-OFF                  1 cells                 CLAIM NULL-PTR-CELL
   ENGINE-MAIN:XT-CELL                1 cells                 CLAIM ENGINE-MAIN-XT-CELL
   NCOMP-DISPATCH:CALLBACK-STUB-CELL  1 cells                 CLAIM NCOMP-DISPATCH:CALLBACK-STUB-CELL
   NCOMP-DISPATCH:CALLBACK-ENTRY-CELL  1 cells                 CLAIM NCOMP-DISPATCH:CALLBACK-ENTRY-CELL
   NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:ORIGIN-HOOK cells +  1 cells                 CLAIM NATIVE-BODY-ABI:ORIGIN-HOOK
   NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:ORIGIN-TAKE cells +  1 cells                 CLAIM NATIVE-BODY-ABI:ORIGIN-TAKE
   NCOMP-DISPATCH:CALLBACK-ENTRY-CELL CELL + NATIVE-BODY-ABI:PENDING-TAKE cells +  1 cells                 CLAIM NATIVE-BODY-ABI:PENDING-TAKE
   ENGINE-MAIN:REPORT-CELL            1 cells                 CLAIM ENGINE-MAIN-REPORT-CELL
   PROVIDED-XT:EVALUATE-CELL          1 cells                 CLAIM PROVIDED-XT-EVALUATE-CELL
   PROVIDED-XT:EVALUATE-RANGE-CELL    1 cells                 CLAIM PROVIDED-XT-EVALUATE-RANGE-CELL
   POLICY-ABI:KEYWORD-CELL            1 cells                 CLAIM POLICY-KEYWORD-CELL
   ENGINE-MAIN:ARENA-CELL             1 cells                 CLAIM ENGINE-MAIN-ARENA-CELL
   $3A00                              $288                    CLAIM FFI-BUFFERS
   TASK-TCB-CELL                      1 cells                 CLAIM TASK-TCB-CELL
   TASKS-LIVE-CELL                    1 cells                 CLAIM TASKS-LIVE-CELL
   HIDXP-CELL                         1 cells                 CLAIM HIDXP-CELL
   EVALREC-CELL                       1 cells                 CLAIM EVALREC-CELL
   AOT-SEED-DONE-CELL                 1 cells                 CLAIM AOT-SEED-DONE-CELL
   BOOT-SRC:USER-END                  1 cells                 CLAIM BOOT-SRC-USER-END
   PROT-REG-OFF                       PROT-REG-LEN            CLAIM PROT-REG
   $40C8                              $100                    CLAIM FFI-LEN-BUFFERS
   CB-XDS                             CB-BAND-BYTES           CLAIM CALLBACK-BAND
   DECLARED-LOG:ADDR-CELL             1 cells                 CLAIM DECLARED-LOG-ADDR
   APP-ENTRY:XT-CELL                  1 cells                 CLAIM APP-ENTRY-XT-CELL
   AOT-CELLS:T0-CELL                  1 cells                 CLAIM AOT-WINDOW-T0
   AOT-CELLS:D0-CELL                  1 cells                 CLAIM AOT-WINDOW-D0
   AOT-CELLS:B0-CELL                  1 cells                 CLAIM AOT-WINDOW-B0
   EVAL-TOP-CELL                      PROT:RHI EVAL-TOP-CELL -  CLAIM EVAL-POINTER-BAND
   PROT:RHI                           1 cells                 CLAIM PROT-RHI
   PROT:CF                            1 cells                 CLAIM PROT-CF
   AOT-CELLS:SIG-POOL-CELL            1 cells                 CLAIM AOT-SIG-POOL
   AOT-CELLS:SIG-LEN-CELL             1 cells                 CLAIM AOT-SIG-LEN
   BOOT-LAYOUT:HEAP-START-CELL        1 cells                 CLAIM BOOT-HEAP-START
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
   DECLARED-LOG:OFF                   DECLARED-LOG:END DECLARED-LOG:OFF -  CLAIM DECLARED-LOG
   USE-BAND-OFF                       USE-BAND-END USE-BAND-OFF -  CLAIM USE-BAND
   SNAP-RELOC:CALLMAP-OFF             SNAP-RELOC:CALLMAP-BYTES  CLAIM SNAP-CALLMAP
   SNAP-RELOC:ADDRMAP-OFF             SNAP-RELOC:ADDRMAP-BYTES  CLAIM SNAP-ADDRMAP
   SNAP-RELOC:XTCELL-N-CELL           SNAP-RELOC:XTCELL-END SNAP-RELOC:XTCELL-N-CELL -  CLAIM SNAP-XTCELL
   JIT-SNAP:STK-OFF                   JIT-SNAP:END JIT-SNAP:STK-OFF -  CLAIM JIT-SNAP-FRAMES
   TIER-PROV:OPEN-CELL                TIER-PROV:END TIER-PROV:OPEN-CELL -  CLAIM TIER-PROV
   UNIT-COMPILE-CELL                  1 cells                 CLAIM UNIT-COMPILE
   SOURCE-EXEC-CTX-CELL               1 cells                 CLAIM SOURCE-EXEC-CTX-CELL
   REPLAY-SCOPE:LATCH                 REPLAY-SCOPE:END REPLAY-SCOPE:LATCH -  CLAIM REPLAY-SCOPE
   RPKG:FLOOR                         1 cells                 CLAIM RPKG:FLOOR
   RPKG:WIDS                          USE-MAX cells           CLAIM RPKG:WIDS
   POLICY-NDICT-CELL                  1 cells                 CLAIM POLICY-NDICT-CELL
   POLICY-BITS-OFF                    PROT-BITS-BYTES         CLAIM POLICY-BITS
   POLICY-REQUIRE-CELL                1 cells                 CLAIM POLICY-REQUIRE-CELL
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

\ EVERY GUARDED BAND MUST BE A DECLARED CLAIM, start and length both. The
\ bands are src/habu/layout.f's DATA-BANDS table, which every engine's prefix
\ carries. The transaction row guarded TXN-STATE-LEN when that constant was
\ $3000 and the transaction's own cells ended after $300, so PROT-GUARD refused
\ stores across 11520 bytes no claim owned - which is exactly what kept the
\ task-user arena out of the only run in the per-task header big enough to hold
\ it. Nothing compared the guard against the map until this did. It dies when
\ an engine builder loads this file, so a band that outgrows or outlives its
\ claim cannot ship.

private

\ The claim at band ix's offset, else -1.
: CLAIM-AT ( n -- n )
   {: ix:n :}
   COUNT-ROWS 0 ?do
      i ROW-OFF ix DATA-BANDS:OFF = if i unloop exit then
   loop -1 ;

: DECLARED-AT ( n -- )
   {: ix:n :}
   ix CLAIM-AT {: hit:n :}
   hit 0 < if
      s" data-bands: PROT-GUARD band has no DATA-CLAIMS claim at its offset" 76 die
   then
   hit ROW-LEN ix DATA-BANDS:LEN <> if
      MSG-RESET
      s" data-bands: PROT-GUARD band length differs from its claim: " MSG+
      hit NAME-AT MSG+
      MSG$ 76 die
   then ;

public

: BANDS-ASSERT ( -- )
   0 BEGIN dup DATA-BANDS:LEN 0 <> WHILE  dup DECLARED-AT  1+  REPEAT drop ;

BANDS-ASSERT
;package
