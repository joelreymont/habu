\ Build-only DATA extent checks; runtime images need only layout.f.
require src/habu/layout.f
require src/habu/regalloc-abi.f
require src/habu/address-cells.f
require src/habu/prof-abi.f
require src/habu/native-observer-cells.f
require src/habu/native-host-cells.f

\ --- DATA claim map and the layout-time overlap assertion ----------------------
\ WHY IT EXISTS. lib/task.f handed out TASK:+USER rows from $41C8 bounded by
\ TXN-STATE-OFF, because the comments in this file said that run was free. It was
\ not: APP-ENTRY:XT-CELL, the AOT capture window, the evaluator-pointer band,
\ PROT, AOT-SIG, BOOT-LAYOUT and src/habu/stack-abi.f all sit inside it. Five
\ shipped libraries fitted into the 472 bytes before the first of them by luck,
\ and a sixth would have overwritten the AOT window with no diagnostic at all -
\ measured, before the bound was corrected: one $40 row past the mark exits 134
\ at teardown with a register dump.
\
\ A COMMENT COULD NOT CATCH THAT AND DID NOT. This table can. Every claim on the
\ DATA region states its own extent, and CLAIMS-ASSERT refuses an overlapping
\ pair at ENGINE BUILD TIME, naming both. src/habu/data-bands.f runs a second
\ check over the same table: every protected band must BE a declared claim,
\ start and length both, which is the check that would have caught the
\ transaction guarding $3000 of a band whose cells end after $300.
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

\ Names live in their own blob, one counted string per row, in row order: the
\ rows have to stay two contiguous cells each, so the bytes cannot sit in them.
\ The blob is ALLOTTED and filled by index, never built with `c,`: an
\ interpreted `s"` appends its own bytes at HERE, so a blob grown with `c,`
\ between two `s"` literals interleaves each name with a copy of itself.
$1000 constant NAMES-CAP
create NAMES NAMES-CAP allot
variable NAMES-U

: NAME-C+ ( n -- ) {: b :}
   NAMES-U @ NAMES-CAP < if
      b NAMES NAMES-U @ + c!
      NAMES-U @ 1+ NAMES-U !
   then ;

: NAME, ( ptr u8 n -- ) {: a u :}
   u NAME-C+
   0 begin dup u < while dup a + c@ NAME-C+ 1+ repeat drop ;

   s" DP-CELL" NAME,
   s" HND-CELL" NAME,
   s" LOCN-CELL" NAME,
   s" LOCF-CELL" NAME,
   s" FRIEND-ARENA" NAME,
   s" CMBK-CELL" NAME,
   s" CMTAG-CELL" NAME,
   s" CMPADS-CELL" NAME,
   s" CMFRD-CELL" NAME,
   s" CMFR-STACK" NAME,
   s" ADDRESS-CELLS-LOCK" NAME,
   s" CMFAM-CELL" NAME,
   s" BODYLEN-CELL" NAME,
   s" RBASE-CELL" NAME,
   s" LOOPSP-CELL" NAME,
   s" STACK-ABI-BASE-CELL" NAME,
   s" SSCR-CELL" NAME,
   s" GTOD-SCRATCH" NAME,
   s" DOESP-CELL" NAME,
   s" VSP-CELL" NAME,
   s" VRFREE-CELL" NAME,
   s" VTAG-STACK" NAME,
   s" CREATEP-CELL" NAME,
   s" QPATCH-CELL" NAME,
   s" QENT-CELL" NAME,
   s" QXH-CELL" NAME,
   s" VVAL-STACK" NAME,
   s" NCOMP-XT-CELL" NAME,
   s" NCOMP-DECL-CELL" NAME,
   s" NCOMP-TARGET-DECL-CELL" NAME,
   s" NCOMP-TIER-CELL" NAME,
   s" JIT-SNAP-SP-CELL" NAME,
   s" NCOMP-DEF-TIER" NAME,
   s" LASTC-CELL" NAME,
   s" RSP-CELL" NAME,
   s" EXITH-CELL" NAME,
   s" LVD-CELL" NAME,
   s" LVH-LEVELS" NAME,
   s" GENIO-ABI" NAME,
   s" AOT-SPAN" NAME,
   s" SIGNAL-ABI" NAME,
   s" LVF-LEVELS" NAME,
   s" LVQ-LEVELS" NAME,
   s" FRAME-CELL" NAME,
   s" QFRAME-CELL" NAME,
   s" BODYBUF" NAME,
   s" RPKG-SNAPSHOT" NAME,
   s" CMM-CELL" NAME,
   s" DOESB-CELL" NAME,
   s" TRUSTED-CELL" NAME,
   s" SRCLOC-PATH" NAME,
   s" SRCLOC-PATHLEN" NAME,
   s" SRCLOC-INB" NAME,
   s" TOKBODY-CELL" NAME,
   s" TOKLOCN-CELL" NAME,
   s" TOKCP-CELL" NAME,
   s" TOKVSP-CELL" NAME,
   s" TOKVRF-CELL" NAME,
   s" TOKFRF-CELL" NAME,
   s" PENDTKA-CELL" NAME,
   s" PKGRESYNC-CELL" NAME,
   s" HIDX-CLAIMS" NAME,
   s" PROT-WINDOW" NAME,
   s" PROT-WLO" NAME,
   s" PROT-RLO" NAME,
   s" ENGINE-HOOK" NAME,
   s" REFUSAL-ABI:CODE-CELL" NAME,
   s" EXIT-HOOK-CELL" NAME,
   s" JIT-QUOT-SP-CELL" NAME,
   s" JIT-QUOT-FRAMES" NAME,
   s" FLOORREC-CELL" NAME,
   s" CLOSED-FREE-CELL" NAME,
   s" CODE-END-CELL" NAME,
   s" DATA-FLOOR-CELL" NAME,
   s" NATIVE-OBS-CELLS:OBSERVE" NAME,
   s" NATIVE-OBS-CELLS:PUBLISHED" NAME,
   s" NATIVE-OBS-CELLS:INVALIDATE" NAME,
   s" NATIVE-OBS-CELLS:HOST-INVALIDATE" NAME,
   s" NATIVE-HOST-CELLS:SELECT" NAME,
   s" NATIVE-HOST-CELLS:SCALAR-PRIM" NAME,
   s" DEF-OCC:PTR-CELL" NAME,
   s" LOCNAMES" NAME,
   s" VRTAB" NAME,
   s" VRITAB" NAME,
   s" REPLH-CELL" NAME,
   s" RSAVCP-CELL" NAME,
   s" RSAVND-CELL" NAME,
   s" RSAVDP-CELL" NAME,
   s" RSAVSP-CELL" NAME,
   s" RRECP-CELL" NAME,
   s" ARGC-CELL" NAME,
   s" ARGV-CELL" NAME,
   s" ENVP-CELL" NAME,
   s" PEND-CELL" NAME,
   s" TKA-CELL" NAME,
   s" TKL-CELL" NAME,
   s" INP-CELL" NAME,
   s" INE-CELL" NAME,
   s" FRFREE-CELL" NAME,
   s" FRCLM-CELL" NAME,
   s" BPA-CELL" NAME,
   s" ADDRESS-CELLS-INDEX" NAME,
   s" BPTAB" NAME,
   s" EVALD-CELL" NAME,
   s" EVALERR-CELL" NAME,
   s" LMAINP-CELL" NAME,
   s" BPWBASE-CELL" NAME,
   s" BPWN-CELL" NAME,
   s" SNAP-CELL" NAME,
   s" NULL-PTR-CELL" NAME,
   s" ENGINE-MAIN-XT-CELL" NAME,
   s" FFI-BUFFERS" NAME,
   s" TASK-TCB-CELL" NAME,
   s" TASKS-LIVE-CELL" NAME,
   s" HIDXP-CELL" NAME,
   s" EVALREC-CELL" NAME,
   s" AOT-SEED-DONE-CELL" NAME,
   s" BOOT-SRC-USER-END" NAME,
   s" PROT-REG" NAME,
   s" FFI-LEN-BUFFERS" NAME,
   s" CALLBACK-BAND" NAME,
   s" APP-ENTRY-XT-CELL" NAME,
   s" AOT-WINDOW-T0" NAME,
   s" AOT-WINDOW-D0" NAME,
   s" AOT-WINDOW-B0" NAME,
   s" EVAL-POINTER-BAND" NAME,
   s" PROT-RHI" NAME,
   s" PROT-CF" NAME,
   s" AOT-SIG-POOL" NAME,
   s" AOT-SIG-LEN" NAME,
   s" BOOT-HEAP-START" NAME,
   s" STACK-ABI-CAP" NAME,
   s" STACK-ABI-REPL-BASE" NAME,
   s" STACK-ABI-REPL-CAP" NAME,
   s" STACK-ABI-RETURN-BASE" NAME,
   s" STACK-ABI-LOOP-BASE" NAME,
   s" TXN-STATE" NAME,
   s" USER-BAND" NAME,
   s" FS-MUT-ABI" NAME,
   s" FS-ABI" NAME,
   s" FMT-ABI" NAME,
   s" STRING-ABI" NAME,
   s" PD-TABLE" NAME,
   s" USE-BAND" NAME,
   s" SNAP-CALLMAP" NAME,
   s" SNAP-ADDRMAP" NAME,
   s" SNAP-XTCELL" NAME,
   s" JIT-SNAP-FRAMES" NAME,
   s" TIER-PROV" NAME,
   s" UNIT-COMPILE" NAME,
   s" RPKG:FLOOR" NAME,
   s" RPKG:WIDS" NAME,
   s" POLICY-NDICT-CELL" NAME,
   s" POLICY-BITS" NAME,
   s" PROF-BAND" NAME,

create TAB
   DP-CELL                        ,  1 cells ,
   HND-CELL                       ,  1 cells ,
   LOCN-CELL                      ,  1 cells ,
   LOCF-CELL                      ,  1 cells ,
   FRIEND-ARENA                   ,  FRIEND-ARENA-LEN ,
   CMBK-CELL                      ,  1 cells ,
   CMTAG-CELL                     ,  1 cells ,
   CMPADS-CELL                    ,  1 cells ,
   CMFRD-CELL                     ,  1 cells ,
   CMFR-OFF                       ,  CMFR-MAX cells ,
   ADDRESS-CELLS:LOCK-CELL        ,  1 cells ,
   CMFAM-CELL                     ,  1 cells ,
   BODYLEN-CELL                   ,  1 cells ,
   RBASE-CELL                     ,  1 cells ,
   LOOPSP-CELL                    ,  1 cells ,
   STACK-ABI:BASE-CELL            ,  1 cells ,
   SSCR-CELL                      ,  1 cells ,
   GTOD-SCRATCH                   ,  2 cells ,
   DOESP-CELL                     ,  1 cells ,
   VSP-CELL                       ,  1 cells ,
   REGALLOC-ABI:VRFREE-CELL       ,  1 cells ,
   VTAG-OFF                       ,  VSMAX ,
   CREATEP-CELL                   ,  1 cells ,
   QPATCH-CELL                    ,  1 cells ,
   QENT-CELL                      ,  1 cells ,
   QXH-CELL                       ,  1 cells ,
   VVAL-OFF                       ,  VSMAX cells ,
   NCOMP-DISPATCH:XT-CELL         ,  1 cells ,
   NCOMP-DISPATCH:DECL-CELL       ,  1 cells ,
   NCOMP-DISPATCH:TARGET-DECL-CELL ,  1 cells ,
   NCOMP-DISPATCH:TIER-CELL       ,  1 cells ,
   JIT-SNAP:SP-CELL               ,  1 cells ,
   NCOMP-DISPATCH:DEF-TIER-CELL   ,  3 cells ,
   LASTC-CELL                     ,  1 cells ,
   RSP-CELL                       ,  1 cells ,
   EXITH-CELL                     ,  1 cells ,
   LVD-CELL                       ,  1 cells ,
   LVH-OFF                        ,  LV-LEVELS cells ,
   GENIO-ABI:OUT-CELL             ,  GENIO-ABI:END GENIO-ABI:OUT-CELL - ,
   AOT-CELLS:SPAN-TABLE-CELL      ,  3 cells ,
   SIGNAL-ABI:STUB-CELL           ,  3 cells ,
   LVF-OFF                        ,  LV-LEVELS cells ,
   LVQ-OFF                        ,  LV-LEVELS cells ,
   FRAME-CELL                     ,  1 cells ,
   QFRAME-CELL                    ,  1 cells ,
   BODYBUF-OFF                    ,  BODYBUF-CAP 2 + ,
   RPKG-CUR                       ,  RPKG-REC RPKG-CUR - 1 cells + ,
   CMM-CELL                       ,  1 cells ,
   DOESB-CELL                     ,  1 cells ,
   TRUSTED-CELL                   ,  1 cells ,
   SRCLOC:PATH-CELL               ,  1 cells ,
   SRCLOC:PATHLEN-CELL            ,  1 cells ,
   SRCLOC:INB-CELL                ,  1 cells ,
   TOKBODY-CELL                   ,  1 cells ,
   TOKLOCN-CELL                   ,  1 cells ,
   TOKCP-CELL                     ,  1 cells ,
   TOKVSP-CELL                    ,  1 cells ,
   TOKVRF-CELL                    ,  1 cells ,
   TOKFRF-CELL                    ,  1 cells ,
   PENDTKA-CELL                   ,  1 cells ,
   PKGRESYNC-CELL                 ,  1 cells ,
   HIDX:CLAIMS                    ,  1 cells ,
   PROT:WINDOW                    ,  1 cells ,
   PROT:WLO                       ,  1 cells ,
   PROT:RLO                       ,  1 cells ,
   ENGINE-HOOK-OFF                ,  ENGINE-HOOK-LEN ,
   REFUSAL-ABI:CODE-CELL          ,  1 cells ,
   EXIT-HOOK-CELL                 ,  1 cells ,
   JIT-QUOT:SP-CELL               ,  1 cells ,
   JIT-QUOT:STK-OFF               ,  JIT-QUOT:END JIT-QUOT:STK-OFF - ,
   FLOORREC-CELL                  ,  1 cells ,
   CLOSED-FREE-CELL               ,  1 cells ,
   CODE-END-CELL                  ,  1 cells ,
   DATA-FLOOR-CELL                ,  1 cells ,
   NATIVE-OBS-CELLS:OBSERVE       ,  1 cells ,
   NATIVE-OBS-CELLS:PUBLISHED     ,  1 cells ,
   NATIVE-OBS-CELLS:INVALIDATE    ,  1 cells ,
   NATIVE-OBS-CELLS:HOST-INVALIDATE ,  1 cells ,
   NATIVE-HOST-CELLS:SELECT        ,  1 cells ,
   NATIVE-HOST-CELLS:SCALAR-PRIM   ,  1 cells ,
   DEF-OCC:PTR-CELL               ,  1 cells ,
   LOCNAMES                       ,  LOC-RECS LOC-REC * ,
   REGALLOC-ABI:VRTAB-OFF         ,  REGALLOC-ABI:VRTAB-BYTES ,
   REGALLOC-ABI:VRITAB-OFF        ,  REGALLOC-ABI:VRTAB-BYTES ,
   REPLH-CELL                     ,  1 cells ,
   RSAVCP-CELL                    ,  1 cells ,
   RSAVND-CELL                    ,  1 cells ,
   RSAVDP-CELL                    ,  1 cells ,
   RSAVSP-CELL                    ,  1 cells ,
   RRECP-CELL                     ,  1 cells ,
   ARGC-CELL                      ,  1 cells ,
   ARGV-CELL                      ,  1 cells ,
   ENVP-CELL                      ,  1 cells ,
   PEND-CELL                      ,  1 cells ,
   TKA-CELL                       ,  1 cells ,
   TKL-CELL                       ,  1 cells ,
   INP-CELL                       ,  1 cells ,
   INE-CELL                       ,  1 cells ,
   REGALLOC-ABI:FRFREE-CELL       ,  1 cells ,
   FRCLM-CELL                     ,  1 cells ,
   BPA-CELL                       ,  1 cells ,
   ADDRESS-CELLS:INDEX-CELL       ,  1 cells ,
   BPTAB-OFF                      ,  EVALD-CELL BPTAB-OFF - ,
   EVALD-CELL                     ,  1 cells ,
   EVALERR-CELL                   ,  1 cells ,
   LMAINP-CELL                    ,  1 cells ,
   BPWBASE-CELL                   ,  1 cells ,
   BPWN-CELL                      ,  1 cells ,
   SNAP-CELL                      ,  1 cells ,
   NULL-PTR-CELL-OFF              ,  1 cells ,
   ENGINE-MAIN:XT-CELL            ,  1 cells ,
   $3A00                          ,  $288 ,
   TASK-TCB-CELL                  ,  1 cells ,
   TASKS-LIVE-CELL                ,  1 cells ,
   HIDXP-CELL                     ,  1 cells ,
   EVALREC-CELL                   ,  1 cells ,
   AOT-SEED-DONE-CELL             ,  1 cells ,
   BOOT-SRC:USER-END              ,  1 cells ,
   PROT-REG-OFF                   ,  PROT-REG-LEN ,
   $40C8                          ,  $100 ,
   CB-XDS                         ,  CB-BAND-BYTES ,
   APP-ENTRY:XT-CELL              ,  1 cells ,
   AOT-CELLS:T0-CELL              ,  1 cells ,
   AOT-CELLS:D0-CELL              ,  1 cells ,
   AOT-CELLS:B0-CELL              ,  1 cells ,
   EVAL-TOP-CELL                  ,  PROT:RHI EVAL-TOP-CELL - ,
   PROT:RHI                       ,  1 cells ,
   PROT:CF                        ,  1 cells ,
   AOT-CELLS:SIG-POOL-CELL        ,  1 cells ,
   AOT-CELLS:SIG-LEN-CELL         ,  1 cells ,
   BOOT-LAYOUT:HEAP-START-CELL    ,  1 cells ,
   STACK-ABI:CAP-CELL             ,  1 cells ,
   STACK-ABI:REPL-BASE-CELL       ,  1 cells ,
   STACK-ABI:REPL-CAP-CELL        ,  1 cells ,
   STACK-ABI:RETURN-BASE-CELL     ,  1 cells ,
   STACK-ABI:LOOP-BASE-CELL       ,  1 cells ,
   TXN-STATE-OFF                  ,  TXN-STATE-LEN ,
   USER-BAND:START                ,  USER-BAND:END USER-BAND:START - ,
   FS-MUT-ABI:START               ,  FS-MUT-ABI:BYTES ,
   FS-ABI:START                   ,  FS-ABI:BYTES ,
   FMT-ABI:START                  ,  FMT-ABI:BYTES ,
   STRING-ABI:START               ,  STRING-ABI:BYTES ,
   PD-TABLE-OFF                   ,  PD-TABLE-END PD-TABLE-OFF - ,
   USE-BAND-OFF                   ,  USE-BAND-END USE-BAND-OFF - ,
   SNAP-RELOC:CALLMAP-OFF         ,  SNAP-RELOC:CALLMAP-BYTES ,
   SNAP-RELOC:ADDRMAP-OFF         ,  SNAP-RELOC:ADDRMAP-BYTES ,
   SNAP-RELOC:XTCELL-N-CELL       ,  SNAP-RELOC:XTCELL-END SNAP-RELOC:XTCELL-N-CELL - ,
   JIT-SNAP:STK-OFF               ,  JIT-SNAP:END JIT-SNAP:STK-OFF - ,
   TIER-PROV:OPEN-CELL            ,  TIER-PROV:END TIER-PROV:OPEN-CELL - ,
   UNIT-COMPILE-CELL              ,  1 cells ,
   RPKG:FLOOR                     ,  1 cells ,
   RPKG:WIDS                      ,  USE-MAX cells ,
   POLICY-NDICT-CELL              ,  1 cells ,
   POLICY-BITS-OFF                ,  PROT-BITS-BYTES ,
   \ The profiler band stays the last row, where BAND-ASSERT places it again.
   0 DATA-SIZE PROF-ABI:PROF-BAND-AT ,  PROF-CNT-BYTES ,
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
