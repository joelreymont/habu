split-str
\ Build-only DATA extent checks; runtime images need only layout.f.
require src/core/exit-status.f
require src/habu/boot-cells.f
require src/habu/layout.f
require src/habu/address-cells.f
require src/habu/prof-abi.f

\ Every claim on DATA states its extent. CLAIMS-ASSERT rejects an overlapping
\ pair at engine build time, naming both, when an engine builder
\ (src/habu/habu1.f, src/habu/kernel-x64.f) loads this file.
\
\ The map holds every claim with a declared extent, the profiler band among
\ them: the top PROF-CNT-BYTES of DATA, as src/habu/prof-abi.f PROF-BAND-AT
\ places it for the global DATA-SIZE. On a macOS host that is not an x86-64
\ build's LINUX-LAYOUT:DATA-SIZE, so kernel-x64.f places the band again through
\ BAND-ASSERT.
\
\ Deliberate aliases are one row: VVAL-STACK of VSMAX cells, inside which
\ DEF-TKA-CELL and DEF-TKL-CELL live at $250/$258 because they are live only while the virtual stack is empty (the
\ note at CMM-CELL). Separate rows would assert a conflict the engine takes on
\ purpose.
package DATA-CLAIMS
private

: NO-NAME ( n n -- ) {: off:n len:n :}
   s" E-DATA-CLAIM" REJECTION:OPEN
   s" the CLAIM is missing its display name" REJECTION:EMIT-STR
   s" offset" REJECTION:NOTE  off REJECTION:EMIT-U
   s" length" REJECTION:NOTE  len REJECTION:EMIT-U
   REJECTION:HELP  s" name the claim after `CLAIM`" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

public
\ Each claim is one line, `<offset> <length> CLAIM <display name>`. A row in
\ TAB is the offset, the length and the name's byte count, then the name,
\ padded to a cell. A row is named by its byte offset from TAB.
: CLAIM ( n n -- ) {: off len :}
   parse-name {: a u :}
   u 0 = if off len NO-NAME then
   off , len , u ,
   u 0 ?do a i + c@ c, loop  align ;
parses: CLAIM 1

create TAB
   DP-CELL                            1 cells                 CLAIM DP-CELL
   HND-CELL                           1 cells                 CLAIM HND-CELL
   LOCN-CELL                          1 cells                 CLAIM LOCN-CELL
   ADDRESS-CELLS:LOCK-CELL            1 cells                 CLAIM ADDRESS-CELLS-LOCK
   BODYLEN-CELL                       1 cells                 CLAIM BODYLEN-CELL
   RBASE-CELL                         1 cells                 CLAIM RBASE-CELL
   LOOPSP-CELL                        1 cells                 CLAIM LOOPSP-CELL
   STACK-ABI:BASE-CELL                1 cells                 CLAIM STACK-ABI-BASE-CELL
   SSCR-CELL                          1 cells                 CLAIM SSCR-CELL
   GTOD-SCRATCH                       2 cells                 CLAIM GTOD-SCRATCH
   VSP-CELL                           1 cells                 CLAIM VSP-CELL
   CREATEP-CELL                       1 cells                 CLAIM CREATEP-CELL
   VVAL-OFF                           VSMAX cells             CLAIM VVAL-STACK
   NCOMP-DISPATCH:COMP-CELL           1 cells                 CLAIM NCOMP-COMP-CELL
   NCOMP-DISPATCH:FIXED-COMP-CELL     1 cells                 CLAIM NCOMP-FIXED-COMP-CELL
   NCOMP-DISPATCH:DECL-CELL           1 cells                 CLAIM NCOMP-DECL-CELL
   NCOMP-DISPATCH:TARGET-DECL-CELL    1 cells                 CLAIM NCOMP-TARGET-DECL-CELL
   NCOMP-DISPATCH:COMP-LATCH-CELL     1 cells                 CLAIM NCOMP-COMP-LATCH-CELL
   NCOMP-DISPATCH:RECOVER-CELL        1 cells                 CLAIM NCOMP-RECOVER-CELL
   LASTC-CELL                         1 cells                 CLAIM LASTC-CELL
   RSP-CELL                           1 cells                 CLAIM RSP-CELL
   EXITH-CELL                         1 cells                 CLAIM EXITH-CELL
   GENIO-ABI:OUT-CELL                 GENIO-ABI:END GENIO-ABI:OUT-CELL -  CLAIM GENIO-ABI
   SIGNAL-ABI:STUB-CELL               3 cells                 CLAIM SIGNAL-ABI
   COMMA-RUN-CELL                     1 cells                 CLAIM COMMA-RUN-CELL
   COMMA-END-CELL                     1 cells                 CLAIM COMMA-END-CELL
   COMMA-BASE-CELL                    1 cells                 CLAIM COMMA-BASE-CELL
   BODY-TEXT:BASE-CELL                4 cells                 CLAIM BODY-TEXT
   BODY-ITEMS:BASE-CELL               5 cells                 CLAIM BODY-ITEMS
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
   NCOMP-DISPATCH:DOES-SHADOW-CELL    1 cells                 CLAIM NCOMP-DOES-SHADOW-CELL
   TASK-CHAIN-CELL                    1 cells                 CLAIM TASK-CHAIN-CELL
   HIDX:CLAIMS                        1 cells                 CLAIM HIDX-CLAIMS
   COMPILE-PREFLIGHT-CELL             1 cells                 CLAIM COMPILE-PREFLIGHT-CELL
   SUSP-HOOK-OFF                      SUSP-HOOK-LEN           CLAIM SUSP-HOOK
   THROWS-CELL                        1 cells                 CLAIM THROWS-CELL
   STAMP-CELL                         1 cells                 CLAIM STAMP-CELL
   EXIT-HOOK-CELL                     1 cells                 CLAIM EXIT-HOOK-CELL
   EXIT-OWNER-CELL                    1 cells                 CLAIM EXIT-OWNER-CELL
   FLOORREC-CELL                      1 cells                 CLAIM FLOORREC-CELL
   FAULT-MARK-CELL                    1 cells                 CLAIM FAULT-MARK-CELL
   DBASE-CELL                         1 cells                 CLAIM DBASE-CELL
   REJECTION-JSON-CELL                1 cells                 CLAIM REJECTION-JSON-CELL
   REJECTION-SENT-CELL                1 cells                 CLAIM REJECTION-SENT-CELL
   REJECTION-CARRY-A-CELL             1 cells                 CLAIM REJECTION-CARRY-A-CELL
   REJECTION-CARRY-U-CELL             1 cells                 CLAIM REJECTION-CARRY-U-CELL
   MAIN-STACK-TOP-CELL                1 cells                 CLAIM MAIN-STACK-TOP-CELL
   MAIN-THREAD-CELL                   1 cells                 CLAIM MAIN-THREAD-CELL
   CLOSED-FREE-CELL                   1 cells                 CLAIM CLOSED-FREE-CELL
   CODE-END-CELL                      1 cells                 CLAIM CODE-END-CELL
   PRIM-TABLE-CELL                    1 cells                 CLAIM PRIM-TABLE-CELL
   TEXT-SIZE-CELL                     1 cells                 CLAIM TEXT-SIZE-CELL
   DPHW-CELL                          1 cells                 CLAIM DPHW-CELL
   NDICT-HW-CELL                      1 cells                 CLAIM NDICT-HW-CELL
   DATA-FLOOR-CELL                    1 cells                 CLAIM DATA-FLOOR-CELL
   DEF-OCC:PTR-CELL                   1 cells                 CLAIM DEF-OCC:PTR-CELL
   CAPDEPTH-CELL                      1 cells                 CLAIM CAPDEPTH-CELL
   TOKDEPTH-CELL                      1 cells                 CLAIM TOKDEPTH-CELL
   RAWNEXT-CELL                       1 cells                 CLAIM RAWNEXT-CELL
   TOKRAW-CELL                        1 cells                 CLAIM TOKRAW-CELL
   ARGC-CELL                          1 cells                 CLAIM ARGC-CELL
   ARGV-CELL                          1 cells                 CLAIM ARGV-CELL
   ENVP-CELL                          1 cells                 CLAIM ENVP-CELL
   PROGRAM-FILE-CELL                  1 cells                 CLAIM PROGRAM-FILE-CELL
   PEND-CELL                          1 cells                 CLAIM PEND-CELL
   TKA-CELL                           1 cells                 CLAIM TKA-CELL
   TKL-CELL                           1 cells                 CLAIM TKL-CELL
   TOKENS-CELL                        1 cells                 CLAIM TOKENS-CELL
   INP-CELL                           1 cells                 CLAIM INP-CELL
   INE-CELL                           1 cells                 CLAIM INE-CELL
   BPTRAMP-CELL                       1 cells                 CLAIM BPTRAMP-CELL
   ADDRESS-CELLS:INDEX-CELL           1 cells                 CLAIM ADDRESS-CELLS-INDEX
   BPTAB-OFF                          EVALD-CELL BPTAB-OFF -  CLAIM BPTAB
   EVALD-CELL                         1 cells                 CLAIM EVALD-CELL
   BPWBASE-CELL                       1 cells                 CLAIM BPWBASE-CELL
   BPWN-CELL                          1 cells                 CLAIM BPWN-CELL
   ENGINE-MAIN:XT-CELL                1 cells                 CLAIM ENGINE-MAIN-XT-CELL
   NCOMP-DISPATCH:CALLBACK-STUB-CELL  1 cells                 CLAIM NCOMP-DISPATCH:CALLBACK-STUB-CELL
   NCOMP-DISPATCH:CALLBACK-ENTRY-CELL  1 cells                 CLAIM NCOMP-DISPATCH:CALLBACK-ENTRY-CELL
   POINTER-STORAGE:DATUM-CELL         1 cells                 CLAIM DATUM-CELL
   NCOMP-DISPATCH:BODY-HOOK-CELL      1 cells                 CLAIM NCOMP-DISPATCH:BODY-HOOK-CELL
   PROVIDED-XT:EVALUATE-CELL          1 cells                 CLAIM PROVIDED-XT-EVALUATE-CELL
   PROVIDED-XT:EVALUATE-CLOSED-CELL   1 cells                 CLAIM PROVIDED-XT-EVALUATE-CLOSED-CELL
   PROVIDED-XT:EVALUATE-RANGE-CELL    1 cells                 CLAIM PROVIDED-XT-EVALUATE-RANGE-CELL
   PROVIDED-XT:DEFINE-CELL            1 cells                 CLAIM PROVIDED-XT-DEFINE-CELL
   PROVIDED-XT:COMPILE-CELL           1 cells                 CLAIM PROVIDED-XT-COMPILE-CELL
   PROVIDED-XT:END-CELL               1 cells                 CLAIM PROVIDED-XT-END-CELL
   PROVIDED-XT:ITEM-CELL              1 cells                 CLAIM PROVIDED-XT-ITEM-CELL
   PROVIDED-XT:CHILD-CELL             1 cells                 CLAIM PROVIDED-XT-CHILD-CELL
   PROVIDED-XT:SEAL-CELL              1 cells                 CLAIM PROVIDED-XT-SEAL-CELL
   PROVIDED-XT:STORAGE-LINE-CELL      1 cells                 CLAIM PROVIDED-XT-STORAGE-LINE-CELL
   PROVIDED-XT:CELL-LINE-CELL         1 cells                 CLAIM PROVIDED-XT-CELL-LINE-CELL
   PROVIDED-XT:LITERAL-CELL           1 cells                 CLAIM PROVIDED-XT-LITERAL-CELL
   PROVIDED-XT:TYPE-OP-CELL           1 cells                 CLAIM PROVIDED-XT-TYPE-OP-CELL
   PROVIDED-XT:VARIABLE-CELL          1 cells                 CLAIM PROVIDED-XT-VARIABLE-CELL
   POLICY-ABI:KEYWORD-CELL            1 cells                 CLAIM POLICY-KEYWORD-CELL
   HALT-UNWIND-CELL                   1 cells                 CLAIM HALT-UNWIND-CELL
   FFI-ABI:START                      FFI-ABI:BYTES           CLAIM FFI-ABI
   TASK-TCB-CELL                      1 cells                 CLAIM TASK-TCB-CELL
   TASKS-LIVE-CELL                    1 cells                 CLAIM TASKS-LIVE-CELL
   HIDXP-CELL                         1 cells                 CLAIM HIDXP-CELL
   BOOT:USER-END                      1 cells                 CLAIM BOOT-USER-END
   CB-XDS                             CB-BAND-BYTES           CLAIM CALLBACK-BAND
   CB-TLS-GET                         2 cells                 CLAIM CALLBACK-TLS
   APP-ENTRY:XT-CELL                  1 cells                 CLAIM APP-ENTRY-XT-CELL
   REGION-ALIAS:DELTA-CELL            1 cells                 CLAIM REGION-ALIAS-DELTA
   EVAL-TOP-CELL                      1 cells                 CLAIM EVAL-TOP-CELL
   BOOT:HEAP-START-CELL               1 cells                 CLAIM BOOT-HEAP-START
   BOOT:DP-FLOOR-CELL                 1 cells                 CLAIM BOOT-DP-FLOOR
   BOOT:DP-CEILING-CELL               1 cells                 CLAIM BOOT-DP-CEILING
   BOOT:GLOBAL-WL-CELL                1 cells                 CLAIM BOOT-GLOBAL-WL
   BOOT:NAMESPACE-WL-CELL             1 cells                 CLAIM BOOT-NAMESPACE-WL
   BOOT:TYPES-WL-CELL                 1 cells                 CLAIM BOOT-TYPES-WL
   BOOT:IMAGE-BASE-CELL               1 cells                 CLAIM BOOT-IMAGE-BASE
   STACK-ABI:CAP-CELL                 1 cells                 CLAIM STACK-ABI-CAP
   STACK-ABI:RETURN-BASE-CELL         1 cells                 CLAIM STACK-ABI-RETURN-BASE
   STACK-ABI:LOOP-BASE-CELL           1 cells                 CLAIM STACK-ABI-LOOP-BASE
   USER-BAND:START                    USER-BAND:END USER-BAND:START -  CLAIM USER-BAND
   MEM-ABI:START                      MEM-ABI:BYTES           CLAIM MEM-ABI
   FS-MUT-ABI:START                   FS-MUT-ABI:BYTES        CLAIM FS-MUT-ABI
   FS-ABI:START                       FS-ABI:BYTES            CLAIM FS-ABI
   REASON-ABI:START                   REASON-ABI:BYTES        CLAIM REASON-ABI
   FMT-ABI:START                      FMT-ABI:BYTES           CLAIM FMT-ABI
   STRING-ABI:START                   STRING-ABI:BYTES        CLAIM STRING-ABI
   PD-TABLE-OFF                       PD-TABLE-END PD-TABLE-OFF -  CLAIM PD-TABLE
   USE-BAND-OFF                       USE-BAND-END USE-BAND-OFF -  CLAIM USE-BAND
   RELOC:CALLMAP-OFF             RELOC:CALLMAP-BYTES  CLAIM CALLMAP
   RELOC:ADDRMAP-OFF             RELOC:ADDRMAP-BYTES  CLAIM ADDRMAP
   RELOC:XTCELL-N-CELL           RELOC:XTCELL-END RELOC:XTCELL-N-CELL -  CLAIM XTCELL
   REPLAY-SCOPE:LATCH                 REPLAY-SCOPE:END REPLAY-SCOPE:LATCH -  CLAIM REPLAY-SCOPE
   ENCLOSING:PUB                      ENCLOSING:END ENCLOSING:PUB -  CLAIM ENCLOSING
   REPLAY-SCOPE:ENCLOSING             ENCLOSING:END ENCLOSING:PUB -  CLAIM REPLAY-SCOPE:ENCLOSING
   REPLAY-SCOPE:PEND                  1 cells                 CLAIM REPLAY-SCOPE:PEND
   REPLAY-SCOPE:PEND-REC              DREC                    CLAIM REPLAY-SCOPE:PEND-REC
   POLICY-NDICT-CELL                  1 cells                 CLAIM POLICY-NDICT-CELL
   POLICY-BITS-OFF                    POLICY-BITS-BYTES       CLAIM POLICY-BITS
   POLICY-REQUIRE-CELL                1 cells                 CLAIM POLICY-REQUIRE-CELL
   CLAUSE-CELL                        1 cells                 CLAIM CLAUSE-CELL
   PKG-OUTSIDE-CELL                   1 cells                 CLAIM PKG-OUTSIDE-CELL
   SECTION-CELL                       1 cells                 CLAIM SECTION-CELL
   \ The profiler band stays the last row, where BAND-ASSERT places it again.
   0 DATA-SIZE PROF-ABI:PROF-BAND-AT  PROF-CNT-BYTES          CLAIM PROF-BAND
   0 ,  0 ,  0 ,

: ROW-OFF ( n -- n )
   TAB + @ ;

: ROW-LEN ( n -- n )
   TAB + 1 cells + @ ;

: NAME-AT ( n -- str ) {: row :}
   TAB row + 3 cells +  TAB row + 2 cells + @ ;

private
: NEXT-ROW ( n -- n ) {: row :}
   TAB row + 2 cells + @  1 cells 1- +  1 cells / cells
   row 3 cells + + ;

: END-ROW? ( n -- bool )
   ROW-LEN 0 = ;

: LAST-ROW ( -- n )
   0 begin dup NEXT-ROW END-ROW? 0= while NEXT-ROW repeat ;

: EXTENT ( n -- ) {: row :}
   row NAME-AT REJECTION:NOTE
   s" offset " REJECTION:EMIT-STR  row ROW-OFF REJECTION:EMIT-U
   s" , length " REJECTION:EMIT-STR  row ROW-LEN REJECTION:EMIT-U ;

public
\ Half-open [off, off+len) intersection. A zero-length row cannot exist: the
\ walk stops on one, so a claim declared with no extent ends the table early
\ instead of being silently skipped - which is why every row states `1 cells`
\ rather than nothing.
: OVERLAP? ( n n -- bool ) {: a b :}
   a ROW-OFF b ROW-OFF b ROW-LEN + < 
   b ROW-OFF a ROW-OFF a ROW-LEN + < and ;

: OVERLAP-REJECT ( n n -- ) {: a b :}
   s" E-DATA-CLAIM" REJECTION:OPEN
   s" the DATA claims `" REJECTION:EMIT-STR  a NAME-AT REJECTION:EMIT-STR
   s" ` and `" REJECTION:EMIT-STR  b NAME-AT REJECTION:EMIT-STR
   s" ` overlap" REJECTION:EMIT-STR
   a EXTENT  b EXTENT
   REJECTION:HELP
   s" move one so the extents do not overlap" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

: CLAIMS-ASSERT ( -- )
   0 begin dup END-ROW? 0= while
      dup NEXT-ROW begin dup END-ROW? 0= while
         2dup OVERLAP? if 2dup OVERLAP-REJECT then
         NEXT-ROW
      repeat drop
      NEXT-ROW
   repeat drop ;

LAST-ROW constant PROF-ROW

\ The last row is the band PROF-BAND-AT placed for the global DATA-SIZE; a row
\ added after it would be the one BAND-ASSERT moves instead.
: PROF-ROW-ASSERT ( -- )
   PROF-ROW ROW-OFF 0 DATA-SIZE PROF-ABI:PROF-BAND-AT <> if
      s" E-DATA-CLAIM" REJECTION:OPEN
      s" the last row is not `PROF-BAND`" REJECTION:EMIT-STR
      PROF-ROW EXTENT
      REJECTION:HELP  s" keep `PROF-BAND` the last row" REJECTION:EMIT-STR
      EXIT-STATUS:REJECTED REJECTION:STOP
   then ;
PROF-ROW-ASSERT

\ Place the profiler band for a target whose DATA is n bytes and check the
\ table again, for a build whose target's DATA-SIZE is not the global one.
: BAND-ASSERT ( n -- )
   {: size :}
   0 size PROF-ABI:PROF-BAND-AT  TAB PROF-ROW + !
   CLAIMS-ASSERT ;

CLAIMS-ASSERT
;package
