\ data-claims.fs - the recovery engine's DATA claims, proved disjoint at build.
\
\ This stage places every DATA cell and band by a hand-written constant. Two
\ overlaps reached a built seed that way and showed only when a deep nest ran:
\ the BEGIN frames over LASTC..LVH, and frame 0 over the declaration-owner
\ cells src/core/checker.f keeps at $360/$368. Each claim below states its
\ extent, and CLAIMS-ASSERT refuses an overlapping pair while the seed is
\ built, naming both: the check native runs over its own map
\ (src/habu/data-claims.f CLAIMS-ASSERT).
\
\ WHAT IS IN IT. Every DATA cell this stage names and every band with a
\ declared extent. A band is one row, and the cells laid out inside it are not
\ rows of their own: FRIEND-ARENA (the latch, CUR..DEF-WL, the TRUSTED and
\ CREATE signature cells, PKG-*, DEFER-*, SEAL-NDICT), ENGINE-HOOK
\ (COMPILE-PREFLIGHT), PROT-REG (its tag and bitmap), TXN-STATE (P2-* and
\ TXN-*) and USE-BAND (the depth, the saved depth and the wids). Two names
\ share a cell on purpose and are one row: S0-CELL is STACK-ABI:BASE-CELL, and
\ DEF-TKA/DEF-TKL sit in VVAL, live only while the definition name is the
\ token and the virtual stack is empty (native makes the same trade).
\
\ SRC ROWS. Code under src/ and lib/ that this engine runs - its boot prefix
\ and the stage2 build - reaches some DATA cells at native's fixed offsets on
\ whatever engine loads it. Where this stage names the same cell (the PKG
\ cells, WIDN, SEAL-NDICT, TKA/TKL, DP, EVALERR, the USE depth), that row
\ covers it; lib/string.f's STRING-ABI is named in forth.fs for its row. The
\ cells it does not name get a row each, labeled src:, so no cell of this stage
\ can be laid over one.

variable CLAIM-LINK  0 CLAIM-LINK !

\ A row: link, offset, length, counted label.
: CLAIM ( off len "label" -- )
   dup 0= abort" data-claims: a claim has no extent"
   align here  CLAIM-LINK @ ,  CLAIM-LINK !  swap , ,
   parse-name  dup c,  here over allot  swap move ;

\ The claim at the constant NAME, labeled NAME.
: BAND-CLAIM ( len "name" -- )
   >in @ >r  parse-name evaluate  swap  r> >in !  CLAIM ;
: CELL-CLAIM ( "name" -- )  1 cells BAND-CLAIM ;

: CLAIM-OFF ( row -- off )     cell+ @ ;
: CLAIM-END ( row -- end )     dup CLAIM-OFF swap 2 cells + @ + ;
: CLAIM-NAME ( row -- a u )    3 cells + count ;

\ Half-open [off, end) intersection.
: CLAIMS-OVERLAP? ( row row -- f )
   2dup CLAIM-END swap CLAIM-OFF > >r
   swap CLAIM-END swap CLAIM-OFF > r> and ;

variable CLAIMS-MSG
: CLAIMS-DIE ( row row -- )
   s" data-claims: DATA claims overlap: " CLAIMS-MSG $!
   CLAIM-NAME CLAIMS-MSG $+!  s"  and " CLAIMS-MSG $+!  CLAIM-NAME CLAIMS-MSG $+!
   CLAIMS-MSG $@ exception throw ;

: CLAIMS-ASSERT ( -- )
   CLAIM-LINK @ begin dup while
      dup @ begin dup while
         2dup CLAIMS-OVERLAP? if 2dup CLAIMS-DIE then
         @
      repeat drop
      @
   repeat drop ;

CELL-CLAIM DP-CELL
CELL-CLAIM HND-CELL
CELL-CLAIM LOCN-CELL
CELL-CLAIM LOCF-CELL
FRIEND-ARENA-LEN BAND-CLAIM FRIEND-ARENA
CELL-CLAIM CMBK-CELL
CELL-CLAIM CMTAG-CELL
CELL-CLAIM CMPADS-CELL
CELL-CLAIM CMFRD-CELL
CMFR-MAX cells BAND-CLAIM CMFR-OFF
CELL-CLAIM CMFAM-CELL
CELL-CLAIM BODYLEN-CELL
CELL-CLAIM RBASE-CELL
CELL-CLAIM LOOPSP-CELL
CELL-CLAIM STACK-ABI:BASE-CELL
CELL-CLAIM SSCR-CELL
CELL-CLAIM PROF-TOT
CELL-CLAIM PROF-LIM
CELL-CLAIM DOESP-CELL
CELL-CLAIM PROF-OTHER
CELL-CLAIM VSP-CELL
CELL-CLAIM VRFREE-CELL
VSMAX BAND-CLAIM VTAG-OFF
CELL-CLAIM CREATEP-CELL
CELL-CLAIM QPATCH-CELL
CELL-CLAIM QENT-CELL
CELL-CLAIM QXH-CELL
VSMAX cells BAND-CLAIM VVAL-OFF
CELL-CLAIM SNAPSP-CELL
$360 1 cells CLAIM src:checker.f:SOURCE-CELL
$368 1 cells CLAIM src:checker.f:TARGET-CELL
CELL-CLAIM LASTC-CELL
CELL-CLAIM RSP-CELL
CELL-CLAIM EXITH-CELL
CELL-CLAIM LVD-CELL
LV-LEVELS cells BAND-CLAIM LVH-OFF
CELL-CLAIM SIGNAL-ABI:STUB-CELL
CELL-CLAIM SIGNAL-ABI:FD-PTR-CELL
CELL-CLAIM SIGNAL-ABI:FD-CELL
LV-LEVELS cells BAND-CLAIM LVF-OFF
LV-LEVELS cells BAND-CLAIM LVQ-OFF
CELL-CLAIM FRAME-CELL
CELL-CLAIM QFRAME-CELL
BODYBUF-LEN BAND-CLAIM BODYBUF-OFF
CELL-CLAIM CMM-CELL
CELL-CLAIM DOESB-CELL
CELL-CLAIM TRUSTED-CELL
CELL-CLAIM PKGRESYNC-CELL
ENGINE-HOOK-LEN BAND-CLAIM ENGINE-HOOK-OFF
$27F8 1 cells CLAIM src:checker.f:CHECKER-REFUSAL-CELL
$2800 1 cells CLAIM src:include.f:INCLUDE-SRCLOC-PATH-CELL
$2808 1 cells CLAIM src:include.f:INCLUDE-SRCLOC-PATHLEN-CELL
CELL-CLAIM EXIT-HOOK-CELL
CELL-CLAIM FLOORREC-CELL
CELL-CLAIM CLOSED-FREE-CELL
CELL-CLAIM CODE-END-CELL
$2CE8 1 cells CLAIM src:generated-declaration-dictionary.f:DATA-FLOOR-CELL
LOC-RECS LOC-REC * BAND-CLAIM LOCNAMES
32 BAND-CLAIM VRTAB-OFF
32 BAND-CLAIM VRITAB-OFF
CELL-CLAIM REPLH-CELL
CELL-CLAIM RSAVCP-CELL
CELL-CLAIM RSAVND-CELL
CELL-CLAIM RSAVDP-CELL
CELL-CLAIM RSAVSP-CELL
CELL-CLAIM RRECP-CELL
CELL-CLAIM ARGC-CELL
CELL-CLAIM ARGV-CELL
CELL-CLAIM ENVP-CELL
CELL-CLAIM PEND-CELL
CELL-CLAIM TKA-CELL
CELL-CLAIM TKL-CELL
CELL-CLAIM INP-CELL
CELL-CLAIM INE-CELL
CELL-CLAIM FRFREE-CELL
CELL-CLAIM FRCLM-CELL
CELL-CLAIM BPA-CELL
BP-MAX BP-SLOT-SHIFT lshift BAND-CLAIM BPTAB-OFF
CELL-CLAIM EVALD-CELL
CELL-CLAIM EVALERR-CELL
CELL-CLAIM LMAINP-CELL
CELL-CLAIM SNAP-CELL
$3800 1 cells CLAIM src:pointer-storage.f:NULL-PTR-OFF
PROT-REG-LEN BAND-CLAIM PROT-REG-OFF
$43A0 1 cells CLAIM src:layout.f:APP-ENTRY:XT-CELL
CELL-CLAIM EVAL-TOP-CELL
$47D0 1 cells CLAIM src:checker.f:CK-AOT-SIG-POOL-OFF
$47D8 1 cells CLAIM src:checker.f:CK-AOT-SIG-LEN-OFF
CELL-CLAIM STACK-ABI:CAP-CELL
CELL-CLAIM STACK-ABI:REPL-BASE-CELL
CELL-CLAIM STACK-ABI:REPL-CAP-CELL
CELL-CLAIM STACK-ABI:RETURN-BASE-CELL
CELL-CLAIM STACK-ABI:LOOP-BASE-CELL
TXN-STATE-LEN BAND-CLAIM TXN-STATE-OFF
STRING-ABI:BYTES BAND-CLAIM STRING-ABI:START
USE-BAND-END USE-BAND-OFF - BAND-CLAIM USE-BAND-OFF
SNAPSTK-END SNAPSTK-OFF - BAND-CLAIM SNAPSTK-OFF
PD-TABLE-END PD-TABLE-OFF - BAND-CLAIM PD-TABLE-OFF
PROF-CNT DATA-START - BAND-CLAIM DATA-START   \ the DP heap; DP-CHECK caps it at PROF-CNT
PROF-CNT-BYTES BAND-CLAIM PROF-CNT

CLAIMS-ASSERT
