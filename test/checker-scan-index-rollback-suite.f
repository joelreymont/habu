\ checker-scan-index-rollback-suite.f — rollback of records a rejected frame
\ creates, and capacity refusal, for the checker's symbol-keyed store indexes.
\
\ The other half of test/checker-scan-index-suite.f, whose header says what is
\ under test and why every case is a top-level interpret line. This row holds
\ the cases that touch no state that suite's section 1 creates, so the two run
\ side by side in the gate pool; section numbers are that suite's, so a case
\ keeps its name across the two rows. The shared fixture is
\ test/checker-scan-index-lib.f. A WHITEBOX-SUITE row: standalone under bin/hb
\ it exits 70 (docs/gate.md "How a suite runs").
\
\ The guard and differential of section 2 run here first too: an index mark a
\ rollback case reads is meaningful only once all five indexes exist, and the
\ differential is what builds them (SCX-MARKS-EXACT).
\
\ Section 4 proves the effect and control tables refuse a key they have no
\ cell for, in a child process, because the refusal is a process exit. The
\ family tail index has no such refusal to test: its bucket array grows with
\ the record arena it indexes (TFX-RESIZE), so the other row's section 5 proves
\ the grown case answers instead.

require lib/string.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/checker-scan-index-lib.f

\ Reopened, not imported: the cases run inside the fixture's package, where
\ the definitions they make land (test/checker-scan-index-lib.f).
using TFAM

package SCANIDX-TEST

\ ---------------------------------------------------------------------------
\ 2. DIFFERENTIAL, run first so every index exists before a rollback case.
\ ---------------------------------------------------------------------------
SCX-TFAM-N 0 > TTRUE                           \ the family differential is not vacuous
SCX-SYM-N 1 > TTRUE                            \ nor the symbol ones
SCX-DIFF-ALL

\ ---------------------------------------------------------------------------
\ 3. ROLLBACK. The checker's own rollback frames retire store records; the
\    index has to retire with them, and a name redefined AFTER a rollback must
\    answer with its new effect and not with the rolled-back one. Here the
\    symbol or family is new inside the frame; 3d and 3e, in the other row,
\    roll back records of symbols that outlive it.
\ ---------------------------------------------------------------------------

\ 3a. a rejected candidate frame: a signature added inside it is visible there
\     and gone after, and the same name then takes a DIFFERENT effect cleanly.
s" SCXR" SCX-SIG-MIN-IN -1 T=
CHECK-CANDIDATE-START
   s" n n -- n" s" SCXR" CHECKER-USIG-ADD
   s" SCXR" SCX-SIG-MIN-IN 2 T=                      \ visible inside the candidate
0 CHECK-CANDIDATE-DONE drop
SCX-MARKS-EXACT                                      \ read FIRST: a lookup would rebuild
s" SCXR" SCX-SIG-MIN-IN -1 T=                        \ retired with the frame
s" n -- n" s" SCXR" CHECKER-USIG-ADD
s" SCXR" SCX-SIG-MIN-IN 1 T=                         \ the new effect answers
SCX-DIFF-ALL

\ 3b. the same shape through the real load path: a definition the checker
\     REJECTS rolls its scope back, and the name then takes a different effect
\     that its callers are held to.
: SCX-BADDEF ( -- ) s" : SCXB ( n -- n ) drop ;" evaluate-closed ;
: SCX-GOODDEF ( -- ) s" : SCXB ( n -- ) drop ;" evaluate-closed ;
: SCX-GOODUSE ( -- ) s" : SCXBU ( n -- ) SCXB ;" evaluate-closed ;
: SCX-BADUSE ( -- ) s" : SCXBU2 ( n -- n ) SCXB ;" evaluate-closed ;

' SCX-BADDEF catch TC !   TC @ 0 <> TTRUE            \ rejected: the body drops its output
s" SCXB" SCX-SIG-MIN-IN -1 T=                        \ ... and left no record behind
' SCX-GOODDEF catch TC !  TC @ 0 T=
s" SCXB" SCX-SIG-MIN-IN 1 T=
' SCX-GOODUSE catch TC !  TC @ 0 T=                  \ a caller certifies against the new effect
' SCX-BADUSE catch TC !   TC @ 0 <> TTRUE            \ ... and against nothing else
SCX-DIFF-ALL

\ 3c. a family declared inside a rejected candidate leaves no row and no chain,
\     and the same (package, tail) can then be declared with a different kind.
\     Its variant leaves no tail chain either (VNX-RETIRE).
variable CAND-FAM                                    \ the candidate family's id
s" scxctor" SCX-NAME! SCX-SYM-INTERN IX !
IX @ SUMV-FROM-CTOR-SYM TFALSE drop
s" scxrb" s" cand" TFAM-FIND-IN TFALSE drop
CHECK-CANDIDATE-START
   s" scxrb" CHECKER-PACKAGE-PRIVATE s" cand" 2 TK-PRODUCT TFAM-DECL CAND-FAM !
   s" scxrb" CHECKER-PACKAGE-PRIVATE s" cand2" 2 TK-PRODUCT TFAM-DECL drop
   s" scxrb" s" cand" TFAM-FIND-IN TTRUE drop
   CAND-FAM @ s" cand" 0 0 0 0 SUMV-ADD IX @ SUMV-CTOR-SYM!
   CAND-FAM @ s" cand" SUMV-FIND TTRUE drop
   IX @ SUMV-FROM-CTOR-SYM TTRUE drop
0 CHECK-CANDIDATE-DONE drop
SCX-MARKS-EXACT                                      \ read FIRST: a lookup would rebuild
s" scxrb" s" cand" TFAM-FIND-IN TFALSE drop
s" scxrb" s" cand2" TFAM-FIND-IN TFALSE drop
CAND-FAM @ s" cand" SUMV-FIND TFALSE drop
IX @ SUMV-FROM-CTOR-SYM TFALSE drop
s" scxrb" CHECKER-PACKAGE-PUBLIC s" cand" 0 TK-CELL ' TFAM-DECL catch TC ! drop
TC @ 0 T=
s" scxrb" s" cand" TFAM-FIND-IN TTRUE drop
SCX-MARKS-EXACT
SCX-DIFF-ALL

\ ---------------------------------------------------------------------------
\ 4. CAPACITY REFUSAL. The mapping has exactly SYM-CAP cells per table, so a
\    key at or above that cap — or below the first real symbol id — has no cell
\    and the store and the symbol table have disagreed. Each table refuses
\    through its own linking word, and all three linking words call the one
\    range test (checker.f IDX-SYM-OK). The refusal is a process exit, so each
\    case is a child. SVX-LINK is private to `package TFAM`, so no child
\    program can spell it; USX-LINK and NRX-LINK are global, and a deleted
\    guard reds their cases.
\ ---------------------------------------------------------------------------
$1000 constant IO-CAP
30000 constant TIMEOUT-MS
create SCX-OUT IO-CAP allot
create SCX-ERR IO-CAP allot
variable SCX-ERR-U
variable SCX-RC

: SCX-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" exit then ;

: SCX-CHILD ( ptr u8 n -- ) {: src:ptr srcu:n :}
   PROC-ARGV-RESET
   SCX-HB$ >LEN src srcu >LEN
   SCX-OUT IO-CAP >LEN SCX-ERR IO-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} e LEN>N SCX-ERR-U ! 0 SCX-RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} e LEN>N SCX-ERR-U ! c RC>N SCX-RC ! ENDOF
   ;MATCH ;

: SCX-ERR$ ( -- ptr u8 n )
   SCX-ERR SCX-ERR-U @ ;

\ The refusal must be the NAMED one: an rc alone cannot tell this refusal apart
\ from any other exit 76 the child could reach.
: SCX-REFUSED ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu SCX-CHILD
   SCX-RC @ 76 T=
   SCX-ERR$ s" checker: store record symbol outside index range" CONTAINS? TTRUE ;

s" : SCXCAP ( -- ) 0 SYM-CAP USX-LINK ; SCXCAP" SCX-REFUSED
s" : SCXCAP ( -- ) 0 SYM-CAP NRX-LINK ; SCXCAP" SCX-REFUSED

\ id 0 is the symbol table's own "no symbol", not a key: the control store has
\ no early return for it, so its linking word is where that shows.
s" : SCXCAP ( -- ) 0 0 NRX-LINK ; SCXCAP" SCX-REFUSED

\ and a key one below the cap is inside the mapping, so it does NOT refuse
s" : SCXOK ( -- ) 0 SYM-CAP 1 - NRX-LINK ; SCXOK" SCX-CHILD
SCX-RC @ 0 T=

s" checker-scan-index-rollback-suite: failures" REPORT

;package

;using
