\ checker-scan-index-suite.f — the checker's symbol-keyed store indexes.
\
\ Run by the engine, like test/type-family-rollback-suite.f: every case is a
\ top-level interpret line, because the stores and their indexes are checker
\ internals that resolve only there, reached through named TRUSTED: shims.
\
\     bin/hb --load test/checker-scan-index-suite.f
\
\ WHAT IS UNDER TEST. Four lookups stopped walking their store and started
\ asking an index (dot habu-the-checker-s-8c4e7273):
\
\   SCAN-USIGS-SYM       newest effect record for a symbol      HT-USX
\   NORET-SCAN-SYM       newest control-flag entry for a symbol HT-NRX
\   SUMV-FROM-CTOR-SYM   lowest variant for a constructor sym   HT-SVX
\   TFAM-FIND-IN         family row for a (package, tail)       TFX buckets
\
\ Each store still carries the walk that defines the answer — USIG-NEWEST-LINEAR,
\ NORET-NEWEST-LINEAR, SUMV-CTOR-FIRST-LINEAR, TFAM-FIND-IN-LINEAR — and section
\ 2 differentials the index against it for EVERY symbol and EVERY family in the
\ live image. Sections 1 and 3 come first and are the ones that would notice a
\ specification and an index that are wrong together: they pin the ORDER the
\ answer depends on (redefinition, deletion, shadowing) through the ordinary
\ load path, before any index word is named.
\
\ Section 4 proves each symbol-keyed table refuses a key it has no cell for,
\ in a child process, because the refusal is a process exit. The family tail
\ index has no such refusal to test: its bucket array grows with the record
\ arena it indexes (TFX-RESIZE), so section 5 proves the grown case answers
\ instead — for the symbol tables too, by forcing the symbol table past its
\ initial capacity and differentialling again.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

\ Every definition here is a fixture helper, so they live in this file's own
\ package. The cases still run as top-level interpret lines inside it: the
\ checker's rollback and registry operations resolve only at top level, and the
\ open package is where the definitions those cases make land — which is why
\ symbols are resolved with CHECKER-FIND-ACTIVE-SYM (the current scope) rather
\ than as globals.
package SCANIDX-TEST

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;

: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

: TTRUE ( bool -- )
   if -1 else 0 then -1 T= ;

: TFALSE ( bool -- )
   if -1 else 0 then 0 T= ;

\ ---------------------------------------------------------------------------
\ whitebox shims. The stores, their indexes, and the walks that specify them
\ are checker-internal colon words; each is reached through one named boundary.
\ ---------------------------------------------------------------------------
TRUSTED: SCX-SYM-N ( -- n ) SYM-N @ ;
TRUSTED: SCX-SYM-CAP ( -- n ) SYM-CAP ;
TRUSTED: SCX-ACTIVE-SYM ( ptr u8 n -- n ) CHECKER-FIND-ACTIVE-SYM ;
TRUSTED: SCX-SIG-MIN-IN ( ptr u8 n -- n ) SIG-MIN-IN ;
TRUSTED: SCX-CTL-FLAGS ( ptr u8 n -- n ) CTL-FLAGS ;

TRUSTED: SCX-USIG-NEWEST ( n -- n ) USIG-NEWEST ;
TRUSTED: SCX-USIG-NEWEST-LINEAR ( n -- n ) USIG-NEWEST-LINEAR ;
TRUSTED: SCX-NORET-NEWEST ( n -- n ) NORET-NEWEST ;
TRUSTED: SCX-NORET-NEWEST-LINEAR ( n -- n ) NORET-NEWEST-LINEAR ;

TRUSTED: SCX-SVX@ ( n -- n ) SVX-ENSURE SVX@ ;
TRUSTED: SCX-SUMV-CTOR-FIRST-LINEAR ( n -- n ) SUMV-CTOR-FIRST-LINEAR ;
TRUSTED: SCX-SUMV-FROM-CTOR ( n -- n bool ) SUMV-FROM-CTOR-SYM ;

TRUSTED: SCX-TFAM-N ( -- n ) TFAM-N@ ;
TRUSTED: SCX-TFAM-PKG$ ( n -- ptr u8 n ) TFAM-PKG$ ;
TRUSTED: SCX-TFAM-NAME$ ( n -- ptr u8 n ) TFAM-NAME$ ;
TRUSTED: SCX-TFAM-FIND-IN ( ptr u8 n ptr u8 n -- n bool ) TFAM-FIND-IN ;
TRUSTED: SCX-TFAM-FIND-IN-LINEAR ( ptr u8 n ptr u8 n -- n bool ) TFAM-FIND-IN-LINEAR ;
TRUSTED: SCX-TFAM-FIND-PUBLIC ( ptr u8 n -- n bool ) TFAM-FIND-PUBLIC ;
TRUSTED: SCX-TFAM-DECL ( ptr u8 n n ptr u8 n n n -- n ) TFAM-DECL ;
TRUSTED: SCX-TFX-SLOTS ( -- n ) TFX-SLOTS ;
TRUSTED: SCX-TFX-SLOTS-INIT ( -- n ) TFX-SLOTS-INIT ;

\ The span cells are pinned to what they hold, so the store is checked rather
\ than asserted (test/typed-storage-structural-test.f §2). SCX-SYM-INTERN stays
\ trusted for SYM-INTERN, not for these cells.
TYPED-VARIABLE SCX-NA ptr u8
TYPED-VARIABLE SCX-NU n
: SCX-NAME! ( ptr u8 n -- ) SCX-NU ! SCX-NA ! ;
TRUSTED: SCX-SYM-INTERN ( -- n ) s" " SYM-GLOBAL SCX-NA @ SCX-NU @ SYM-INTERN ;

\ the two scans the dot names, read directly: SCAN-USIGS-SYM leaves its answer
\ in FEP/FMEND and NORET-SCAN-SYM in NORET-FLAG, and the memoizing entry points
\ above them can hide a wrong answer behind a cache hit.
TRUSTED: SCX-SCAN-USIG ( n -- ) SCAN-USIGS-SYM ;
TRUSTED: SCX-FEP-HIT? ( -- bool ) FEP-HIT? ;
TRUSTED: SCX-FEP-MINI ( -- n ) FEP @ ER.MINI @ ;
TRUSTED: SCX-FMEND ( -- n ) FMEND @ ;
TRUSTED: SCX-SCAN-NORET ( n -- ) NORET-SCAN-SYM ;
TRUSTED: SCX-NORET-FLAG ( -- n ) NORET-FLAG @ ;

\ Each index carries the store end it was last made exact at. A rollback that
\ repaired the index in place leaves that mark at or below the store's new end;
\ a rollback that did NOT leaves it above, and the next lookup is forced to
\ rebuild the whole index from the store. Both answer correctly, so the mark is
\ the only thing that tells them apart — and it is the whole point of the seam.
TRUSTED: SCX-UEND ( -- n ) UEND @ ;
TRUSTED: SCX-USX-HI ( -- n ) USX-HI @ ;
TRUSTED: SCX-NORET-END ( -- n ) NORET-END @ ;
TRUSTED: SCX-NRX-HI ( -- n ) NRX-HI @ ;
TRUSTED: SCX-SUMV-N ( -- n ) SUMV-N@ ;
TRUSTED: SCX-SVX-HI ( -- n ) SVX-HI @ ;
TRUSTED: SCX-TFX-HI ( -- n ) TFX-HI @ ;

: SCX-MARKS-EXACT ( -- )
   SCX-USX-HI SCX-UEND > TFALSE
   SCX-NRX-HI SCX-NORET-END > TFALSE
   SCX-SVX-HI SCX-SUMV-N > TFALSE
   SCX-TFX-HI SCX-TFAM-N > TFALSE ;

TRUSTED: SCX-CAND-START ( -- ) CHECK-CANDIDATE-START ;
TRUSTED: SCX-CAND-DONE ( n -- n ) CHECK-CANDIDATE-DONE ;
TRUSTED: SCX-USIG-ADD ( ptr u8 n ptr u8 n -- ) CHECKER-USIG-ADD ;
TRUSTED: SCX-SUMV-ADD ( n ptr u8 n n n n n -- n ) SUMV-ADD ;
TRUSTED: SCX-SUMV-CTOR-SYM! ( n n -- ) SUMV-CTOR-SYM! ;

variable TC                    \ last caught throw code
variable NMIS                  \ differential mismatches in the current section
variable IX

\ ---------------------------------------------------------------------------
\ 1. ORDER PINNING. Every one of these answers depends on WHICH record of a
\    symbol wins. They run through the ordinary load path — a definition, an
\    `undefine`, a redefinition — and read the answer back through the entry
\    points the checker itself uses, naming no index.
\ ---------------------------------------------------------------------------

\ 1a. effect records: newest wins, and a deletion is an absence rather than a
\     fall-back to the record it shadows.
TRUSTED: SCX-DEF1 ( -- ) s" : SCXA ( n -- n ) ;" evaluate ;
TRUSTED: SCX-UNDEF ( -- ) s" undefine SCXA" evaluate ;
TRUSTED: SCX-DEF2 ( -- ) s" : SCXA ( n n -- n ) drop ;" evaluate ;
TRUSTED: SCX-DEF3 ( -- ) s" undefine SCXA : SCXA ( n n n -- n ) drop drop ;" evaluate ;
TRUSTED: SCX-DEF4 ( -- ) s" : SCXA ( n n n -- n ) drop drop ;" evaluate ;

s" SCXA" SCX-SIG-MIN-IN -1 T=                  \ nothing recorded yet
' SCX-DEF1 catch TC !   TC @ 0 T=
s" SCXA" SCX-SIG-MIN-IN 1 T=                   \ the first record answers
' SCX-UNDEF catch TC !  TC @ 0 T=
s" SCXA" SCX-SIG-MIN-IN -1 T=                  \ the deletion shadows it, not the reverse
' SCX-DEF2 catch TC !   TC @ 0 T=
s" SCXA" SCX-SIG-MIN-IN 2 T=                   \ the newest record answers again
' SCX-DEF3 catch TC !   TC @ 0 T=
s" SCXA" SCX-SIG-MIN-IN 3 T=                   \ four records deep, still the newest

\ the same symbol, read straight off the index, off the walk that specifies it,
\ and off the scan that consumes it — the entry point above caches, so a wrong
\ scan can hide behind a hit.
s" SCXA" SCX-ACTIVE-SYM IX !
IX @ 0 <> TTRUE
IX @ SCX-USIG-NEWEST 0 <> TTRUE                \ the differential below is not vacuous
IX @ SCX-USIG-NEWEST  IX @ SCX-USIG-NEWEST-LINEAR T=
IX @ SCX-SCAN-USIG
SCX-FEP-HIT? TTRUE
SCX-FEP-MINI 3 T=                              \ the scan reports the newest record's arity
SCX-FMEND 0 <> TTRUE

\ ... and with the newest record a DELETION, the scan reports no record at all
\ rather than the live one it shadows.
' SCX-UNDEF catch TC !  TC @ 0 T=
IX @ SCX-SCAN-USIG
SCX-FEP-HIT? TFALSE
SCX-FMEND 0 <> TTRUE                           \ the deletion is still a record it depends on
s" SCXA" SCX-SIG-MIN-IN -1 T=
' SCX-DEF4 catch TC !   TC @ 0 T=
IX @ SCX-SCAN-USIG
SCX-FEP-HIT? TTRUE
SCX-FEP-MINI 3 T=

\ 1b. control flags: later wins, and a redefinition clears the stale metadata.
TRUSTED: SCX-CTLDEF ( -- ) s" : SCXT ( n -- n ) 7101 throw ;" evaluate ;
TRUSTED: SCX-CTLREDEF ( -- ) s" undefine SCXT : SCXT ( n -- n ) ;" evaluate ;

s" SCXT" SCX-CTL-FLAGS 0 T=
' SCX-CTLDEF catch TC !   TC @ 0 T=
s" SCXT" SCX-CTL-FLAGS CTL-THROW and CTL-THROW T=    \ the throw edge is recorded
' SCX-CTLREDEF catch TC !  TC @ 0 T=
s" SCXT" SCX-CTL-FLAGS CTL-THROW and 0 T=            \ ... and the redefinition clears it

s" SCXT" SCX-ACTIVE-SYM IX !
IX @ SCX-NORET-NEWEST 0 <> TTRUE               \ the differential below is not vacuous
IX @ SCX-NORET-NEWEST  IX @ SCX-NORET-NEWEST-LINEAR T=
IX @ SCX-SCAN-NORET
SCX-NORET-FLAG CTL-THROW and 0 T=              \ the scan reports the LATEST entry's flags

\ 1e. an entry the store cannot key. CHECKER-RECORD-SYM answers 0 for a token it
\     cannot resolve, and a NORETS entry keyed 0 is indistinguishable from the
\     store's own terminator — it would hide every entry appended after it from
\     every reader. Nothing is recorded for it instead.
TRUSTED: SCX-RECSYM ( ptr u8 n -- n ) CHECKER-RECORD-SYM ;
TRUSTED: SCX-CTLADD-BAD ( -- ) s" a:b:c" CTL-DEAD NORET-ADD ;
TRUSTED: SCX-CTLADD-DEAD ( -- ) s" SCXT" CTL-DEAD NORET-ADD ;
TRUSTED: SCX-CTLADD-CLEAR ( -- ) s" SCXT" 0 NORET-ADD ;

s" a:b:c" SCX-RECSYM 0 T=                            \ the token really is unkeyable
' SCX-CTLADD-BAD catch TC !  TC @ 0 T=
' SCX-CTLADD-DEAD catch TC !  TC @ 0 T=              \ an entry appended AFTER it
s" SCXT" SCX-CTL-FLAGS CTL-DEAD and CTL-DEAD T=      \ ... is still visible
s" SCXT" SCX-ACTIVE-SYM IX !
IX @ SCX-NORET-NEWEST  IX @ SCX-NORET-NEWEST-LINEAR T=
' SCX-CTLADD-CLEAR catch TC !  TC @ 0 T=
s" SCXT" SCX-CTL-FLAGS CTL-DEAD and 0 T=

\ 1c. family rows: a package row and a global row may share a tail, and each
\     exact (package, tail) resolves to its own row.
s" " CHECKER-PACKAGE-PUBLIC s" scxfam" 0 TK-CELL SCX-TFAM-DECL IX !
s" scxpk" CHECKER-PACKAGE-PUBLIC s" scxfam" 0 TK-CELL SCX-TFAM-DECL
IX @ <> TTRUE                                        \ two distinct rows, one tail
s" " s" scxfam" SCX-TFAM-FIND-IN TTRUE IX @ T=
s" scxpk" s" scxfam" SCX-TFAM-FIND-IN TTRUE IX @ <> TTRUE
s" scxpk" s" nosuchtail" SCX-TFAM-FIND-IN TFALSE drop
\ the global row is lexical and never enters the public fallback set, so the
\ package row is the sole public answer for this tail.
s" scxfam" SCX-TFAM-FIND-PUBLIC TTRUE IX @ <> TTRUE

\ a second package exporting the same tail makes the unqualified answer
\ genuinely ambiguous, and the index must reproduce the refusal, not pick one.
s" scxpk2" CHECKER-PACKAGE-PUBLIC s" scxfam" 0 TK-CELL SCX-TFAM-DECL drop
s" scxfam" ' SCX-TFAM-FIND-PUBLIC catch TC ! 2drop
TC @ E-TFAM-AMBIG T=

\ 1d. constructor symbols: a generated constructor resolves to its variant, and
\     an ordinary word symbol resolves to nothing.
TRUSTED: SCX-SUMDECL ( -- )
   s" SUMTYPE scxsum 0 VARIANT scxva n ;VARIANT VARIANT scxvb ;VARIANT ;SUMTYPE" evaluate ;
' SCX-SUMDECL catch TC !  TC @ 0 T=
s" SCXA" SCX-ACTIVE-SYM SCX-SUMV-FROM-CTOR TFALSE drop

\ ---------------------------------------------------------------------------
\ 2. DIFFERENTIAL. For every symbol the image has interned and every family it
\    has declared, the index and the walk that specifies it agree. One assertion
\    per store: the number of disagreements is zero.
\ ---------------------------------------------------------------------------
: SCX-DIFF-USIG ( -- n )
   0 NMIS !
   1 IX !
   BEGIN IX @ SCX-SYM-N < WHILE
      IX @ SCX-USIG-NEWEST  IX @ SCX-USIG-NEWEST-LINEAR <> IF 1 NMIS +! THEN
      IX @ 1 + IX !
   REPEAT
   NMIS @ ;

: SCX-DIFF-NORET ( -- n )
   0 NMIS !
   1 IX !
   BEGIN IX @ SCX-SYM-N < WHILE
      IX @ SCX-NORET-NEWEST  IX @ SCX-NORET-NEWEST-LINEAR <> IF 1 NMIS +! THEN
      IX @ 1 + IX !
   REPEAT
   NMIS @ ;

: SCX-DIFF-SUMV ( -- n )
   0 NMIS !
   1 IX !
   BEGIN IX @ SCX-SYM-N < WHILE
      IX @ SCX-SVX@  IX @ SCX-SUMV-CTOR-FIRST-LINEAR <> IF 1 NMIS +! THEN
      IX @ 1 + IX !
   REPEAT
   NMIS @ ;

\ Each family is looked up by its own (package, tail), which is the pair the
\ registry guarantees unique, so the indexed and walked answers must be the
\ same row id and the same found flag.
: SCX-DIFF-TFAM ( -- n )
   0 NMIS !
   0 IX !
   BEGIN IX @ SCX-TFAM-N < WHILE
      IX @ SCX-TFAM-PKG$ IX @ SCX-TFAM-NAME$ SCX-TFAM-FIND-IN {: gid:n gf:bool :}
      IX @ SCX-TFAM-PKG$ IX @ SCX-TFAM-NAME$ SCX-TFAM-FIND-IN-LINEAR {: wid:n wf:bool :}
      gid wid <> IF 1 NMIS +! THEN
      gf IF wf 0= IF 1 NMIS +! THEN ELSE wf IF 1 NMIS +! THEN THEN
      IX @ 1 + IX !
   REPEAT
   NMIS @ ;

: SCX-DIFF-ALL ( -- )
   SCX-DIFF-USIG 0 T=
   SCX-DIFF-NORET 0 T=
   SCX-DIFF-SUMV 0 T=
   SCX-DIFF-TFAM 0 T= ;

SCX-TFAM-N 0 > TTRUE                           \ the family differential is not vacuous
SCX-SYM-N 1 > TTRUE                            \ nor the symbol ones
SCX-DIFF-ALL

\ ---------------------------------------------------------------------------
\ 3. ROLLBACK. The checker's own rollback frames retire store records; the
\    index has to retire with them, and a name redefined AFTER a rollback must
\    answer with its new effect and not with the rolled-back one.
\ ---------------------------------------------------------------------------

\ 3a. a rejected candidate frame: a signature added inside it is visible there
\     and gone after, and the same name then takes a DIFFERENT effect cleanly.
s" SCXR" SCX-SIG-MIN-IN -1 T=
SCX-CAND-START
   s" n n -- n" s" SCXR" SCX-USIG-ADD
   s" SCXR" SCX-SIG-MIN-IN 2 T=                      \ visible inside the candidate
0 SCX-CAND-DONE drop
SCX-MARKS-EXACT                                      \ read FIRST: a lookup would rebuild
s" SCXR" SCX-SIG-MIN-IN -1 T=                        \ retired with the frame
s" n -- n" s" SCXR" SCX-USIG-ADD
s" SCXR" SCX-SIG-MIN-IN 1 T=                         \ the new effect answers
SCX-DIFF-ALL

\ 3d. a record added inside a rejected frame for a symbol that EXISTED before it.
\     The symbol survives the rollback, so its head cannot simply be dropped —
\     it has to revert to the record the frame did not touch.
s" SCXA" SCX-SIG-MIN-IN 3 T=
SCX-CAND-START
   s" n n n n -- n" s" SCXA" SCX-USIG-ADD            \ TWO records for the one symbol, so
   s" n n n n n -- n" s" SCXA" SCX-USIG-ADD          \ the repair has to reach past both
   s" SCXA" SCX-SIG-MIN-IN 5 T=
0 SCX-CAND-DONE drop
SCX-MARKS-EXACT                                      \ read FIRST: a lookup would rebuild
s" SCXA" SCX-ACTIVE-SYM IX !
IX @ SCX-USIG-NEWEST  IX @ SCX-USIG-NEWEST-LINEAR T=
IX @ SCX-SCAN-USIG
SCX-FEP-HIT? TTRUE
SCX-FEP-MINI 3 T=                                    \ the record below the frame answers
s" SCXA" SCX-SIG-MIN-IN 3 T=
SCX-DIFF-ALL

\ 3e. the same for the control store: a flag entry added for an existing symbol
\     inside a rejected frame reverts to the entry below it, in place.
s" SCXT" SCX-CTL-FLAGS CTL-DEAD and 0 T=
SCX-CAND-START
   ' SCX-CTLADD-DEAD catch TC !  TC @ 0 T=
   ' SCX-CTLADD-DEAD catch TC !  TC @ 0 T=           \ two entries, one symbol
   s" SCXT" SCX-CTL-FLAGS CTL-DEAD and CTL-DEAD T=
0 SCX-CAND-DONE drop
SCX-MARKS-EXACT                                      \ read FIRST: a lookup would rebuild
s" SCXT" SCX-ACTIVE-SYM IX !
IX @ SCX-SCAN-NORET
SCX-NORET-FLAG CTL-DEAD and 0 T=                     \ the entry below the frame answers
s" SCXT" SCX-CTL-FLAGS CTL-DEAD and 0 T=
SCX-DIFF-ALL

\ 3b. the same shape through the real load path: a definition the checker
\     REJECTS rolls its scope back, and the name then takes a different effect
\     that its callers are held to.
TRUSTED: SCX-BADDEF ( -- ) s" : SCXB ( n -- n ) drop ;" evaluate ;
TRUSTED: SCX-GOODDEF ( -- ) s" : SCXB ( n -- ) drop ;" evaluate ;
TRUSTED: SCX-GOODUSE ( -- ) s" : SCXBU ( n -- ) SCXB ;" evaluate ;
TRUSTED: SCX-BADUSE ( -- ) s" : SCXBU2 ( n -- n ) SCXB ;" evaluate ;

' SCX-BADDEF catch TC !   TC @ 0 <> TTRUE            \ rejected: the body drops its output
s" SCXB" SCX-SIG-MIN-IN -1 T=                        \ ... and left no record behind
' SCX-GOODDEF catch TC !  TC @ 0 T=
s" SCXB" SCX-SIG-MIN-IN 1 T=
' SCX-GOODUSE catch TC !  TC @ 0 T=                  \ a caller certifies against the new effect
' SCX-BADUSE catch TC !   TC @ 0 <> TTRUE            \ ... and against nothing else
SCX-DIFF-ALL

\ 3c. a family declared inside a rejected candidate leaves no row and no chain,
\     and the same (package, tail) can then be declared with a different kind.
s" scxctor" SCX-NAME! SCX-SYM-INTERN IX !
IX @ SCX-SUMV-FROM-CTOR TFALSE drop
s" scxrb" s" cand" SCX-TFAM-FIND-IN TFALSE drop
SCX-CAND-START
   s" scxrb" CHECKER-PACKAGE-PRIVATE s" cand" 2 TK-PRODUCT SCX-TFAM-DECL drop
   s" scxrb" CHECKER-PACKAGE-PRIVATE s" cand2" 2 TK-PRODUCT SCX-TFAM-DECL drop
   s" scxrb" s" cand" SCX-TFAM-FIND-IN TTRUE drop
   s" scxrb" s" cand" 0 0 0 0 SCX-SUMV-ADD IX @ SCX-SUMV-CTOR-SYM!
   IX @ SCX-SUMV-FROM-CTOR TTRUE drop
0 SCX-CAND-DONE drop
SCX-MARKS-EXACT                                      \ read FIRST: a lookup would rebuild
s" scxrb" s" cand" SCX-TFAM-FIND-IN TFALSE drop
s" scxrb" s" cand2" SCX-TFAM-FIND-IN TFALSE drop
IX @ SCX-SUMV-FROM-CTOR TFALSE drop
s" scxrb" CHECKER-PACKAGE-PUBLIC s" cand" 0 TK-CELL ' SCX-TFAM-DECL catch TC ! drop
TC @ 0 T=
s" scxrb" s" cand" SCX-TFAM-FIND-IN TTRUE drop
SCX-MARKS-EXACT
SCX-DIFF-ALL

\ ---------------------------------------------------------------------------
\ 4. CAPACITY REFUSAL, one case per symbol-keyed table. The mapping has exactly
\    SYM-CAP cells per table, so a key at or above that cap — or below the
\    first real symbol id — has no cell and the store and the symbol table have
\    disagreed. Each table refuses through its own linking word. The refusal is
\    a process exit, so each case is a child.
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

s" TRUSTED: SCXCAP ( -- ) 0 SYM-CAP USX-LINK ; SCXCAP" SCX-REFUSED
s" TRUSTED: SCXCAP ( -- ) 0 SYM-CAP NRX-LINK ; SCXCAP" SCX-REFUSED
s" TRUSTED: SCXCAP ( -- ) 0 SYM-CAP SVX-LINK ; SCXCAP" SCX-REFUSED
\ id 0 is the symbol table's own "no symbol", not a key: the control store has
\ no early return for it, so its linking word is where that shows.
s" TRUSTED: SCXCAP ( -- ) 0 0 NRX-LINK ; SCXCAP" SCX-REFUSED

\ and a key one below the cap is inside the mapping, so it does NOT refuse
s" TRUSTED: SCXOK ( -- ) 0 SYM-CAP 1 - NRX-LINK ; SCXOK" SCX-CHILD
SCX-RC @ 0 T=

\ ---------------------------------------------------------------------------
\ 5. GROWTH. Both index families are sized from the store they key, so both
\    have to survive that store outgrowing its initial capacity: the mapping is
\    re-laid-out at the new SYM-CAP and every table in it rebuilds, and the
\    family tail buckets are resized and rehashed.
\ ---------------------------------------------------------------------------

\ the boot image already declares far more than TFX-SLOTS-INIT families, so the
\ bucket array has already been resized and rehashed at least once
SCX-TFX-SLOTS SCX-TFX-SLOTS-INIT > TTRUE
SCX-DIFF-TFAM 0 T=

\ force the symbol table past its current capacity, which drops the mapping and
\ rebuilds every table in it at the new cap.
10 constant SCX-RADIX
48 constant SCX-ZERO

: SCX-NAME$ ( n -- ptr u8 n ) {: n:n :}
   SB-RESET
   s" scxsym" SB-APPEND
   1000000
   BEGIN dup 0 > WHILE
      n over / SCX-RADIX mod SCX-ZERO + SB-APPEND-C
      SCX-RADIX /
   REPEAT drop
   SB$ ;

: SCX-FILL-SYMS ( n -- ) {: target:n :}
   0 IX !
   BEGIN SCX-SYM-N target < WHILE
      IX @ SCX-NAME$ SCX-NAME! SCX-SYM-INTERN drop
      IX @ 1 + IX !
   REPEAT ;

SCX-SYM-CAP IX !
IX @ 1 + SCX-FILL-SYMS
SCX-SYM-CAP IX @ > TTRUE                       \ the symbol table really did grow
SCX-DIFF-ALL                                   \ ... and every index answers at the new cap

\ the answers pinned in section 1 survive the rebuild
s" SCXA" SCX-SIG-MIN-IN 3 T=
s" SCXT" SCX-CTL-FLAGS CTL-THROW and 0 T=
s" scxpk" s" scxfam" SCX-TFAM-FIND-IN TTRUE drop

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" checker-scan-index-suite: failures" 1 die ;
REPORT

;package
