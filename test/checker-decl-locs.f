\ checker-decl-locs.f - a declaration's location stays with the row it names.
\ It reads checker internals, so it is a WHITEBOX-SUITE row and runs on the
\ unsealed engine (test/whitebox-engine.f):
\   hb-whitebox --load test/checker-decl-locs.f
\
\ The checker keeps, beside its record store, where each named declaration was
\ written (src/core/checker.f DECL-LOCS). The verifier arms a location around a
\ named registrar call through the owner slot VERIFY-DECL-ARM-OFF, and the row
\ that registrar retains takes it. The rows a declaration GENERATES take none
\ in this commit: a constructor, an address word, an initialized field's
\ receiver, helper and accessors, a storage definer's suffix words. Each is
\ registered while the declaring registrar is armed, between NAV-PAUSE and
\ NAV-RESUME (sumtype.f TDPLAN-PREFLIGHT-DEFINITIONS, TDINIT-HELP-REPLAY and
\ TDINIT-REPLAY-ROW, checker.f CHECKER-DEFSUFFIX-NAME); without the pauses
\ every generated row below carries the armed location. The declarations
\ replay as the verifier reads them (VERIFY:SOURCE-BUF-THEN-IN-SCOPE), so each
\ row is the replay path's, and each case reads its rows by name where the
\ replay binds them: in the replay's own scope, or in a neutral checker scope
\ around several replays. A generated name with no row fails its own case, so
\ a missing row never passes as an unlocated one.
\
\ One uses scope runs at a time (VERIFY-USES-OFF): a nested one is refused by
\ name, and the refusal leaves no scope open.
\
\ A `does>` clause is walked outside CHECK! (CHECKER-SOURCE-DOES!), and that
\ entry publishes the uses its walk resolved, including later tokens when a
\ using refusal is held until the scan ends. A refused lookup has no target.
\
\ A rollback takes a located row's location with it (USIGS-RESTORE-END), so
\ the row that reuses its offset answers only for itself. This is the one
\ lifetime case a verifier child cannot show: the only frame that rewinds a
\ located row there is the child's own, at the end of its composition.

require lib/test.f
require src/habu/verify-source.f
require test/checker-decl-locs-lib.f

using TFAM

package NAVL

private

\ ---- 1. a record: constructor, address words, initialized-field rows -------
\ The checker records each generated row under the declaring package's
\ spelling (REC-MAKE, REC-X, REC-X@); NAVL-REC:MAKE binds nothing here.
: FAMILY ( ptr u8 n -- n ) TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE drop ;

: RECORD-ROWS ( -- )
   s" REC-MAKE" UNLOCATED
   s" REC-X" UNLOCATED
   s" REC-CELLS" UNLOCATED
   s" REC-X@" UNLOCATED
   s" REC-X!" UNLOCATED
   s" rec" FAMILY {: fam:n :}
   fam TFAM-INIT-RECEIVER$ UNLOCATED
   fam fam TFAM-FLD-START@ TFAM-INIT-HELPER$ UNLOCATED ;

: CASE-RECORD ( -- )
   s" rec" s" STRUCTURE rec 0 DERIVE addr init FIELD x n ;STRUCTURE"
   [: RECORD-ROWS ;] ARMED-REPLAY ;

\ ---- 2. a storage definer: its own row, then its suffix words --------------
: STORAGE-ROWS ( -- )
   s" NAVDB" LOCATED
   s" NAVDB-RESERVE" UNLOCATED
   s" NAVDB-RELEASE" UNLOCATED ;

: CASE-STORAGE ( -- )
   s" NAVDB" s" DYNAMIC-BUFFER NAVDB n" [: STORAGE-ROWS ;] ARMED-REPLAY ;

\ ---- 3. one uses scope at a time ---------------------------------------------
: NOTE-USE ( n n n n n n -- ) 2drop 2drop 2drop ;
: NO-CHECK ( -- ) ;
: INNER ( -- ) [: NOTE-USE ;] [: NO-CHECK ;] WITH-USES ;
: OUTER ( -- ) [: NOTE-USE ;] [: INNER ;] WITH-USES ;

: CASE-NESTED ( -- )
   s" a uses scope inside a running one is refused by name" T-LABEL
   [: OUTER ;] E-USES-NESTED TTHROWSQ
   s" the refusal leaves no uses scope open" T-LABEL
   [: INNER ;] 0 TTHROWSQ ;

\ ---- 4. a does> clause publishes its uses -------------------------------------
\ Only NAVT is located, so a published use is a call of it: the spelling's
\ range in the replayed text, and NAVT's armed location. The handler keeps the
\ last use and counts them, so a count of one pins the only use. The later
\ replays bind NAVT and NAVS where the earlier ones declared them, so the case
\ runs in one neutral checker scope (MAIN), as a verifier child runs its
\ composition.
variable USE-N   variable USE-S   variable USE-E   variable USE-SYM
variable USE-V   variable USE-DS   variable USE-DE

: KEEP-USE ( n n n n n n -- )
   {: s:n e:n sym:n v:n ds:n de:n :}
   1 USE-N +!
   s USE-S !  e USE-E !  sym USE-SYM !
   v USE-V !  ds USE-DS !  de USE-DE ! ;

PTR-VARIABLE REPLAY-A   variable REPLAY-U
: REPLAY ( -- ) REPLAY-A @ REPLAY-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;
: KEEP-USES ( -- ) [: KEEP-USE ;] [: REPLAY ;] WITH-USES ;

\ Replay TEXT, unarmed, in a uses scope that keeps what it publishes; the
\ replay's throw code, 0 when it ran through.
: USES-REPLAY ( ptr u8 n -- n )
   {: a:ptr u:n :}
   0 USE-N !
   a REPLAY-A !  u REPLAY-U !
   [: KEEP-USES ;] catch ;

\ The scope published one use, NAVT spelled at bytes START to END.
: ONE-USE ( n n -- )
   {: s:n e:n :}
   USE-N @ 1 T=
   USE-S @ s T=
   USE-E @ e T=
   USE-SYM @ s" NAVT" CHECKER-FIND-ACTIVE-SYM T=
   USE-V @ VISIT T=
   USE-DS @ AT-START T=
   USE-DE @ AT-END T= ;

: CASE-DOES ( -- )
   s" NAVT" s" : NAVT ( -- n ) 9 ;" [: ;] ARMED-REPLAY
   s" package NAVP public : NAVS ( -- n ) 1 ; ;package : NAVS ( -- n ) 2 ;"
   VERIFY:SOURCE-BUF-IN-SCOPE
   s" a certified clause publishes its call of NAVT" T-LABEL
   s" : NAVOK ( -- ) create does> ( -- n ) drop NAVT ;" USES-REPLAY 0 T=
   42 46 ONE-USE
   \ The using refusal is held: both NAVT calls resolve, but NAVS does not.
   s" a refused clause publishes its resolved calls" T-LABEL
   s" using NAVP : NAVBAD ( -- ) create does> ( -- n ) drop NAVT NAVS NAVT ; ;using"
   USES-REPLAY E-USING-SHADOW-GLOBAL T=
   USE-N @ 2 T=
   USE-S @ 64 T=
   USE-E @ 68 T=
   USE-V @ VISIT T=
   USE-DS @ AT-START T=
   USE-DE @ AT-END T= ;

\ ---- 5. a rollback, then the same offset -------------------------------------
\ A neutral checker scope (IN-NEUTRAL) is the rollback frame a verifier child
\ checks its whole composition in. One nested in the case's own (MAIN) takes
\ the located row back, and with it the record its name bound, so the name
\ binds nothing in the case's scope; the next row the store retains lands at
\ the same offset. That row is declared unarmed, so it has no location: a row
\ a lost truncation left in DECL-LOCS would answer for it.

variable GONE-ROW

: GONE-ROWS ( -- )
   s" NAVR-GONE" LOCATED
   s" NAVR-GONE" ROW GONE-ROW ! ;

: GONE ( -- )
   s" NAVR-GONE" s" : NAVR-GONE ( -- n ) 1 ;" [: GONE-ROWS ;] ARMED-REPLAY ;

: NEXT-ROWS ( -- )
   s" the next row takes the same offset" T-LABEL
   s" NAVR-NEXT" ROW GONE-ROW @ T=
   s" NAVR-NEXT" UNLOCATED ;

: CASE-ROLLBACK ( -- )
   [: GONE ;] IN-NEUTRAL
   s" the rewind takes the row" T-LABEL
   s" NAVR-GONE" ROW 0 T=
   s" : NAVR-NEXT ( -- n ) 2 ;" [: NEXT-ROWS ;] VERIFY:SOURCE-BUF-THEN-IN-SCOPE ;

: MAIN ( -- )
   T-RESET
   CASE-RECORD
   CASE-STORAGE
   CASE-NESTED
   [: CASE-DOES ;] IN-NEUTRAL
   [: CASE-ROLLBACK ;] IN-NEUTRAL
   T-REPORT ;

MAIN

;package

;using
