\ native-tape-owner.f - which unit owns the checker's source-tape observer.
\
\ The observer is three quotations the checker fires while it scans: the text, each
\ token, and the verdict. TWO compilers legitimately reach one live checker - the
\ engine's baked front end, and the one a window loads from source while that
\ engine is still hosting the build - and each arms the tape with its own trio. So
\ CHECKER-TAPE:INSTALL is last-writer-wins, and the installer names itself so the
\ takeover is a stated fact rather than a silent overwrite.
\
\ THE THREE CLAIMS. Installing twice is legal and the SECOND trio is the live one;
\ INSTALLED-BY names whichever unit installed it; and the engine's own observer can
\ be put back, which is what makes this suite safe to run inside a live engine at
\ all. The last one is not politeness: the tape here is the checker the whole
\ process compiles through, so a suite that took it and kept it would leave the
\ session compiling into a dead tape.
\
\ WHY LAST-WRITER-WINS AND NOT A ONE-SHOT GRANT, which is what the certificate
\ producer beside it uses: the observer is called BEFORE a token is judged and its
\ answer is never read, so it can abort a compilation and can never accept one.
\ Refusing the second installer would die on a correct sequence - and a seeded
\ engine, whose captured dispatch cells arrive trapped, needs the first install of
\ the session to refill them.
\
\ Run: bin/hb --load test/compiler/native-tape-owner.f
\
\ Tier 1 first: the observer the claims below read is armed by the optimizing
\ compiler, so INSTALLED-BY is a tier-1 fact (1 row fails at the default tier).
1 set-tier

require lib/test.f
require lib/errors.f
require src/compiler/native/feed.f

package TAPE-OWNER-TEST

\ Two units, named by numbers nothing here interprets - exactly what the cell
\ holds. They are deliberately not execution tokens: the identity is the
\ installer's own statement, and a test that could only spell it as an xt could
\ not tell a takeover from a reinstall by the same unit.
$A1 constant UNIT-A
$B2 constant UNIT-B

variable A-SCANS  variable A-TOKENS  variable A-DONES
variable B-SCANS

: A-SCAN ( ptr u8 n -- )
   2drop  1 A-SCANS +! ;

: A-TOKEN ( ptr u8 n n n n n -- )
   2drop 2drop 2drop  1 A-TOKENS +! ;

: A-DONE ( ptr u8 n n -- )
   2drop drop  1 A-DONES +! ;

: B-SCAN ( ptr u8 n -- )
   2drop  1 B-SCANS +! ;

: INSTALL-A ( -- )
   UNIT-A [: A-SCAN ;] [: A-TOKEN ;] [: A-DONE ;] CHECKER-TAPE:INSTALL ;

: INSTALL-B ( -- )
   UNIT-B [: B-SCAN ;] [: A-TOKEN ;] [: A-DONE ;] CHECKER-TAPE:INSTALL ;

\ ---- the cases ---------------------------------------------------------------
\ Read BEFORE anything is installed: the live observer is the engine's own front
\ end, which names its own producer instance. Asserting that first is what makes
\ the restore at the end an assertion rather than a hope.
: TO-ENGINE-OWNS-IT ( -- )
   s" the engine's own front end owns the observer" T-LABEL
   CHECKER-TAPE:INSTALLED-BY NFEED:SCAN-ID T= ;

: TO-TWO-UNITS ( -- )
   s" a second install is accepted, not refused" T-LABEL
   INSTALL-A
   CHECKER-TAPE:INSTALLED-BY UNIT-A T=
   INSTALL-B
   CHECKER-TAPE:INSTALLED-BY UNIT-B T=

   s" reinstalling the first unit hands it back" T-LABEL
   INSTALL-A
   CHECKER-TAPE:INSTALLED-BY UNIT-A T= ;

\ The trio that is live is the LAST one installed, not the first: A's counter must
\ stay put while B's moves. Without this the identity above could be a label on a
\ cell nobody dispatches through.
: TO-LAST-WRITER-RUNS ( -- )
   s" the last installed trio is the one the checker fires" T-LABEL
   0 A-SCANS !  0 B-SCANS !
   INSTALL-A
   CHECKER-TAPE:ARM
   s" : TOA ( -- n ) 1 ;" CHECK! drop
   CHECKER-TAPE:DISARM
   A-SCANS @ 1 T=
   B-SCANS @ 0 T=

   0 A-SCANS !  0 B-SCANS !
   INSTALL-B
   CHECKER-TAPE:ARM
   s" : TOB ( -- n ) 2 ;" CHECK! drop
   CHECKER-TAPE:DISARM
   B-SCANS @ 1 T=
   A-SCANS @ 0 T= ;

\ Every event of one scan reaches the trio, so a tape cannot be half-owned: the
\ token count is the source's own token count and the verdict arrives once.
: TO-ALL-THREE-EVENTS ( -- )
   s" one scan fires all three events" T-LABEL
   0 A-SCANS !  0 A-TOKENS !  0 A-DONES !
   INSTALL-A
   CHECKER-TAPE:ARM
   s" : TOC ( n -- n ) 1+ ;" CHECK! drop
   CHECKER-TAPE:DISARM
   A-SCANS @ 1 T=
   A-DONES @ 1 T=
   A-TOKENS @ 0 > TTRUE ;

\ A DONE observer may inspect a candidate while CHECK! still owns its outer
\ result. The candidate's independent analysis must restore recovery taint on
\ return; it must not turn a diagnostic-only outer row into a source grant.
: TO-MULTI+ ( -- ) MULTI-ERR-BEGIN ;
: TO-MULTI- ( -- n ) MULTI-ERR-END ;
TRUSTED: TO-RECOVERY? ( -- bool ) CHECKER-EFFECT-AUTHORITY:RECOVERY-USED? ;
TRUSTED: TO-SOURCE-MIN ( ptr u8 n -- n ) EFFECT-EXTERNAL-MIN-IN ;
variable NESTED-DONES

: NESTED-DONE ( ptr u8 n n -- )
   2drop drop 1 NESTED-DONES +!
   TO-RECOVERY? TTRUE
   s" TO-NESTED-INNER ( n -- n )" CHECK-CANDIDATE! -1 T=
   TO-RECOVERY? TTRUE ;

: TO-NESTED-RECOVERY ( -- )
   s" a nested candidate restores the enclosing recovery analysis" T-LABEL
   TO-MULTI+
   s" TO-RECOVERY-BAD ( n -- n ) drop" CHECK! 0 T=
   0 NESTED-DONES !
   UNIT-A [: A-SCAN ;] [: A-TOKEN ;] [: NESTED-DONE ;] CHECKER-TAPE:INSTALL
   CHECKER-TAPE:ARM
   s" TO-RECOVERY-OUTER ( n -- n ) TO-RECOVERY-BAD" CHECK! -1 T=
   CHECKER-TAPE:DISARM
   NESTED-DONES @ 1 T=
   s" TO-RECOVERY-OUTER" TO-SOURCE-MIN -1 T=
   TO-MULTI- 1 T=
   s" TO-NESTED-LATER ( n -- n )" CHECK! -1 T=
   TO-RECOVERY? TFALSE
   s" TO-NESTED-LATER" TO-SOURCE-MIN 1 T= ;

\ Put the engine's own observer back, and prove it is back by its identity. This
\ runs last and is the reason the session survives the suite.
: TO-RESTORE-ENGINE ( -- )
   s" the engine's observer can be put back" T-LABEL
   NFEED:OBSERVE
   CHECKER-TAPE:INSTALLED-BY NFEED:SCAN-ID T= ;

: MAIN ( -- )
   T-RESET
   TO-ENGINE-OWNS-IT
   TO-TWO-UNITS
   TO-LAST-WRITER-RUNS
   TO-ALL-THREE-EVENTS
   TO-NESTED-RECOVERY
   TO-RESTORE-ENGINE
   T-REPORT
   s" native-tape-owner: ok" type cr ;

MAIN

;package
