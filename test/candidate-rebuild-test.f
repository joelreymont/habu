\ candidate-rebuild-test.f - real scheduler candidate-source regression.
\
\ Run: bin/hb --load test/candidate-rebuild-test.f -- rebuild; bin/hb --load test/candidate-rebuild-test.f -- import

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/runner.f
require test/gate-stats.f

package CANDIDATE-REBUILD-TEST

using GATE

$10000 constant CAP
300000 constant TIMEOUT-MS
64 constant HEX-U
64 constant USAGE-RC

create ROOT FS-PATH-CAP allot
create XDG FS-PATH-CAP allot
create RUN1 FS-PATH-CAP allot
create RUN2 FS-PATH-CAP allot
create UNDER FS-PATH-CAP allot
create STAT FS-PATH-CAP allot
create SRC-EXE FS-PATH-CAP allot
create UNDER-EXE FS-PATH-CAP allot
create OUT CAP allot
create ERR CAP allot
create SRC-HEX HEX-U allot
create UNDER-HEX HEX-U allot

variable ROOT-U
variable XDG-U
variable RUN1-U
variable RUN2-U
variable UNDER-U
variable STAT-U
variable SRC-EXE-U
variable UNDER-EXE-U
variable ARG-N
variable OUT-I
variable OUT-START

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: XDG$ ( -- ptr u8 n ) XDG XDG-U @ ;
: RUN1$ ( -- ptr u8 n ) RUN1 RUN1-U @ ;
: RUN2$ ( -- ptr u8 n ) RUN2 RUN2-U @ ;
: UNDER$ ( -- ptr u8 n ) UNDER UNDER-U @ ;
: STAT$ ( -- ptr u8 n ) STAT STAT-U @ ;
: SRC-EXE$ ( -- ptr u8 n ) SRC-EXE SRC-EXE-U @ ;
: UNDER-EXE$ ( -- ptr u8 n ) UNDER-EXE UNDER-EXE-U @ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" candidate-rebuild-test" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY
   u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" xdg" XDG JOIN-PATH XDG-U !
   ROOT$ s" run1" RUN1 JOIN-PATH RUN1-U !
   ROOT$ s" run2" RUN2 JOIN-PATH RUN2-U !
   ROOT$ s" under" UNDER JOIN-PATH UNDER-U !
   XDG$ MAKE-DIRS
   RUN1$ MAKE-DIRS
   RUN2$ MAKE-DIRS
   UNDER$ MAKE-DIRS
   UNDER$ s" hb-under-test" UNDER-EXE JOIN-PATH UNDER-EXE-U ! ;

: ARGS ( n -- ) {: imported:n :}
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/run.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   imported 0 <> if
      s" --under" >LEN PROC-ARGV+
      SRC-EXE$ >LEN PROC-ARGV+
   then
   PROC-ARGV-N @ COUNT>N ARG-N ! ;

: HB$ ( -- ptr u8 n )
   s" bin/hb" ;

: ARG$ ( n -- ptr u8 n )
   1+ >IDX PROC-ARGV-SLOT @ dup ZLEN ;

: ARGV. ( -- )
   s" argv:" type cr
   0 begin dup ARG-N @ < while
      s"   " type dup ARG$ type cr
      1+
   repeat drop ;

: FAIL ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" FAIL: " type label labelu type cr
   s" executable: " type HB$ type cr
   ARGV.
   s" outcome exited: " type GT-EXITED @ .
   s" outcome timed out: " type GT-TIMED-OUT @ .
   s" outcome code: " type GT-CODE @ .
   s" stdout bytes: " type GT-OUT-U @ GT-U-TYPE s"  / " type CAP GT-U-TYPE cr
   s" stderr bytes: " type GT-ERR-U @ GT-U-TYPE s"  / " type CAP GT-U-TYPE cr
   s" stdout:" type cr
   OUT GT-OUT-U @ type cr
   s" stderr:" type cr
   ERR GT-ERR-U @ type cr
   s" candidate gate failed" 1 die ;

: LABEL$ ( n -- ptr u8 n )
   0 <> if s" imported gate" exit then
   s" ordinary gate" ;

: RUN-GATE ( ptr u8 n n -- ) {: root:ptr rootu:n imported:n :}
   imported ARGS
   s" XDG_CACHE_HOME" >LEN XDG$ >LEN PROC-ENV+
   s" HB_TMP" >LEN root rootu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   HB$ >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME GT-STORE-RUN
   GT-RC@ 0 <> if imported LABEL$ FAIL then ;

: SCAN ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu s" gate-stats.tsv" STAT JOIN-PATH STAT-U !
   STAT$ GS-COPY-PATH!
   GS-READ
   GS-SCAN ;

: BUILD-ROW? ( -- bool )
   0 begin dup GS-TROW-N @ < while
      dup GS-TROW-LABEL$ GS-UNQUAL$ s" native engine build slice" STR= if
         drop 0 0= exit
      then
      1+
   repeat drop 0 0= 0= ;

: OUT-LINE? ( ptr u8 n -- bool ) {: want:ptr wantu:n :}
   0 OUT-I !
   0 OUT-START !
   begin OUT-I @ GT-OUT-U @ < while
      OUT OUT-I @ + c@ STR-LF = if
         OUT OUT-START @ + OUT-I @ OUT-START @ - want wantu STR= if
            0 0= exit
         then
         OUT-I @ 1 + OUT-START !
      then
      OUT-I @ 1 + OUT-I !
   repeat
   OUT-START @ GT-OUT-U @ < if
      OUT OUT-START @ + GT-OUT-U @ OUT-START @ - want wantu STR= exit
   then
   0 0= 0= ;

: ORDINARY ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu 0 RUN-GATE
   s" candidate=1" OUT-LINE? TTRUE
   s" candidate-validate=1" OUT-LINE? TTRUE
   root rootu SCAN
   s" ordinary candidate build event" T-LABEL GS-CANDIDATE @ 1 T=
   s" ordinary candidate not imported" T-LABEL GS-CANDIDATE-IMPORT @ 0 T=
   s" ordinary candidate ready" T-LABEL GS-CANDIDATE-READY @ 1 T=
   s" ordinary candidate validated" T-LABEL GS-CANDIDATE-VALIDATE @ 1 T=
   s" ordinary phase 15 row" T-LABEL BUILD-ROW? TTRUE
   s" ordinary engine build passed" T-LABEL
      OUT GT-OUT-U @ s" PASS: native engine build slice (" CONTAINS? TTRUE ;

: SOURCE-PREPARE ( -- )
   RUN1$ s" hb-under-test" SRC-EXE JOIN-PATH SRC-EXE-U !
   s" first run produced executable" T-LABEL SRC-EXE$ EXECUTABLE? TTRUE
   s" first run source hash" T-LABEL SRC-EXE$ SRC-HEX SHA256-FILE-HEX 0 T= ;

: IMPORT-HASH ( -- )
   SRC-EXE$ SRC-HEX SHA256-FILE-HEX 0 T=
   UNDER-EXE$ EXECUTABLE? TTRUE
   UNDER-EXE$ UNDER-HEX SHA256-FILE-HEX 0 T=
   SRC-HEX HEX-U UNDER-HEX HEX-U STR= TTRUE
   SB-RESET
   s" Habu-under-test: " SB-APPEND
   UNDER-EXE$ SB-APPEND
   s"  sha256=" SB-APPEND
   UNDER-HEX HEX-U SB-APPEND
   SB$ OUT-LINE? TTRUE ;

: IMPORTED ( -- )
   UNDER$ 1 RUN-GATE
   s" candidate=0" OUT-LINE? TTRUE
   s" candidate-import=1" OUT-LINE? TTRUE
   s" candidate-ready=1" OUT-LINE? TTRUE
   s" candidate-validate=1" OUT-LINE? TTRUE
   UNDER$ SCAN
   s" imported candidate not built" T-LABEL GS-CANDIDATE @ 0 T=
   s" imported candidate event" T-LABEL GS-CANDIDATE-IMPORT @ 1 T=
   s" imported candidate ready" T-LABEL GS-CANDIDATE-READY @ 1 T=
   s" imported candidate validated" T-LABEL GS-CANDIDATE-VALIDATE @ 1 T=
   s" imported phase 15 absent" T-LABEL BUILD-ROW? TFALSE
   s" imported engine build absent" T-LABEL
      OUT GT-OUT-U @ s" native engine build slice" CONTAINS? TFALSE
   IMPORT-HASH ;

: MAIN ( -- )
   SCRIPT-ARGC 1 <> if
      s" usage: test/candidate-rebuild-test.f -- [rebuild|import]" USAGE-RC die
   then
   0 SCRIPT-ARGV$ {: mode:ptr modeu:n :}
   mode modeu s" rebuild" STR= mode modeu s" import" STR= or 0= if
      s" usage: test/candidate-rebuild-test.f -- [rebuild|import]" USAGE-RC die
   then
   T-RESET
   SETUP
   RUN1$ ORDINARY
   SOURCE-PREPARE
   mode modeu s" rebuild" STR= if
      RUN2$ ORDINARY
      s" second run maker cache remains active" T-LABEL GS-MAKER-HIT @ 0 > TTRUE
      s" second run artifact cache remains active" T-LABEL GS-ARTIFACT-HIT @ 0 > TTRUE
   else
      IMPORTED
   then
   CLEANUP-RUN
   T-REPORT
   s" candidate-rebuild-test: ok" type cr ;

MAIN

;package
