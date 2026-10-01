\ chain-run-test.f - hash comparison and refusal boundary for chain-run.

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-command.f
require lib/engine-candidate.f
require tools/chain-run.f

package CHAIN-RUN-TEST
using SOURCE-ROOT

create ROOT FS-PATH-CAP allot
variable ROOT-U
create A FS-PATH-CAP allot
variable A-U
create B FS-PATH-CAP allot
variable B-U
create HOST FS-PATH-CAP allot
variable HOST-U
create BIG-A 32769 allot
create BIG-B 32769 allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: A$ ( -- ptr u8 n ) A A-U @ ;
: B$ ( -- ptr u8 n ) B B-U @ ;
: HOST$ ( -- ptr u8 n ) HOST HOST-U @ ;

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY u up ! ;

: PREP ( -- )
   CLEANUP-RESET
   s" chain-run-test" HB-TMP-MKDIR ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" a" JOIN A A-U COPY!
   ROOT$ s" b" JOIN B B-U COPY!
   ROOT$ s" host" JOIN HOST HOST-U COPY!
   A$ s" same" WRITE-ALL
   B$ s" same" WRITE-ALL ;

: BIG-PREP ( -- )
   32769 0 ?do i 255 and BIG-A i + c! i 255 and BIG-B i + c! loop ;

\ The CLI entry on a stand-in host: the script given replaces the engine that
\ builds the first generation, so BUILD reads its exit status with no native
\ build. Returns the entry's own exit status.
: CHAIN-RC ( ptr u8 n -- n ) {: script:ptr scriptu:n :}
   HOST$ script scriptu WRITE-ALL
   HOST$ CHMOD-X
   PROC-CMD:RESET
   s" --load" >LEN PROC-CMD:ARG+
   s" tools/chain-run-build.f" >LEN PROC-CMD:ARG+
   s" --" >LEN PROC-CMD:ARG+
   HOST$ >LEN PROC-CMD:ARG+
   ROOT$ s" gen1" JOIN >LEN PROC-CMD:ARG+
   ROOT$ s" gen2" JOIN >LEN PROC-CMD:ARG+
   ROOT$ s" gen3" JOIN >LEN PROC-CMD:ARG+
   ROOT$ s" tmp" JOIN >LEN PROC-CMD:ARG+
   ENGINE-CANDIDATE:PATH$ >LEN 60000 >MS PROC-CMD:RUN-OUTCOME
   PROC-OUTCOME>RC RC>N ;

\ The entry replays the failed child's stderr and names the code it caught.
: CHAIN-NAMED ( -- )
   PROC-CMD:OUT$ s" chain-run: native-build stderr: chain-run-test host" CONTAINS? TTRUE
   PROC-CMD:OUT$ s" chain-run: uncaught throw code " CONTAINS? TTRUE ;

\ 124 is PROC-TIMEOUT-RC, the status a native build exits with when a deadline
\ expired in it (tools/native-build-args.f).
: CHAIN-EXITS ( -- )
   s" a native-build deadline exits the chain with PROC-TIMEOUT-RC" T-LABEL
   S\" #!/bin/sh\necho chain-run-test host >&2\nexit 124\n" CHAIN-RC
   PROC-TIMEOUT-RC T=
   CHAIN-NAMED
   s" any other native-build failure exits the chain with FAIL-RC" T-LABEL
   S\" #!/bin/sh\necho chain-run-test host >&2\nexit 7\n" CHAIN-RC
   CHAIN-RUN:FAIL-RC T=
   CHAIN-NAMED ;

: MAIN ( -- )
   T-RESET PREP
   s" equal files share a digest" T-LABEL
   A$ B$ CHAIN-RUN:SAME-FILES? TTRUE
   B$ s" changed" WRITE-ALL
   s" changed files do not share a digest" T-LABEL
   A$ B$ CHAIN-RUN:SAME-FILES? 0= TTRUE
   BIG-PREP
   A$ BIG-A 32769 WRITE-ALL
   B$ BIG-B 32769 WRITE-ALL
   s" exact comparison crosses a chunk boundary" T-LABEL
   A$ B$ CHAIN-RUN:SAME-FILES? TTRUE
   1 BIG-B 32768 + c!
   B$ BIG-B 32769 WRITE-ALL
   A$ B$ CHAIN-RUN:SAME-FILES? 0= TTRUE
   CHAIN-EXITS
   ROOT$ REMOVE-TREE
   T-REPORT ;

MAIN
;package
