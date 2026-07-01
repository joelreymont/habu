\ hb-baseline-contracts-test.f - checked baseline contracts for public bin/hb.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/hb-baseline-contracts-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f

$8000 constant HBT-CAP
10000 constant HBT-TIMEOUT-MS
1 constant HBT-X-OK

create HBT-OUT HBT-CAP allot
create HBT-ERR HBT-CAP allot
create HBT-ROOT FS-PATH-CAP allot
create HBT-SCRIPT FS-PATH-CAP allot
create HBT-MULTI-A FS-PATH-CAP allot
create HBT-MULTI-B FS-PATH-CAP allot
create HBT-MULTI-MAIN FS-PATH-CAP allot

variable HBT-ROOT-U
variable HBT-SCRIPT-U
variable HBT-MULTI-A-U
variable HBT-MULTI-B-U
variable HBT-MULTI-MAIN-U
variable HBT-PUBLIC-N
variable HBT-PUBLIC-BAD

: HBT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: HBT-ROOT$ ( -- ptr u8 n )
   HBT-ROOT HBT-ROOT-U @ ;

: HBT-SCRIPT$ ( -- ptr u8 n )
   HBT-SCRIPT HBT-SCRIPT-U @ ;

: HBT-MULTI-A$ ( -- ptr u8 n )
   HBT-MULTI-A HBT-MULTI-A-U @ ;

: HBT-MULTI-B$ ( -- ptr u8 n )
   HBT-MULTI-B HBT-MULTI-B-U @ ;

: HBT-MULTI-MAIN$ ( -- ptr u8 n )
   HBT-MULTI-MAIN HBT-MULTI-MAIN-U @ ;

: HBT-LF ( -- )
   10 SB-APPEND-C ;

: HBT-EXEC? ( ptr u8 n -- bool )
   FS-PATHZ HBT-X-OK access 0= ;

: HBT-BIN-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u HBT-EXEC? if
      HBT-PUBLIC-N @ 1+ HBT-PUBLIC-N !
      a u s" bin/hb" STR= 0= if -1 HBT-PUBLIC-BAD ! then
   then ;

: HBT-SCRIPT$SRC ( -- ptr u8 n )
   SB-RESET
   s" ." SB-APPEND 34 SB-APPEND-C s"  SCRIPT" SB-APPEND 34 SB-APPEND-C s"  cr" SB-APPEND HBT-LF
   s" SCRIPT-ARGC ." SB-APPEND HBT-LF
   s" 0 SCRIPT-ARGV$ type cr" SB-APPEND HBT-LF
   SB$ ;

: HBT-MULTI-A$SRC ( -- ptr u8 n )
   s" : MS-A ( -- i64 ) 40 ;" ;

: HBT-MULTI-B$SRC ( -- ptr u8 n )
   s" : MS-B ( i64 -- i64 ) 2 + ;" ;

: HBT-MULTI-MAIN$SRC ( -- ptr u8 n )
   SB-RESET
   s" MS-A MS-B ." SB-APPEND HBT-LF
   s" SCRIPT-ARGC ." SB-APPEND HBT-LF
   s" 0 SCRIPT-ARGV$ type cr" SB-APPEND HBT-LF
   SB$ ;

: HBT-PREPARE ( -- )
   CLEANUP-RESET
   s" hb-baseline-contracts" TMPDIR-MKDIR HBT-ROOT HBT-ROOT-U HBT-COPY!
   HBT-ROOT$ CLEANUP-TREE+
   HBT-ROOT$ s" script-mode.f" HBT-SCRIPT JOIN-PATH HBT-SCRIPT-U !
   HBT-ROOT$ s" multi-a.f" HBT-MULTI-A JOIN-PATH HBT-MULTI-A-U !
   HBT-ROOT$ s" multi-b.f" HBT-MULTI-B JOIN-PATH HBT-MULTI-B-U !
   HBT-ROOT$ s" multi-main.f" HBT-MULTI-MAIN JOIN-PATH HBT-MULTI-MAIN-U !
   HBT-SCRIPT$ HBT-SCRIPT$SRC WRITE-ALL
   HBT-MULTI-A$ HBT-MULTI-A$SRC WRITE-ALL
   HBT-MULTI-B$ HBT-MULTI-B$SRC WRITE-ALL
   HBT-MULTI-MAIN$ HBT-MULTI-MAIN$SRC WRITE-ALL ;

: HBT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: HBT-RUN-STDIN ( ptr u8 n -- n n n ) {: input:ptr inputu :}
   s" bin/hb" >LEN input inputu >LEN HBT-OUT HBT-CAP >LEN
   HBT-ERR HBT-CAP >LEN HBT-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE
   HBT-CAPTURE>N ;

: HBT-RUN-CAPTURE ( -- n n n )
   s" bin/hb" >LEN HBT-OUT HBT-CAP >LEN HBT-ERR HBT-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE HBT-CAPTURE>N ;

: HBT-TEST-PUBLIC-BIN ( -- )
   s" bin/hb" FILE? TTRUE
   s" bin/hb" HBT-EXEC? TTRUE
   s" bin/hbi" EXISTS? TFALSE
   s" bin/habu" EXISTS? TFALSE
   0 HBT-PUBLIC-N !
   0 HBT-PUBLIC-BAD !
   s" bin" [: HBT-BIN-FILE ;] WALK-FILES
   HBT-PUBLIC-N @ 1 T=
   HBT-PUBLIC-BAD @ 0 T= ;

: HBT-TEST-PIPELINE ( -- )
   PROC-ARGV-RESET
   s" 41 1 + ." HBT-RUN-STDIN 0 T= 0 T= 3 T=
   SB-RESET
   s" 42" SB-APPEND HBT-LF
   HBT-OUT 3 SB$ T$= ;

: HBT-TEST-PIPE-WINS ( -- )
   PROC-ARGV-RESET
   HBT-SCRIPT$  >LEN PROC-ARGV+
   SB-RESET
   s" ." SB-APPEND 34 SB-APPEND-C s"  PIPE" SB-APPEND 34 SB-APPEND-C s"  cr" SB-APPEND HBT-LF
   SB$ HBT-RUN-STDIN 0 T= 0 T= 5 T=
   SB-RESET
   s" PIPE" SB-APPEND HBT-LF
   HBT-OUT 5 SB$ T$= ;

: HBT-TEST-MULTI-SOURCE ( -- )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   HBT-MULTI-A$  >LEN PROC-ARGV+
   HBT-MULTI-B$  >LEN PROC-ARGV+
   HBT-MULTI-MAIN$  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" theta"  >LEN PROC-ARGV+
   s" " HBT-RUN-STDIN 0 T= 0 T= 11 T=
   SB-RESET
   s" 42" SB-APPEND HBT-LF
   s" 1" SB-APPEND HBT-LF
   s" theta" SB-APPEND HBT-LF
   HBT-OUT 11 SB$ T$= ;

: HBT-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-TEST-PUBLIC-BIN
   HBT-TEST-PIPELINE
   HBT-TEST-PIPE-WINS
   HBT-TEST-MULTI-SOURCE
   CLEANUP-RUN
   HBT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" hb-baseline-contracts-test: ok" type cr ;

HBT-MAIN
