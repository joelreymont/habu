\ hb-baseline-contracts-test.f - checked baseline contracts for public bin/hb.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/hb-baseline-contracts-test.f

$8000 constant HBT-CAP
$5000 constant HBT-PROP-CAP
10000 constant HBT-TIMEOUT-MS
1 constant HBT-X-OK

create HBT-OUT HBT-CAP allot
create HBT-ERR HBT-CAP allot
create HBT-PROP-SRC HBT-PROP-CAP allot
create HBT-ROOT FS-PATH-CAP allot
create HBT-SCRIPT FS-PATH-CAP allot
create HBT-MULTI-A FS-PATH-CAP allot
create HBT-MULTI-B FS-PATH-CAP allot
create HBT-MULTI-MAIN FS-PATH-CAP allot
create HBT-STDIN-TOOL FS-PATH-CAP allot

variable HBT-ROOT-U
variable HBT-SCRIPT-U
variable HBT-MULTI-A-U
variable HBT-MULTI-B-U
variable HBT-MULTI-MAIN-U
variable HBT-STDIN-TOOL-U
variable HBT-PUBLIC-N
variable HBT-PUBLIC-BAD
variable HBT-PROP-U

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

: HBT-STDIN-TOOL$ ( -- ptr u8 n )
   HBT-STDIN-TOOL HBT-STDIN-TOOL-U @ ;

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

: HBT-STDIN-TOOL$SRC ( -- ptr u8 n )
   SB-RESET
   s" create HBT-DATA-BUF 32 allot" SB-APPEND HBT-LF
   s" : MAIN ( -- ) HBT-DATA-BUF 32 >LEN READ-STDIN-ALL LEN>N dup . HBT-DATA-BUF swap type cr ;" SB-APPEND HBT-LF
   s" MAIN" SB-APPEND HBT-LF
   SB$ ;

: HBT-PREPARE ( -- )
   CLEANUP-RESET
   s" hb-baseline-contracts" TMPDIR-MKDIR HBT-ROOT HBT-ROOT-U HBT-COPY!
   HBT-ROOT$ CLEANUP-TREE+
   HBT-ROOT$ s" script-mode.f" HBT-SCRIPT JOIN-PATH HBT-SCRIPT-U !
   HBT-ROOT$ s" multi-a.f" HBT-MULTI-A JOIN-PATH HBT-MULTI-A-U !
   HBT-ROOT$ s" multi-b.f" HBT-MULTI-B JOIN-PATH HBT-MULTI-B-U !
   HBT-ROOT$ s" multi-main.f" HBT-MULTI-MAIN JOIN-PATH HBT-MULTI-MAIN-U !
   HBT-ROOT$ s" stdin-data-tool.f" HBT-STDIN-TOOL JOIN-PATH HBT-STDIN-TOOL-U !
   HBT-SCRIPT$ HBT-SCRIPT$SRC WRITE-ALL
   HBT-MULTI-A$ HBT-MULTI-A$SRC WRITE-ALL
   HBT-MULTI-B$ HBT-MULTI-B$SRC WRITE-ALL
   HBT-MULTI-MAIN$ HBT-MULTI-MAIN$SRC WRITE-ALL
   HBT-STDIN-TOOL$ HBT-STDIN-TOOL$SRC WRITE-ALL
   s" test/prop-test.f" HBT-PROP-SRC HBT-PROP-CAP READ-ALL HBT-PROP-U ! ;

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

: HBT-TEST-SCRIPT-MODE ( -- )
   PROC-ARGV-RESET
   HBT-SCRIPT$  >LEN PROC-ARGV+
   s" omega"  >LEN PROC-ARGV+
   s" " HBT-RUN-STDIN 0 T= 0 T= 15 T=
   SB-RESET
   s" SCRIPT" SB-APPEND HBT-LF
   s" 1" SB-APPEND HBT-LF
   s" omega" SB-APPEND HBT-LF
   HBT-OUT 15 SB$ T$= ;

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

: HBT-TEST-STDIN-DATA ( -- )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   HBT-STDIN-TOOL$  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" DATA" HBT-RUN-STDIN 0 T= 0 T= 7 T=
   SB-RESET
   s" 4" SB-APPEND HBT-LF
   s" DATA" SB-APPEND HBT-LF
   HBT-OUT 7 SB$ T$= ;

: HBT-TEST-PROP-ARGV ( -- )
   PROC-ARGV-RESET
   s" 123"  >LEN PROC-ARGV+
   s" 4"  >LEN PROC-ARGV+
   HBT-PROP-SRC HBT-PROP-U @ HBT-RUN-STDIN 0 T=
   {: outu erru :}
   erru 0 T<>
   HBT-OUT outu s" prop-test: self-test OK" CONTAINS? TTRUE
   HBT-OUT outu s" prop-test: 4" CONTAINS? TTRUE
   HBT-OUT outu s" programs," CONTAINS? TTRUE ;

: HBT-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-TEST-PUBLIC-BIN
   HBT-TEST-PIPELINE
   HBT-TEST-PIPE-WINS
   HBT-TEST-SCRIPT-MODE
   HBT-TEST-MULTI-SOURCE
   HBT-TEST-STDIN-DATA
   HBT-TEST-PROP-ARGV
   CLEANUP-RUN
   HBT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" hb-baseline-contracts-test: ok" type cr ;

HBT-MAIN
