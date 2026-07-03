\ hb-cli-contracts-test.f - checked coverage for hb startup/data-stdin contracts.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/hb-cli-contracts-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f

2048 constant HCT-CAP
10000 constant HCT-TIMEOUT-MS

variable HCT-ROOT-U
variable HCT-CHILD-U
variable HCT-OUT-U
variable HCT-ERR-U
variable HCT-RC
variable HCT-KIND

create HCT-ROOT-BUF FS-PATH-CAP allot
create HCT-CHILD-BUF FS-PATH-CAP allot
create HCT-OUT HCT-CAP allot
create HCT-ERR HCT-CAP allot
create HCT-EMPTY 1 allot   \ zero-length stdin

: HCT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: HCT-ROOT ( -- ptr u8 n )
   HCT-ROOT-BUF HCT-ROOT-U @ ;

: HCT-CHILD ( -- ptr u8 n )
   HCT-CHILD-BUF HCT-CHILD-U @ ;

: HCT-LF ( -- )
   10 SB-APPEND-C ;

: HCT-CHILD$ ( -- ptr u8 n )
   SB-RESET
   s" create HCT-BUF 32 allot" SB-APPEND HCT-LF
   s" : MAIN ( -- )" SB-APPEND HCT-LF
   s"    HCT-BUF 32 >LEN READ-STDIN-ALL LEN>N dup ." SB-APPEND HCT-LF
   s"    HCT-BUF swap type cr ;" SB-APPEND HCT-LF
   s" MAIN" SB-APPEND HCT-LF
   SB$ ;

: HCT-WANT$ ( -- ptr u8 n )
   SB-RESET
   s" 4" SB-APPEND HCT-LF
   s" DATA" SB-APPEND HCT-LF
   SB$ ;

: HCT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-cli-contracts" TMPDIR-MKDIR {: a:ptr u :}
   a u HCT-ROOT-BUF HCT-ROOT-U HCT-COPY!
   HCT-ROOT CLEANUP-TREE+
   HCT-ROOT s" stdin-data-tool.f" HCT-CHILD-BUF JOIN-PATH HCT-CHILD-U !
   HCT-CHILD HCT-CHILD$ WRITE-ALL ;

: HCT-EXPECT-LOAD-STDIN ( -- )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   HCT-CHILD  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" bin/hb" >LEN s" DATA" >LEN HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN HCT-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME
   {: outu erru kind code :}
   kind HCT-KIND !
   code HCT-RC !
   erru LEN>N HCT-ERR-U !
   outu LEN>N HCT-OUT-U !
   HCT-KIND @ PROC-OUTCOME-EXIT T= HCT-RC @ 0 T=
   HCT-ERR-U @ 0 T= HCT-OUT-U @ 7 T=
   HCT-OUT 7 HCT-WANT$ T$= ;

: HCT-STORE! ( len len n n -- ) {: outu:len erru:len kind:n code:n :}
   kind HCT-KIND !  code HCT-RC !
   erru LEN>N HCT-ERR-U !  outu LEN>N HCT-OUT-U ! ;

\ printf '' | hb --loadx  ->  rc 64, stderr echoes the flag and lists --load.
: HCT-EXPECT-UNKNOWN-FLAG ( -- )
   PROC-ARGV-RESET
   s" --loadx"  >LEN PROC-ARGV+
   s" bin/hb" >LEN  HCT-EMPTY 0 >LEN  HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN HCT-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME HCT-STORE!
   s" unknown flag exits 64" T-LABEL
   HCT-KIND @ PROC-OUTCOME-EXIT T=  HCT-RC @ 64 T=
   s" stderr echoes the offending flag" T-LABEL
   HCT-ERR HCT-ERR-U @ s" --loadx" CONTAINS? TTRUE
   s" usage names the known --load flag" T-LABEL
   HCT-ERR HCT-ERR-U @ s" --load" CONTAINS? TTRUE ;

\ printf '1 2 + . cr' | hb --nonsense  ->  rc 64: the flag is rejected before the
\ piped program can run (the old fail-open path silently ran stdin and printed 3).
: HCT-EXPECT-FLAG-BEFORE-STDIN ( -- )
   PROC-ARGV-RESET
   s" --nonsense"  >LEN PROC-ARGV+
   s" bin/hb" >LEN  s" 1 2 + . cr" >LEN  HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN HCT-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME HCT-STORE!
   s" unknown flag rejected even with a piped program" T-LABEL
   HCT-KIND @ PROC-OUTCOME-EXIT T=  HCT-RC @ 64 T=
   s" the piped stdin program did not run" T-LABEL
   HCT-OUT-U @ 0 T= ;

\ printf '' | hb --load /dev/null  ->  rc 0: a known flag with an empty file loads.
: HCT-EXPECT-LOAD-EMPTY-FILE ( -- )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" /dev/null"  >LEN PROC-ARGV+
   s" bin/hb" >LEN  HCT-EMPTY 0 >LEN  HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN HCT-TIMEOUT-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME HCT-STORE!
   s" --load of an empty file exits 0" T-LABEL
   HCT-KIND @ PROC-OUTCOME-EXIT T=  HCT-RC @ 0 T= ;

: HCT-MAIN ( -- )
   T-RESET
   HCT-PREPARE
   HCT-EXPECT-LOAD-STDIN
   HCT-EXPECT-UNKNOWN-FLAG
   HCT-EXPECT-FLAG-BEFORE-STDIN
   HCT-EXPECT-LOAD-EMPTY-FILE
   CLEANUP-RUN
   HCT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-cli-contracts-test: ok" type cr ;

HCT-MAIN
