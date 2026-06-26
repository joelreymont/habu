\ hb-cli-contracts-test.f - checked coverage for hb startup/data-stdin contracts.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/hb-cli-contracts-test.f

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
   s" create B 32 allot" SB-APPEND HCT-LF
   s" : MAIN ( -- )" SB-APPEND HCT-LF
   s"    B 32 >LEN READ-STDIN-ALL LEN>N dup ." SB-APPEND HCT-LF
   s"    B swap type cr ;" SB-APPEND HCT-LF
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

: HCT-MAIN ( -- )
   T-RESET
   HCT-PREPARE
   HCT-EXPECT-LOAD-STDIN
   CLEANUP-RUN
   HCT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-cli-contracts-test: ok" type cr ;

HCT-MAIN
