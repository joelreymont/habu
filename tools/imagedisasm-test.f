\ imagedisasm-test.f - fixture coverage for native raw image disassembly.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/imagedisasm-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require src/arch/arm64/disasm.f
require tools/imagedisasm.f

$4000 constant IMDT-CAP
10000 constant IMDT-TIMEOUT-MS
30000 constant IMDT-TRUST-TIMEOUT-MS

create IMDT-OUT IMDT-CAP allot
create IMDT-ERR IMDT-CAP allot
create IMDT-ROOT FS-PATH-CAP allot
create IMDT-RET FS-PATH-CAP allot
create IMDT-LDRB FS-PATH-CAP allot
create IMDT-BYTES 4 allot

variable IMDT-ROOT-U
variable IMDT-RET-U
variable IMDT-LDRB-U

: IMDT-COPY! ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: IMDT-ROOT$ ( -- ptr u8 n )
   IMDT-ROOT IMDT-ROOT-U @ ;

: IMDT-RET$ ( -- ptr u8 n )
   IMDT-RET IMDT-RET-U @ ;

: IMDT-LDRB$ ( -- ptr u8 n )
   IMDT-LDRB IMDT-LDRB-U @ ;

: IMDT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: IMDT-WRITE-RET ( -- )
   $C0 IMDT-BYTES c!
   $03 IMDT-BYTES 1 + c!
   $5F IMDT-BYTES 2 + c!
   $D6 IMDT-BYTES 3 + c!
   IMDT-RET$ IMDT-BYTES 4 WRITE-ALL ;

: IMDT-WRITE-LDRB ( -- )
   $69 IMDT-BYTES c!
   $01 IMDT-BYTES 1 + c!
   $40 IMDT-BYTES 2 + c!
   $39 IMDT-BYTES 3 + c!
   IMDT-LDRB$ IMDT-BYTES 4 WRITE-ALL ;

: IMDT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-imagedisasm-test" TMPDIR-MKDIR IMDT-ROOT IMDT-ROOT-U IMDT-COPY!
   IMDT-ROOT$ CLEANUP-TREE+
   IMDT-ROOT$ s" ret.bin" IMDT-RET JOIN-PATH IMDT-RET-U !
   IMDT-ROOT$ s" ldrb.bin" IMDT-LDRB JOIN-PATH IMDT-LDRB-U !
   IMDT-WRITE-RET
   IMDT-WRITE-LDRB ;

: IMDT-ARGV-BASE ( -- )
   PROC-ARGV-RESET
   s" --load" IMDT-ARG+
   s" lib/errors.f" IMDT-ARG+
   s" lib/string.f" IMDT-ARG+
   s" src/arch/arm64/disasm.f" IMDT-ARG+
   s" tools/imagedisasm.f" IMDT-ARG+
   s" --" IMDT-ARG+ ;

: IMDT-TRUST-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" IMDT-ARG+
   s" tools/trust-lint.f" IMDT-ARG+
   s" --" IMDT-ARG+
   s" source-only" IMDT-ARG+
   s" tools/imagedisasm.f" IMDT-ARG+
   s" ." IMDT-ARG+ ;

: IMDT-CAPTURE>N ( len len n n -- n n n n )
   {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: IMDT-RUN ( ptr u8 n ptr u8 n ptr u8 n -- n n n n )
   {: path:ptr pathu off:ptr offu count:ptr countu :}
   IMDT-ARGV-BASE
   path pathu IMDT-ARG+
   off offu IMDT-ARG+
   count countu IMDT-ARG+
   s" bin/hb" >LEN IMDT-OUT IMDT-CAP >LEN IMDT-ERR IMDT-CAP >LEN
   IMDT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME IMDT-CAPTURE>N ;

: IMDT-RUN-TRUST ( -- n n n n )
   IMDT-TRUST-ARGV
   s" bin/hb" >LEN IMDT-OUT IMDT-CAP >LEN IMDT-ERR IMDT-CAP >LEN
   IMDT-TRUST-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME IMDT-CAPTURE>N ;

: IMDT-EXPECT-EXIT ( n n n n n -- n n ) {: outu erru kind code expect :}
   kind PROC-OUTCOME-EXIT T=
   code expect T=
   outu erru ;

: IMDT-TEST-RET ( -- )
   IMDT-RET$ s" 0" s" 1" IMDT-RUN 0 IMDT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   IMDT-OUT outu s" ret" CONTAINS? TTRUE ;

: IMDT-TEST-HEX-OFFSET ( -- )
   IMDT-RET$ s" $0" s" 1" IMDT-RUN 0 IMDT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   IMDT-OUT outu s" ret" CONTAINS? TTRUE ;

: IMDT-TEST-LDRB ( -- )
   IMDT-LDRB$ s" 0" s" 1" IMDT-RUN 0 IMDT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   IMDT-OUT outu s" ldrb" CONTAINS? TTRUE ;

: IMDT-TEST-RANGE ( -- )
   IMDT-RET$ s" 4" s" 1" IMDT-RUN 74 IMDT-EXPECT-EXIT {: outu erru :}
   outu 0 T=
   IMDT-ERR erru s" imagedisasm: range outside image" CONTAINS? TTRUE ;

: IMDT-TEST-TRUST ( -- )
   IMDT-RUN-TRUST 0 IMDT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   IMDT-OUT outu s" TRUST site(s), " CONTAINS? TTRUE ;

\ switchover wave A: the imagedisasm number parsers return option<n> (SOME
\ parsed value, else NONE). Both branches through IMGD>NUMBER? ($hex, decimal,
\ bad, hex-overflow via IMGD-HEX-STEP).
: IMDT-NUM-SOME ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u IMGD>NUMBER? MATCH option
     none OF 0 0= 0= ENDOF
     some OF want = ENDOF
   ;MATCH TTRUE ;
: IMDT-NUM-NONE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u IMGD>NUMBER? MATCH option
     none OF 0 0= ENDOF
     some OF drop 0 0= 0= ENDOF
   ;MATCH TTRUE ;
: IMDT-TEST-PARSERS ( -- )
   s" $ff" 255 IMDT-NUM-SOME
   s" 42" 42 IMDT-NUM-SOME
   s" $zz" IMDT-NUM-NONE
   s" nope" IMDT-NUM-NONE
   s" $fffffffffffffffff" IMDT-NUM-NONE ;

: IMDT-MAIN ( -- )
   T-RESET
   IMDT-PREPARE
   IMDT-TEST-PARSERS
   IMDT-TEST-RET
   IMDT-TEST-HEX-OFFSET
   IMDT-TEST-LDRB
   IMDT-TEST-RANGE
   IMDT-TEST-TRUST
   CLEANUP-RUN
   T-REPORT
   s" imagedisasm-test: ok" type cr ;

IMDT-MAIN
