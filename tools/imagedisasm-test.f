\ imagedisasm-test.f - fixture coverage for native raw image disassembly.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f tools/imagedisasm-test.f

$4000 constant IMDT-CAP
1000 constant IMDT-TIMEOUT-MS

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
   s" tools/date.f" IMDT-ARG+
   s" lib/errors.f" IMDT-ARG+
   s" lib/string.f" IMDT-ARG+
   s" lib/fs.f" IMDT-ARG+
   s" tools/lint/text.f" IMDT-ARG+
   s" tools/lint/token.f" IMDT-ARG+
   s" tools/lint/lib.f" IMDT-ARG+
   s" tools/argv.f" IMDT-ARG+
   s" tools/trust-lint.f" IMDT-ARG+
   s" --" IMDT-ARG+
   s" source-only" IMDT-ARG+
   s" tools/imagedisasm.f" IMDT-ARG+
   s" ." IMDT-ARG+ ;

: IMDT-CAPTURE>N ( len len rc -- n n n )
   {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: IMDT-RUN ( ptr u8 n ptr u8 n ptr u8 n -- n n n )
   {: path:ptr pathu off:ptr offu count:ptr countu :}
   IMDT-ARGV-BASE
   path pathu IMDT-ARG+
   off offu IMDT-ARG+
   count countu IMDT-ARG+
   s" bin/hb" >LEN IMDT-OUT IMDT-CAP >LEN IMDT-ERR IMDT-CAP >LEN
   IMDT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE IMDT-CAPTURE>N ;

: IMDT-RUN-TRUST ( -- n n n )
   IMDT-TRUST-ARGV
   s" bin/hb" >LEN IMDT-OUT IMDT-CAP >LEN IMDT-ERR IMDT-CAP >LEN
   IMDT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE IMDT-CAPTURE>N ;

: IMDT-TEST-RET ( -- )
   IMDT-RET$ s" 0" s" 1" IMDT-RUN 0 T=
   {: outu erru :}
   erru 0 T=
   IMDT-OUT outu s" ret" CONTAINS? TTRUE ;

: IMDT-TEST-HEX-OFFSET ( -- )
   IMDT-RET$ s" $0" s" 1" IMDT-RUN 0 T=
   {: outu erru :}
   erru 0 T=
   IMDT-OUT outu s" ret" CONTAINS? TTRUE ;

: IMDT-TEST-LDRB ( -- )
   IMDT-LDRB$ s" 0" s" 1" IMDT-RUN 0 T=
   {: outu erru :}
   erru 0 T=
   IMDT-OUT outu s" ldrb" CONTAINS? TTRUE ;

: IMDT-TEST-RANGE ( -- )
   IMDT-RET$ s" 4" s" 1" IMDT-RUN 74 T=
   {: outu erru :}
   outu 0 T=
   IMDT-ERR erru s" imagedisasm: range outside image" CONTAINS? TTRUE ;

: IMDT-TEST-TRUST ( -- )
   IMDT-RUN-TRUST 0 T=
   {: outu erru :}
   erru 0 T=
   IMDT-OUT outu s" TRUST site(s), " CONTAINS? TTRUE ;

: IMDT-MAIN ( -- )
   T-RESET
   IMDT-PREPARE
   IMDT-TEST-RET
   IMDT-TEST-HEX-OFFSET
   IMDT-TEST-LDRB
   IMDT-TEST-RANGE
   IMDT-TEST-TRUST
   CLEANUP-RUN
   T-REPORT
   s" imagedisasm-test: ok" type cr ;

IMDT-MAIN
