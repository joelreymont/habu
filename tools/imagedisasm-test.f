split-str
\ imagedisasm-test.f - fixture coverage for native raw image disassembly.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/test/ending.f
require src/lib/process-argv.f
require src/arch/arm64/disasm.f
require tools/imagedisasm.f

$4000 constant IMDT-CAP

create IMDT-OUT IMDT-CAP allot
create IMDT-ERR IMDT-CAP allot
create IMDT-ROOT PATH-CAP allot
create IMDT-RET PATH-CAP allot
create IMDT-LDRB PATH-CAP allot
create IMDT-FORMS PATH-CAP allot
create IMDT-BYTES 4 allot
16 constant IMDT-FORM-N
create IMDT-FORM-BYTES IMDT-FORM-N 4 * allot

variable IMDT-ROOT-U
variable IMDT-RET-U
variable IMDT-LDRB-U
variable IMDT-FORMS-U

: IMDT-COPY! ( str ^u8 ^n -- )
   {: a:^ u dst:^ lenp:^ :}
   a dst u BYTE-COPY
   u lenp ! ;

: IMDT-ROOT$ ( -- str )
   IMDT-ROOT IMDT-ROOT-U @ ;

: IMDT-RET$ ( -- str )
   IMDT-RET IMDT-RET-U @ ;

: IMDT-LDRB$ ( -- str )
   IMDT-LDRB IMDT-LDRB-U @ ;

: IMDT-FORMS$ ( -- str )
   IMDT-FORMS IMDT-FORMS-U @ ;

: IMDT-ARG+ ( str -- )
   PROC-ARGV+ ;

: IMDT-WRITE-RET ( -- )
   $C0 IMDT-BYTES c!
   $03 IMDT-BYTES 1 + c!
   $5F IMDT-BYTES 2 + c!
   $D6 IMDT-BYTES 3 + c!
   IMDT-RET$ IMDT-BYTES 4 FS:WRITE-ALL ;

: IMDT-WRITE-LDRB ( -- )
   $69 IMDT-BYTES c!
   $01 IMDT-BYTES 1 + c!
   $40 IMDT-BYTES 2 + c!
   $39 IMDT-BYTES 3 + c!
   IMDT-LDRB$ IMDT-BYTES 4 FS:WRITE-ALL ;

: IMDT-FORM! ( n n -- )
   {: w idx :}
   4 0 ?do w i 8 * rshift 255 and IMDT-FORM-BYTES idx 4 * + i + c! loop ;

\ One word of each form IMDT-TEST-FORMS expects, in its order; words 10 and 11
\ print ?: a reserved bitmask and an encoding no row decodes.
: IMDT-WRITE-FORMS ( -- )
   $F8008665 0 IMDT-FORM!
   $F85F8E6B 1 IMDT-FORM!
   $D63F0120 2 IMDT-FORM!
   $92401C20 3 IMDT-FORM!
   $92041C20 4 IMDT-FORM!
   $F100413F 5 IMDT-FORM!
   $54FFFF81 6 IMDT-FORM!
   $30FFFFE3 7 IMDT-FORM!
   $B40000C2 8 IMDT-FORM!
   $17FFFFFF 9 IMDT-FORM!
   $9240FC20 10 IMDT-FORM!
   $0E201C00 11 IMDT-FORM!
   $D2A00020 12 IMDT-FORM!
   $F2E00041 13 IMDT-FORM!
   $91400420 14 IMDT-FORM!
   $F1400D3F 15 IMDT-FORM!
   IMDT-FORMS$ IMDT-FORM-BYTES IMDT-FORM-N 4 * FS:WRITE-ALL ;

: IMDT-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-imagedisasm-test" FS:MKTEMP-DIR IMDT-ROOT IMDT-ROOT-U IMDT-COPY!
   IMDT-ROOT$ FS:CLEANUP-TREE+
   IMDT-ROOT$ s" ret.bin" IMDT-RET FS:JOIN-PATH IMDT-RET-U !
   IMDT-ROOT$ s" ldrb.bin" IMDT-LDRB FS:JOIN-PATH IMDT-LDRB-U !
   IMDT-ROOT$ s" forms.bin" IMDT-FORMS FS:JOIN-PATH IMDT-FORMS-U !
   IMDT-WRITE-RET
   IMDT-WRITE-LDRB
   IMDT-WRITE-FORMS ;

: IMDT-ARGV-BASE ( -- )
   PROC-ARGV-RESET
   s" --load" IMDT-ARG+
   s" src/lib/error.f" IMDT-ARG+
   s" src/lib/string.f" IMDT-ARG+
   s" src/arch/arm64/disasm.f" IMDT-ARG+
   s" tools/imagedisasm.f" IMDT-ARG+
   s" --" IMDT-ARG+ ;

: IMDT-RUN ( str str str -- len len PROCESS:ending )
   {: path:^ pathu off:^ offu count:^ countu :}
   IMDT-ARGV-BASE
   path pathu IMDT-ARG+
   off offu IMDT-ARG+
   count countu IMDT-ARG+
   s" bin/hb" IMDT-OUT IMDT-CAP >LEN IMDT-ERR IMDT-CAP >LEN
   PROCESS:WAIT-FOREVER RUN-ARGV-CAPTURE-OUTCOME ;

: IMDT-EXPECT-EXIT ( len len PROCESS:ending n -- n n )
   {: outu:len erru:len oc expect:n :}
   s" tools/imagedisasm.f" IMDT-OUT outu LEN>N IMDT-ERR erru LEN>N oc expect
   T-ENDING-EXITED=
   outu LEN>N erru LEN>N ;

: IMDT-TEST-RET ( -- )
   IMDT-RET$ s" 0" s" 1" IMDT-RUN 0 IMDT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   IMDT-OUT outu s" ret" STR:CONTAINS? TTRUE ;

: IMDT-TEST-HEX-OFFSET ( -- )
   IMDT-RET$ s" $0" s" 1" IMDT-RUN 0 IMDT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   IMDT-OUT outu s" ret" STR:CONTAINS? TTRUE ;

: IMDT-TEST-LDRB ( -- )
   IMDT-LDRB$ s" 0" s" 1" IMDT-RUN 0 IMDT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   IMDT-OUT outu s" ldrb" STR:CONTAINS? TTRUE ;

: IMDT-TEST-FORMS ( -- )
   IMDT-FORMS$ s" 0" s" 16" IMDT-RUN 0 IMDT-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   outu 192 T=
   IMDT-OUT outu s\" str-post 5 19 8\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" ldr-pre 11 19 -8\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" blr 9\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" and 0 1 255\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" and 0 1 -1152921436155805681\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" cmp 9 16\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" b.ne -16\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" adr 3 -3\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" cbz 2 24\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" b -4\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" ? 9240FC20\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" ? 0E201C00\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" movz 0 1 16\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" movk 1 2 48\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" add 0 1 4096\n" STR:CONTAINS? TTRUE
   IMDT-OUT outu s\" cmp 9 12288\n" STR:CONTAINS? TTRUE ;

: IMDT-TEST-RANGE ( -- )
   IMDT-RET$ s" 4" s" 1" IMDT-RUN 64 IMDT-EXPECT-EXIT {: outu erru :}
   outu 0 T=
   IMDT-ERR erru s" imagedisasm: range outside image" STR:CONTAINS? TTRUE ;

\ Invalid digits and overflowing hex inputs are absent from the CLI fixtures.
: IMDT-NUM-NONE ( str -- ) {: a:^ u:n :}
   a u IMAGE-DISASM:IMGD>NUMBER? MATCH option
     none OF 0 0= ENDOF
     some OF drop 0 0= 0= ENDOF
   ;MATCH TTRUE ;
: IMDT-TEST-PARSERS ( -- )
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
   IMDT-TEST-FORMS
   IMDT-TEST-RANGE
   FS:CLEANUP-RUN
   T-REPORT
   s" imagedisasm-test: ok" type cr ;

IMDT-MAIN
