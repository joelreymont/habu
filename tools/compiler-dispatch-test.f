\ compiler-dispatch-test.f - source-shape regression for compiler dispatch factoring.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f tools/compiler-dispatch-test.f

$20000 constant CDT-CAP

create CDT-BUF CDT-CAP allot
variable CDT-LEN

: CDT-SOURCE ( -- ptr u8 n )
   CDT-BUF CDT-LEN @ ;

: CDT-LOAD ( ptr u8 n -- )
   CDT-BUF CDT-CAP READ-ALL CDT-LEN ! ;

: CDT-HAS? ( ptr u8 n -- bool )
   CDT-SOURCE 2swap CONTAINS? ;

: CDT-MUST-HAVE ( ptr u8 n -- )
   CDT-HAS? TTRUE ;

: CDT-MUST-LACK ( ptr u8 n -- )
   CDT-HAS? 0= TTRUE ;

: CDT-COUNT ( ptr u8 n -- n ) {: needle:ptr needleu :}
   needleu 0= if 0 exit then
   CDT-LEN @ needleu < if 0 exit then
   0 0 begin dup CDT-LEN @ needleu - <= while
      CDT-BUF over + needleu needle needleu STR= if swap 1+ swap then
      1+
   repeat drop ;

: CDT-COUNT= ( ptr u8 n n -- ) {: needle:ptr needleu want :}
   needle needleu CDT-COUNT want T= ;

: CDT-TEST-NATIVE-INTERPRET ( -- )
   s" : EM-INTERPRET-DEFINE-KEYWORDS ( -- )" CDT-MUST-HAVE
   s" : EM-INTERPRET-STRING-KEYWORDS ( -- )" CDT-MUST-HAVE
   s" : EM-INTERPRET-NUMBER ( n -- )" CDT-MUST-HAVE
   s" : EM-INTERPRET-FIND ( -- )" CDT-MUST-HAVE
   s" EM-INTERPRET-DEFINE-KEYWORDS" 2 CDT-COUNT=
   s" EM-INTERPRET-STRING-KEYWORDS" 2 CDT-COUNT=
   s" EM-INTERPRET-NUMBER" 2 CDT-COUNT=
   s" EM-INTERPRET-FIND" 2 CDT-COUNT= ;

: CDT-TEST-NATIVE-COMPILE ( -- )
   s" : EM-COMPILE-CONTROL-KEYWORDS ( -- )" CDT-MUST-HAVE
   s" : EM-COMPILE-STRING-KEYWORDS ( -- )" CDT-MUST-HAVE
   s" : EM-COMPILE-META-KEYWORDS ( -- )" CDT-MUST-HAVE
   s" : EM-COMPILE-LOOP-KEYWORDS ( -- )" CDT-MUST-HAVE
   s" : EM-COMPILE-ARITH-OPS ( -- )" CDT-MUST-HAVE
   s" : EM-COMPILE-SHUFFLE-OPS ( -- )" CDT-MUST-HAVE
   s" : EM-COMPILE-COMPARE-OPS ( -- )" CDT-MUST-HAVE
   s" : EM-COMPILE-UNARY-OPS ( -- )" CDT-MUST-HAVE
   s" : EM-COMPILE-FLOAT-OPS ( -- )" CDT-MUST-HAVE
   s" EM-COMPILE-CONTROL-KEYWORDS" 2 CDT-COUNT=
   s" EM-COMPILE-STRING-KEYWORDS" 2 CDT-COUNT=
   s" EM-COMPILE-META-KEYWORDS" 2 CDT-COUNT=
   s" EM-COMPILE-LOOP-KEYWORDS" 2 CDT-COUNT=
   s" EM-COMPILE-ARITH-OPS" 2 CDT-COUNT=
   s" EM-COMPILE-SHUFFLE-OPS" 2 CDT-COUNT=
   s" EM-COMPILE-COMPARE-OPS" 2 CDT-COUNT=
   s" EM-COMPILE-UNARY-OPS" 2 CDT-COUNT=
   s" EM-COMPILE-FLOAT-OPS" 2 CDT-COUNT= ;

: CDT-TEST-NATIVE-LOCAL ( -- )
   s" variable CLOC-MAIN  variable CLOC-NOT" CDT-MUST-HAVE
   s" variable CLOC-MEM   variable CLOC-QOK" CDT-MUST-HAVE
   s" : C-LOCAL-REF-ARGS ( n n -- )" CDT-MUST-HAVE
   s" : C-LOCAL-REF-LABELS ( -- )" CDT-MUST-HAVE
   s" : EMIT-RESET-BUILDER ( ptr u8 n -- )" CDT-MUST-HAVE
   s" C-LOCAL-REF-ARGS" 2 CDT-COUNT=
   s" C-LOCAL-REF-LABELS" 2 CDT-COUNT=
   s" {: lmainlbl notloc :}" CDT-MUST-LACK
   s" LBL LBL {: lmem qlrefok :}" CDT-MUST-LACK
   s" {: a:ptr u :}" CDT-MUST-LACK
   s" CLOC-MAIN @ B," 2 CDT-COUNT=
   s" CLOC-MAIN @ B ;" CDT-MUST-LACK
   s" CLOC-QOK @ LBL," CDT-MUST-HAVE
   s" CLOC-MEM @ LBL," CDT-MUST-HAVE ;

: CDT-TEST-SECTIONS ( -- )
   s" : EMIT-PRIMITIVE-SECTIONS ( -- )" CDT-MUST-HAVE
   s" : EMIT-DICTIONARY-SECTIONS ( -- )" CDT-MUST-HAVE
   s" : EMIT-RUNTIME-SECTIONS ( -- )" CDT-MUST-HAVE
   s" EMIT-PRIMITIVE-SECTIONS" 2 CDT-COUNT=
   s" EMIT-DICTIONARY-SECTIONS" 2 CDT-COUNT=
   s" EMIT-RUNTIME-SECTIONS" 2 CDT-COUNT=
   s" EMIT-PRIMS  EMIT-PROF-PRIMS  EMIT-FP-PRIMS" CDT-MUST-LACK
   s" EMIT-CF-HELPERS  EMIT-LOC-FIND  EMIT-KWDATA" CDT-MUST-LACK ;

: CDT-TEST-NATIVE ( -- )
   s" src/habu/habu2.f" CDT-LOAD
   CDT-TEST-NATIVE-INTERPRET
   CDT-TEST-NATIVE-COMPILE
   CDT-TEST-NATIVE-LOCAL
   CDT-TEST-SECTIONS ;

: CDT-TEST-HABU1 ( -- )
   s" src/habu/habu1.f" CDT-LOAD
   s" variable PR-A  variable PR-U  variable PR-L  variable PR-E" CDT-MUST-HAVE
   s" variable FP-A  variable FP-U  variable FP-XT" CDT-MUST-HAVE
   s" variable SDA-FD  variable SDA-NEW  variable SDA-SKIP" CDT-MUST-HAVE
   s" variable BSP-OK  variable BSP-DN  variable BSP-SAD" CDT-MUST-HAVE
   s" variable SZA-I" CDT-MUST-HAVE
   s" : REG-PRIM ( ptr u8 n n n -- )" CDT-MUST-HAVE
   s" : FPRIM ( ptr u8 n n -- )" CDT-MUST-HAVE
   s" : FPRIM-L ( ptr u8 n n -- )" CDT-MUST-HAVE
   s" : PR-COPY-NAME ( -- )" CDT-MUST-HAVE
   s" : BSP-LABELS3 ( -- )" CDT-MUST-HAVE
   s" : FPRIM {: na:ptr nu xt :}" CDT-MUST-LACK
   s" : FPRIM-L {: na:ptr nu xt :}" CDT-MUST-LACK
   s" : REG-PRIM {: na:ptr nu lbl elbl :}" CDT-MUST-LACK
   s" : ?PRIM-SPACE {: na:ptr nu :}" CDT-MUST-LACK
   s" : SPAWN-DUP2-ACTION ( reg fd -- )" CDT-MUST-HAVE
   s" : SPAWN-CHDIR-ACTION ( reg label -- )" CDT-MUST-HAVE
   s" : SPAWN-DUP2-ACTION ( n n -- ) {: fdreg newfd :}" CDT-MUST-LACK
   s" : SPAWN-CHDIR-ACTION ( n n -- ) {: cwdreg fail :}" CDT-MUST-LACK
   s" : BSPAWNIO" CDT-MUST-HAVE
   s" 14 SP SPAWN-ADESC-OFF SZA-I @ + STR," CDT-MUST-HAVE
   s" 14 SP SPAWN-ADESC-OFF + over + STR," CDT-MUST-LACK
   s" LBL LBL LBL {: spok spdn sad :}" CDT-MUST-LACK
   s" LBL LBL {: spok spdn :}" CDT-MUST-LACK ;

: CDT-TEST-ICODE ( -- )
   s" src/arch/arm64/icode.f" CDT-LOAD
   s" variable BYA" CDT-MUST-HAVE
   s" variable BYU" CDT-MUST-HAVE
   s" : BYA@ ( -- ptr u8 )" CDT-MUST-HAVE
   s" : BYTES-ARGS ( ptr u8 n -- )" CDT-MUST-HAVE
   s" : BYTES-CAP ( -- )" CDT-MUST-HAVE
   s" : BYTES-COPY ( -- )" CDT-MUST-HAVE
   s" : BYTES-PAD ( -- )" CDT-MUST-HAVE
   s" : BYTES, ( ptr u8 n -- )" CDT-MUST-HAVE
   s" {: a:ptr u :}" CDT-MUST-LACK ;

: CDT-TEST-BOOTSTRAP-COMPILE ( -- )
   s" : EMIT-COMPILE-CONTROL-KEYWORDS ( n -- )" CDT-MUST-HAVE
   s" : EMIT-COMPILE-STRING-KEYWORDS ( n -- )" CDT-MUST-HAVE
   s" : EMIT-COMPILE-META-KEYWORDS ( n -- )" CDT-MUST-HAVE
   s" : EMIT-COMPILE-LOOP-KEYWORDS ( n -- )" CDT-MUST-HAVE
   s" : EMIT-COMPILE-ARITH-OPS ( n -- )" CDT-MUST-HAVE
   s" : EMIT-COMPILE-SHUFFLE-OPS ( n -- )" CDT-MUST-HAVE
   s" : EMIT-COMPILE-COMPARE-OPS ( n -- )" CDT-MUST-HAVE
   s" : EMIT-COMPILE-UNARY-OPS ( n -- )" CDT-MUST-HAVE
   s" : EMIT-COMPILE-FLOAT-OPS ( n -- )" CDT-MUST-HAVE
   s" EMIT-COMPILE-CONTROL-KEYWORDS" 2 CDT-COUNT=
   s" EMIT-COMPILE-STRING-KEYWORDS" 2 CDT-COUNT=
   s" EMIT-COMPILE-META-KEYWORDS" 2 CDT-COUNT=
   s" EMIT-COMPILE-LOOP-KEYWORDS" 2 CDT-COUNT=
   s" EMIT-COMPILE-ARITH-OPS" 2 CDT-COUNT=
   s" EMIT-COMPILE-SHUFFLE-OPS" 2 CDT-COUNT=
   s" EMIT-COMPILE-COMPARE-OPS" 2 CDT-COUNT=
   s" EMIT-COMPILE-UNARY-OPS" 2 CDT-COUNT=
   s" EMIT-COMPILE-FLOAT-OPS" 2 CDT-COUNT= ;

: CDT-TEST-BOOTSTRAP ( -- )
   s" bootstrap/cg/forth.fs" CDT-LOAD
   CDT-TEST-BOOTSTRAP-COMPILE
   CDT-TEST-SECTIONS ;

: CDT-MAIN ( -- )
   T-RESET
   CDT-TEST-NATIVE
   CDT-TEST-HABU1
   CDT-TEST-ICODE
   CDT-TEST-BOOTSTRAP
   T-REPORT
   s" compiler-dispatch-test: ok" type cr ;

CDT-MAIN
