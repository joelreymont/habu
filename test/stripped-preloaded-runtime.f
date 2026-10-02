\ A saved linker already contains FFI and TASK before the application opens its
\ window. Its live link must claim those declarations with the current window.
require test/gate-common.f
require test/preloaded-engine.f

package STRIPPED-PRELOADED-RUNTIME-TEST
private

600000 constant TIMEOUT-MS
create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: PREPARE ( -- )
   s" stripped-preloaded-runtime" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" hb-aot-got" IMAGE GT-PATH IMAGE-U ! ;

: WRITE-SUBJECT ( -- )
   SB-RESET
   S\" require lib/task.f\nrequire lib/ffi-abi.f\nPROCESS-SYMBOLS\nFUNCTION: PID getpid ( -- i32 ) ;FUNCTION\n"
   SB-APPEND
   S\" package STRIPPED-PRELOADED-RUNTIME-SUBJECT\nprivate\nTASK:#USER CELL TASK:+USER USER-SLOT drop\nvariable USER-CEILING\nTASK:#USER USER-CEILING !\npublic\n: RUN ( -- )\n   PID 0 > 0= if s\" stripped-preloaded-runtime: pid\" 74 die then\n   TASK:#USER USER-CEILING @ <> if s\" stripped-preloaded-runtime: task user\" 74 die then\n   s\" stripped-preloaded-runtime: ok\" type cr ;\n;package\n: MAIN ( -- ) STRIPPED-PRELOADED-RUNTIME-SUBJECT:RUN ;\n"
   SB-APPEND
   SUBJECT$ SB$ WRITE-ALL ;

: MAKER$ ( -- ptr u8 n )
   S\" require tools/aot-build-open.f\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n" ;

: BUILD ( -- )
   PRELOADED-ENGINE:LINKER$ {: linker:ptr linkeru:n :}
   GE-HB-RESET
   linker linkeru GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ s" 0" GE-ARG+
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   linker linkeru MAKER$ TIMEOUT-MS GE-RUN-STDIN
   s" preloaded runtime build" GE-EXPECT-OK
   IMAGE$ EXECUTABLE? 0= if s" preloaded runtime image missing" GE-FAIL then ;

: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   s" preloaded runtime image" GE-EXPECT-OK
   S\" stripped-preloaded-runtime: ok\n" s" preloaded runtime output" GE-EXPECT-OUT
   s" " s" preloaded runtime stderr" GE-EXPECT-ERR ;

: BODY ( -- )
   PREPARE WRITE-SUBJECT BUILD RUN-IMAGE
   s" PASS: stripped preloaded runtime" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
