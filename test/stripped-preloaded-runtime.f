\ A saved linker already contains FFI and TASK before the application opens its
\ window. Its live link must claim those declarations with the current window.
require test/gate-common.f
require test/preloaded-engine.f
require lib/codegen.f

package STRIPPED-PRELOADED-RUNTIME-TEST
private

600000 constant TIMEOUT-MS
create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U
4096 CODEGEN:BUFFER SUBJECT-SRC

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: PREPARE ( -- )
   s" stripped-preloaded-runtime" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" hb-aot-got" IMAGE GT-PATH IMAGE-U ! ;

: WRITE-SUBJECT ( -- )
   SUBJECT-SRC CODEGEN:RESET
   S\" require lib/task.f\nrequire lib/ffi-abi.f\n" SUBJECT-SRC CODEGEN:APPEND-STRING
   HB-TARGET-MACOS? if
      S\" LIBRARY /usr/lib/libSystem.B.dylib\n" SUBJECT-SRC CODEGEN:APPEND-STRING
   else
      S\" LIBRARY libc.so.6\n" SUBJECT-SRC CODEGEN:APPEND-STRING
   then
   S\" FUNCTION: PID getpid ( -- i32 ) ;FUNCTION\n: PRE-CAP ( -- ) PID 0 > 0= if s\" stripped-preloaded-runtime: pre-capture pid\" 74 die then ;\nPRE-CAP\n"
   SUBJECT-SRC CODEGEN:APPEND-STRING
   S\" package STRIPPED-PRELOADED-RUNTIME-SUBJECT\nprivate\nTASK:#USER CELL TASK:+USER USER-SLOT drop\nvariable USER-CEILING\nTASK:#USER USER-CEILING !\nvariable SHORT-ID\nvariable LONG-ID\ns\" /tmp/habu-ffi-capture-path\" FFI:LIBRARY-PATH SHORT-ID !\ns\" /tmp/habu-ffi-capture-path-extra\" FFI:LIBRARY-PATH LONG-ID !\ncreate EXTRA-PATH 64 allot\nvariable PREFIX-U\nvariable EXTRA-N\n: FILL-PATHS ( -- )\n   s\" /tmp/habu-ffi-extra-\" dup PREFIX-U ! EXTRA-PATH swap BYTE-COPY\n   begin FFI:LIBRARY-ROOM? while\n      EXTRA-PATH PREFIX-U @ + {: dst:ptr :}\n      EXTRA-N @ {: idx:n :}\n      idx 100 / [char] 0 + dst c!\n      idx 10 / 10 mod [char] 0 + dst 1+ c!\n      idx 10 mod [char] 0 + dst 2 + c!\n      EXTRA-PATH PREFIX-U @ 3 + FFI:LIBRARY-PATH drop\n      1 EXTRA-N +!\n   repeat ;\npublic\n: RUN ( -- )\n   PID 0 > 0= if s\" stripped-preloaded-runtime: pid\" 74 die then\n   FILL-PATHS\n   s\" /tmp/habu-ffi-capture-path\" FFI:LIBRARY-PATH SHORT-ID @ <> if s\" stripped-preloaded-runtime: short path\" 74 die then\n   s\" /tmp/habu-ffi-capture-path-extra\" FFI:LIBRARY-PATH LONG-ID @ <> if s\" stripped-preloaded-runtime: long path\" 74 die then\n   PID 0 > 0= if s\" stripped-preloaded-runtime: full pid\" 74 die then\n   TASK:#USER USER-CEILING @ <> if s\" stripped-preloaded-runtime: task user\" 74 die then\n   s\" stripped-preloaded-runtime: ok\" type cr ;\n;package\n: MAIN ( -- ) STRIPPED-PRELOADED-RUNTIME-SUBJECT:RUN ;\n"
   SUBJECT-SRC CODEGEN:APPEND-STRING
   SUBJECT$ SUBJECT-SRC CODEGEN:CONTENTS WRITE-ALL ;

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
