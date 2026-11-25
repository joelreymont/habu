\ hb-cli-contracts-test.f - checked coverage for hb startup/data-stdin contracts and tools/check.f's usage record.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/memory.f src/lib/fs.f src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f tools/hb-cli-contracts-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/ending.f

package CLI-CONTRACTS

2048 constant HCT-CAP

variable HCT-ROOT-U
variable HCT-CHILD-U
variable HCT-OUT-U
variable HCT-ERR-U
variable HCT-RC
variable HCT-EXITED

create HCT-ROOT-BUF PATH-CAP allot
create HCT-CHILD-BUF PATH-CAP allot
create HCT-OUT HCT-CAP allot
create HCT-ERR HCT-CAP allot
create HCT-EMPTY 1 allot   \ zero-length stdin

\ Larger than READ-STDIN-ALL's first few rooms, so the vector grows more than once.
20000 constant HCT-BIG-CAP
create HCT-BIG HCT-BIG-CAP allot

: HCT-COPY! ( str ^u8 ^n -- ) {: src:str dst:^ lenp:^ :}
   src STR:LEN PATH-CAP > if E-FS-CAPACITY throw then
   src STR:DATA dst src STR:LEN BYTE-COPY
   src STR:LEN lenp ! ;

: HCT-ROOT ( -- str )
   HCT-ROOT-BUF HCT-ROOT-U @ >str ;

: HCT-CHILD ( -- str )
   HCT-CHILD-BUF HCT-CHILD-U @ >str ;

: HCT-LF ( -- )
   10 SB-APPEND-C ;

: HCT-REQUIRES ( -- )
   s" require src/lib/heap.f" SB-APPEND HCT-LF
   s" require src/lib/vector.f" SB-APPEND HCT-LF
   s" require src/lib/source.f" SB-APPEND HCT-LF ;

: HCT-CHILD$ ( -- str )
   SB-RESET
   HCT-REQUIRES
   s" : MAIN ( -- )" SB-APPEND HCT-LF
   s"    HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}" SB-APPEND HCT-LF
   s"    v SOURCE:READ-STDIN-ALL" SB-APPEND HCT-LF
   s"    v VEC:COUNT ." SB-APPEND HCT-LF
   s"    v VEC:BYTES type cr" SB-APPEND HCT-LF
   s"    v VEC:DELETE ;" SB-APPEND HCT-LF
   s" MAIN" SB-APPEND HCT-LF
   SB$ ;

: HCT-BIG-BYTE ( n -- n )
   26 mod 97 + ;

: HCT-BIG-FILL ( -- )
   HCT-BIG-CAP 0 ?do i HCT-BIG-BYTE HCT-BIG i + c! loop ;

\ Appends standard input, HCT-BIG-BYTE's pattern, to a vector holding `OLD`;
\ prints the count, the first three bytes and whether the pattern follows them.
: HCT-BIG-CHILD$ ( -- str )
   SB-RESET
   HCT-REQUIRES
   s" : HCT-BYTE ( n -- n ) 26 mod 97 + ;" SB-APPEND HCT-LF
   s" : HCT-AFTER? ( str -- bool )" SB-APPEND HCT-LF
   s"    {: s:str :}" SB-APPEND HCT-LF
   s"    s STR:LEN 3 ?do s STR:DATA i + c@  i 3 - HCT-BYTE <>" SB-APPEND HCT-LF
   s"       if unloop false exit then" SB-APPEND HCT-LF
   s"    loop" SB-APPEND HCT-LF
   s"    true ;" SB-APPEND HCT-LF
   s" : MAIN ( -- )" SB-APPEND HCT-LF
   s"    HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}" SB-APPEND HCT-LF
   S\"    s\" OLD\" v VEC:APPEND" SB-APPEND HCT-LF
   s"    v SOURCE:READ-STDIN-ALL" SB-APPEND HCT-LF
   s"    v VEC:COUNT ." SB-APPEND HCT-LF
   s"    v VEC:BYTES {: s:str :}" SB-APPEND HCT-LF
   s"    s STR:DATA 3 >str type cr" SB-APPEND HCT-LF
   s"    s HCT-AFTER? bool>n ." SB-APPEND HCT-LF

   s"    v VEC:DELETE ;" SB-APPEND HCT-LF
   s" MAIN" SB-APPEND HCT-LF
   SB$ ;

\ The count (HCT-BIG-CAP plus OLD's three bytes), OLD and HCT-AFTER?'s true.
: HCT-BIG-WANT$ ( -- str )
   SB-RESET
   s" 20003" SB-APPEND HCT-LF
   s" OLD" SB-APPEND HCT-LF
   s" -1" SB-APPEND HCT-LF
   SB$ ;

: HCT-WANT$ ( -- str )
   SB-RESET
   s" 4" SB-APPEND HCT-LF
   s" DATA" SB-APPEND HCT-LF
   SB$ ;


: HCT-ARGV$ ( -- str )
   SB-RESET
   s" : HCT-ARGV-NEG ( -- ) [: -1 SCRIPT-ARGV$ drop ;] catch . ;" SB-APPEND HCT-LF
   s" : HCT-ARGV-ZERO ( -- ) [: 0 SCRIPT-ARGV$ drop ;] catch . ;" SB-APPEND HCT-LF
   s" : HCT-ARGV-PAST ( -- ) [: 3 SCRIPT-ARGV$ drop ;] catch . ;" SB-APPEND HCT-LF
   s" : HCT-ARGV-SHOW ( -- )" SB-APPEND HCT-LF
   s"    SCRIPT-ARGC ." SB-APPEND HCT-LF
   s"    HCT-ARGV-NEG" SB-APPEND HCT-LF
   s"    SCRIPT-ARGC 0= if HCT-ARGV-ZERO exit then" SB-APPEND HCT-LF
   s"    0 SCRIPT-ARGV$ type cr" SB-APPEND HCT-LF
   s"    1 SCRIPT-ARGV$ type cr" SB-APPEND HCT-LF
   s"    2 SCRIPT-ARGV$ type cr" SB-APPEND HCT-LF
   s"    HCT-ARGV-PAST ;" SB-APPEND HCT-LF
   s" HCT-ARGV-SHOW" SB-APPEND HCT-LF
   SB$ ;


: HCT-ARGV-NONE-WANT$ ( -- str )
   SB-RESET
   s" 0" SB-APPEND HCT-LF
   s" 7180" SB-APPEND HCT-LF
   s" 7180" SB-APPEND HCT-LF
   SB$ ;


: HCT-ARGV-VALUES-WANT$ ( -- str )
   SB-RESET
   s" 3" SB-APPEND HCT-LF
   s" 7180" SB-APPEND HCT-LF
   s" alpha" SB-APPEND HCT-LF
   HCT-LF
   s" omega" SB-APPEND HCT-LF
   s" 7180" SB-APPEND HCT-LF
   SB$ ;


: HCT-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-cli-contracts" FS:MKTEMP-DIR {: dir:str :}
   dir HCT-ROOT-BUF HCT-ROOT-U HCT-COPY!
   HCT-ROOT FS:CLEANUP-TREE+
   HCT-ROOT s" stdin-data-tool.f" HCT-CHILD-BUF FS:JOIN-PATH HCT-CHILD-U !
   HCT-CHILD HCT-CHILD$ FS:WRITE-ALL ;

: HCT-STORE! ( len len PROCESS:ending str -- )
   {: outu:len erru:len oc src:str :}
   outu LEN>N HCT-OUT-U !  erru LEN>N HCT-ERR-U !
   oc MATCH PROCESS:ending
     exited OF HCT-RC ! 0 0= HCT-EXITED ! ENDOF
     signaled OF HCT-RC ! 0 0= 0= HCT-EXITED ! ENDOF
     timeout OF
        src HCT-OUT HCT-OUT-U @ >str HCT-ERR HCT-ERR-U @ >str T-TIMED-OUT
     ENDOF
   ;MATCH ;

: HCT-STDIN-LOAD ( -- )
   PROC-ARGV-RESET
   s" --load"  PROC-ARGV+
   s" src/lib/error.f"  PROC-ARGV+
   s" src/lib/string.f"  PROC-ARGV+
   s" src/lib/memory.f"  PROC-ARGV+
   s" src/lib/fs.f"  PROC-ARGV+
   s" src/lib/heap.f"  PROC-ARGV+
   s" src/lib/vector.f"  PROC-ARGV+
   s" src/lib/source.f"  PROC-ARGV+
   HCT-CHILD  PROC-ARGV+
   s" --"  PROC-ARGV+ ;

: HCT-EXPECT-LOAD-STDIN ( -- )
   HCT-STDIN-LOAD
   s" bin/hb" s" DATA" HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   HCT-CHILD HCT-STORE!
   HCT-EXITED @ TTRUE HCT-RC @ 0 T=
   HCT-ERR-U @ 0 T= HCT-OUT-U @ 7 T=
   HCT-OUT 7 >str HCT-WANT$ T$= ;

: HCT-EXPECT-STDIN-GROWS ( -- )
   HCT-CHILD HCT-BIG-CHILD$ FS:WRITE-ALL
   HCT-BIG-FILL
   HCT-STDIN-LOAD
   s" bin/hb" HCT-BIG HCT-BIG-CAP >str HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   HCT-CHILD HCT-STORE!
   s" READ-STDIN-ALL grows the vector and appends after its bytes" T-LABEL
   HCT-EXITED @ TTRUE HCT-RC @ 0 T=
   HCT-ERR-U @ 0 T=
   HCT-OUT HCT-OUT-U @ >str HCT-BIG-WANT$ T$= ;

\ printf '' | hb --loadx  ->  rc 64, a record that names the flag, with a help
\ that lists --load.
: HCT-EXPECT-UNKNOWN-FLAG ( -- )
   PROC-ARGV-RESET
   s" --loadx"  PROC-ARGV+
   s" bin/hb"  HCT-EMPTY 0 >str  HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   s" bin/hb --loadx" HCT-STORE!
   s" unknown flag exits 64" T-LABEL
   HCT-EXITED @ TTRUE  HCT-RC @ 64 T=
   s" the record names the offending flag" T-LABEL
   HCT-ERR HCT-ERR-U @ >str s" error[E-HB-USAGE]: unknown flag `--loadx`" STR:CONTAINS? TTRUE
   s" its help names the known --load flag" T-LABEL
   HCT-ERR HCT-ERR-U @ >str s" = help: usage: hb file.f [args ...], hb --load" STR:CONTAINS? TTRUE ;

\ printf '1 2 + . cr' | hb --nonsense  ->  rc 64: the flag is rejected before the
\ piped program can run.
: HCT-EXPECT-FLAG-BEFORE-STDIN ( -- )
   PROC-ARGV-RESET
   s" --nonsense"  PROC-ARGV+
   s" bin/hb"  s" 1 2 + . cr"  HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   s" bin/hb --nonsense" HCT-STORE!
   s" unknown flag rejected even with a piped program" T-LABEL
   HCT-EXITED @ TTRUE  HCT-RC @ 64 T=
   s" the piped stdin program did not run" T-LABEL
   HCT-OUT-U @ 0 T= ;

\ printf '' | hb --load tools/check.f -- [--json-errors] --deadline-ms x  ->  rc 64.
: HCT-CHECK-BAD-DEADLINE ( bool -- ) {: json:bool :}
   PROC-ARGV-RESET
   s" --load"  PROC-ARGV+
   s" tools/check.f"  PROC-ARGV+
   s" --"  PROC-ARGV+
   json if s" --json-errors"  PROC-ARGV+ then
   s" --deadline-ms"  PROC-ARGV+
   s" x"  PROC-ARGV+
   s" bin/hb"  HCT-EMPTY 0 >str  HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   json if s" bin/hb --load tools/check.f -- --json-errors --deadline-ms x"
   else s" bin/hb --load tools/check.f -- --deadline-ms x" then HCT-STORE!
   s" a bad --deadline-ms exits 64" T-LABEL
   HCT-EXITED @ TTRUE  HCT-RC @ 64 T= ;

: HCT-EXPECT-CHECK-USAGE ( -- )
   false HCT-CHECK-BAD-DEADLINE
   s" a bad --deadline-ms is one E-CHECK-USAGE record" T-LABEL
   HCT-ERR HCT-ERR-U @ >str s" error[E-CHECK-USAGE]: " STR:STARTS-WITH? TTRUE
   HCT-ERR HCT-ERR-U @ >str s" error[" STR:COUNT-SUB 1 T=
   s" the record names the bad value" T-LABEL
   HCT-ERR HCT-ERR-U @ >str s" not `x`" STR:CONTAINS? TTRUE
   s" its help is the usage line" T-LABEL
   HCT-ERR HCT-ERR-U @ >str s"   = help: usage: tools/check.f [" STR:CONTAINS? TTRUE
   true HCT-CHECK-BAD-DEADLINE
   s" with --json-errors the record is one JSON line on stderr" T-LABEL
   HCT-ERR HCT-ERR-U @ >str S\" {\"severity\":\"error\",\"code\":\"E-CHECK-USAGE\"," STR:STARTS-WITH? TTRUE
   HCT-ERR HCT-ERR-U @ >str 10 STR:COUNT-CHAR 1 T=
   HCT-ERR HCT-ERR-U @ >str S\" }\n" STR:ENDS-WITH? TTRUE ;

\ printf '' | hb --load FILE, FILE empty  ->  rc 0: a known flag with an empty
\ file loads.
: HCT-EXPECT-LOAD-EMPTY-FILE ( -- )
   HCT-CHILD HCT-EMPTY 0 >str FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load"  PROC-ARGV+
   HCT-CHILD PROC-ARGV+
   s" bin/hb"  HCT-EMPTY 0 >str  HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   HCT-CHILD HCT-STORE!
   s" --load of an empty file exits 0" T-LABEL
   HCT-EXITED @ TTRUE  HCT-RC @ 0 T= ;


: HCT-ARGV-LOAD ( -- )
   HCT-CHILD HCT-ARGV$ FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   HCT-CHILD PROC-ARGV+ ;


: HCT-EXPECT-ARGV-NONE ( -- )
   HCT-ARGV-LOAD
   s" bin/hb" HCT-EMPTY 0 >str HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   HCT-CHILD HCT-STORE!
   s" zero script args reject negative and zero indexes" T-LABEL
   HCT-EXITED @ TTRUE HCT-RC @ 0 T=
   HCT-ERR-U @ 0 T=
   HCT-OUT HCT-OUT-U @ >str HCT-ARGV-NONE-WANT$ T$= ;


: HCT-EXPECT-ARGV-VALUES ( -- )
   HCT-ARGV-LOAD
   s" --" PROC-ARGV+
   s" alpha" PROC-ARGV+
   s" " PROC-ARGV+
   s" omega" PROC-ARGV+
   s" bin/hb" HCT-EMPTY 0 >str HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   HCT-CHILD HCT-STORE!
   s" script argv preserves first, last, and empty values and rejects bounds" T-LABEL
   HCT-EXITED @ TTRUE HCT-RC @ 0 T=
   HCT-ERR-U @ 0 T=
   HCT-OUT HCT-OUT-U @ >str HCT-ARGV-VALUES-WANT$ T$= ;


: HCT-MAIN ( -- )
   T-RESET
   HCT-PREPARE
   HCT-EXPECT-LOAD-STDIN
   HCT-EXPECT-STDIN-GROWS
   HCT-EXPECT-UNKNOWN-FLAG
   HCT-EXPECT-FLAG-BEFORE-STDIN
   HCT-EXPECT-CHECK-USAGE
   HCT-EXPECT-LOAD-EMPTY-FILE
   HCT-EXPECT-ARGV-NONE
   HCT-EXPECT-ARGV-VALUES
   FS:CLEANUP-RUN
   HCT-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" hb-cli-contracts-test: ok" type cr ;

HCT-MAIN

;package
