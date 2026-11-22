split-str
\ hb-cli-contracts-test.f - checked coverage for hb startup/data-stdin contracts and tools/check.f's usage record.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f tools/hb-cli-contracts-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
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

: HCT-COPY! ( str ^u8 ^n -- ) {: src:^ u dst:^ lenp:^ :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: HCT-ROOT ( -- str )
   HCT-ROOT-BUF HCT-ROOT-U @ ;

: HCT-CHILD ( -- str )
   HCT-CHILD-BUF HCT-CHILD-U @ ;

: HCT-LF ( -- )
   10 SB-APPEND-C ;

: HCT-CHILD$ ( -- str )
   SB-RESET
   s" : MAIN ( -- )" SB-APPEND HCT-LF
   s"    HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}" SB-APPEND HCT-LF
   s"    v SOURCE:READ-STDIN-ALL" SB-APPEND HCT-LF
   s"    v VEC:COUNT ." SB-APPEND HCT-LF
   s"    v VEC:SPAN SPAN:$ type cr" SB-APPEND HCT-LF
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
   s" : HCT-BYTE ( n -- n ) 26 mod 97 + ;" SB-APPEND HCT-LF
   s" : HCT-AFTER? ( ^u8 n -- bool )" SB-APPEND HCT-LF
   s"    {: a:^ u:n :}" SB-APPEND HCT-LF
   s"    u 3 ?do a i + c@  i 3 - HCT-BYTE <> if unloop false exit then loop" SB-APPEND HCT-LF
   s"    true ;" SB-APPEND HCT-LF
   s" : MAIN ( -- )" SB-APPEND HCT-LF
   s"    HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}" SB-APPEND HCT-LF
   S\"    s\" OLD\" v VEC:APPEND" SB-APPEND HCT-LF
   s"    v SOURCE:READ-STDIN-ALL" SB-APPEND HCT-LF
   s"    v VEC:COUNT ." SB-APPEND HCT-LF
   s"    v VEC:SPAN SPAN:$ {: a:^ u:n :}" SB-APPEND HCT-LF
   s"    a 3 type cr" SB-APPEND HCT-LF
   s"    a u HCT-AFTER? bool>n ." SB-APPEND HCT-LF
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
   s" : HCT-ARGV-NEG ( -- ) [: -1 SCRIPT-ARGV$ 2drop ;] catch . ;" SB-APPEND HCT-LF
   s" : HCT-ARGV-ZERO ( -- ) [: 0 SCRIPT-ARGV$ 2drop ;] catch . ;" SB-APPEND HCT-LF
   s" : HCT-ARGV-PAST ( -- ) [: 3 SCRIPT-ARGV$ 2drop ;] catch . ;" SB-APPEND HCT-LF
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
   s" habu-cli-contracts" FS:MKTEMP-DIR {: a:^ u :}
   a u HCT-ROOT-BUF HCT-ROOT-U HCT-COPY!
   HCT-ROOT FS:CLEANUP-TREE+
   HCT-ROOT s" stdin-data-tool.f" HCT-CHILD-BUF FS:JOIN-PATH HCT-CHILD-U !
   HCT-CHILD HCT-CHILD$ FS:WRITE-ALL ;

: HCT-STORE! ( len len PROCESS:ending str -- )
   {: outu:len erru:len oc src:^ srcu:n :}
   outu LEN>N HCT-OUT-U !  erru LEN>N HCT-ERR-U !
   oc MATCH PROCESS:ending
     exited OF HCT-RC ! 0 0= HCT-EXITED ! ENDOF
     signaled OF HCT-RC ! 0 0= 0= HCT-EXITED ! ENDOF
     timeout OF
        src srcu HCT-OUT HCT-OUT-U @ HCT-ERR HCT-ERR-U @ T-TIMED-OUT
     ENDOF
   ;MATCH ;

: HCT-STDIN-LOAD ( -- )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" src/lib/error.f"  >LEN PROC-ARGV+
   s" src/lib/string.f"  >LEN PROC-ARGV+
   s" src/lib/memory.f"  >LEN PROC-ARGV+
   s" src/lib/fs.f"  >LEN PROC-ARGV+
   s" src/lib/heap.f"  >LEN PROC-ARGV+
   s" src/lib/vector.f"  >LEN PROC-ARGV+
   s" src/lib/source.f"  >LEN PROC-ARGV+
   HCT-CHILD  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: HCT-EXPECT-LOAD-STDIN ( -- )
   HCT-STDIN-LOAD
   s" bin/hb" >LEN s" DATA" >LEN HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   HCT-CHILD HCT-STORE!
   HCT-EXITED @ TTRUE HCT-RC @ 0 T=
   HCT-ERR-U @ 0 T= HCT-OUT-U @ 7 T=
   HCT-OUT 7 HCT-WANT$ T$= ;

: HCT-EXPECT-STDIN-GROWS ( -- )
   HCT-CHILD HCT-BIG-CHILD$ FS:WRITE-ALL
   HCT-BIG-FILL
   HCT-STDIN-LOAD
   s" bin/hb" >LEN HCT-BIG HCT-BIG-CAP >LEN HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   HCT-CHILD HCT-STORE!
   s" READ-STDIN-ALL grows the vector and appends after its bytes" T-LABEL
   HCT-EXITED @ TTRUE HCT-RC @ 0 T=
   HCT-ERR-U @ 0 T=
   HCT-OUT HCT-OUT-U @ HCT-BIG-WANT$ T$= ;

\ printf '' | hb --loadx  ->  rc 64, a record that names the flag, with a help
\ that lists --load.
: HCT-EXPECT-UNKNOWN-FLAG ( -- )
   PROC-ARGV-RESET
   s" --loadx"  >LEN PROC-ARGV+
   s" bin/hb" >LEN  HCT-EMPTY 0 >LEN  HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   s" bin/hb --loadx" HCT-STORE!
   s" unknown flag exits 64" T-LABEL
   HCT-EXITED @ TTRUE  HCT-RC @ 64 T=
   s" the record names the offending flag" T-LABEL
   HCT-ERR HCT-ERR-U @ s" error[E-HB-USAGE]: unknown flag `--loadx`" CONTAINS? TTRUE
   s" its help names the known --load flag" T-LABEL
   HCT-ERR HCT-ERR-U @ s" = help: usage: hb file.f [args ...], hb --load" CONTAINS? TTRUE ;

\ printf '1 2 + . cr' | hb --nonsense  ->  rc 64: the flag is rejected before the
\ piped program can run.
: HCT-EXPECT-FLAG-BEFORE-STDIN ( -- )
   PROC-ARGV-RESET
   s" --nonsense"  >LEN PROC-ARGV+
   s" bin/hb" >LEN  s" 1 2 + . cr" >LEN  HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   s" bin/hb --nonsense" HCT-STORE!
   s" unknown flag rejected even with a piped program" T-LABEL
   HCT-EXITED @ TTRUE  HCT-RC @ 64 T=
   s" the piped stdin program did not run" T-LABEL
   HCT-OUT-U @ 0 T= ;

\ printf '' | hb --load tools/check.f -- [--json-errors] --deadline-ms x  ->  rc 64.
: HCT-CHECK-BAD-DEADLINE ( bool -- ) {: json:bool :}
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" tools/check.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   json if s" --json-errors"  >LEN PROC-ARGV+ then
   s" --deadline-ms"  >LEN PROC-ARGV+
   s" x"  >LEN PROC-ARGV+
   s" bin/hb" >LEN  HCT-EMPTY 0 >LEN  HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   json if s" bin/hb --load tools/check.f -- --json-errors --deadline-ms x"
   else s" bin/hb --load tools/check.f -- --deadline-ms x" then HCT-STORE!
   s" a bad --deadline-ms exits 64" T-LABEL
   HCT-EXITED @ TTRUE  HCT-RC @ 64 T= ;

: HCT-EXPECT-CHECK-USAGE ( -- )
   false HCT-CHECK-BAD-DEADLINE
   s" a bad --deadline-ms is one E-CHECK-USAGE record" T-LABEL
   HCT-ERR HCT-ERR-U @ s" error[E-CHECK-USAGE]: " STARTS-WITH? TTRUE
   HCT-ERR HCT-ERR-U @ s" error[" COUNT-SUB 1 T=
   s" the record names the bad value" T-LABEL
   HCT-ERR HCT-ERR-U @ s" not `x`" CONTAINS? TTRUE
   s" its help is the usage line" T-LABEL
   HCT-ERR HCT-ERR-U @ s"   = help: usage: tools/check.f [" CONTAINS? TTRUE
   true HCT-CHECK-BAD-DEADLINE
   s" with --json-errors the record is one JSON line on stderr" T-LABEL
   HCT-ERR HCT-ERR-U @ S\" {\"severity\":\"error\",\"code\":\"E-CHECK-USAGE\"," STARTS-WITH? TTRUE
   HCT-ERR HCT-ERR-U @ 10 COUNT-CHAR 1 T=
   HCT-ERR HCT-ERR-U @ S\" }\n" ENDS-WITH? TTRUE ;

\ printf '' | hb --load FILE, FILE empty  ->  rc 0: a known flag with an empty
\ file loads.
: HCT-EXPECT-LOAD-EMPTY-FILE ( -- )
   HCT-CHILD HCT-EMPTY 0 FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   HCT-CHILD >LEN PROC-ARGV+
   s" bin/hb" >LEN  HCT-EMPTY 0 >LEN  HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   HCT-CHILD HCT-STORE!
   s" --load of an empty file exits 0" T-LABEL
   HCT-EXITED @ TTRUE  HCT-RC @ 0 T= ;


: HCT-ARGV-LOAD ( -- )
   HCT-CHILD HCT-ARGV$ FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   HCT-CHILD >LEN PROC-ARGV+ ;


: HCT-EXPECT-ARGV-NONE ( -- )
   HCT-ARGV-LOAD
   s" bin/hb" >LEN HCT-EMPTY 0 >LEN HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   HCT-CHILD HCT-STORE!
   s" zero script args reject negative and zero indexes" T-LABEL
   HCT-EXITED @ TTRUE HCT-RC @ 0 T=
   HCT-ERR-U @ 0 T=
   HCT-OUT HCT-OUT-U @ HCT-ARGV-NONE-WANT$ T$= ;


: HCT-EXPECT-ARGV-VALUES ( -- )
   HCT-ARGV-LOAD
   s" --" >LEN PROC-ARGV+
   s" alpha" >LEN PROC-ARGV+
   s" " >LEN PROC-ARGV+
   s" omega" >LEN PROC-ARGV+
   s" bin/hb" >LEN HCT-EMPTY 0 >LEN HCT-OUT HCT-CAP >LEN
   HCT-ERR HCT-CAP >LEN PROCESS:WAIT-FOREVER RUN-ARGV-STDIN-CAPTURE-OUTCOME
   HCT-CHILD HCT-STORE!
   s" script argv preserves first, last, and empty values and rejects bounds" T-LABEL
   HCT-EXITED @ TTRUE HCT-RC @ 0 T=
   HCT-ERR-U @ 0 T=
   HCT-OUT HCT-OUT-U @ HCT-ARGV-VALUES-WANT$ T$= ;


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
