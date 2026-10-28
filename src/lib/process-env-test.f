\ process-env-test.f - focused tests for src/lib/process-env.f.
\ Run: bin/hb --load src/lib/process-env-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/outcome.f
require src/lib/test/guard-page.f
require src/lib/process-env.f
require src/lib/task.f

$8000 constant PET-CAP
$20000 constant PET-EARLY-IN-CAP
$1388 constant PET-HB-TIMEOUT-MS
$3E8 constant PET-CMD-TIMEOUT-MS
$32 constant PET-SHORT-TIMEOUT-MS
$2 constant PET-ENOENT
$258 constant PET-BIG-N                  \ variables the big-environment child must inherit
$9 constant PET-BIG-PREFIX-U             \ "HABU_BIG_"
$C constant PET-BIG-NAME-U               \ prefix plus three digits

create PET-BIG-NAME PET-BIG-NAME-U allot
create PET-OUT PET-CAP allot
create PET-ERR PET-CAP allot
create PET-PATH FS-PATH-CAP allot
create PET-EARLY-IN PET-EARLY-IN-CAP allot
variable PET-I
variable PET-START-NS

PTR-VARIABLE PET-HOME
variable PET-HOME-U
PTR-VARIABLE PET-GETENV-PATH-PTR
variable PET-GETENV-PATH-U
TASK:MIN-STACK TASK:TASK PET-ENV-READER-A
TASK:MIN-STACK TASK:TASK PET-ENV-READER-B
TASK:SEMAPHORE PET-ENV-START
100000 constant PET-ENV-ROUNDS

: PET-RESET ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-DEFAULT-RESET ;

: PET-EARLY-IN! ( -- )
   0 PET-I !
   begin PET-I @ PET-EARLY-IN-CAP < while
      $61 PET-EARLY-IN PET-I @ + c!
      PET-I @ 1+ PET-I !
   repeat ;

: PET-U-TYPE ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + emit ;

: PET-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: PET-FIND>N ( option<len> -- n bool )                 \ flatten for the test asserts
   MATCH option
     none OF 0 0 0= 0= ENDOF
     some OF LEN>N 0 0= ENDOF
   ;MATCH ;

: PET-ENV+ ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu val:ptr valu :}
   name nameu >LEN val valu >LEN PROC-ENV+ ;

: PET-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-ENV-CAPTURE
   PET-CAPTURE>N ;

: PET-OUTCOME ( ptr u8 n ptr u8 n ptr u8 n n -- len len outcome )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN out outcap >LEN err errcap >LEN timeout >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: PET-STDIN-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN in inu >LEN out outcap >LEN err errcap >LEN timeout >MS
   RUN-ARGV-ENV-STDIN-CAPTURE PET-CAPTURE>N ;

: PET-STDIN-OUTCOME ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- len len outcome )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN in inu >LEN out outcap >LEN err errcap >LEN timeout >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME ;

: PET-FIND-IN-PATH ( ptr u8 n ptr u8 n ptr u8 -- n bool )
   {: cmd:ptr cmdu path:ptr pathu dst:ptr :}
   cmd cmdu >LEN path pathu >LEN dst FIND-EXECUTABLE-IN-PATH
   PET-FIND>N ;

: PET-RESOLVE ( ptr u8 n ptr u8 -- n )
   {: cmd:ptr cmdu dst:ptr :}
   cmd cmdu >LEN dst RESOLVE-EXECUTABLE LEN>N ;

: PET-SB-N ( n -- ) {: v:n :}
   v 0 < if E-STR-BOUNDS throw then
   v 10 >= if v 10 / RECURSE then
   v 10 mod STR-ZERO + SB-APPEND-C ;

\ HABU_BIG_001 .. HABU_BIG_600: fixed width, so the child can count them by
\ prefix and the parent never has to parse the name back.
: PET-BIG-NAME$ ( n -- ptr u8 n ) {: i:n :}
   s" HABU_BIG_" PET-BIG-NAME swap BYTE-COPY
   i 100 / STR-ZERO + PET-BIG-NAME PET-BIG-PREFIX-U + c!
   i 100 mod 10 / STR-ZERO + PET-BIG-NAME PET-BIG-PREFIX-U 1 + + c!
   i 10 mod STR-ZERO + PET-BIG-NAME PET-BIG-PREFIX-U 2 + + c!
   PET-BIG-NAME PET-BIG-NAME-U ;

: PET-ENV-CMD$ ( -- ptr u8 n )
   s" /usr/bin/env" ;

: PET-ALPHA-LINE$ ( -- ptr u8 n )
   SB-RESET
   s" HABU_PROC_ENV_TEST=alpha" SB-APPEND
   $0A SB-APPEND-C
   SB$ ;

: PET-CASE ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   mono-ns PET-START-NS !
   q execute
   s" PASS: " type label labelu type
   s"  (" type mono-ns PET-START-NS @ - PROC-NS-PER-MS / PET-U-TYPE s" ms)" type cr ;

: PET-RUN-ENV-CHILD ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" s" alpha" PET-ENV+
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= {: outu:n :}
   PET-OUT outu PET-ALPHA-LINE$ T$=
   PROC-ARGV-N @ 0 T=
   PROC-ENV-N @ 0 T= ;

: PET-RUN-EMPTY-ENV-CHILD ( -- )
   PET-RESET
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= 0 T= ;

: PET-HAS-ENV-LINE-BODY
   ( ptr u8 n ptr u8 n ptr u8 n n ptr u8 NUM:alloc-byte-len -- bool )
   drop
   {: out:ptr outu:n name:ptr nameu:n val:ptr valu:n lineu:n expected:ptr :}
   $0A expected c!
   name expected 1+ nameu BYTE-COPY
   $3D expected nameu 1+ + c!
   val expected nameu 2 + + valu BYTE-COPY
   $0A expected lineu + c!
   out outu expected 1+ lineu STARTS-WITH?
   out outu expected lineu 1+ CONTAINS? or ;

: PET-HAS-ENV-LINE? ( ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: out:ptr outu:n name:ptr nameu:n val:ptr valu:n :}
   nameu valu + 2 + {: lineu:n :}
   out outu name nameu val valu lineu
   lineu 1+ MEM:BYTES-ALLOC-LEN [: PET-HAS-ENV-LINE-BODY ;] MEM:WITH-BYTES ;

: PET-EXPECT-INHERITED ( n ptr u8 n -- ) {: outu:n name:ptr nameu:n :}
   name nameu GETENV {: val:ptr valu:n :}
   valu 0= if exit then
   PET-OUT outu name nameu val valu PET-HAS-ENV-LINE? TTRUE ;

: PET-ENV-LINE-BOUNDARIES ( -- )
   S\" HOME=alpha\nPATH=beta\n" s" HOME" s" alpha" PET-HAS-ENV-LINE? TTRUE
   S\" X=1\nHOME=alpha\nY=2\n" s" HOME" s" alpha" PET-HAS-ENV-LINE? TTRUE
   S\" SOME_HOME=alpha\n" s" HOME" s" alpha" PET-HAS-ENV-LINE? TFALSE
   S\" OTHER=HOME=alpha\n" s" HOME" s" alpha" PET-HAS-ENV-LINE? TFALSE
   S\" HOME=prefix-alpha\n" s" HOME" s" alpha" PET-HAS-ENV-LINE? TFALSE ;

\ Two readers use different names while scanning the same captured envp.
: PET-GETENV-CHECK ( ptr u8 n ptr u8 n -- n )
   {: name:ptr nameu:n expected:ptr expectedu:n :}
   PET-ENV-ROUNDS 0 ?do
      name nameu GETENV expected expectedu STR= 0= if unloop 0 exit then
   loop
   1 ;

: PET-GETENV-HOME ( -- )
   PET-ENV-START TASK:WAIT
   s" HOME" PET-HOME @ PET-HOME-U @ PET-GETENV-CHECK TASK:RETURN ;

: PET-GETENV-PATH ( -- )
   PET-ENV-START TASK:WAIT
   s" PATH" PET-GETENV-PATH-PTR @ PET-GETENV-PATH-U @ PET-GETENV-CHECK TASK:RETURN ;

: PET-GETENV-JOIN ( result<n,n> -- )
   MATCH result
     ok OF 1 T= ENDOF
     err OF 0 T= ENDOF
   ;MATCH ;

: PET-GETENV-CONCURRENT ( -- )
   s" HOME" GETENV PET-HOME-U ! PET-HOME !
   s" PATH" GETENV PET-GETENV-PATH-U ! PET-GETENV-PATH-PTR !
   PET-HOME-U @ 0 > TTRUE
   PET-GETENV-PATH-U @ 0 > TTRUE
   0 PET-ENV-START TASK:SEMAPHORE-INIT
   ['] PET-GETENV-HOME PET-ENV-READER-A TASK:ACTIVATE
   ['] PET-GETENV-PATH PET-ENV-READER-B TASK:ACTIVATE
   PET-ENV-START TASK:SIGNAL PET-ENV-START TASK:SIGNAL
   PET-ENV-READER-A TASK:JOIN PET-GETENV-JOIN
   PET-ENV-READER-B TASK:JOIN PET-GETENV-JOIN
   PET-ENV-START TASK:SEMAPHORE-DESTROY ;

: PET-INHERIT-ENV-OUT ( n -- ) {: outu:n :}
   PET-OUT outu PET-ALPHA-LINE$ CONTAINS? TTRUE
   outu s" HOME" PET-EXPECT-INHERITED
   outu s" PATH" PET-EXPECT-INHERITED ;

: PET-RUN-INHERIT-ENV-CHILD ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" s" alpha" PET-ENV+
   PROC-ENV-INHERIT-MISSING
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= {: outu:n :}
   outu PET-INHERIT-ENV-OUT ;

: PET-DEFAULT-ENV-CHILD ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" >LEN s" alpha" >LEN PROC-ENV-DEFAULT+
   PROC-ENV-INHERIT-MISSING
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= {: outu:n :}
   outu PET-INHERIT-ENV-OUT ;

: PET-DEFAULT-LOOKUP ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" >LEN s" alpha" >LEN PROC-ENV-DEFAULT+
   s" HABU_PROC_ENV_TEST" >LEN PROC-ENV-DEFAULT$? TTRUE
   LEN>N s" alpha" T$=
   s" HABU_PROC_ENV_MISSING" >LEN PROC-ENV-DEFAULT$? TFALSE
   2drop ;

: PET-EXPLICIT-BEATS-DEFAULT ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" >LEN s" wrong" >LEN PROC-ENV-DEFAULT+
   s" HABU_PROC_ENV_TEST" s" alpha" PET-ENV+
   PROC-ENV-INHERIT-MISSING
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= {: outu:n :}
   PET-OUT outu s" HABU_PROC_ENV_TEST=wrong" CONTAINS? TFALSE
   outu PET-INHERIT-ENV-OUT ;

\ PROC-ENV-SET vs PROC-ENV+: SET replaces an existing row in place (one row
\ survives, the child sees only the last value), while PROC-ENV+ appends and
\ never deduplicates. SET on an absent name appends like PROC-ENV+.
: PET-SET-REPLACES ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" s" wrong" PET-ENV+
   PROC-ENV-N @ COUNT>N 1 T=
   s" HABU_PROC_ENV_TEST" >LEN s" alpha" >LEN PROC-ENV-SET
   PROC-ENV-N @ COUNT>N 1 T=
   PROC-ENV-INHERIT-MISSING
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= {: outu:n :}
   PET-OUT outu s" HABU_PROC_ENV_TEST=wrong" CONTAINS? TFALSE
   outu PET-INHERIT-ENV-OUT ;

: PET-SET-APPENDS-NEW ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" >LEN s" alpha" >LEN PROC-ENV-SET
   PROC-ENV-N @ COUNT>N 1 T=
   PROC-ENV-INHERIT-MISSING
   PET-ENV-CMD$ PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   0 T= 0 T= {: outu:n :}
   outu PET-INHERIT-ENV-OUT ;

: PET-APPEND-KEEPS-DUP ( -- )
   PET-RESET
   s" HABU_PROC_ENV_TEST" s" wrong" PET-ENV+
   s" HABU_PROC_ENV_TEST" s" alpha" PET-ENV+
   PROC-ENV-N @ COUNT>N 2 T= ;

: PET-RUN-ENV-OUTCOME-FALSE ( -- )
   PET-RESET
   s" /usr/bin/false" PET-OUT PET-CAP PET-ERR PET-CAP PET-CMD-TIMEOUT-MS PET-OUTCOME
   {: outu:len erru:len oc :}
   s" /usr/bin/false" PET-OUT outu LEN>N PET-ERR erru LEN>N oc 1 T-OUTCOME-EXITED=
   erru LEN>N 0 T= outu LEN>N 0 T= ;

\ Direct both-arm coverage for RUN-ARGV-ENV-CAPTURE: true -> ok(captured),
\ false -> err(failed) carrying lengths + the completion code.
: PET-RUN-ARGV-ENV-CAPTURE-RESULT ( -- )
   PET-RESET
   s" /usr/bin/true" >LEN PET-OUT PET-CAP >LEN PET-ERR PET-CAP >LEN PET-CMD-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N 0 T=  e LEN>N 0 T= ENDOF
     err OF PCAP-FAILED:UNMAKE 2drop drop 1 0 T= ENDOF
   ;MATCH
   PET-RESET
   s" /usr/bin/false" >LEN PET-OUT PET-CAP >LEN PET-ERR PET-CAP >LEN PET-CMD-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop 1 0 T= ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N 0 T=  e LEN>N 0 T=  c RC>N 1 T= ENDOF
   ;MATCH ;

: PET-RUN-ENV-OUTCOME-TIMEOUT ( -- )
   PET-RESET
   s" 5"  >LEN PROC-ARGV+
   s" /bin/sleep" PET-OUT PET-CAP PET-ERR PET-CAP PET-SHORT-TIMEOUT-MS PET-OUTCOME
   T-OUTCOME-TIMEOUT LEN>N 0 T= LEN>N 0 T= ;

: PET-RUN-ENV-STDIN-OUTCOME ( -- )
   PET-RESET
   s" /bin/cat" s" env-stdin" PET-OUT PET-CAP PET-ERR PET-CAP PET-CMD-TIMEOUT-MS PET-STDIN-OUTCOME
   {: outu:len erru:len oc :}
   s" /bin/cat" PET-OUT outu LEN>N PET-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   erru LEN>N 0 T= outu LEN>N 9 T=
   PET-OUT 9 s" env-stdin" T$= ;

: PET-RUN-ENV-STDIN-FALSE-LARGE ( -- )
   PET-RESET
   PET-EARLY-IN!
   s" /usr/bin/false" PET-EARLY-IN PET-EARLY-IN-CAP
   PET-OUT PET-CAP PET-ERR PET-CAP PET-CMD-TIMEOUT-MS PET-STDIN-CAPTURE
   1 T= 0 T= 0 T= ;

: PET-RUN-ENV-STDIN-OUTCOME-FALSE-LARGE ( -- )
   PET-RESET
   PET-EARLY-IN!
   s" /usr/bin/false" PET-EARLY-IN PET-EARLY-IN-CAP
   PET-OUT PET-CAP PET-ERR PET-CAP PET-CMD-TIMEOUT-MS PET-STDIN-OUTCOME
   {: outu:len erru:len oc :}
   s" /usr/bin/false" PET-OUT outu LEN>N PET-ERR erru LEN>N oc 1 T-OUTCOME-EXITED=
   erru LEN>N 0 T= outu LEN>N 0 T= ;

: PET-RUN-ENV-STDIN-OUTCOME-TIMEOUT ( -- )
   PET-RESET
   s" 5"  >LEN PROC-ARGV+
   s" /bin/sleep" s" " PET-OUT PET-CAP PET-ERR PET-CAP PET-SHORT-TIMEOUT-MS PET-STDIN-OUTCOME
   T-OUTCOME-TIMEOUT LEN>N 0 T= LEN>N 0 T= ;

package PET-GROW-BOTH

DYNAMIC-BUFFER OUT u8
DYNAMIC-BUFFER ERR u8

: OUT-ROOM ( n -- ptr u8 )
   OUT-RESERVE 0 OUT ;

: ERR-ROOM ( n -- ptr u8 )
   ERR-RESERVE 0 ERR ;

: SHELL ( ptr u8 n -- )
   PET-RESET
   s" -c" >LEN PROC-ARGV+
   >LEN PROC-ARGV+ ;

: CAPTURE ( ptr u8 len [ n -- ptr u8 ] [ n -- ptr u8 ] ms -- len len outcome )
   {: in:ptr inu out err timeout :}
   s" /bin/sh" >LEN in inu out err timeout
   RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME ;

\ Failures to distinguish: either pipe truncated or mixed with the other,
\ stdin stalled behind full output pipes, partial output lost on a deadline,
\ and a throwing sink leaving its child or descriptors alive. The tee child
\ copies 128 KiB stdin to both pipes, then gives each pipe a distinct tail and
\ exits nonzero, so complete output must survive unsuccessful children too.
: LARGE ( -- )
   PET-EARLY-IN!
   s" /usr/bin/tee /dev/stderr; printf out; printf err >&2; exit 7" SHELL
   PET-EARLY-IN PET-EARLY-IN-CAP >LEN [: OUT-ROOM ;] [: ERR-ROOM ;]
   PET-HB-TIMEOUT-MS >MS CAPTURE {: outu:len erru:len oc :}
   oc PROC-OUTCOME>RC RC>N 7 T=
   outu LEN>N PET-EARLY-IN-CAP 3 + T=
   erru LEN>N PET-EARLY-IN-CAP 3 + T=
   0 OUT PET-EARLY-IN-CAP PET-EARLY-IN PET-EARLY-IN-CAP T$=
   0 ERR PET-EARLY-IN-CAP PET-EARLY-IN PET-EARLY-IN-CAP T$=
   PET-EARLY-IN-CAP OUT 3 s" out" T$=
   PET-EARLY-IN-CAP ERR 3 s" err" T$= ;

: TIMEOUT ( -- )
   s" printf before-out; printf before-err >&2; exec /bin/sleep 5" SHELL
   s" " >LEN [: OUT-ROOM ;] [: ERR-ROOM ;] 300 >MS CAPTURE
   T-OUTCOME-TIMEOUT LEN>N 10 T= LEN>N 10 T=
   0 OUT 10 s" before-out" T$=
   0 ERR 10 s" before-err" T$= ;

\ Each sink yields while the child fills the other pipe. Readiness therefore
\ continues past the deadline: poll(0) alone cannot end this capture.
: BUSY-TIMEOUT ( -- )
   s" /usr/bin/yes before-out & exec /usr/bin/yes before-err >&2" SHELL
   s" " >LEN
   [: OUT-ROOM 10 >MS TASK:SLEEP ;]
   [: ERR-ROOM 10 >MS TASK:SLEEP ;] 300 >MS CAPTURE
   {: outu:len erru:len oc :}
   oc T-OUTCOME-TIMEOUT
   outu LEN>N 10 > TTRUE erru LEN>N 10 > TTRUE
   0 OUT 10 s" before-out" T$=
   0 ERR 10 s" before-err" T$= ;

: REFUSE ( n -- ptr u8 )
   drop E-STR-CAPACITY throw ;

: SINK-FAILS ( [ n -- ptr u8 ] [ n -- ptr u8 ] -- )
   {: out err :}
   s" printf out; printf err >&2; exec /bin/sleep 5" SHELL
   s" " >LEN out err PET-HB-TIMEOUT-MS >MS CAPTURE
   drop 2drop ;

: CLEANED ( -- )
   PROC-PID @ PROC-NO-PID T=
   PROC-CAPTURE-DONE? TTRUE
   PROC-IN-W @ PROC-NO-FD T= ;

public
: RUN ( -- )
   LARGE
   TIMEOUT
   BUSY-TIMEOUT
   CLEANED
   [: [: REFUSE ;] [: ERR-ROOM ;] SINK-FAILS ;] E-STR-CAPACITY TTHROWSQ
   CLEANED
   [: [: OUT-ROOM ;] [: REFUSE ;] SINK-FAILS ;] E-STR-CAPACITY TTHROWSQ
   CLEANED
   OUT-RELEASE ERR-RELEASE ;

;package

: PET-SPAWN-RAW-MISSING ( -- )
   PET-RESET
   s" /no/such/habu-process-env-test" >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   pathz argv envp -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-ENV-RAW PID>N {: code:n :}
   PROC-ARGV-ENV-RESET
   code 0 < TTRUE
   HB-TARGET-MACOS? if code PET-ENOENT negate T= then ;

: PET-SPAWN-RAW-TRUE-ONE ( -- )
   PET-RESET
   s" /usr/bin/true" >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   pathz argv envp -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-ENV-RAW {: pid:pid :}
   PROC-ARGV-ENV-RESET
   pid PID>N 0 > TTRUE
   pid PROC-WAIT-RC MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH ;

: PET-SPAWN-RAW-TRUE ( -- )
   0 begin dup 16 < while
      PET-SPAWN-RAW-TRUE-ONE
      1+
   repeat drop ;

\ A 600-variable environment is larger than the whole table used to be, so this
\ case only runs at all once the table is sized from the parent's own envp. The
\ child reports both how many of them reached it and how many its own
\ grandchild saw.
: PET-BIG-ENV-BUILD ( -- )
   PET-RESET
   0 PET-I !
   begin PET-I @ PET-BIG-N < while
      PET-I @ 1 + PET-BIG-NAME$ s" 1" PET-ENV+
      PET-I @ 1 + PET-I !
   repeat
   PROC-ENV-INHERIT-MISSING ;

: PET-BIG-EXPECT$ ( -- ptr u8 n )
   SB-RESET
   s" big-env-child " SB-APPEND
   PET-BIG-N PET-SB-N
   s"  " SB-APPEND
   PET-BIG-N PET-SB-N
   SB$ ;

: PET-BIG-INHERIT-CHILD ( -- )
   PET-BIG-ENV-BUILD
   PROC-ENV-N @ COUNT>N PET-BIG-N < TFALSE
   s" --load" >LEN PROC-ARGV+
   s" test/process-env-big-child.f" >LEN PROC-ARGV+
   s" bin/hb" PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   {: outu:n erru:n code:n :}
   code 0 T=
   PET-OUT outu PET-BIG-EXPECT$ CONTAINS? TTRUE ;

: PET-BAD-ENV-NAME ( -- )
   PET-RESET
   s" BAD=NAME" s" x" PET-ENV+ ;

: PET-BAD-ENV-ENTRY ( -- )
   PET-RESET
   s" MISSING_EQUALS"  >LEN PROC-ENV-ENTRY+ ;

: PET-BAD-ENV-EMPTY ( -- )
   PET-RESET
   s" " s" x" PET-ENV+ ;

: PET-PATH-FIND-HB ( -- )
   s" hb" s" bin" PET-PATH PET-FIND-IN-PATH TTRUE
   PET-PATH swap s" bin/hb" T$= ;

: PET-PATH-DIRECT-HB ( -- )
   s" bin/hb" s" nowhere" PET-PATH PET-FIND-IN-PATH TTRUE
   PET-PATH swap s" bin/hb" T$= ;

: PET-PATH-MISSING ( -- )
   s" no-habu-process-env-test" s" bin" PET-PATH PET-FIND-IN-PATH TFALSE
   drop ;

: PET-RESOLVE-MISSING ( -- )
   s" no-habu-process-env-test" PET-PATH PET-RESOLVE drop ;

: PET-BAD-ENV-NAME-THROWS ( -- )
   [: PET-BAD-ENV-NAME ;] E-PROC-ENV TTHROWSQ ;

: PET-BAD-ENV-ENTRY-THROWS ( -- )
   [: PET-BAD-ENV-ENTRY ;] E-PROC-ENV TTHROWSQ ;

: PET-BAD-ENV-EMPTY-THROWS ( -- )
   [: PET-BAD-ENV-EMPTY ;] E-PROC-ENV TTHROWSQ ;

: PET-RESOLVE-MISSING-THROWS ( -- )
   [: PET-RESOLVE-MISSING ;] E-PROC-PATH TTHROWSQ ;

\ A length is checked before a byte of the caller's text is read. The text holds
\ no `=` and ends at an inaccessible page, so a scan that runs first reads past
\ it; a negative length, or one whose row would not fit a cell, is refused unread.
7 constant PET-READABLE                  \ bytes of `x` before the inaccessible page

: PET-UNREAD ( -- ptr u8 )
   PET-READABLE [char] x GUARD-PAGE:TAIL ;

: PET-ENTRY-UNREAD ( n -- ) {: u:n :}
   PET-RESET PET-UNREAD u >LEN PROC-ENV-ENTRY+ ;

: PET-ROW-UNREAD ( n -- ) {: nameu:n :}
   PET-RESET PET-UNREAD nameu >LEN s" " >LEN PROC-ENV+ ;

: PET-SET-UNREAD ( n -- ) {: nameu:n :}
   PET-RESET PET-UNREAD nameu >LEN s" " >LEN PROC-ENV-SET ;

: PET-DEF-UNREAD ( n -- ) {: nameu:n :}
   PET-RESET PET-UNREAD nameu >LEN s" " >LEN PROC-ENV-DEFAULT+ ;

\ The same refusals for a value after the readable name AB.
: PET-VAL-ROW-UNREAD ( n -- ) {: valu:n :}
   PET-RESET s" AB" >LEN PET-UNREAD valu >LEN PROC-ENV+ ;

: PET-VAL-SET-UNREAD ( n -- ) {: valu:n :}
   PET-RESET s" AB" >LEN PET-UNREAD valu >LEN PROC-ENV-SET ;

: PET-VAL-DEF-UNREAD ( n -- ) {: valu:n :}
   PET-RESET s" AB" >LEN PET-UNREAD valu >LEN PROC-ENV-DEFAULT+ ;

\ A refused row leaves nothing staged in either table.
: PET-NONE-STAGED ( -- )
   PROC-ENV-N @ COUNT>N 0 T=
   PROC-ENV-DEF-N @ COUNT>N 0 T= ;

: PET-ENV-BOUND-BEFORE-SCAN ( -- )
   s" an entry's length is checked before it is read" T-LABEL
   [: MEM-MAX-N PET-ENTRY-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: -1 PET-ENTRY-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: STR-MIN-I64 PET-ENTRY-UNREAD ;] E-PROC-ENV TTHROWSQ
   s" and so is a row's name, appended, set or defaulted" T-LABEL
   [: MEM-MAX-N PET-ROW-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: -1 PET-ROW-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: STR-MIN-I64 PET-ROW-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: MEM-MAX-N PET-SET-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: -1 PET-SET-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: STR-MIN-I64 PET-SET-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: MEM-MAX-N PET-DEF-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: -1 PET-DEF-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: STR-MIN-I64 PET-DEF-UNREAD ;] E-PROC-ENV TTHROWSQ
   s" and so is a row's value, appended, set or defaulted" T-LABEL
   [: -1 PET-VAL-ROW-UNREAD ;] E-PROC-ENV TTHROWSQ PET-NONE-STAGED
   [: MEM-MAX-N PET-VAL-ROW-UNREAD ;] E-PROC-ENV TTHROWSQ PET-NONE-STAGED
   [: STR-MIN-I64 PET-VAL-ROW-UNREAD ;] E-PROC-ENV TTHROWSQ PET-NONE-STAGED
   [: -1 PET-VAL-SET-UNREAD ;] E-PROC-ENV TTHROWSQ PET-NONE-STAGED
   [: MEM-MAX-N PET-VAL-SET-UNREAD ;] E-PROC-ENV TTHROWSQ PET-NONE-STAGED
   [: STR-MIN-I64 PET-VAL-SET-UNREAD ;] E-PROC-ENV TTHROWSQ PET-NONE-STAGED
   [: -1 PET-VAL-DEF-UNREAD ;] E-PROC-ENV TTHROWSQ PET-NONE-STAGED
   [: MEM-MAX-N PET-VAL-DEF-UNREAD ;] E-PROC-ENV TTHROWSQ PET-NONE-STAGED
   [: STR-MIN-I64 PET-VAL-DEF-UNREAD ;] E-PROC-ENV TTHROWSQ PET-NONE-STAGED
   PET-RESET ;

\ More rows and bytes than a fixed table holds reach /usr/bin/env whole and in
\ order: $1000 rows of 135 bytes prepared directly, far past a table of $400
\ rows plus the parent's and still under macOS's 1 MiB spawn size, or $44C
\ inherited as defaults.
\ Each row's digits name it, so a lost, repeated, reordered or overwritten row
\ is a mismatch at its own place.
package PET-MANY
private

$1000 constant ENV-N
$44C constant DEF-N
4 constant DIGITS
9 constant PREFIX-U                      \ HABU_ENV_ or HABU_DEF_
PREFIX-U DIGITS + constant NAME-U
DIGITS $1E * constant VAL-U              \ the row's digits, 30 times
NAME-U 1 + VAL-U + 1 + constant LINE-U   \ name=value and the newline env prints
create LINE LINE-U allot
DYNAMIC-BUFFER OUT u8

: DIGITS! ( n ptr u8 -- ) {: k:n dst:ptr :}
   k 1000 / STR-ZERO + dst c!
   k 1000 mod 100 / STR-ZERO + dst 1 + c!
   k 100 mod 10 / STR-ZERO + dst 2 + c!
   k 10 mod STR-ZERO + dst 3 + c! ;

: LINE! ( n ptr u8 -- ) {: k:n prefix:ptr :}
   prefix LINE PREFIX-U BYTE-COPY
   k LINE PREFIX-U + DIGITS!
   PROC-ENV-EQUAL LINE NAME-U + c!
   VAL-U DIGITS / 0 ?do k LINE NAME-U 1 + + i DIGITS * + DIGITS! loop
   $0A LINE LINE-U 1 - + c! ;

: ADD ( n ptr u8 [ ptr u8 len ptr u8 len -- ] -- ) {: rows:n prefix:ptr q :}
   rows 0 ?do
      i prefix LINE!
      LINE NAME-U >LEN LINE NAME-U 1 + + VAL-U >LEN q execute
   loop ;

: OUT-ROOM ( n -- ptr u8 )
   OUT-RESERVE 0 OUT ;

: RUN ( -- n )
   PET-ENV-CMD$ >LEN s" " >LEN [: OUT-ROOM ;] PET-ERR PET-CAP >LEN
   PET-HB-TIMEOUT-MS >MS RUN-ARGV-ENV-STDIN-GROWING-CAPTURE-OUTCOME
   {: outu:len erru:len oc :}
   oc PROC-OUTCOME>RC RC>N 0 T=
   outu LEN>N ;

\ The rows that differ from their expected line, the first rows lines of out.
: MISMATCHES ( n n ptr u8 -- n ) {: rows:n outu:n prefix:ptr :}
   0 rows 0 ?do
      i prefix LINE!
      i 1+ LINE-U * outu > if
         1+
      else
         i LINE-U * OUT LINE-U LINE LINE-U STR= 0= if 1+ then
      then
   loop ;

public

: ENV-ROWS ( -- )
   PET-RESET
   ENV-N s" HABU_ENV_" drop [: PROC-ENV+ ;] ADD
   RUN {: outu:n :}
   outu ENV-N LINE-U * T=
   ENV-N outu s" HABU_ENV_" drop MISMATCHES 0 T=
   PET-RESET OUT-RELEASE ;

\ Defaults come first in an inheriting child's environment, ahead of the
\ parent's own rows.
: DEFAULT-ROWS ( -- )
   PET-RESET
   DEF-N s" HABU_DEF_" drop [: PROC-ENV-DEFAULT+ ;] ADD
   PROC-ENV-INHERIT-MISSING
   DEF-N RUN s" HABU_DEF_" drop MISMATCHES 0 T=
   PET-RESET OUT-RELEASE ;

;package

: PROCESS-ENV-TEST-MAIN ( -- )
   T-RESET
   s" env-child" [: PET-RUN-ENV-CHILD ;] PET-CASE
   s" empty-env-child" [: PET-RUN-EMPTY-ENV-CHILD ;] PET-CASE
   s" env-line-boundaries" [: PET-ENV-LINE-BOUNDARIES ;] PET-CASE
   s" getenv-concurrent" [: PET-GETENV-CONCURRENT ;] PET-CASE
   s" inherit-env-child" [: PET-RUN-INHERIT-ENV-CHILD ;] PET-CASE
   s" default-env-child" [: PET-DEFAULT-ENV-CHILD ;] PET-CASE
   s" default-lookup" [: PET-DEFAULT-LOOKUP ;] PET-CASE
   s" explicit-beats-default" [: PET-EXPLICIT-BEATS-DEFAULT ;] PET-CASE
   s" set-replaces" [: PET-SET-REPLACES ;] PET-CASE
   s" set-appends-new" [: PET-SET-APPENDS-NEW ;] PET-CASE
   s" append-keeps-dup" [: PET-APPEND-KEEPS-DUP ;] PET-CASE
   s" big-inherit-child" [: PET-BIG-INHERIT-CHILD ;] PET-CASE
   s" env-many-rows" [: PET-MANY:ENV-ROWS ;] PET-CASE
   s" default-many-rows" [: PET-MANY:DEFAULT-ROWS ;] PET-CASE
   s" argv-env-capture-result" [: PET-RUN-ARGV-ENV-CAPTURE-RESULT ;] PET-CASE
   s" env-outcome-false" [: PET-RUN-ENV-OUTCOME-FALSE ;] PET-CASE
   s" env-outcome-timeout" [: PET-RUN-ENV-OUTCOME-TIMEOUT ;] PET-CASE
   s" env-stdin-outcome" [: PET-RUN-ENV-STDIN-OUTCOME ;] PET-CASE
   s" env-stdin-false-large" [: PET-RUN-ENV-STDIN-FALSE-LARGE ;] PET-CASE
   s" env-stdin-outcome-false-large" [: PET-RUN-ENV-STDIN-OUTCOME-FALSE-LARGE ;] PET-CASE
   s" env-stdin-outcome-timeout" [: PET-RUN-ENV-STDIN-OUTCOME-TIMEOUT ;] PET-CASE
   s" env-stdin-growing-both" [: PET-GROW-BOTH:RUN ;] PET-CASE
   s" spawn-raw-missing" [: PET-SPAWN-RAW-MISSING ;] PET-CASE
   s" spawn-raw-true" [: PET-SPAWN-RAW-TRUE ;] PET-CASE
   s" bad-env-name" [: PET-BAD-ENV-NAME-THROWS ;] PET-CASE
   s" bad-env-entry" [: PET-BAD-ENV-ENTRY-THROWS ;] PET-CASE
   s" bad-env-empty" [: PET-BAD-ENV-EMPTY-THROWS ;] PET-CASE
   s" env-bound-before-scan" [: PET-ENV-BOUND-BEFORE-SCAN ;] PET-CASE
   s" path-find-hb" [: PET-PATH-FIND-HB ;] PET-CASE
   s" path-direct-hb" [: PET-PATH-DIRECT-HB ;] PET-CASE
   s" path-missing" [: PET-PATH-MISSING ;] PET-CASE
   s" resolve-missing" [: PET-RESOLVE-MISSING-THROWS ;] PET-CASE
   T-REPORT
   s" process-env-test: ok" type cr ;

PROCESS-ENV-TEST-MAIN
