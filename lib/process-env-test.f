\ process-env-test.f - focused tests for lib/process-env.f.
\ Run: bin/hb --load lib/process-env-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/test/outcome.f
require lib/test/guard-page.f
require lib/process-env.f

$8000 constant PET-CAP
$20000 constant PET-EARLY-IN-CAP
$1388 constant PET-HB-TIMEOUT-MS
$3E8 constant PET-CMD-TIMEOUT-MS
$32 constant PET-SHORT-TIMEOUT-MS
$2 constant PET-ENOENT
$258 constant PET-BIG-N                  \ variables the big-environment child must inherit
$9 constant PET-BIG-PREFIX-U             \ "HABU_BIG_"
$C constant PET-BIG-NAME-U               \ prefix plus three digits
3 constant PET-MIN-ENV-N                 \ PATH, HOME, HB_TMP: the overflow child's whole envp

create PET-BIG-NAME PET-BIG-NAME-U allot
create PET-OUT PET-CAP allot
create PET-ERR PET-CAP allot
create PET-PATH FS-PATH-CAP allot
create PET-EARLY-IN PET-EARLY-IN-CAP allot
variable PET-I
variable PET-START-NS

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

\ The overflow child is given exactly PET-MIN-ENV-N entries, so its ceiling is
\ known here and the refusal line can be compared in full: a bare E-PROC-ENV
\ with no count and no limit is what left gates with nothing to act on.
: PET-PASS-ENV ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu GETENV {: val:ptr valu:n :}
   name nameu val valu PET-ENV+ ;

: PET-MIN-ENV ( -- )
   PET-RESET
   s" PATH" PET-PASS-ENV
   s" HOME" PET-PASS-ENV
   s" HB_TMP" PET-PASS-ENV ;

: PET-CEILING-LINE$ ( -- ptr u8 n )
   SB-RESET
   s" process-env: child environment full at " SB-APPEND
   PET-MIN-ENV-N PROC-ENV-EXTRA + PET-SB-N
   s"  entries (" SB-APPEND
   PET-MIN-ENV-N PET-SB-N
   s"  inherited + " SB-APPEND
   PROC-ENV-EXTRA PET-SB-N
   s"  added): entry " SB-APPEND
   PET-MIN-ENV-N PROC-ENV-EXTRA + 1 + PET-SB-N
   s"  refused" SB-APPEND
   SB$ ;

: PET-ENV-CEILING-DIAGNOSTIC ( -- )
   PET-MIN-ENV
   s" --load" >LEN PROC-ARGV+
   s" test/process-env-overflow-child.f" >LEN PROC-ARGV+
   s" bin/hb" PET-OUT PET-CAP PET-ERR PET-CAP PET-HB-TIMEOUT-MS PET-CAPTURE
   {: outu:n erru:n code:n :}
   code 0 = TFALSE
   PET-OUT outu s" overflow-child: no refusal" CONTAINS? TFALSE
   PET-ERR erru PET-CEILING-LINE$ CONTAINS? TTRUE ;

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

\ A row is compared with the room left in its buffer. The ways that rule can
\ fail: an exact fill refused, one byte past it taken, a negative length taken,
\ and a length near the maximum cell wrapping the byte count back into range.
8 constant PET-LEFT                      \ room the boundary rows are tried against
3 constant PET-ROW-EXTRA                 \ a one-byte name, its `=` and the NUL

: PET-ENV-ROOM ( -- n )
   PROC-ENV-BUF-CAP PROC-ENV-OFF @ OFF>N - ;

: PET-FILL-ROW ( n -- ) {: take:n :}
   s" F" >LEN PET-EARLY-IN take PET-ROW-EXTRA - >LEN PROC-ENV-SET ;

\ One name is set again and again, so the buffer fills while the table holds one
\ row. PET-LEFT bytes stay free.
: PET-ENV-FILL ( -- )
   PET-RESET
   begin PET-ENV-ROOM PET-LEFT - dup PET-EARLY-IN-CAP > while
      drop PET-EARLY-IN-CAP 2 / PET-FILL-ROW
   repeat PET-FILL-ROW ;

\ The defaults buffer is PROC-ENV-EXTRA-BYTES, which one row can fill.
: PET-DEF-FILL ( -- )
   PET-RESET
   s" F" >LEN PET-EARLY-IN PROC-ENV-EXTRA-BYTES PET-LEFT - PET-ROW-EXTRA - >LEN PROC-ENV-DEFAULT+ ;

: PET-ENTRY-AT ( n -- ) {: u:n :}
   PET-ENV-FILL s" AB=cdefg" drop u >LEN PROC-ENV-ENTRY+ ;

: PET-ROW-AT ( n -- ) {: valu:n :}
   PET-ENV-FILL s" AB" >LEN s" cdefg" drop valu >LEN PROC-ENV+ ;

: PET-DEF-AT ( n -- ) {: valu:n :}
   PET-DEF-FILL s" AB" >LEN s" cdefg" drop valu >LEN PROC-ENV-DEFAULT+ ;

: PET-ENV-BYTE-BOUNDS ( -- )
   PET-EARLY-IN!
   s" an entry fits when its bytes and NUL fit the room left" T-LABEL
   7 PET-ENTRY-AT PET-ENV-ROOM 0 T=
   [: 8 PET-ENTRY-AT ;] E-PROC-ENV TTHROWSQ
   [: -1 PET-ENTRY-AT ;] E-PROC-ENV TTHROWSQ
   [: MEM-MAX-N PET-ENTRY-AT ;] E-PROC-ENV TTHROWSQ
   s" a row fits when its name, value and two terminators fit the room left" T-LABEL
   4 PET-ROW-AT PET-ENV-ROOM 0 T=
   [: 5 PET-ROW-AT ;] E-PROC-ENV TTHROWSQ
   [: -1 PET-ROW-AT ;] E-PROC-ENV TTHROWSQ
   [: MEM-MAX-N PET-ROW-AT ;] E-PROC-ENV TTHROWSQ
   s" a default row fits by the same rule" T-LABEL
   4 PET-DEF-AT PROC-ENV-DEF-OFF @ OFF>N PROC-ENV-EXTRA-BYTES T=
   [: 5 PET-DEF-AT ;] E-PROC-ENV TTHROWSQ
   [: -1 PET-DEF-AT ;] E-PROC-ENV TTHROWSQ
   [: MEM-MAX-N PET-DEF-AT ;] E-PROC-ENV TTHROWSQ
   PET-RESET ;

\ The room left is checked before a byte of the caller's text is read. The text
\ holds no `=` and ends at an inaccessible page, so a scan that runs first reads
\ past it; a length one past the room, or the maximum cell, is refused unread.

: PET-UNREAD ( n -- ptr u8 ) {: u:n :}     \ u bytes of `x`, then no readable byte
   u [char] x GUARD-PAGE:TAIL ;

: PET-ENTRY-UNREAD ( n -- ) {: u:n :}
   PET-ENV-FILL PET-LEFT 1- PET-UNREAD u >LEN PROC-ENV-ENTRY+ ;

: PET-ROW-UNREAD ( n -- ) {: nameu:n :}
   PET-ENV-FILL PET-LEFT 2 - PET-UNREAD nameu >LEN s" " >LEN PROC-ENV+ ;

: PET-SET-UNREAD ( n -- ) {: nameu:n :}
   PET-ENV-FILL PET-LEFT 2 - PET-UNREAD nameu >LEN s" " >LEN PROC-ENV-SET ;

: PET-DEF-UNREAD ( n -- ) {: nameu:n :}
   PET-DEF-FILL PET-LEFT 2 - PET-UNREAD nameu >LEN s" " >LEN PROC-ENV-DEFAULT+ ;

: PET-ENV-BOUND-BEFORE-SCAN ( -- )
   PET-EARLY-IN!
   s" an entry is measured against the room before it is read" T-LABEL
   [: PET-LEFT PET-ENTRY-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: MEM-MAX-N PET-ENTRY-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: STR-MIN-I64 PET-ENTRY-UNREAD ;] E-PROC-ENV TTHROWSQ
   s" and so is a row's name, appended, set or defaulted" T-LABEL
   [: PET-LEFT 1- PET-ROW-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: MEM-MAX-N PET-ROW-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: -1 PET-ROW-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: STR-MIN-I64 PET-ROW-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: PET-LEFT 1- PET-SET-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: MEM-MAX-N PET-SET-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: -1 PET-SET-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: STR-MIN-I64 PET-SET-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: PET-LEFT 1- PET-DEF-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: MEM-MAX-N PET-DEF-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: -1 PET-DEF-UNREAD ;] E-PROC-ENV TTHROWSQ
   [: STR-MIN-I64 PET-DEF-UNREAD ;] E-PROC-ENV TTHROWSQ
   PET-RESET ;

: PROCESS-ENV-TEST-MAIN ( -- )
   T-RESET
   s" env-child" [: PET-RUN-ENV-CHILD ;] PET-CASE
   s" empty-env-child" [: PET-RUN-EMPTY-ENV-CHILD ;] PET-CASE
   s" env-line-boundaries" [: PET-ENV-LINE-BOUNDARIES ;] PET-CASE
   s" inherit-env-child" [: PET-RUN-INHERIT-ENV-CHILD ;] PET-CASE
   s" default-env-child" [: PET-DEFAULT-ENV-CHILD ;] PET-CASE
   s" default-lookup" [: PET-DEFAULT-LOOKUP ;] PET-CASE
   s" explicit-beats-default" [: PET-EXPLICIT-BEATS-DEFAULT ;] PET-CASE
   s" set-replaces" [: PET-SET-REPLACES ;] PET-CASE
   s" set-appends-new" [: PET-SET-APPENDS-NEW ;] PET-CASE
   s" append-keeps-dup" [: PET-APPEND-KEEPS-DUP ;] PET-CASE
   s" big-inherit-child" [: PET-BIG-INHERIT-CHILD ;] PET-CASE
   s" env-ceiling-diagnostic" [: PET-ENV-CEILING-DIAGNOSTIC ;] PET-CASE
   s" argv-env-capture-result" [: PET-RUN-ARGV-ENV-CAPTURE-RESULT ;] PET-CASE
   s" env-outcome-false" [: PET-RUN-ENV-OUTCOME-FALSE ;] PET-CASE
   s" env-outcome-timeout" [: PET-RUN-ENV-OUTCOME-TIMEOUT ;] PET-CASE
   s" env-stdin-outcome" [: PET-RUN-ENV-STDIN-OUTCOME ;] PET-CASE
   s" env-stdin-false-large" [: PET-RUN-ENV-STDIN-FALSE-LARGE ;] PET-CASE
   s" env-stdin-outcome-false-large" [: PET-RUN-ENV-STDIN-OUTCOME-FALSE-LARGE ;] PET-CASE
   s" env-stdin-outcome-timeout" [: PET-RUN-ENV-STDIN-OUTCOME-TIMEOUT ;] PET-CASE
   s" spawn-raw-missing" [: PET-SPAWN-RAW-MISSING ;] PET-CASE
   s" spawn-raw-true" [: PET-SPAWN-RAW-TRUE ;] PET-CASE
   s" bad-env-name" [: PET-BAD-ENV-NAME-THROWS ;] PET-CASE
   s" bad-env-entry" [: PET-BAD-ENV-ENTRY-THROWS ;] PET-CASE
   s" bad-env-empty" [: PET-BAD-ENV-EMPTY-THROWS ;] PET-CASE
   s" env-byte-bounds" [: PET-ENV-BYTE-BOUNDS ;] PET-CASE
   s" env-bound-before-scan" [: PET-ENV-BOUND-BEFORE-SCAN ;] PET-CASE
   s" path-find-hb" [: PET-PATH-FIND-HB ;] PET-CASE
   s" path-direct-hb" [: PET-PATH-DIRECT-HB ;] PET-CASE
   s" path-missing" [: PET-PATH-MISSING ;] PET-CASE
   s" resolve-missing" [: PET-RESOLVE-MISSING-THROWS ;] PET-CASE
   T-REPORT
   s" process-env-test: ok" type cr ;

PROCESS-ENV-TEST-MAIN
