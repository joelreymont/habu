\ check-verify-test-lib.f - CHECK:VERIFY-BYTES and `tools/check.f --verify-only`.
\ Run: bin/hb --load tools/check-verify-test.f
\
\ The operation is called here directly, as a language server calls it, and the
\ command line is run as a child. Fixtures are files under a fresh HB_TMP
\ directory; the subject's bytes are handed over in memory. What can go wrong,
\ and the case that sees it:
\
\   a word of an earlier check, or of this process, reaches the subject   two-in-turn
\   the subject's top-level code runs, or a dependency's                  no-run
\   the copy on disk is checked instead of the bytes, or positions
\   are counted in it                                                     buffer-not-disk
\   a dependency that requires PATH back meets the copy on disk           requires-back
\   a require of an absent PATH falls back to another file                absent-path
\   an included child lowers the source using floor, then leaves an
\   import visible after clean return                                     load-using-floor
\   a dependency's packets are dropped, or name the subject               failing-dependency
\   an engine-provided path is verified because of its bytes              engine-provided
\   the verifier's own source is verified in the image that holds it      held
\   a child that ends without a result reads as a verdict                 no-result, deadline
\   a child that dies drops the packets it made before, a dependency's
\   or the subject's                                                      died-after-packet
\   a nested duplicate drops earlier all-errors packets                   duplicate-after-packet
\   a closure that cannot be followed reads as verified                   missing-dependency
\   output past the capture loses the packets received before it, or
\   puts prose on --verify-only's stderr                                  truncated, cli-truncated
\   --verify-only drops stdin's closure, names the subject otherwise
\   than the operation, or writes prose on stderr                         cli-file, cli-stdin
\   --stdin-path or --verify-only is taken where it means nothing         cli-usage
\   a source path exceeds the CLI's slot and leaks engine text on stderr cli-path-capacity
\   check.f's image holds the launcher's dependencies, so its default
\   check of one is unavailable                                           default-image
\
\ `measure` prints the time of one check of a one-definition subject and of
\ tools/check-core.f, whose closure is over thirty files.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require tools/json.f
require tools/check-verify-core.f

package CHECK-VERIFY-TEST

\ A deadlock guard, never reached by a check that is merely slow: one takes
\ tens of milliseconds, and a loaded host stretches a child to seconds.
60000 constant GUARD-MS
\ Shorter than any engine takes to start.
1 constant SHORT-MS
\ What a child of this test may write on either stream: check.f's packets from
\ a full capture of the verifier's output fit.
$800000 constant CAP
\ CHECK's capture of the verifier child's stdout, and room for one packet.
$400000 constant OUT-CAPTURE
$1000 constant PACKET-ROOM
\ big.f's definitions, each a packet of some 500 bytes: more in all than
\ OUT-CAPTURE.
10000 constant BIG-DEFS

create ROOT FS-PATH-CAP allot
create AT FS-PATH-CAP allot
create SUBJ FS-PATH-CAP allot
variable ROOT-U
variable AT-U
variable SUBJ-U
DYNAMIC-BUFFER OUT u8
DYNAMIC-BUFFER ERR u8
DYNAMIC-BUFFER FILE-BYTES u8
DYNAMIC-BUFFER GEN u8                   \ generated source
variable GEN-U
variable START-NS
variable CLI-OUT-U


: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;


: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;


\ NAME in the fixture directory, canonical as every packet spells it.
: AT$ ( ptr u8 n -- ptr u8 n )
   ROOT$ 2swap AT JOIN-PATH AT-U !
   AT AT-U @ ;


: FIXTURE ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n src:ptr srcu:n :}
   name nameu AT$ src srcu WRITE-ALL ;


\ Check SRC as the fixture NAME, which keeps its spelling in SUBJ.
: CHECK-AS ( ptr u8 n ptr u8 n n -- CHECK:verdict ) {: src:ptr srcu:n name:ptr nameu:n ms:n :}
   name nameu AT$ SUBJ SUBJ-U COPY!
   src srcu SUBJ SUBJ-U @ ms >MS CHECK:VERIFY-BYTES ;


: SUBJ$ ( -- ptr u8 n )
   SUBJ SUBJ-U @ ;


\ A tree file's canonical path, kept in SUBJ.
: TREE$ ( ptr u8 n -- ptr u8 n )
   SOURCE-ROOT:CANONICAL drop SUBJ SUBJ-U COPY!
   SUBJ$ ;


\ A tree file's bytes, in FILE-BYTES.
: TREE-BYTES ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u FILE-SIZE {: size:n :}
   size 1 max FILE-BYTES-RESERVE
   a u 0 FILE-BYTES size READ-ALL drop
   0 FILE-BYTES size ;


\ 0 verified, 1 refused, 2 engine-provided, 3 held, 4 incomplete.
: KIND ( CHECK:verdict -- n )
   MATCH CHECK:verdict
      verified OF 0 ENDOF
      refused OF 1 ENDOF
      engine-provided OF 2 ENDOF
      held OF 3 ENDOF
      incomplete OF {: status :} 4 ENDOF
   ;MATCH ;


: EXPECT-KIND ( CHECK:verdict n ptr u8 n -- ) {: v want:n label:ptr labelu:n :}
   label labelu T-LABEL v KIND want T=
   v KIND want = if exit then
   s" log: " type CHECK:VERIFY-LOG$ type cr ;


\ How a child that gave no result ended: the exit code, -1 for a signal, -2
\ for the deadline, and -3 for a verdict, which is no incomplete one.
: STATUS ( CHECK:verdict -- n )
   MATCH CHECK:verdict
      incomplete OF
         MATCH outcome
            exited OF ENDOF
            signaled OF drop -1 ENDOF
            timeout OF -2 ENDOF
         ;MATCH
      ENDOF
      verified OF -3 ENDOF
      refused OF -3 ENDOF
      engine-provided OF -3 ENDOF
      held OF -3 ENDOF
   ;MATCH ;


\ The value under KEY in packet NODE when it is of KIND, else -1.
: VALUE ( n ptr u8 n n -- n ) {: node:n key:ptr keyu:n kind:n :}
   node 0 < if -1 exit then
   node key keyu JSON-GET {: v:n :}
   v 0 < if -1 exit then
   v JSON-KIND kind = if v exit then
   -1 ;


: STRING$ ( n ptr u8 n -- ptr u8 n )
   J-STR VALUE dup 0 < if drop s" " exit then
   JSON-STRING$ ;


: NUMBER$ ( n ptr u8 n -- ptr u8 n )
   J-NUM VALUE dup 0 < if drop s" " exit then
   JSON-NUMBER$ ;


\ The first packet in LINES whose string KEY is VALUE, or -1.
: PACKET ( ptr u8 n ptr u8 n ptr u8 n -- n ) {: lines:ptr linesu:n key:ptr keyu:n val:ptr valu:n :}
   lines linesu JSONL-START
   begin
      JSONL-NEXT-OBJECT
      dup 0 < if exit then
      dup key keyu STRING$ val valu STR= 0=
   while
      drop
   repeat ;


\ Every line is a JSON object: the strict reader refuses a line that is not
\ JSON, E-JSON-SYNTAX, or not an object, E-JSON-TYPE.
: ALL-JSON? ( ptr u8 n -- bool )
   JSONL-START-STRICT
   [: begin JSONL-NEXT-OBJECT 0 < until ;] catch {: rc:n :}
   rc 0= if true exit then
   rc E-JSON-SYNTAX = rc E-JSON-TYPE = or if false exit then
   rc throw ;


\ ---- the operation ---------------------------------------------------------

: DEP$SRC ( -- ptr u8 n )
   s\" : CVT-SEVEN ( -- n ) 7 ;\n" ;

: BAD-DEP$SRC ( -- ptr u8 n )
   s\" : CVT-EIGHT ( -- n ) 8 ;\n: CVT-BROKEN ( -- n n ) 8 ;\n" ;

\ A refused definition, then one left open, where the verifier dies.
: OPEN-DEP$SRC ( -- ptr u8 n )
   s\" : CVT-BAD-DEP ( -- n n ) 8 ;\n: CVT-OPEN-DEP ( -- n ) 1\n" ;

\ What the files on disk hold where a case checks other bytes as them: a clean
\ definition on line 1, and a string discovery refuses to follow.
: POS$DISK ( -- ptr u8 n )
   s\" : CVT-POS ( n -- n ) 1 + ;\n" ;

: LOOP$DISK ( -- ptr u8 n )
   s\" s\" never closed\n" ;

: BACK$SRC ( -- ptr u8 n )
   s\" require loop.f\n: CVT-BACK ( -- n ) 5 ;\n" ;

\ The fixture directory has no README.md; the tree root, the fallback, has one.
: BACK-README$SRC ( -- ptr u8 n )
   s\" require README.md\n: CVT-BACK-README ( -- n ) 2 ;\n" ;

\ The marker each file's top-level code writes when it runs.
: MARK$ ( ptr u8 n -- ptr u8 n ) {: name:ptr nameu:n :}
   SB-RESET
   s" require lib/fs.f" SB-APPEND $0a SB-APPEND-C
   s" s" SB-APPEND $22 SB-APPEND-C $20 SB-APPEND-C
   name nameu AT$ SB-APPEND
   $22 SB-APPEND-C s"  s" SB-APPEND $22 SB-APPEND-C s"  ran" SB-APPEND
   $22 SB-APPEND-C s"  WRITE-ALL" SB-APPEND $0a SB-APPEND-C
   SB$ ;

: MARK-DEP$SRC ( -- ptr u8 n )
   s" marker-dep" MARK$ ;

: MARK$SRC ( -- ptr u8 n )
   s" marker" MARK$ 2drop
   s\" require mark-dep.f\n: CVT-MARKED ( -- n ) 3 ;\n" SB-APPEND
   SB$ ;

: GEN+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   GEN-U @ u + GEN-RESERVE
   a GEN-U @ GEN u BYTE-COPY
   GEN-U @ u + GEN-U ! ;

: GEN-C+ ( n -- )
   {: c:n :}
   GEN-U @ 1+ GEN-RESERVE
   c GEN-U @ GEN c!
   GEN-U @ 1+ GEN-U ! ;

: GEN-N+ ( n -- )
   dup 10 >= if dup 10 / RECURSE then
   10 mod $30 + GEN-C+ ;

\ big.f: CVT-B<I> for each I, every one a value short.
: BIG$SRC ( -- ptr u8 n )
   0 GEN-U !
   BIG-DEFS 0 ?do
      s" : CVT-B" GEN+ i GEN-N+ s\"  ( -- n n ) 8 ;\n" GEN+
   loop
   0 GEN GEN-U @ ;

: FIXTURES ( -- )
   s" cvt" HB-TMP-MKDIR SOURCE-ROOT:CANONICAL drop ROOT ROOT-U COPY!
   s" dep.f" DEP$SRC FIXTURE
   s" bad-dep.f" BAD-DEP$SRC FIXTURE
   s" open-dep.f" OPEN-DEP$SRC FIXTURE
   s" pos.f" POS$DISK FIXTURE
   s" loop.f" LOOP$DISK FIXTURE
   s" back.f" BACK$SRC FIXTURE
   s" back-readme.f" BACK-README$SRC FIXTURE
   s" mark-dep.f" MARK-DEP$SRC FIXTURE
   s" order-dep.f" s\" package CVT-ORDER public\n: VALUE ( -- n ) 7 ;\n;package\n" FIXTURE
   s" order-late.f" s\" package CVT-LATE public\n: EARLY ( -- n ) CVT-ORDER:VALUE ;\n;package\nrequire order-dep.f\n" FIXTURE
   s" order-context-dep.f" s\" package CVT-CONTEXT public\n: USE ( -- n ) VALUE ;\n;package\n" FIXTURE
   s" order-context.f" s\" require order-dep.f\nusing CVT-ORDER\nrequire order-context-dep.f\n;using\n" FIXTURE
   s" order-package-dep.f" s\" : CVT-PKG-VALUE ( -- n ) 3 ;\n" FIXTURE
   s" order-package.f" s\" package CVT-PKG public\nrequire order-package-dep.f\n;package\n: CVT-PKG-USE ( -- n ) CVT-PKG:CVT-PKG-VALUE ;\n" FIXTURE
   s" order-floor-child.f" s\" ;package\nusing CVT-FLOOR-CHILD\n" FIXTURE
   s" order-floor-include.f" s\" package CVT-FLOOR-BASE public\n: BASE-VALUE ( -- n ) 11 ;\n;package\npackage CVT-FLOOR-CHILD public\n: CHILD-VALUE ( -- n ) 29 ;\n;package\npackage CVT-FLOOR-PARENT\nusing CVT-FLOOR-BASE\ninclude order-floor-child.f\npackage CVT-FLOOR-AFTER public\n: AFTER-VALUE ( -- n ) CHILD-VALUE ;\n;package\n" FIXTURE
   s" order-floor-require.f" s\" package CVT-FLOOR-BASE public\n: BASE-VALUE ( -- n ) 11 ;\n;package\npackage CVT-FLOOR-CHILD public\n: CHILD-VALUE ( -- n ) 29 ;\n;package\npackage CVT-FLOOR-PARENT\nusing CVT-FLOOR-BASE\nrequire order-floor-child.f\npackage CVT-FLOOR-AFTER public\n: AFTER-VALUE ( -- n ) CHILD-VALUE ;\n;package\n" FIXTURE
   s" order-double.f" s\" include order-dep.f\ninclude order-dep.f\n" FIXTURE
   s" big.f" BIG$SRC FIXTURE ;


\ The second check must see neither what the first declared nor a word of this
\ process.
: TWO-IN-TURN ( -- )
   s\" : CVT-ONE ( -- n ) 1 ;\n" s" one.f" GUARD-MS CHECK-AS
   0 s" two-in-turn: the first" EXPECT-KIND
   s\" : CVT-TWO ( -- n ) CVT-ONE ;\n" s" two.f" GUARD-MS CHECK-AS
   1 s" two-in-turn: the second" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" token" s" CVT-ONE" PACKET {: p:n :}
   s" two-in-turn: CVT-ONE is undefined" T-LABEL p s" code" STRING$ s" E-UNDEFINED" T$=
   s" two-in-turn: in the second" T-LABEL p s" file" STRING$ SUBJ$ T$=
   s\" : CVT-OWN ( -- ptr u8 n ) CHECK:VERIFY-OUT$ ;\n" s" own.f" GUARD-MS CHECK-AS
   1 s" two-in-turn: a word of this process" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" token" s" CHECK:VERIFY-OUT$" PACKET {: q:n :}
   s" two-in-turn: it is undefined" T-LABEL q s" code" STRING$ s" E-UNDEFINED" T$= ;


\ Neither the subject's top-level code nor its dependency's runs; loaded, both do.
: NO-RUN ( -- )
   MARK$SRC s" mark.f" GUARD-MS CHECK-AS
   0 s" no-run: verified" EXPECT-KIND
   s" no-run: the subject wrote nothing" T-LABEL s" marker" AT$ EXISTS? TFALSE
   s" no-run: its dependency wrote nothing" T-LABEL s" marker-dep" AT$ EXISTS? TFALSE
   s" mark.f" MARK$SRC FIXTURE
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" mark.f" AT$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN 0 OUT CAP >LEN 0 ERR CAP >LEN GUARD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME {: outu:len erru:len o :}
   s" no-run: a load runs both" T-LABEL o MATCH outcome
      exited OF ENDOF
      signaled OF drop -1 ENDOF
      timeout OF -2 ENDOF
   ;MATCH 0 T=
   s" no-run: the loaded subject wrote its marker" T-LABEL s" marker" AT$ EXISTS? TTRUE
   s" no-run: the loaded dependency wrote its marker" T-LABEL s" marker-dep" AT$ EXISTS? TTRUE ;


\ The disk copy is clean; the bytes put a rejected definition on line 4.
: BUFFER-NOT-DISK ( -- )
   s\" \n\n\n: CVT-POS ( n -- n ) drop ;\n" s" pos.f" GUARD-MS CHECK-AS
   1 s" buffer-not-disk: the bytes are checked" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" code" s" E-MISMATCH" PACKET {: p:n :}
   s" buffer-not-disk: the packet names PATH" T-LABEL p s" file" STRING$ SUBJ$ T$=
   s" buffer-not-disk: line in the bytes" T-LABEL p s" line" NUMBER$ s" 4" T$=
   s" buffer-not-disk: byte in the bytes" T-LABEL p s" byte_start" NUMBER$ s" 24" T$= ;


\ back.f requires loop.f, whose disk copy discovery refuses; the bytes are clean.
: REQUIRES-BACK ( -- )
   s\" require back.f\n: CVT-LOOP ( -- n ) CVT-BACK ;\n" s" loop.f" GUARD-MS CHECK-AS
   0 s" requires-back: the dependency meets the bytes" EXPECT-KIND ;


\ PATH is absent: a require of it, the subject's own or a dependency's, meets
\ the bytes, never the tree root's README.md.
: ABSENT-PATH ( -- )
   s\" require README.md\n: CVT-SELF ( -- n ) 1 ;\n" s" README.md" GUARD-MS CHECK-AS
   0 s" absent-path: the subject requires itself" EXPECT-KIND
   s\" require back-readme.f\n: CVT-ABSENT ( -- n ) CVT-BACK-README ;\n"
   s" README.md" GUARD-MS CHECK-AS
   0 s" absent-path: a dependency requires it back" EXPECT-KIND ;


: FAILING-DEPENDENCY ( -- )
   s\" require bad-dep.f\n: CVT-USE8 ( -- n ) CVT-EIGHT ;\n" s" use8.f" GUARD-MS CHECK-AS
   1 s" failing-dependency: refused" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" word" s" cvt-broken" PACKET {: p:n :}
   s" failing-dependency: the packet names the dependency" T-LABEL
   p s" file" STRING$ s" bad-dep.f" AT$ T$=
   s" failing-dependency: at its line" T-LABEL p s" line" NUMBER$ s" 2" T$=
   s" failing-dependency: none names the subject" T-LABEL
   CHECK:VERIFY-OUT$ s" file" SUBJ$ PACKET 0 < TTRUE ;


\ Bytes no loader would accept, as the engine's own lib/string.f.
: ENGINE-PROVIDED ( -- )
   s\" : STR= ( -- ) drop ;\ns\" never closed\n" s" lib/string.f" TREE$
   GUARD-MS >MS CHECK:VERIFY-BYTES
   2 s" engine-provided: whatever the bytes hold" EXPECT-KIND
   s" engine-provided: no packet" T-LABEL CHECK:VERIFY-OUT$ nip 0 T= ;


: HELD ( -- )
   s" src/habu/verify-source.f" TREE-BYTES s" src/habu/verify-source.f" TREE$
   GUARD-MS >MS CHECK:VERIFY-BYTES
   3 s" held: the verifier's own source" EXPECT-KIND ;


\ An open definition makes the verifier die: no result line, its exit and its words.
: NO-RESULT ( -- )
   s\" : CVT-OPEN ( -- n ) 1\n" s" open.f" GUARD-MS CHECK-AS {: v :}
   v 4 s" no-result: incomplete" EXPECT-KIND
   s" no-result: the child's exit" T-LABEL v STATUS 74 T=
   s" no-result: the child's words" T-LABEL
   CHECK:VERIFY-LOG$ s" unterminated definition" CONTAINS? TTRUE ;


: DEADLINE ( -- )
   DEP$SRC s" dep.f" SHORT-MS CHECK-AS {: v :}
   v 4 s" deadline: incomplete" EXPECT-KIND
   s" deadline: timed out" T-LABEL v STATUS -2 T= ;


\ The verifier dies at an open definition after it refused one: the packet it
\ made first is kept, a dependency's and then the subject's.
: DIED-AFTER-PACKET ( -- )
   s\" require open-dep.f\n: CVT-OPEN-USE ( -- n ) 1 ;\n" s" open-use.f" GUARD-MS CHECK-AS {: v :}
   v 4 s" died-after-packet: incomplete" EXPECT-KIND
   s" died-after-packet: the verifier's exit" T-LABEL v STATUS 74 T=
   CHECK:VERIFY-OUT$ s" word" s" cvt-bad-dep" PACKET {: p:n :}
   s" died-after-packet: the dependency's packet" T-LABEL
   p s" file" STRING$ s" open-dep.f" AT$ T$=
   s\" : CVT-BAD ( -- n n ) 8 ;\n: CVT-OPEN ( -- n ) 1\n" s" open-own.f" GUARD-MS CHECK-AS {: w :}
   w 4 s" died-after-packet: the subject's, incomplete" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" word" s" cvt-bad" PACKET {: q:n :}
   s" died-after-packet: the subject's packet" T-LABEL q s" file" STRING$ SUBJ$ T$= ;


: MISSING-DEPENDENCY ( -- )
   s\" require cvt-missing.f\n" s" lost.f" GUARD-MS CHECK-AS
   1 s" missing-dependency: refused" EXPECT-KIND
   s" missing-dependency: said so" T-LABEL
   CHECK:VERIFY-LOG$ s" cvt-missing.f: no such source" CONTAINS? TTRUE
   s" missing-dependency: no packet" T-LABEL CHECK:VERIFY-OUT$ nip 0 T= ;


: CHECK-BIG ( -- )
   BIG$SRC s" big.f" GUARD-MS CHECK-AS KIND drop ;

\ More packets than the capture holds: the throw, with every complete packet
\ received before it.
: TRUNCATED ( -- )
   [: CHECK-BIG ;] E-PROC-TRUNCATED TTHROWSQ
   CHECK:VERIFY-OUT$ {: out:ptr outu:n :}
   s" truncated: the packets fill the capture" T-LABEL
   outu OUT-CAPTURE PACKET-ROOM - > TTRUE
   s" truncated: each complete" T-LABEL out outu ALL-JSON? TTRUE ;


\ ---- the command line ------------------------------------------------------

\ The stderr length and exit status of a check.f child given IN on stdin.
: CLI ( ptr u8 n -- n n ) {: in:ptr inu:n :}
   ENGINE-CANDIDATE:PATH$ >LEN in inu >LEN 0 OUT CAP >LEN 0 ERR CAP >LEN GUARD-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N CLI-OUT-U ! e LEN>N 0 ENDOF
      err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N CLI-OUT-U ! e LEN>N c RC>N ENDOF
   ;MATCH ;


: CLI-START ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING ;


: ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;


: CLI-FILE ( -- )
   CLI-START s" --verify-only" ARG+ s" dep.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" cli-file: verified" T-LABEL rc 0 T=
   s" cli-file: nothing on stderr" T-LABEL erru 0 T=
   s" pos.f" s\" \n\n\n: CVT-POS ( n -- n ) drop ;\n" FIXTURE
   CLI-START s" --verify-only" ARG+ s" pos.f" AT$ ARG+
   s" " CLI {: erru2:n rc2:n :}
   s" cli-file: refused" T-LABEL rc2 70 T=
   s" cli-file: only packets on stderr" T-LABEL 0 ERR erru2 ALL-JSON? TTRUE
   0 ERR erru2 s" code" s" E-MISMATCH" PACKET {: p:n :}
   s" cli-file: named as the operation names it" T-LABEL p s" file" STRING$ s" pos.f" AT$ T$=
   s" pos.f" POS$DISK FIXTURE ;


\ stdin's closure resolves against --stdin-path's directory; the path need not exist.
: CLI-STDIN ( -- )
   CLI-START s" --verify-only" ARG+ s" --stdin-path" ARG+ s" new.f" AT$ ARG+
   s\" require dep.f\n: CVT-NEW ( -- n ) CVT-SEVEN ;\n" CLI {: erru:n rc:n :}
   s" cli-stdin: the closure is followed" T-LABEL rc 0 T=
   s" cli-stdin: nothing on stderr" T-LABEL erru 0 T=
   CLI-START s" --json-errors" ARG+ s" --verify-only" ARG+ s" --stdin-path" ARG+ s" new.f" AT$ ARG+
   s\" require dep.f\n: CVT-NEW ( -- n ) CVT-SEVEN drop ;\n" CLI {: erru2:n rc2:n :}
   s" cli-stdin: refused" T-LABEL rc2 70 T=
   s" cli-stdin: only packets on stderr" T-LABEL 0 ERR erru2 ALL-JSON? TTRUE
   0 ERR erru2 s" code" s" E-MISMATCH" PACKET {: p:n :}
   s" cli-stdin: named by the path" T-LABEL p s" file" STRING$ s" new.f" AT$ T$= ;


: CLI-USAGE ( -- )
   CLI-START s" --stdin-path" ARG+ s" new.f" AT$ ARG+
   s" : CVT-X ( -- ) ;" CLI {: erru:n rc:n :}
   s" cli-usage: --stdin-path without --verify-only" T-LABEL rc 64 T=
   s" cli-usage: default mode explains on stderr" T-LABEL
   0 ERR erru s" usage: tools/check.f" CONTAINS? TTRUE
   CLI-START s" --verify-only" ARG+
   s" : CVT-X ( -- ) ;" CLI {: erru2:n rc2:n :}
   s" cli-usage: stdin without a path" T-LABEL rc2 64 T=
   s" cli-usage: verify mode keeps stderr empty" T-LABEL erru2 0 T=
   s" cli-usage: verify mode explains on stdout" T-LABEL
   0 OUT CLI-OUT-U @ s" usage: tools/check.f" CONTAINS? TTRUE
   CLI-START s" --verify-only" ARG+ s" --source-list" ARG+ s" dep.f" AT$ ARG+
   s" " CLI {: erru3:n rc3:n :}
   s" cli-usage: a source list" T-LABEL rc3 64 T=
   s" cli-usage: source list keeps stderr empty" T-LABEL erru3 0 T=
   CLI-START s" --verify-only" ARG+ s" --stdin-path" ARG+
   s" " CLI {: erru4:n rc4:n :}
   s" cli-usage: missing option argument" T-LABEL rc4 64 T=
   s" cli-usage: missing argument keeps stderr empty" T-LABEL erru4 0 T=
   CLI-START s" --stdin-path" ARG+ s" new.f" AT$ ARG+ s" --verify-only" ARG+
   s" : CVT-X ( -- ) ;" CLI {: erru5:n rc5:n :}
   s" cli-usage: later verify option is honored" T-LABEL rc5 0 T=
   s" cli-usage: later verify option keeps stderr empty" T-LABEL erru5 0 T=
   CLI-START s" --stdin-path" ARG+ s" --verify-only" ARG+
   s" : CVT-X ( -- ) ;" CLI {: erru6:n rc6:n :}
   s" cli-usage: option-shaped path is a value" T-LABEL rc6 64 T=
   s" cli-usage: option-shaped value leaves default stderr" T-LABEL
   0 ERR erru6 s" usage: tools/check.f" CONTAINS? TTRUE
   CLI-START s" --" ARG+ s" --verify-only" ARG+
   s" " CLI {: erru7:n rc7:n :}
   s" cli-usage: option after separator is a file" T-LABEL rc7 66 T=
   s" cli-usage: separator leaves default stderr" T-LABEL
   0 ERR erru7 s" check.f: no such source" CONTAINS? TTRUE
   CLI-START s" --unknown" ARG+ s" --verify-only" ARG+
   s" " CLI {: erru8:n rc8:n :}
   s" cli-usage: earlier bad option is usage" T-LABEL rc8 64 T=
   s" cli-usage: later verify option routes early failure" T-LABEL erru8 0 T=
   s" cli-usage: earlier bad option explained" T-LABEL
   0 OUT CLI-OUT-U @ s" usage: tools/check.f" CONTAINS? TTRUE ;


: CLI-EARLY-FAILS ( -- )
   CLI-START s" --verify-only" ARG+ s" cvt-missing.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" cli-early-fails: missing file rc" T-LABEL rc 66 T=
   s" cli-early-fails: missing file stderr empty" T-LABEL erru 0 T=
   s" cli-early-fails: missing file explanation" T-LABEL
   0 OUT CLI-OUT-U @ s" check.f: no such source" CONTAINS? TTRUE
   $100001 GEN-RESERVE
   $100001 0 do 32 i GEN c! loop
   CLI-START s" --verify-only" ARG+ s" --stdin-path" ARG+ s" new.f" AT$ ARG+
   0 GEN $100001 CLI {: erru2:n rc2:n :}
   s" cli-early-fails: oversized stdin rc" T-LABEL rc2 66 T=
   s" cli-early-fails: oversized stdin stderr empty" T-LABEL erru2 0 T=
   s" cli-early-fails: oversized stdin explanation" T-LABEL
   0 OUT CLI-OUT-U @ s" check.f: source exceeds capacity" CONTAINS? TTRUE ;


: LONG-PATH$ ( -- ptr u8 n )
   FS-PATH-CAP 1+ GEN-RESERVE
   FS-PATH-CAP 1+ 0 do 97 i GEN c! loop
   0 GEN FS-PATH-CAP 1+ ;


: CLI-PATH-CAPACITY ( -- )
   LONG-PATH$ {: path:ptr pathu:n :}
   CLI-START s" --verify-only" ARG+ path pathu ARG+
   s" " CLI {: erru:n rc:n :}
   s" cli-path-capacity: verify FILE status" T-LABEL rc 67 T=
   s" cli-path-capacity: verify FILE stderr" T-LABEL erru 0 T=
   s" cli-path-capacity: verify FILE explanation" T-LABEL
   0 OUT CLI-OUT-U @ s" check.f: source path exceeds capacity" CONTAINS? TTRUE
   CLI-START path pathu ARG+
   s" " CLI {: erru2:n rc2:n :}
   s" cli-path-capacity: ordinary FILE status" T-LABEL rc2 67 T=
   s" cli-path-capacity: ordinary FILE explanation" T-LABEL
   0 ERR erru2 s" check.f: source path exceeds capacity" CONTAINS? TTRUE
   CLI-START s" --verify-only" ARG+ s" --stdin-path" ARG+ path pathu ARG+
   s" : CVT-X ( -- ) ;" CLI {: erru3:n rc3:n :}
   s" cli-path-capacity: verify stdin path status" T-LABEL rc3 67 T=
   s" cli-path-capacity: verify stdin path stderr" T-LABEL erru3 0 T=
   s" cli-path-capacity: verify stdin path explanation" T-LABEL
   0 OUT CLI-OUT-U @ s" check.f: source path exceeds capacity" CONTAINS? TTRUE
   CLI-START s" --stdin-path" ARG+ path pathu ARG+
   s" : CVT-X ( -- ) ;" CLI {: erru4:n rc4:n :}
   s" cli-path-capacity: ordinary stdin path status" T-LABEL rc4 67 T=
   s" cli-path-capacity: ordinary stdin path explanation" T-LABEL
   0 ERR erru4 s" check.f: source path exceeds capacity" CONTAINS? TTRUE ;


: CLI-TRUNCATED ( -- )
   CLI-START s" --verify-only" ARG+ s" big.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" cli-truncated: unavailable" T-LABEL rc 69 T=
   s" cli-truncated: the packets on stderr" T-LABEL erru OUT-CAPTURE PACKET-ROOM - > TTRUE
   s" cli-truncated: only packets on stderr" T-LABEL 0 ERR erru ALL-JSON? TTRUE ;


\ These cases compare the real loader with both CHECK entry points. An ordered
\ preload lets EARLY see a later file, loses the using inherited by USE, and
\ verifies an included file only once. None has a top-level runtime action.
: NATIVE-RC ( ptr u8 n -- n )
   PROC-ARGV-ENV-RESET
   s" --load" ARG+
   ARG+
   PROC-ENV-INHERIT-MISSING
   s" " CLI nip ;

: CHECK-RC ( ptr u8 n bool bool -- n )
   {: path:ptr pathu:n verify:bool all:bool :}
   CLI-START
   all if s" --all-errors" ARG+ then
   verify if s" --verify-only" ARG+ then
   path pathu ARG+
   s" " CLI nip ;

: LOAD-ORDER ( -- )
   s" order-late.f" AT$ {: path:ptr pathu:n :}
   s" load-order: native refuses early reference" T-LABEL path pathu NATIVE-RC 70 T=
   s" load-order: verify-only refuses early reference" T-LABEL path pathu true false CHECK-RC 70 T=
   s" load-order: ordinary refuses early reference" T-LABEL path pathu false false CHECK-RC 70 T=
   s" load-order: all-errors refuses early reference" T-LABEL path pathu false true CHECK-RC 70 T= ;

: LOAD-CONTEXT ( -- )
   s" order-context.f" AT$ {: path:ptr pathu:n :}
   s" load-context: native inherits using" T-LABEL path pathu NATIVE-RC 0 T=
   s" load-context: verify-only inherits using" T-LABEL path pathu true false CHECK-RC 0 T=
   s" load-context: ordinary inherits using" T-LABEL path pathu false false CHECK-RC 0 T=
   s" load-context: all-errors inherits using" T-LABEL path pathu false true CHECK-RC 0 T= ;

: LOAD-PACKAGE ( -- )
   s" order-package.f" AT$ {: path:ptr pathu:n :}
   s" load-package: native inherits package" T-LABEL path pathu NATIVE-RC 0 T=
   s" load-package: verify-only inherits package" T-LABEL path pathu true false CHECK-RC 0 T=
   s" load-package: ordinary inherits package" T-LABEL path pathu false false CHECK-RC 0 T=
   s" load-package: all-errors inherits package" T-LABEL path pathu false true CHECK-RC 0 T= ;

: LOAD-USING-FLOOR ( -- )
   s" order-floor-include.f" AT$ {: path:ptr pathu:n :}
   s" load-using-floor: native include refuses stale import" T-LABEL path pathu NATIVE-RC 70 T=
   s" load-using-floor: verify-only include refuses stale import" T-LABEL path pathu true false CHECK-RC 70 T=
   s" load-using-floor: ordinary include refuses stale import" T-LABEL path pathu false false CHECK-RC 70 T=
   s" load-using-floor: all-errors include refuses stale import" T-LABEL path pathu false true CHECK-RC 70 T=
   s" order-floor-require.f" AT$ {: req:ptr requ:n :}
   s" load-using-floor: native require refuses stale import" T-LABEL req requ NATIVE-RC 70 T=
   s" load-using-floor: verify-only require refuses stale import" T-LABEL req requ true false CHECK-RC 70 T=
   s" load-using-floor: ordinary require refuses stale import" T-LABEL req requ false false CHECK-RC 70 T=
   s" load-using-floor: all-errors require refuses stale import" T-LABEL req requ false true CHECK-RC 70 T= ;

: LOAD-REPEAT ( -- )
   s" order-double.f" AT$ {: path:ptr pathu:n :}
   s" load-repeat: native sees second include" T-LABEL path pathu NATIVE-RC 78 T=
   s" load-repeat: verify-only sees second include" T-LABEL path pathu true false CHECK-RC 70 T=
   s" load-repeat: ordinary sees second include" T-LABEL path pathu false false CHECK-RC 78 T=
   s" load-repeat: all-errors sees second include" T-LABEL path pathu false true CHECK-RC 78 T= ;

: DUPLICATE-AFTER-PACKET ( -- )
   s" order-errors.f"
   s\" package CVT-ORDER-ERR public\n: CVT-ORDER-BAD ( -- n ) drop ;\n;package\nrequire order-dups.f\n" FIXTURE
   s" order-dups.f"
   s\" package CVT-ORDER-DUP public\n: SAME ( -- n ) 1 ;\n: SAME ( -- n ) 2 ;\n;package\n" FIXTURE
   CLI-START s" --all-errors" ARG+ s" --json-errors" ARG+ s" order-errors.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" duplicate-after-packet: original exit" T-LABEL rc 78 T=
   s" duplicate-after-packet: JSON packets only" T-LABEL 0 ERR erru ALL-JSON? TTRUE
   0 ERR erru JSONL-START-STRICT
   s" duplicate-after-packet: earlier rejection first" T-LABEL
   JSONL-NEXT-OBJECT s" word" STRING$ s" cvt-order-bad" T$=
   s" duplicate-after-packet: duplicate second" T-LABEL
   JSONL-NEXT-OBJECT s" code" STRING$ s" E-DUPLICATE-DEFINITION" T$=
   s" duplicate-after-packet: both packets retained" T-LABEL JSONL-NEXT-OBJECT -1 T= ;


\ 69 is check.f refusing a file its own image holds. The check itself goes on
\ to whatever lib/engine-candidate.f's closure earns, which this case leaves be.
: DEFAULT-IMAGE ( -- )
   CLI-START s" lib/engine-candidate.f" ARG+
   s" " CLI {: erru:n rc:n :}
   s" default-image: check.f's image does not hold lib/engine-candidate.f" T-LABEL rc 69 T<> ;


\ ---- the measurement -------------------------------------------------------

: MS. ( -- )
   mono-ns START-NS @ - PROC-NS-PER-MS / FMT:.INT s"  ms" type ;


: MEASURE ( -- )
   mono-ns START-NS !
   DEP$SRC s" dep.f" GUARD-MS CHECK-AS KIND
   s" measure: one-definition file " type MS. s" , outcome " type FMT:.INT cr
   s" tools/check-core.f" TREE-BYTES s" tools/check-core.f" TREE$
   mono-ns START-NS !
   GUARD-MS >MS CHECK:VERIFY-BYTES KIND
   s" measure: tools/check-core.f " type MS. s" , outcome " type FMT:.INT cr ;


: RUN-CASE ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   mono-ns START-NS !
   q execute
   s" case " type label labelu type s"  (" type MS. s" )" type cr ;

public

: MAIN ( -- )
   T-RESET
   CAP OUT-RESERVE
   CAP ERR-RESERVE
   FIXTURES
   s" two-in-turn" [: TWO-IN-TURN ;] RUN-CASE
   s" no-run" [: NO-RUN ;] RUN-CASE
   s" buffer-not-disk" [: BUFFER-NOT-DISK ;] RUN-CASE
   s" requires-back" [: REQUIRES-BACK ;] RUN-CASE
   s" absent-path" [: ABSENT-PATH ;] RUN-CASE
   s" failing-dependency" [: FAILING-DEPENDENCY ;] RUN-CASE
   s" engine-provided" [: ENGINE-PROVIDED ;] RUN-CASE
   s" held" [: HELD ;] RUN-CASE
   s" no-result" [: NO-RESULT ;] RUN-CASE
   s" deadline" [: DEADLINE ;] RUN-CASE
   s" died-after-packet" [: DIED-AFTER-PACKET ;] RUN-CASE
   s" missing-dependency" [: MISSING-DEPENDENCY ;] RUN-CASE
   s" truncated" [: TRUNCATED ;] RUN-CASE
   s" cli-file" [: CLI-FILE ;] RUN-CASE
   s" cli-stdin" [: CLI-STDIN ;] RUN-CASE
   s" cli-usage" [: CLI-USAGE ;] RUN-CASE
   s" cli-early-fails" [: CLI-EARLY-FAILS ;] RUN-CASE
   s" cli-path-capacity" [: CLI-PATH-CAPACITY ;] RUN-CASE
   s" cli-truncated" [: CLI-TRUNCATED ;] RUN-CASE
   s" load-order" [: LOAD-ORDER ;] RUN-CASE
   s" load-context" [: LOAD-CONTEXT ;] RUN-CASE
   s" load-package" [: LOAD-PACKAGE ;] RUN-CASE
   s" load-using-floor" [: LOAD-USING-FLOOR ;] RUN-CASE
   s" load-repeat" [: LOAD-REPEAT ;] RUN-CASE
   s" duplicate-after-packet" [: DUPLICATE-AFTER-PACKET ;] RUN-CASE
   s" default-image" [: DEFAULT-IMAGE ;] RUN-CASE
   MEASURE
   ROOT$ REMOVE-TREE
   T-REPORT
   s" check-verify-test: ok" type cr ;

;package
