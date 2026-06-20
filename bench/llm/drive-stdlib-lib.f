\ drive-stdlib-lib.f - native stdlib stack benchmark driver library.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, tools/argv.f,
\ bench/llm/manifest.f, bench/llm/model.f, bench/llm/parse-resp-lib.f,
\ bench/llm/model-run.f, bench/llm/vectors.f, lib/json-write.f,
\ src/core/sha256.f, and bench/llm/live-row.f.

4096 constant DS-PROMPT-CAP
8192 constant DS-CAND-CAP
8192 constant DS-TEST-CAP
8192 constant DS-OUT-CAP
8192 constant DS-ERR-CAP
1024 constant DS-MSG-CAP
45 constant DS-DASH
48 constant DS-ZERO
57 constant DS-NINE
59 constant DS-SEMI
36 constant DS-DOLLAR
10 constant DS-LF
10000 constant DS-HB-TIMEOUT-MS
-3230 constant E-DS-USAGE
-3231 constant E-DS-CAPACITY
-3232 constant E-DS-CANDIDATE

create DS-PROMPT-BUF DS-PROMPT-CAP allot
create DS-CAND-BUF DS-CAND-CAP allot
create DS-TEST-BUF DS-TEST-CAP allot
create DS-OUT-BUF DS-OUT-CAP allot
create DS-ERR-BUF DS-ERR-CAP allot
create DS-MSG-BUF DS-MSG-CAP allot

create DS-ROOT-BUF FS-PATH-CAP allot
create DS-PROMPT-PATH FS-PATH-CAP allot
create DS-RAW-PATH FS-PATH-CAP allot
create DS-CAND-PATH FS-PATH-CAP allot
create DS-DIAG-PATH FS-PATH-CAP allot
create DS-REPAIR-PATH FS-PATH-CAP allot
create DS-TEST-PATH FS-PATH-CAP allot
create DS-BUNDLE-PATH FS-PATH-CAP allot

variable DS-PROMPT-U
variable DS-CAND-U
variable DS-TEST-U
variable DS-OUT-U
variable DS-ERR-U
variable DS-MSG-U
variable DS-ROOT-U
variable DS-PROMPT-PATH-U
variable DS-RAW-PATH-U
variable DS-CAND-PATH-U
variable DS-DIAG-PATH-U
variable DS-REPAIR-PATH-U
variable DS-TEST-PATH-U
variable DS-BUNDLE-PATH-U

variable DS-ID
variable DS-TRIAL
variable DS-TASK-ORDER
variable DS-K
variable DS-MAX-REPAIRS
variable DS-TOKENS
variable DS-RC
variable DS-DIAG-COUNT

variable DS-NAME-A
variable DS-NAME-U
variable DS-SIG-A
variable DS-SIG-U
variable DS-CATEGORY-A
variable DS-CATEGORY-U
variable DS-TESTS-A
variable DS-TESTS-U
variable DS-SPEC-A
variable DS-SPEC-U
variable DS-SEED-A
variable DS-SEED-U

variable DS-LINE-NEXT
variable DS-LINE-A
variable DS-LINE-U
variable DS-EXTRACT-STARTED
variable DS-EXTRACT-SEEN
variable DS-EXTRACT-DONE

: DS-TRUE ( -- bool )
   0 0= ;

: DS-FALSE ( -- bool )
   DS-TRUE 0= ;

: DS-BUF-ROOM ( n n n -- ) {: add cap used :}
   add 0 < if E-DS-CAPACITY throw then
   add cap used - > if E-DS-CAPACITY throw then ;

: DS-BUF+ ( ptr u8 n ptr u8 n ptr a -- ) {: a:ptr u dst:ptr cap lenp:ptr :}
   u cap lenp @ DS-BUF-ROOM
   a dst lenp @ + u BYTE-COPY
   lenp @ u + lenp ! ;

: DS-BUF-C ( n ptr u8 n ptr a -- ) {: c dst:ptr cap lenp:ptr :}
   1 cap lenp @ DS-BUF-ROOM
   c dst lenp @ + c!
   lenp @ 1+ lenp ! ;

: DS-BUF-LN ( ptr u8 n ptr u8 n ptr a -- )
   {: a:ptr u dst:ptr cap lenp:ptr :}
   a u dst cap lenp DS-BUF+
   DS-LF dst cap lenp DS-BUF-C ;

TRUSTED: DS-SET$ ( ptr u8 n ptr n ptr n -- ) {: a:ptr u ap:ptr up:ptr :}
   a ap !
   u up ! ;

: DS-NAME! ( ptr u8 n -- )
   DS-NAME-A DS-NAME-U DS-SET$ ;

: DS-SIG! ( ptr u8 n -- )
   DS-SIG-A DS-SIG-U DS-SET$ ;

: DS-CATEGORY! ( ptr u8 n -- )
   DS-CATEGORY-A DS-CATEGORY-U DS-SET$ ;

: DS-TESTS! ( ptr u8 n -- )
   DS-TESTS-A DS-TESTS-U DS-SET$ ;

: DS-SPEC! ( ptr u8 n -- )
   DS-SPEC-A DS-SPEC-U DS-SET$ ;

: DS-SEED! ( ptr u8 n -- )
   DS-SEED-A DS-SEED-U DS-SET$ ;

TRUSTED: DS-NAME$ ( -- ptr u8 n )
   DS-NAME-A @ DS-NAME-U @ ;

TRUSTED: DS-SIG$ ( -- ptr u8 n )
   DS-SIG-A @ DS-SIG-U @ ;

TRUSTED: DS-CATEGORY$ ( -- ptr u8 n )
   DS-CATEGORY-A @ DS-CATEGORY-U @ ;

TRUSTED: DS-TESTS$ ( -- ptr u8 n )
   DS-TESTS-A @ DS-TESTS-U @ ;

TRUSTED: DS-SPEC$ ( -- ptr u8 n )
   DS-SPEC-A @ DS-SPEC-U @ ;

TRUSTED: DS-SEED$ ( -- ptr u8 n )
   DS-SEED-A @ DS-SEED-U @ ;

: DS-PROMPT-RESET ( -- )
   0 DS-PROMPT-U ! ;

: DS-PROMPT+ ( ptr u8 n -- )
   DS-PROMPT-BUF DS-PROMPT-CAP DS-PROMPT-U DS-BUF+ ;

: DS-PROMPT-C ( n -- )
   DS-PROMPT-BUF DS-PROMPT-CAP DS-PROMPT-U DS-BUF-C ;

: DS-PROMPT-LN ( ptr u8 n -- )
   DS-PROMPT-BUF DS-PROMPT-CAP DS-PROMPT-U DS-BUF-LN ;

: DS-PROMPT$ ( -- ptr u8 n )
   DS-PROMPT-BUF DS-PROMPT-U @ ;

: DS-CAND-RESET ( -- )
   0 DS-CAND-U ! ;

: DS-CAND+ ( ptr u8 n -- )
   DS-CAND-BUF DS-CAND-CAP DS-CAND-U DS-BUF+ ;

: DS-CAND-C ( n -- )
   DS-CAND-BUF DS-CAND-CAP DS-CAND-U DS-BUF-C ;

: DS-CAND-LN ( ptr u8 n -- )
   DS-CAND-BUF DS-CAND-CAP DS-CAND-U DS-BUF-LN ;

: DS-CAND$ ( -- ptr u8 n )
   DS-CAND-BUF DS-CAND-U @ ;

: DS-TEST-RESET ( -- )
   0 DS-TEST-U ! ;

: DS-TEST+ ( ptr u8 n -- )
   DS-TEST-BUF DS-TEST-CAP DS-TEST-U DS-BUF+ ;

: DS-TEST-LN ( ptr u8 n -- )
   DS-TEST-BUF DS-TEST-CAP DS-TEST-U DS-BUF-LN ;

: DS-TEST$ ( -- ptr u8 n )
   DS-TEST-BUF DS-TEST-U @ ;

: DS-MSG-RESET ( -- )
   0 DS-MSG-U ! ;

: DS-MSG+ ( ptr u8 n -- )
   DS-MSG-BUF DS-MSG-CAP DS-MSG-U DS-BUF+ ;

: DS-MSG-C ( n -- )
   DS-MSG-BUF DS-MSG-CAP DS-MSG-U DS-BUF-C ;

: DS-MSG-LN ( ptr u8 n -- )
   DS-MSG-BUF DS-MSG-CAP DS-MSG-U DS-BUF-LN ;

: DS-MSG$ ( -- ptr u8 n )
   DS-MSG-BUF DS-MSG-U @ ;

: DS-ROOT$ ( -- ptr u8 n )
   DS-ROOT-BUF DS-ROOT-U @ ;

: DS-PROMPT-PATH$ ( -- ptr u8 n )
   DS-PROMPT-PATH DS-PROMPT-PATH-U @ ;

: DS-RAW-PATH$ ( -- ptr u8 n )
   DS-RAW-PATH DS-RAW-PATH-U @ ;

: DS-CAND-PATH$ ( -- ptr u8 n )
   DS-CAND-PATH DS-CAND-PATH-U @ ;

: DS-DIAG-PATH$ ( -- ptr u8 n )
   DS-DIAG-PATH DS-DIAG-PATH-U @ ;

: DS-REPAIR-PATH$ ( -- ptr u8 n )
   DS-REPAIR-PATH DS-REPAIR-PATH-U @ ;

: DS-TEST-PATH$ ( -- ptr u8 n )
   DS-TEST-PATH DS-TEST-PATH-U @ ;

: DS-BUNDLE-PATH$ ( -- ptr u8 n )
   DS-BUNDLE-PATH DS-BUNDLE-PATH-U @ ;

: DS-COPY-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: DS-JOIN! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu dst:ptr up:ptr :}
   DS-ROOT$ name nameu dst JOIN-PATH up ! ;

: DS-TEMP ( -- )
   s" habu-stdlib-driver" TMPDIR-MKDIR DS-ROOT-BUF DS-ROOT-U DS-COPY-PATH!
   DS-ROOT$ CLEANUP-TREE+
   s" prompt.txt" DS-PROMPT-PATH DS-PROMPT-PATH-U DS-JOIN!
   s" raw.txt" DS-RAW-PATH DS-RAW-PATH-U DS-JOIN!
   s" candidate.f" DS-CAND-PATH DS-CAND-PATH-U DS-JOIN!
   s" checker-diagnostics.txt" DS-DIAG-PATH DS-DIAG-PATH-U DS-JOIN!
   s" repair-packet.json" DS-REPAIR-PATH DS-REPAIR-PATH-U DS-JOIN!
   s" test-output.txt" DS-TEST-PATH DS-TEST-PATH-U DS-JOIN!
   s" bundle.f" DS-BUNDLE-PATH DS-BUNDLE-PATH-U DS-JOIN! ;

: DS-U+ ( n -- ) {: n :}
   n 0 < if DS-DASH DS-MSG-C n negate recurse exit then
   n 10 >= if n 10 / recurse then
   n 10 mod DS-ZERO + DS-MSG-C ;

: DS-PARSE-U ( ptr u8 n -- n ) {: a:ptr u :}
   a u STR>NUMBER? 0= if E-DS-USAGE throw then
   dup 0 < if E-DS-USAGE throw then ;

: DS-ENV$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: key:ptr keyu def:ptr defu :}
   key keyu GETENV dup 0= if 2drop def defu then ;

: DS-ENV-U ( ptr u8 n n -- n ) {: key:ptr keyu def :}
   key keyu GETENV dup 0= if 2drop def exit then
   DS-PARSE-U ;

: DS-DEF-NEEDLE$ ( -- ptr u8 n )
   DS-MSG-RESET
   s" : " DS-MSG+
   DS-NAME$ DS-MSG+
   DS-MSG$ ;

: DS-DIGIT? ( n -- bool )
   dup DS-ZERO 1- > swap DS-NINE 1+ < and ;

: DS-CONSTANT-LINE? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 0= if DS-FALSE exit then
   a c@ dup DS-DIGIT? swap DS-DASH = or
   a c@ DS-DOLLAR = or 0= if DS-FALSE exit then
   a u s"  constant " CONTAINS? ;

: DS-CODE-LINE? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u TRIM {: b:ptr v :}
   v 0= if DS-FALSE exit then
   b v s" :" STARTS-WITH? if DS-TRUE exit then
   b v s" variable " STARTS-WITH? if DS-TRUE exit then
   b v s" create " STARTS-WITH? if DS-TRUE exit then
   b v DS-CONSTANT-LINE? ;

: DS-PUBLIC-LINE? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u DS-DEF-NEEDLE$ CONTAINS? ;

: DS-LINE-SEMI? ( ptr u8 n -- bool )
   DS-SEMI INDEX-OF 0 >= ;

TRUSTED: DS-LINE! ( ptr u8 n -- )
   DS-LINE-U !
   DS-LINE-A ! ;

TRUSTED: DS-LINE$ ( -- ptr u8 n )
   DS-LINE-A @ DS-LINE-U @ ;

: DS-EXTRACT-RESET ( -- )
   DS-CAND-RESET
   0 DS-LINE-NEXT !
   0 DS-EXTRACT-STARTED !
   0 DS-EXTRACT-SEEN !
   0 DS-EXTRACT-DONE ! ;

: DS-EXTRACT-APPEND ( ptr u8 n -- )
   DS-CAND-LN ;

: DS-EXTRACT-LINE ( ptr u8 n -- ) {: a:ptr u :}
   DS-EXTRACT-DONE @ if exit then
   DS-EXTRACT-STARTED @ 0= if
      a u DS-CODE-LINE? 0= if exit then
      -1 DS-EXTRACT-STARTED !
   then
   a u DS-EXTRACT-APPEND
   a u DS-PUBLIC-LINE? if -1 DS-EXTRACT-SEEN ! then
   DS-EXTRACT-SEEN @ if
      a u DS-LINE-SEMI? if -1 DS-EXTRACT-DONE ! then
   then ;

: DS-EXTRACT-CANDIDATE ( ptr u8 n -- ) {: a:ptr u :}
   DS-EXTRACT-RESET
   begin
      a u DS-LINE-NEXT @ BM-LINE-NEXT
   while
      DS-LINE-NEXT !
      DS-LINE!
      DS-LINE$ DS-EXTRACT-LINE
   repeat
   drop 2drop
   DS-CAND-U @ 0= if s" \ no candidate extracted" DS-CAND-LN then ;

: DS-CAND-HAS-PUBLIC? ( -- bool )
   DS-CAND$ DS-DEF-NEEDLE$ CONTAINS? ;

: DS-CAND-FORBIDDEN? ( -- bool )
   DS-CAND$ s" TRUST" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" trust" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" set-check" CONTAINS? ;

: DS-CAND-COMPLETE? ( -- bool )
   DS-CAND$ DS-SEMI INDEX-OF 0 >= ;

: DS-CAND-CONTAINS? ( ptr u8 n -- bool ) {: a:ptr u :}
   DS-CAND$ a u CONTAINS? ;

: DS-NAME= ( ptr u8 n -- bool ) {: a:ptr u :}
   DS-NAME$ a u STR= ;

: DS-REQ-DATE-PARSE? ( -- bool )
   s" DATE-PARSE-OK?" DS-NAME= if s" PARSE-YMD" DS-CAND-CONTAINS? exit then
   s" INVALID-DATE?" DS-NAME= if s" PARSE-YMD" DS-CAND-CONTAINS? exit then
   DS-TRUE ;

: DS-REQ-DATE-FORMAT? ( -- bool )
   s" DATE-FORMAT-OK?" DS-NAME= if s" FORMAT-YMD" DS-CAND-CONTAINS? exit then
   s" EPOCH-UTC-OK?" DS-NAME= if s" FORMAT-EPOCH-UTC" DS-CAND-CONTAINS? exit then
   DS-TRUE ;

: DS-REQ-TIME? ( -- bool )
   s" MONO-ELAPSED?" DS-NAME= if s" TIME-MONO-NS" DS-CAND-CONTAINS? exit then
   DS-TRUE ;

: DS-REQ-STRING? ( -- bool )
   s" STR-TRIM-OK?" DS-NAME= if s" TRIM" DS-CAND-CONTAINS? exit then
   s" STR-SPLIT-OK?" DS-NAME= if s" SPLIT-NEXT" DS-CAND-CONTAINS? exit then
   s" STR-BUILDER-OK?" DS-NAME= if s" SB-APPEND" DS-CAND-CONTAINS? exit then
   s" STR-PARSE-I64-OK?" DS-NAME= if s" STR>NUMBER?" DS-CAND-CONTAINS? exit then
   s" STR-PREFIX-SUFFIX-OK?" DS-NAME= if
      s" STARTS-WITH?" DS-CAND-CONTAINS? s" ENDS-WITH?" DS-CAND-CONTAINS? and exit
   then
   s" STR-SEARCH-OK?" DS-NAME= if
      s" FIND-SUB" DS-CAND-CONTAINS? s" CONTAINS?" DS-CAND-CONTAINS? and exit
   then
   DS-TRUE ;

: DS-REQ-MAP? ( -- bool )
   DS-CATEGORY$ s" maps" STR= if s" MAP-" DS-CAND-CONTAINS? exit then
   DS-TRUE ;

: DS-REQ-REGEX? ( -- bool )
   DS-CATEGORY$ s" regex" STR= if s" RX-COMPILE" DS-CAND-CONTAINS? exit then
   DS-TRUE ;

: DS-REQ-FS? ( -- bool )
   s" FS-PATH-KINDS-OK?" DS-NAME= if
      s" EXISTS?" DS-CAND-CONTAINS? s" FILE?" DS-CAND-CONTAINS? and
      s" DIR?" DS-CAND-CONTAINS? and exit
   then
   s" FS-BASENAME-OK?" DS-NAME= if s" BASENAME" DS-CAND-CONTAINS? exit then
   s" FS-JOIN-OK?" DS-NAME= if s" JOIN-PATH" DS-CAND-CONTAINS? exit then
   DS-TRUE ;

: DS-CAND-USES-REQUIRED? ( -- bool )
   DS-REQ-DATE-PARSE? 0= if DS-FALSE exit then
   DS-REQ-DATE-FORMAT? 0= if DS-FALSE exit then
   DS-REQ-TIME? 0= if DS-FALSE exit then
   DS-REQ-STRING? 0= if DS-FALSE exit then
   DS-REQ-MAP? 0= if DS-FALSE exit then
   DS-REQ-REGEX? 0= if DS-FALSE exit then
   DS-REQ-FS? ;

: DS-CAND-VALID? ( -- bool )
   DS-CAND-HAS-PUBLIC? 0= if DS-FALSE exit then
   DS-CAND-FORBIDDEN? if DS-FALSE exit then
   DS-CAND-COMPLETE? 0= if DS-FALSE exit then
   DS-CAND-USES-REQUIRED? ;

: DS-BUILD-PROMPT ( -- )
   DS-PROMPT-RESET
   s" You write Habu, a checked Forth. Return checked source only." DS-PROMPT-LN
   s" Define the public benchmark word exactly as:" DS-PROMPT-LN
   s" : " DS-PROMPT+
   DS-NAME$ DS-PROMPT+
   s"  ( " DS-PROMPT+
   DS-SIG$ DS-PROMPT+
   s"  ) ... ;" DS-PROMPT-LN
   s" " DS-PROMPT-LN
   s" Task:" DS-PROMPT-LN
   DS-SPEC$ DS-PROMPT-LN
   s" " DS-PROMPT-LN
   s" Expected stack examples:" DS-PROMPT-LN
   DS-TESTS$ DS-PROMPT-LN
   s" " DS-PROMPT-LN
   s" The driver preloads the checked public stdlib: strings, time, date, maps, regex, and filesystem helpers." DS-PROMPT-LN
   s" You may define small helper words, constants, variables, or buffers when needed." DS-PROMPT-LN
   s" The public word must keep the exact name and effect." DS-PROMPT-LN
   s" Do not use TRUST, TRUSTED:, trust, set-check, evaluate, or unchecked boundaries." DS-PROMPT-LN ;

: DS-STACK-DSL ( -- )
   s" variable DST-FAIL" DS-TEST-LN
   s" variable DST-CASE" DS-TEST-LN
   s" variable DST-START-DEPTH" DS-TEST-LN
   s" variable DST-ACTUAL-DEPTH" DS-TEST-LN
   s" create DST-ACTUAL 32 cells allot" DS-TEST-LN
   s" : DST= ( n n -- ) {: got want :} DST-CASE @ 1 + DST-CASE ! got want <> if [char] F emit DST-CASE @ . DST-FAIL @ 1 + DST-FAIL ! then ;" DS-TEST-LN
   s" 0 set-check" DS-TEST-LN
   s" : T{ ( -- ) depth DST-START-DEPTH ! ;" DS-TEST-LN
   s" : -> ( R -- ) depth DST-START-DEPTH @ - dup DST-ACTUAL-DEPTH ! 0 ?do DST-ACTUAL i cells + ! loop ;" DS-TEST-LN
   s" : }T ( R -- ) depth DST-START-DEPTH @ - dup DST-ACTUAL-DEPTH @ DST= 0 ?do DST-ACTUAL i cells + @ DST= loop ;" DS-TEST-LN
   s" ' HB-CHECK-HOOK set-check" DS-TEST-LN
   s" : DST-REPORT ( -- ) DST-FAIL @ 0= if 111 emit 107 emit cr exit then DST-FAIL @ . 1 die ;" DS-TEST-LN ;

: DS-BUILD-TESTS ( -- )
   DS-TEST-RESET
   DS-STACK-DSL
   s" stack" DS-NAME$ DS-TESTS$ BV-HABU-TESTS DS-TEST+
   s" DST-REPORT" DS-TEST-LN ;

: DS-WRITE-EMPTY-ARTIFACTS ( -- )
   DS-DIAG-PATH$ s" " WRITE-ALL
   DS-REPAIR-PATH$ s" {}" WRITE-ALL
   DS-TEST-PATH$ s" " WRITE-ALL
   DS-BUNDLE-PATH$ s" " WRITE-ALL
   DS-CAND-PATH$ s" \ no candidate extracted" WRITE-ALL ;

: DS-ADD-LIBS ( -- )
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/test.f" PROC-ARGV+
   s" lib/time.f" PROC-ARGV+
   s" lib/date.f" PROC-ARGV+
   s" lib/map.f" PROC-ARGV+
   s" lib/regex.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+ ;

: DS-HB-CAPTURE ( -- )
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" DS-OUT-BUF DS-OUT-CAP DS-ERR-BUF DS-ERR-CAP DS-HB-TIMEOUT-MS RUN-ARGV-ENV-CAPTURE
   DS-RC !
   DS-ERR-U !
   DS-OUT-U ! ;

: DS-WRITE-CAPTURE ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu DS-OUT-BUF DS-OUT-U @ WRITE-ALL
   DS-ERR-U @ 0 > if path pathu DS-ERR-BUF DS-ERR-U @ APPEND-FILE then ;

: DS-CHECK-CLEAN? ( -- bool )
   DS-RC @ 0= if
      DS-OUT-U @ 0= DS-ERR-U @ 0= and exit
   then
   DS-FALSE ;

: DS-RUN-CHECK ( -- )
   PROC-ARGV-ENV-RESET
   DS-ADD-LIBS
   DS-CAND-PATH$ PROC-ARGV+
   DS-HB-CAPTURE
   DS-CHECK-CLEAN? if
      DS-DIAG-PATH$ s" " WRITE-ALL
      0 DS-DIAG-COUNT !
      exit
   then
   DS-DIAG-PATH$ DS-WRITE-CAPTURE
   1 DS-DIAG-COUNT ! ;

: DS-RUN-TESTS ( -- )
   PROC-ARGV-ENV-RESET
   DS-ADD-LIBS
   DS-CAND-PATH$ PROC-ARGV+
   DS-BUNDLE-PATH$ PROC-ARGV+
   DS-HB-CAPTURE
   DS-TEST-PATH$ DS-WRITE-CAPTURE ;

: DS-TEST-PASS? ( -- bool )
   DS-RC @ 0= if DS-OUT-BUF DS-OUT-U @ s" ok" CONTAINS? else DS-FALSE then ;

: DS-CONFIG-LR-FILES ( -- )
   DS-PROMPT-PATH$ LR-PROMPT!
   DS-RAW-PATH$ LR-RAW!
   DS-CAND-PATH$ LR-CANDIDATE!
   DS-DIAG-PATH$ LR-DIAGNOSTICS!
   DS-REPAIR-PATH$ LR-REPAIR!
   DS-TEST-PATH$ LR-TEST!
   DS-BUNDLE-PATH$ LR-BUNDLE! ;

: DS-CONFIG-LR-COMMON ( -- )
   LR-RESET
   DS-SEED$ LR-RUN-ID!
   DS-ID @ LR-TASK-ID !
   DS-NAME$ LR-NAME!
   MR-ID$ LR-MODEL-ID!
   MR-LABEL$ LR-MODEL!
   s" habu-stdlib" LR-ARM!
   DS-SEED$ LR-SEED!
   DS-TRIAL @ LR-TRIAL !
   DS-TASK-ORDER @ LR-TASK-ORDER !
   DS-K @ LR-K !
   DS-TOKENS @ LR-TOKENS !
   DS-CAND-U @ LR-SOURCE-CHARS !
   DS-CATEGORY$ LR-FAMILY!
   s" unknown" LR-MODEL-VERSION!
   s" unknown" LR-MODEL-DATE!
   1 LR-ROUNDS !
   DS-CONFIG-LR-FILES ;

: DS-LR-REJECT ( ptr u8 n -- )
   DS-CONFIG-LR-COMMON
   LR-OUTCOME!
   s" rejected" LR-FIRST-CHECKER!
   0 LR-FIRST-PASS !
   0 LR-FIRST-TESTS !
   0 LR-TESTS-PASSED !
   DS-DIAG-COUNT @ LR-DIAG-COUNT !
   1 LR-CHECKER-ITERATIONS ! ;

: DS-LR-PASS ( -- )
   DS-CONFIG-LR-COMMON
   s" pass" LR-OUTCOME!
   s" certified" LR-FIRST-CHECKER!
   -1 LR-FIRST-PASS !
   -1 LR-FIRST-TESTS !
   -1 LR-TESTS-PASSED !
   0 LR-DIAG-COUNT !
   1 LR-CHECKER-ITERATIONS ! ;

: DS-LR-FAIL ( -- )
   DS-CONFIG-LR-COMMON
   s" fail" LR-OUTCOME!
   s" certified" LR-FIRST-CHECKER!
   -1 LR-FIRST-PASS !
   0 LR-FIRST-TESTS !
   0 LR-TESTS-PASSED !
   0 LR-DIAG-COUNT !
   1 LR-CHECKER-ITERATIONS ! ;

: DS-WRITE-INVALID-DIAG ( ptr u8 n -- )
   DS-DIAG-PATH$ 2swap WRITE-ALL
   1 DS-DIAG-COUNT ! ;

: DS-INVALID-CANDIDATE ( -- )
   DS-CAND-HAS-PUBLIC? 0= if
      s" missing public task definition" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      exit
   then
   DS-CAND-FORBIDDEN? if
      s" forbidden unchecked boundary" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      exit
   then
   DS-CAND-COMPLETE? if
      s" required stdlib word missing" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      exit
   then
   s" incomplete Forth definition" DS-WRITE-INVALID-DIAG
   s" reject" DS-LR-REJECT ;

: DS-EVALUATE-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   text textu DS-EXTRACT-CANDIDATE
   DS-CAND-PATH$ DS-CAND$ WRITE-ALL
   DS-BUILD-TESTS
   DS-BUNDLE-PATH$ DS-TEST$ WRITE-ALL
   DS-CAND-VALID? 0= if DS-INVALID-CANDIDATE exit then
   DS-RUN-CHECK
   DS-RC @ 0 <> if s" reject" DS-LR-REJECT exit then
   DS-RUN-TESTS
   DS-TEST-PASS? if DS-LR-PASS else DS-LR-FAIL then ;

: DS-PREPARE ( -- )
   CLEANUP-RESET
   DS-TEMP
   DS-BUILD-PROMPT
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-WRITE-EMPTY-ARTIFACTS ;

: DS-MODEL-ERROR ( -- )
   DS-DIAG-PATH$ MRUN-ERR$ WRITE-ALL
   MRUN-RC @ 0= if DS-DIAG-PATH$ s" model parse failed" WRITE-ALL then
   1 DS-DIAG-COUNT !
   DS-CONFIG-LR-COMMON
   s" error" LR-OUTCOME!
   s" rejected" LR-FIRST-CHECKER!
   0 LR-FIRST-PASS !
   0 LR-FIRST-TESTS !
   0 LR-TESTS-PASSED !
   1 LR-DIAG-COUNT ! ;

: DS-RUN-MODEL ( -- )
   DS-PREPARE
   DS-PROMPT$ MRUN-RUN
   MRUN-OUT$ DS-RAW-PATH$ 2swap WRITE-ALL
   MRUN-TOKENS @ DS-TOKENS !
   MRUN-RC @ 0= 0= if DS-MODEL-ERROR exit then
   MRUN-TEXT$ DS-EVALUATE-TEXT ;

: DS-RUN-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   textu DS-OUT-CAP > if E-DS-CAPACITY throw then
   text DS-OUT-BUF textu BYTE-COPY
   textu DS-OUT-U !
   DS-PREPARE
   0 DS-TOKENS !
   DS-RAW-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
   DS-OUT-BUF DS-OUT-U @ DS-EVALUATE-TEXT ;

: DS-DEFAULTS ( -- )
   s" BENCH_TRIAL" 1 DS-ENV-U DS-TRIAL !
   s" BENCH_TASK_ORDER" 0 DS-ENV-U DS-TASK-ORDER !
   s" BENCH_K" 1 DS-ENV-U DS-K !
   s" BENCH_MAX_REPAIRS" 1 DS-ENV-U DS-MAX-REPAIRS !
   s" BENCH_SEED" s" manifest" DS-ENV$ DS-SEED! ;

: DS-USAGE ( -- )
   s" usage: bench/llm/drive-stdlib.f <id> <name> <sig> <category> <tests> <spec> [maxr]" E-DS-USAGE die ;

: DS-CONFIG ( -- )
   SCRIPT-ARGC 6 < if DS-USAGE then
   SCRIPT-ARGC 7 > if DS-USAGE then
   0 SCRIPT-ARGV$ DS-PARSE-U DS-ID !
   1 SCRIPT-ARGV$ DS-NAME!
   2 SCRIPT-ARGV$ DS-SIG!
   3 SCRIPT-ARGV$ DS-CATEGORY!
   4 SCRIPT-ARGV$ DS-TESTS!
   5 SCRIPT-ARGV$ DS-SPEC!
   SCRIPT-ARGC 6 > if 6 SCRIPT-ARGV$ DS-PARSE-U else 1 then DS-MAX-REPAIRS !
   DS-DEFAULTS
   s" MODEL_REGISTRY" s" bench/llm/models.tsv" DS-ENV$ MR-LOAD
   s" MODEL_ID" GETENV MR-REQUIRE ;

: DS-MAIN ( -- )
   DS-CONFIG
   DS-RUN-MODEL
   LR-EMIT
   CLEANUP-RUN ;
