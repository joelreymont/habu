\ stale-status-lint-core.f - enforce STATUS.md as the only live self-check count.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f, and tools/argv.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/fs.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/argv.f
\ tools/stale-status-lint-core.f provides STALE-STATUS-LINT:MAIN;
\ tools/stale-status-lint.f is the CLI entrypoint.

package STALE-STATUS-LINT
private

32 constant NUM-CAP

10 constant LF-C
47 constant SLASH-C
48 constant ZERO-C
58 constant COLON-C

create NUM-BUF NUM-CAP allot
create ONE-BUF 1 allot

variable BAD
variable FILE-A
variable FILE-CAP
variable NUM-L
variable LINE-N
variable DISP-A
variable DISP-U
variable SCAN-X
variable SLASH-I
variable DIGITS
variable ROOT-A
variable ROOT-U
variable SRC-A
variable SRC-U
variable LINE-S
variable LINE-E
variable FENCE
variable OUT-A
variable OUT-CAP
variable OUT-U
variable ERR-A
variable ERR-CAP
variable ERR-U

: ROOT-A-FIELD ( -- ptr ptr u8 )
   ROOT-A 0 ptr-field ;

: FILE-A-FIELD ( -- ptr ptr u8 )
   FILE-A 0 ptr-field ;

: SRC-A-FIELD ( -- ptr ptr u8 )
   SRC-A 0 ptr-field ;

: DISP-A-FIELD ( -- ptr ptr u8 )
   DISP-A 0 ptr-field ;

: OUT-A-FIELD ( -- ptr ptr u8 )
   OUT-A 0 ptr-field ;

: ERR-A-FIELD ( -- ptr ptr u8 )
   ERR-A 0 ptr-field ;

: ROOT-A@ ( -- ptr u8 )
   ROOT-A-FIELD @ ;

: FILE-A@ ( -- ptr u8 )
   FILE-A-FIELD @ ;

: SRC-A@ ( -- ptr u8 )
   SRC-A-FIELD @ ;

: DISP-A@ ( -- ptr u8 )
   DISP-A-FIELD @ ;

: OUT-A@ ( -- ptr u8 )
   OUT-A-FIELD @ ;

: ERR-A@ ( -- ptr u8 )
   ERR-A-FIELD @ ;

: ROOT-A! ( ptr u8 -- )
   ROOT-A-FIELD ! ;

: FILE-A! ( ptr u8 -- )
   FILE-A-FIELD ! ;

: SRC-A! ( ptr u8 -- )
   SRC-A-FIELD ! ;

: DISP-A! ( ptr u8 -- )
   DISP-A-FIELD ! ;

: OUT-A! ( ptr u8 -- )
   OUT-A-FIELD ! ;

: ERR-A! ( ptr u8 -- )
   ERR-A-FIELD ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT-A@ ROOT-U @ ;

: SRC$ ( -- ptr u8 n )
   SRC-A@ SRC-U @ ;

: DISP$ ( -- ptr u8 n )
   DISP-A@ DISP-U @ ;

: BYTE! ( n -- ) ONE-BUF c! ;

public

: OUT-BUFFER! ( ptr u8 n -- ) {: a:ptr cap:n :}
   a OUT-A!
   cap OUT-CAP !
   0 OUT-U ! ;

: ERR-BUFFER! ( ptr u8 n -- ) {: a:ptr cap:n :}
   a ERR-A!
   cap ERR-CAP !
   0 ERR-U ! ;

: BUFFERS-OFF ( -- )
   NULL$ drop OUT-A!
   NULL$ drop ERR-A!
   0 OUT-CAP !
   0 ERR-CAP !
   0 OUT-U !
   0 ERR-U ! ;

: OUT$ ( -- ptr u8 n )
   OUT-A@ OUT-U @ ;

: ERR$ ( -- ptr u8 n )
   ERR-A@ ERR-U @ ;

private

: OUT-BUFFERED? ( -- bool )
   OUT-A@ 0= 0= ;

: ERR-BUFFERED? ( -- bool )
   ERR-A@ 0= 0= ;

: SAY ( ptr u8 n -- )
   dup 0= IF 2drop exit THEN
   OUT-BUFFERED? IF
      STR:LENGTH OUT-A@ OUT-CAP @ STR:LENGTH OUT-U STR:BUF-APPEND
      exit
   THEN
   1 -rot write drop ;

: SAY-ERR ( ptr u8 n -- )
   dup 0= IF 2drop exit THEN
   ERR-BUFFERED? IF
      STR:LENGTH ERR-A@ ERR-CAP @ STR:LENGTH ERR-U STR:BUF-APPEND
      exit
   THEN
   2 -rot write drop ;

: SAY-C ( n -- )
   BYTE!
   ONE-BUF 1 SAY ;

: SAY-NL ( -- ) LF-C SAY-C ;

: SAY-U ( n -- )
   0 NUM-L !
   dup 0= IF drop ZERO-C SAY-C exit THEN
   begin dup 0 > while
      dup 10 mod ZERO-C + NUM-BUF NUM-L @ + c!
      10 /
      NUM-L @ 1+ NUM-L !
   repeat drop
   begin NUM-L @ 0 > while
      NUM-L @ 1- NUM-L !
      NUM-BUF NUM-L @ + c@ SAY-C
   repeat ;

: DIGIT? ( n -- bool )
   dup 47 > swap 58 < and ;

: ALNUM? ( n -- bool ) {: c:n :}
   c DIGIT? IF LINT-TRUE exit THEN
   c 64 > c 91 < and IF LINT-TRUE exit THEN
   c 96 > c 123 < and ;

: REL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   ROOT$ {: root:ptr rootu:n :}
   rootu 0 >  root rootu s" ." LINT-STR= 0= and IF
      u rootu > IF
         a rootu root rootu LINT-STR= IF
            a rootu + c@ SLASH-C = IF
               a rootu 1 + +  u rootu 1 + -  exit
            THEN
         THEN
      THEN
   THEN
   u 2 >= IF
      a c@ 46 =  a 1 + c@ SLASH-C = and IF a 2 + u 2 - exit THEN
   THEN
   a u ;

public

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT-A!
   u ROOT-U ! ;

private

: DISPLAY! ( ptr u8 n -- )
   REL$ DISP-U ! DISP-A! ;

: ALLOWED? ( ptr u8 n -- bool )
   REL$
   2dup s" STATUS.md" LINT-STR= IF 2drop LINT-TRUE exit THEN
   2dup s" LESSONS.md" LINT-STR= IF 2drop LINT-TRUE exit THEN
   s" docs/archive/lessons-" LINT-STARTS-WITH? ;

: SKIP-PATH? ( ptr u8 n -- bool )
   REL$
   2dup s" .jj-ws/" LINT-STARTS-WITH? IF 2drop LINT-TRUE exit THEN
   \ .blackboard/ is untracked, so it is outside the tree this gate speaks for,
   \ and its dated messages quote one past gate run as evidence, not as a live
   \ claim. Root-anchored like every skip here: notes/.blackboard/x.md is scanned.
   2dup s" .blackboard/" LINT-STARTS-WITH? IF 2drop LINT-TRUE exit THEN
   2dup s" docs/archive/" LINT-STARTS-WITH? IF 2drop LINT-TRUE exit THEN   \ frozen history: counts there are records, not claims
   s" maki/" LINT-STARTS-WITH? ;

: MD? ( ptr u8 n -- bool )
   s" .md" HAS-EXT? ;

: FENCE-LINE? ( ptr u8 n -- bool )
   LINT-TRIM s" ```" LINT-STARTS-WITH? ;

: FENCE-TOGGLE ( -- )
   FENCE @ 0= IF -1 ELSE 0 THEN FENCE ! ;

: LINE-END ( ptr u8 n n -- n ) {: a:ptr u:n start:n :}
   start begin dup u < while
      dup a + c@ LF-C = IF exit THEN
      1+
   repeat ;

: FILE-ENSURE ( n -- ) {: need:n :}
   need 0 <= IF 1 ELSE need THEN {: cap:n :}
   FILE-A@ 0= 0= FILE-CAP @ cap >= and IF exit THEN
   cap MEM-ALLOC-64K-SPAN FILE-CAP ! FILE-A! ;

: LOAD-FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE FILE-ENSURE
   path pathu FILE-A@ FILE-CAP @ READ-FILE
   SRC-U ! SRC-A! ;

: SET-LINE-END ( -- )
   SRC$ LINE-S @ LINE-END LINE-E ! ;

: CURRENT-LINE ( -- ptr u8 n )
   SRC$ {: src:ptr srcu:n :}
   src LINE-S @ +  LINE-E @ LINE-S @ - ;

: ADVANCE-LINE ( -- )
   LINE-E @ 1+ LINE-S ! ;

: BAD+ ( -- )
   BAD @ 1+ BAD ! ;

: BEFORE-BOUND? ( ptr u8 n -- bool ) {: a:ptr pos:n :}
   pos 0= IF LINT-TRUE exit THEN
   a pos 1- + c@ ALNUM? LINT-NOT ;

: AFTER-BOUND? ( ptr u8 n n -- bool ) {: a:ptr u:n pos:n :}
   pos u >= IF LINT-TRUE exit THEN
   a pos + c@ ALNUM? LINT-NOT ;

: SLASH-RUN ( ptr u8 n n -- n bool ) {: a:ptr u:n pos:n :}
   pos u >= IF pos LINT-FALSE exit THEN
   a pos + c@ SLASH-C <> IF pos LINT-FALSE exit THEN
   pos 1+ SLASH-I !
   SLASH-I @ DIGITS !
   begin SLASH-I @ u <  a SLASH-I @ + c@ DIGIT? and while
      SLASH-I @ 1+ SLASH-I !
   repeat
   SLASH-I @  SLASH-I @ DIGITS @ > ;

: WORD-AT? ( ptr u8 n n ptr u8 n -- bool ) {: a:ptr u:n pos:n b:ptr v:n :}
   u pos - v < IF LINT-FALSE exit THEN
   a pos + v b v LINT-STR=CI ;

: SCAN-ADVANCE ( -- )
   SCAN-X @ 1+ SCAN-X ! ;

: SCAN-C@ ( ptr u8 -- n )
   SCAN-X @ + c@ ;

: SCAN-DIGITS ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 DIGITS !
   begin SCAN-X @ u <  a SCAN-C@ DIGIT? and while
      DIGITS @ 1+ DIGITS !
      SCAN-ADVANCE
   repeat ;

: LONG-DIGIT-RUN? ( -- bool )
   DIGITS @ 3 >= ;

: CURRENT-WS? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   SCAN-X @ u >= IF LINT-FALSE exit THEN
   a SCAN-C@ LINT-WS? ;

: SKIP-WS ( ptr u8 n -- )
   {: a:ptr u:n :}
   begin a u CURRENT-WS? while
      SCAN-ADVANCE
   repeat ;

: TAKE-SLASH-RUN? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u SCAN-X @ SLASH-RUN IF
      SCAN-X !
      LINT-TRUE
   ELSE
      drop
      LINT-FALSE
   THEN ;

: COUNT-RATIO? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u TAKE-SLASH-RUN? IF
      a u TAKE-SLASH-RUN? IF
         a u SCAN-X @ AFTER-BOUND?
      ELSE
         LINT-FALSE
      THEN
   ELSE
      LINT-FALSE
   THEN ;

: COUNT-KEYWORD? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u CURRENT-WS? IF
      a u SKIP-WS
      a u SCAN-X @ s" certified" WORD-AT? IF LINT-TRUE exit THEN
      a u SCAN-X @ s" uncheckable" WORD-AT?
   ELSE
      LINT-FALSE
   THEN ;

: COUNT-TAIL? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   SCAN-X @ u >= IF LINT-FALSE exit THEN
   a SCAN-C@ SLASH-C = IF a u COUNT-RATIO? exit THEN
   a u COUNT-KEYWORD? ;

: COUNT-CANDIDATE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a SCAN-C@ DIGIT? IF
      a SCAN-X @ BEFORE-BOUND? IF
         a u SCAN-DIGITS
         LONG-DIGIT-RUN? IF a u COUNT-TAIL? ELSE LINT-FALSE THEN
      ELSE
         SCAN-ADVANCE
         LINT-FALSE
      THEN
   ELSE
      SCAN-ADVANCE
      LINT-FALSE
   THEN ;

: COUNT-LINE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 SCAN-X !
   begin SCAN-X @ u < while
      a u COUNT-CANDIDATE? IF LINT-TRUE exit THEN
   repeat
   LINT-FALSE ;

: FINDING ( -- )
   s" STALE-STATUS " SAY
   DISP$ SAY
   COLON-C SAY-C
   LINE-N @ SAY-U
   s" : count-shaped string - point to STATUS.md instead of quoting a number" SAY
   SAY-NL
   BAD+ ;

: SCAN-CURRENT-LINE ( -- )
   CURRENT-LINE FENCE-LINE? IF FENCE-TOGGLE exit THEN
   FENCE @ 0= IF
      CURRENT-LINE COUNT-LINE? IF FINDING THEN
   THEN ;

: SCAN-MD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SKIP-PATH? IF exit THEN
   a u ALLOWED? IF exit THEN
   a u MD? LINT-NOT IF exit THEN
   a u EXISTS? LINT-NOT IF exit THEN
   a u DISPLAY!
   a u LOAD-FILE
   0 FENCE !
   0 LINE-N !
   0 LINE-S !
   begin LINE-S @ SRC$ nip < while
      LINE-N @ 1+ LINE-N !
      SET-LINE-END
      SCAN-CURRENT-LINE
      ADVANCE-LINE
   repeat ;

: SCAN-TREE ( -- )
   0 BAD !
   ROOT$ [: SCAN-MD ;] WALK-FILES
   s" stale-status-lint: " SAY BAD @ SAY-U s"  finding(s)" SAY SAY-NL ;

: CONFIG ( -- )
   s" tools/stale-status-lint.f [ROOT]" ARGV:USAGE!
   ARGV:PARSE
   0 1 ARGV:EXPECT-POS
   ARGV:POS# 0 > IF 0 ARGV:POS$ ROOT! ELSE s" ." ROOT! THEN ;

public

: RUN ( -- n )   \ scan every tracked doc; leaves the count-shaped-string finding count
   SCAN-TREE
   BAD @ ;

: MAIN ( -- )
   CONFIG
   RUN 0 > IF 1 throw THEN ;

;package
