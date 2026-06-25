\ stale-status-lint.f - enforce STATUS.md as the only live self-check count.
\ Load after tools/date.f, lib/errors.f, lib/string.f, lib/fs.f, tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f, and tools/argv.f.
\ Run: bin/hb --load tools/date.f lib/errors.f lib/string.f lib/fs.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/argv.f
\ tools/stale-status-lint.f -- [ROOT] [TODAY]

$20000 constant SS-FILE-CAP
32 constant SS-NUM-CAP

10 constant SS-LF
47 constant SS-SLASH
48 constant SS-ZERO
58 constant SS-COLON

create SS-FILE-BUF SS-FILE-CAP allot
create SS-NUM-BUF SS-NUM-CAP allot
create SS-TODAY-BUF DATE-LEN allot
create SS-PATH-BUF FS-PATH-CAP allot
create SS-ONE 1 allot

variable SS-BAD
variable SS-NUM-L
variable SS-LINE-N
variable SS-FOUND?
variable SS-DATE-A
variable SS-DATE-U
variable SS-PATH-A
variable SS-PATH-U
variable SS-DISP-A
variable SS-DISP-U
variable SS-SCAN-X
variable SS-RUN
variable SS-DIGITS
variable SS-TODAY-DAYS
variable SS-ROOT-A
variable SS-ROOT-U
variable SS-SRC-A
variable SS-SRC-U
variable SS-LINE-S
variable SS-LINE-E
variable SS-FENCE

: SS-ROOT-A-FIELD ( -- ptr ptr u8 )
   SS-ROOT-A 0 ptr-field ;

: SS-SRC-A-FIELD ( -- ptr ptr u8 )
   SS-SRC-A 0 ptr-field ;

: SS-DATE-A-FIELD ( -- ptr ptr u8 )
   SS-DATE-A 0 ptr-field ;

: SS-DISP-A-FIELD ( -- ptr ptr u8 )
   SS-DISP-A 0 ptr-field ;

: SS-ROOT-A@ ( -- ptr u8 )
   SS-ROOT-A-FIELD @ ;

: SS-SRC-A@ ( -- ptr u8 )
   SS-SRC-A-FIELD @ ;

: SS-DATE-A@ ( -- ptr u8 )
   SS-DATE-A-FIELD @ ;

: SS-DISP-A@ ( -- ptr u8 )
   SS-DISP-A-FIELD @ ;

: SS-ROOT-A! ( ptr u8 -- )
   SS-ROOT-A-FIELD ! ;

: SS-SRC-A! ( ptr u8 -- )
   SS-SRC-A-FIELD ! ;

: SS-DATE-A! ( ptr u8 -- )
   SS-DATE-A-FIELD ! ;

: SS-DISP-A! ( ptr u8 -- )
   SS-DISP-A-FIELD ! ;

: SS-ROOT$ ( -- ptr u8 n )
   SS-ROOT-A@ SS-ROOT-U @ ;

: SS-SRC$ ( -- ptr u8 n )
   SS-SRC-A@ SS-SRC-U @ ;

: SS-DATE$ ( -- ptr u8 n )
   SS-DATE-A@ SS-DATE-U @ ;

: SS-DISP$ ( -- ptr u8 n )
   SS-DISP-A@ SS-DISP-U @ ;

: SS-SKIP ( ptr u8 n n -- ptr u8 n ) {: a:ptr u n :}
   a n +  u n - ;

: SS-C! ( n -- ) SS-ONE c! ;

: SS-OUT ( ptr u8 n -- )
   dup 0= IF 2drop exit THEN
   1 -rot write drop ;

: SS-ERR ( ptr u8 n -- )
   dup 0= IF 2drop exit THEN
   2 -rot write drop ;

: SS-C ( n -- )
   SS-C!
   SS-ONE 1 SS-OUT ;

: SS-NL ( -- ) SS-LF SS-C ;

: SS-U. ( n -- )
   0 SS-NUM-L !
   dup 0= IF drop SS-ZERO SS-C exit THEN
   begin dup 0 > while
      dup 10 mod SS-ZERO + SS-NUM-BUF SS-NUM-L @ + c!
      10 /
      SS-NUM-L @ 1+ SS-NUM-L !
   repeat drop
   begin SS-NUM-L @ 0 > while
      SS-NUM-L @ 1- SS-NUM-L !
      SS-NUM-BUF SS-NUM-L @ + c@ SS-C
   repeat ;

: SS-2D ( n -- )
   dup 10 < IF SS-ZERO SS-C THEN
   SS-U. ;

: SS-4D ( n -- )
   dup 1000 < IF SS-ZERO SS-C THEN
   dup 100 < IF SS-ZERO SS-C THEN
   dup 10 < IF SS-ZERO SS-C THEN
   SS-U. ;

: SS-DIGIT? ( n -- bool )
   dup 47 > swap 58 < and ;

: SS-TRUE ( -- bool )
   0 0= ;

: SS-FALSE ( -- bool )
   0 1 = ;

: SS-NOT ( bool -- bool )
   IF SS-FALSE ELSE SS-TRUE THEN ;

: SS-ALNUM? ( n -- bool ) {: c :}
   c SS-DIGIT? IF SS-TRUE exit THEN
   c 64 > c 91 < and IF SS-TRUE exit THEN
   c 96 > c 123 < and ;

: SS-REL ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   SS-ROOT$ {: root:ptr rootu :}
   rootu 0 >  root rootu s" ." STR= 0= and IF
      u rootu > IF
         a rootu root rootu STR= IF
            a rootu + c@ SS-SLASH = IF
               a rootu 1 + +  u rootu 1 + -  exit
            THEN
         THEN
      THEN
   THEN
   u 2 >= IF
      a c@ 46 =  a 1 + c@ SS-SLASH = and IF a 2 + u 2 - exit THEN
   THEN
   a u ;

: SS-ROOT ( -- ptr u8 n )
   SS-ROOT$ ;

: SS-ROOT! ( ptr u8 n -- ) {: a:ptr u :}
   a SS-ROOT-A!
   u SS-ROOT-U ! ;

: SS-ROOT-SELF? ( -- bool )
   SS-ROOT$ {: root:ptr rootu :}
   rootu 0= IF SS-TRUE exit THEN
   root rootu s" ." STR= ;

: SS-ROOTED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   SS-ROOT$ {: root:ptr rootu :}
   SS-ROOT-SELF? IF a u exit THEN
   rootu u + 1 + FS-PATH-CAP > IF s" stale-status-lint: root path too long" 1 die THEN
   root SS-PATH-BUF rootu BYTE-COPY
   root rootu 1 - + c@ SS-SLASH = IF
      a SS-PATH-BUF rootu + u BYTE-COPY
      SS-PATH-BUF rootu u + exit
   THEN
   SS-SLASH SS-PATH-BUF rootu + c!
   a SS-PATH-BUF rootu 1 + + u BYTE-COPY
   SS-PATH-BUF rootu 1 + u + ;

: SS-DISPLAY! ( ptr u8 n -- )
   SS-REL SS-DISP-U ! SS-DISP-A! ;

: SS-ALLOWED? ( ptr u8 n -- bool )
   SS-REL
   2dup s" STATUS.md" STR= IF 2drop SS-TRUE exit THEN
   s" LESSONS.md" STR= ;

: SS-MD? ( ptr u8 n -- bool )
   s" .md" HAS-EXT? ;

: SS-LINE-PREFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   a u b v STARTS-WITH? ;

: SS-FENCE-LINE? ( ptr u8 n -- bool )
   TRIM s" ```" STARTS-WITH? ;

: SS-FENCE-TOGGLE ( -- )
   SS-FENCE @ 0= IF -1 ELSE 0 THEN SS-FENCE ! ;

: SS-LINE-END ( ptr u8 n n -- n ) {: a:ptr u start :}
   start begin dup u < while
      dup a + c@ SS-LF = IF exit THEN
      1+
   repeat ;

: SS-STATUS-LINE ( ptr u8 n -- ) {: a:ptr u :}
   a u TRIM
   2dup s" Last verified:" SS-LINE-PREFIX? IF
      14 SS-SKIP LTRIM
      SS-DATE-U ! SS-DATE-A!
      -1 SS-FOUND? !
   ELSE
      2drop
   THEN ;

: SS-LOAD-FILE ( ptr u8 n -- )
   SS-FILE-BUF SS-FILE-CAP READ-FILE
   SS-SRC-U ! SS-SRC-A! ;

: SS-SET-LINE-END ( -- )
   SS-SRC$ SS-LINE-S @ SS-LINE-END SS-LINE-E ! ;

: SS-CURRENT-LINE ( -- ptr u8 n )
   SS-SRC$ {: src:ptr srcu :}
   src SS-LINE-S @ +  SS-LINE-E @ SS-LINE-S @ - ;

: SS-ADVANCE-LINE ( -- )
   SS-LINE-E @ 1+ SS-LINE-S ! ;

: SS-STATUS-DATE! ( -- )
   0 SS-FOUND? !
   s" STATUS.md" SS-ROOTED$ SS-LOAD-FILE
   0 SS-LINE-S !
   begin SS-LINE-S @ SS-SRC$ nip < while
      SS-SET-LINE-END
      SS-CURRENT-LINE SS-STATUS-LINE
      SS-ADVANCE-LINE
   repeat ;

: SS-TODAY-FROM-EPOCH ( -- ptr u8 n )
   SS-TODAY-DAYS @ SS-TODAY-BUF DATE-LEN FORMAT-YMD ;

: SS-BAD+ ( -- )
   SS-BAD @ 1+ SS-BAD ! ;

: SS-BAD-TODAY ( ptr u8 n -- )
   s" BAD-TODAY today argument invalid `" SS-OUT
   SS-OUT
   s" `" SS-OUT SS-NL
   1 throw ;

: SS-PARSE-TODAY ( ptr u8 n -- n ) {: a:ptr u :}
   a u PARSE-YMD 0= IF drop a u SS-BAD-TODAY 0 THEN ;

: SS-BAD-STATUS-DATE ( -- )
   s" BAD-STATUS-DATE STATUS.md: Last verified invalid `" SS-OUT
   SS-DATE$ SS-OUT
   s" `" SS-OUT SS-NL
   SS-BAD+ ;

: SS-TODAY$ ( -- ptr u8 n )
   SS-TODAY-FROM-EPOCH ;

: SS-MISSING-STATUS ( -- )
   s" STALE-STATUS STATUS.md: missing `Last verified: YYYY-MM-DD`" SS-OUT SS-NL
   SS-BAD+ ;

: SS-DATE-MISMATCH ( ptr u8 n -- )
   s" STALE-STATUS STATUS.md: Last verified is " SS-OUT
   SS-DATE$ SS-OUT
   s" , expected " SS-OUT
   SS-OUT SS-NL
   SS-BAD+ ;

: SS-CHECK-STATUS ( -- )
   SS-STATUS-DATE!
   SS-FOUND? @ 0= IF SS-MISSING-STATUS exit THEN
   SS-DATE$ PARSE-YMD 0= IF drop SS-BAD-STATUS-DATE exit THEN
   drop
   SS-TODAY$ 2dup SS-DATE$ STR= 0= IF SS-DATE-MISMATCH ELSE 2drop THEN ;

: SS-BEFORE-BOUND? ( ptr u8 n -- bool ) {: a:ptr pos :}
   pos 0= IF SS-TRUE exit THEN
   a pos 1- + c@ SS-ALNUM? SS-NOT ;

: SS-AFTER-BOUND? ( ptr u8 n n -- bool ) {: a:ptr u pos :}
   pos u >= IF SS-TRUE exit THEN
   a pos + c@ SS-ALNUM? SS-NOT ;

: SS-SLASH-RUN ( ptr u8 n n -- n bool ) {: a:ptr u pos :}
   pos u >= IF pos SS-FALSE exit THEN
   a pos + c@ SS-SLASH <> IF pos SS-FALSE exit THEN
   pos 1+ SS-RUN !
   SS-RUN @ SS-DIGITS !
   begin SS-RUN @ u <  a SS-RUN @ + c@ SS-DIGIT? and while
      SS-RUN @ 1+ SS-RUN !
   repeat
   SS-RUN @  SS-RUN @ SS-DIGITS @ > ;

: SS-WORD-AT? ( ptr u8 n n ptr u8 n -- bool ) {: a:ptr u pos b:ptr v :}
   u pos - v < IF SS-FALSE exit THEN
   a pos + v b v STR=CI ;

: SS-COUNT-LINE? ( ptr u8 n -- bool ) {: a:ptr u :}
   0 SS-SCAN-X !
   begin SS-SCAN-X @ u < while
      a SS-SCAN-X @ + c@ SS-DIGIT? IF
         a SS-SCAN-X @ SS-BEFORE-BOUND? IF
            0 SS-DIGITS !
            begin SS-SCAN-X @ u <  a SS-SCAN-X @ + c@ SS-DIGIT? and while
               SS-DIGITS @ 1+ SS-DIGITS !
               SS-SCAN-X @ 1+ SS-SCAN-X !
            repeat
            SS-DIGITS @ 3 >= IF
               SS-SCAN-X @ u < IF
                  a SS-SCAN-X @ + c@ SS-SLASH = IF
                     a u SS-SCAN-X @ SS-SLASH-RUN IF
                        SS-SCAN-X !
                        a u SS-SCAN-X @ SS-SLASH-RUN IF
                           SS-SCAN-X !
                           a u SS-SCAN-X @ SS-AFTER-BOUND? IF SS-TRUE exit THEN
                        ELSE
                           drop
                        THEN
                     ELSE
                        drop
                     THEN
                  ELSE
                     a SS-SCAN-X @ + c@ WS? IF
                        begin SS-SCAN-X @ u <  a SS-SCAN-X @ + c@ WS? and while
                           SS-SCAN-X @ 1+ SS-SCAN-X !
                        repeat
                        a u SS-SCAN-X @ s" certified" SS-WORD-AT? IF SS-TRUE exit THEN
                        a u SS-SCAN-X @ s" uncheckable" SS-WORD-AT? IF SS-TRUE exit THEN
                     THEN
                  THEN
               THEN
            THEN
         ELSE
            SS-SCAN-X @ 1+ SS-SCAN-X !
         THEN
      ELSE
         SS-SCAN-X @ 1+ SS-SCAN-X !
      THEN
   repeat
   SS-FALSE ;

: SS-FINDING ( -- )
   s" STALE-STATUS " SS-OUT
   SS-DISP$ SS-OUT
   SS-COLON SS-C
   SS-LINE-N @ SS-U.
   s" : count-shaped string - point to STATUS.md instead of quoting a number" SS-OUT
   SS-NL
   SS-BAD+ ;

: SS-SCAN-CURRENT-LINE ( -- )
   SS-CURRENT-LINE SS-FENCE-LINE? IF SS-FENCE-TOGGLE exit THEN
   SS-FENCE @ 0= IF
      SS-CURRENT-LINE SS-COUNT-LINE? IF SS-FINDING THEN
   THEN ;

: SS-SCAN-MD ( ptr u8 n -- ) {: a:ptr u :}
   a u SS-ALLOWED? IF exit THEN
   a u SS-MD? SS-NOT IF exit THEN
   a u EXISTS? SS-NOT IF exit THEN
   a u SS-DISPLAY!
   a u SS-LOAD-FILE
   0 SS-FENCE !
   0 SS-LINE-N !
   0 SS-LINE-S !
   begin SS-LINE-S @ SS-SRC$ nip < while
      SS-LINE-N @ 1+ SS-LINE-N !
      SS-SET-LINE-END
      SS-SCAN-CURRENT-LINE
      SS-ADVANCE-LINE
   repeat ;

: STALE-STATUS-LINT ( -- )
   0 SS-BAD !
   SS-CHECK-STATUS
   SS-ROOT [: SS-SCAN-MD ;] WALK-FILES
   s" stale-status-lint: " SS-OUT SS-BAD @ SS-U. s"  finding(s)" SS-OUT SS-NL
   SS-BAD @ 0 > IF 1 throw THEN ;

: SS-CONFIG ( -- )
   s" tools/stale-status-lint.f [ROOT] [TODAY]" ARGV-USAGE!
   ARGV-PARSE
   0 2 ARGV-EXPECT-POS
   ARGV-POS# 0 > IF 0 ARGV-POS$ SS-ROOT! ELSE s" ." SS-ROOT! THEN
   ARGV-POS# 1 > IF
      1 ARGV-POS$ SS-PARSE-TODAY SS-TODAY-DAYS !
   ELSE
      epoch-seconds DATE-SECONDS-DAY / SS-TODAY-DAYS !
   THEN ;

: SS-MAIN ( -- )
   SS-CONFIG
   STALE-STATUS-LINT ;

SS-MAIN
