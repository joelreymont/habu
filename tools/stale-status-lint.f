\ stale-status-lint.f - enforce STATUS.md as the only live self-check count.
\ Load after tools/date.f, tools/lint/lib.f, tools/fs.f, and tools/argv.f.
\ Run: bin/hb --load tools/date.f tools/lint/lib.f tools/fs.f tools/argv.f tools/stale-status-lint.f -- [ROOT] [TODAY]

0 set-check

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

: SS-CHECK-HOOK ( -- )
   CHECK! ;
' SS-CHECK-HOOK set-check

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
   SS-ROOT-U @ 0 >  SS-ROOT-A @ SS-ROOT-U @ s" ." STR= 0= and IF
      u SS-ROOT-U @ > IF
         a SS-ROOT-U @ SS-ROOT-A @ SS-ROOT-U @ STR= IF
            a SS-ROOT-U @ + c@ SS-SLASH = IF
               a SS-ROOT-U @ 1 + +  u SS-ROOT-U @ 1 + -  exit
            THEN
         THEN
      THEN
   THEN
   u 2 >= IF
      a c@ 46 =  a 1 + c@ SS-SLASH = and IF a 2 + u 2 - exit THEN
   THEN
   a u ;

: SS-ROOT ( -- ptr u8 n )
   SS-ROOT-A @ SS-ROOT-U @ ;

: SS-ROOT! ( ptr u8 n -- ) {: a:ptr u :}
   a SS-ROOT-A !
   u SS-ROOT-U ! ;

: SS-ROOT-SELF? ( -- bool )
   SS-ROOT-U @ 0= IF SS-TRUE exit THEN
   SS-ROOT-A @ SS-ROOT-U @ s" ." STR= ;

: SS-ROOTED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   SS-ROOT-SELF? IF a u exit THEN
   SS-ROOT-U @ u + 1 + FS-PATH-CAP > IF s" stale-status-lint: root path too long" 1 die THEN
   SS-ROOT-A @ SS-PATH-BUF SS-ROOT-U @ COPY-BYTES
   SS-ROOT-A @ SS-ROOT-U @ 1 - + c@ SS-SLASH = IF
      a SS-PATH-BUF SS-ROOT-U @ + u COPY-BYTES
      SS-PATH-BUF SS-ROOT-U @ u + exit
   THEN
   SS-SLASH SS-PATH-BUF SS-ROOT-U @ + c!
   a SS-PATH-BUF SS-ROOT-U @ 1 + + u COPY-BYTES
   SS-PATH-BUF SS-ROOT-U @ 1 + u + ;

: SS-DISPLAY! ( ptr u8 n -- )
   SS-REL SS-DISP-U ! SS-DISP-A ! ;

: SS-ALLOWED? ( ptr u8 n -- bool )
   SS-REL
   2dup s" STATUS.md" STR= IF 2drop SS-TRUE exit THEN
   s" LESSONS.md" STR= ;

: SS-MD? ( ptr u8 n -- bool )
   s" .md" HAS-EXT? ;

: SS-LINE-PREFIX? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   a u b v STARTS-WITH? ;

: SS-STATUS-DATE! ( -- )
   0 SS-FOUND? !
   s" STATUS.md" SS-ROOTED$ SS-FILE-BUF SS-FILE-CAP READ-FILE SPLIT-LINES
   0 begin dup SN# @ < while
      dup S@ TRIM
      2dup s" Last verified:" SS-LINE-PREFIX? IF
         14 SS-SKIP LTRIM
         SS-DATE-U ! SS-DATE-A !
         -1 SS-FOUND? !
      ELSE
         2drop
      THEN
      1+
   repeat drop ;

: SS-TODAY-FROM-EPOCH ( -- ptr u8 n )
   SS-TODAY-DAYS @ SS-TODAY-BUF DATE-LEN FORMAT-YMD ;

: SS-BAD+ ( -- )
   SS-BAD @ 1+ SS-BAD ! ;

: SS-BAD-TODAY ( ptr u8 n -- )
   s" BAD-TODAY today argument invalid `" SS-OUT
   SS-OUT
   s" `" SS-OUT SS-NL
   1 throw ;

: SS-BAD-STATUS-DATE ( -- )
   s" BAD-STATUS-DATE STATUS.md: Last verified invalid `" SS-OUT
   SS-DATE-A @ SS-DATE-U @ SS-OUT
   s" `" SS-OUT SS-NL
   SS-BAD+ ;

: SS-TODAY$ ( -- ptr u8 n )
   SS-TODAY-FROM-EPOCH ;

: SS-MISSING-STATUS ( -- )
   s" STALE-STATUS STATUS.md: missing `Last verified: YYYY-MM-DD`" SS-OUT SS-NL
   SS-BAD+ ;

: SS-DATE-MISMATCH ( ptr u8 n -- )
   s" STALE-STATUS STATUS.md: Last verified is " SS-OUT
   SS-DATE-A @ SS-DATE-U @ SS-OUT
   s" , expected " SS-OUT
   SS-OUT SS-NL
   SS-BAD+ ;

: SS-CHECK-STATUS ( -- )
   SS-STATUS-DATE!
   SS-FOUND? @ 0= IF SS-MISSING-STATUS exit THEN
   SS-DATE-A @ SS-DATE-U @ PARSE-YMD 0= IF drop SS-BAD-STATUS-DATE exit THEN
   drop
   SS-TODAY$ 2dup SS-DATE-A @ SS-DATE-U @ STR= 0= IF SS-DATE-MISMATCH ELSE 2drop THEN ;

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
   SS-DISP-A @ SS-DISP-U @ SS-OUT
   SS-COLON SS-C
   SS-LINE-N @ SS-U.
   s" : count-shaped string - point to STATUS.md instead of quoting a number" SS-OUT
   SS-NL
   SS-BAD+ ;

: SS-SCAN-MD ( ptr u8 n -- ) {: a:ptr u :}
   a u SS-ALLOWED? IF exit THEN
   a u SS-MD? SS-NOT IF exit THEN
   a u EXISTS? SS-NOT IF exit THEN
   a u SS-DISPLAY!
   a u SS-FILE-BUF SS-FILE-CAP READ-FILE SPLIT-LINES
   0 begin dup SN# @ < while
      dup 1+ SS-LINE-N !
      dup S@ SS-COUNT-LINE? IF SS-FINDING THEN
      1+
   repeat drop ;

: STALE-STATUS-LINT ( -- )
   0 SS-BAD !
   SS-CHECK-STATUS
   SS-ROOT ['] SS-SCAN-MD WALK-FILES
   s" stale-status-lint: " SS-OUT SS-BAD @ SS-U. s"  finding(s)" SS-OUT SS-NL
   SS-BAD @ 0 > IF 1 throw THEN ;

: SS-CONFIG ( -- )
   s" tools/stale-status-lint.f [ROOT] [TODAY]" ARGV-USAGE!
   ARGV-PARSE
   0 2 ARGV-EXPECT-POS
   ARGV-POS# 0 > IF 0 ARGV-POS$ SS-ROOT! ELSE s" ." SS-ROOT! THEN
   ARGV-POS# 1 > IF
      1 ARGV-POS$ 2dup PARSE-YMD 0= IF drop SS-BAD-TODAY THEN
      SS-TODAY-DAYS ! 2drop
   ELSE
      epoch-seconds DATE-SECONDS-DAY / SS-TODAY-DAYS !
   THEN ;

: SS-MAIN ( -- )
   SS-CONFIG
   STALE-STATUS-LINT ;

SS-MAIN
