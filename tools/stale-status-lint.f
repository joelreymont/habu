\ stale-status-lint.f - enforce STATUS.md as the only live self-check count.
\ Load after tools/lint/lib.f and tools/fs.f. Run with bin/hb.

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

: SS-SKIP {: a u n :} ( a u n -- a' u' )
   a n +  u n - ;

: SS-C! ( c -- ) SS-ONE c! ;

: SS-OUT ( a u -- )
   dup 0= IF 2drop exit THEN
   1 -rot write drop ;

: SS-ERR ( a u -- )
   dup 0= IF 2drop exit THEN
   2 -rot write drop ;

: SS-C ( c -- )
   SS-C!
   SS-ONE 1 SS-OUT ;

: SS-NL ( -- ) SS-LF SS-C ;

: SS-U. ( u -- )
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

: SS-2D ( u -- )
   dup 10 < IF SS-ZERO SS-C THEN
   SS-U. ;

: SS-4D ( u -- )
   dup 1000 < IF SS-ZERO SS-C THEN
   dup 100 < IF SS-ZERO SS-C THEN
   dup 10 < IF SS-ZERO SS-C THEN
   SS-U. ;

: SS-DIGIT? ( c -- f )
   dup 47 > swap 58 < and ;

: SS-ALNUM? ( c -- f )
   dup SS-DIGIT? IF drop -1 exit THEN
   dup 64 > over 91 < and IF drop -1 exit THEN
   dup 96 > swap 123 < and ;

: SS-REL {: a u :} ( a u -- a' u' )
   u 2 >= IF
      a c@ 46 =  a 1 + c@ SS-SLASH = and IF a 2 + u 2 - exit THEN
   THEN
   a u ;

: SS-DISPLAY! ( a u -- )
   SS-REL SS-DISP-U ! SS-DISP-A ! ;

: SS-ALLOWED? ( a u -- f )
   SS-REL
   2dup s" STATUS.md" STR= IF 2drop -1 exit THEN
   s" LESSONS.md" STR= ;

: SS-MD? ( a u -- f )
   s" .md" HAS-EXT? ;

: SS-LINE-PREFIX? {: a u b v :} ( a u b v -- f )
   a u b v STARTS-WITH? ;

: SS-STATUS-DATE! ( -- )
   0 SS-FOUND? !
   s" STATUS.md" SS-FILE-BUF SS-FILE-CAP READ-FILE SPLIT-LINES
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

: SS-TODAY-FROM-EPOCH ( -- a u )
   SS-TODAY-DAYS @ SS-TODAY-BUF DATE-LEN FORMAT-YMD ;

: SS-BAD+ ( -- )
   SS-BAD @ 1+ SS-BAD ! ;

: SS-BAD-TODAY ( a u -- )
   s" BAD-TODAY STALE_STATUS_TODAY invalid `" SS-OUT
   SS-OUT
   s" `" SS-OUT SS-NL
   1 throw ;

: SS-BAD-STATUS-DATE ( -- )
   s" BAD-STATUS-DATE STATUS.md: Last verified invalid `" SS-OUT
   SS-DATE-A @ SS-DATE-U @ SS-OUT
   s" `" SS-OUT SS-NL
   SS-BAD+ ;

: SS-TODAY$ ( -- a u )
   s" STALE_STATUS_TODAY" GETENV dup 0 > IF
      2dup PARSE-YMD 0= IF drop SS-BAD-TODAY THEN
      drop exit
   THEN
   2drop SS-TODAY-FROM-EPOCH ;

: SS-MISSING-STATUS ( -- )
   s" STALE-STATUS STATUS.md: missing `Last verified: YYYY-MM-DD`" SS-OUT SS-NL
   SS-BAD+ ;

: SS-DATE-MISMATCH ( a u -- )
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

: SS-BEFORE-BOUND? {: a pos :} ( a pos -- f )
   pos 0= IF -1 exit THEN
   a pos 1- + c@ SS-ALNUM? 0= ;

: SS-AFTER-BOUND? {: a u pos :} ( a u pos -- f )
   pos u >= IF -1 exit THEN
   a pos + c@ SS-ALNUM? 0= ;

: SS-SLASH-RUN {: a u pos :} ( a u pos -- pos' ok )
   pos u >= IF pos 0 exit THEN
   a pos + c@ SS-SLASH <> IF pos 0 exit THEN
   pos 1+ SS-RUN !
   SS-RUN @ SS-DIGITS !
   begin SS-RUN @ u <  a SS-RUN @ + c@ SS-DIGIT? and while
      SS-RUN @ 1+ SS-RUN !
   repeat
   SS-RUN @  SS-RUN @ SS-DIGITS @ > ;

: SS-WORD-AT? {: a u pos b v :} ( a u pos b v -- f )
   u pos - v < IF 0 exit THEN
   a pos + v b v STR=CI ;

: SS-COUNT-LINE? {: a u :} ( a u -- f )
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
                           a u SS-SCAN-X @ SS-AFTER-BOUND? IF -1 exit THEN
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
                        a u SS-SCAN-X @ s" certified" SS-WORD-AT? IF -1 exit THEN
                        a u SS-SCAN-X @ s" uncheckable" SS-WORD-AT? IF -1 exit THEN
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
   0 ;

: SS-FINDING ( -- )
   s" STALE-STATUS " SS-OUT
   SS-DISP-A @ SS-DISP-U @ SS-OUT
   SS-COLON SS-C
   SS-LINE-N @ SS-U.
   s" : count-shaped string - point to STATUS.md instead of quoting a number" SS-OUT
   SS-NL
   SS-BAD+ ;

: SS-SCAN-MD {: a u :} ( a u -- )
   a u SS-ALLOWED? IF exit THEN
   a u SS-MD? 0= IF exit THEN
   a u EXISTS? 0= IF exit THEN
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
   s" ." ['] SS-SCAN-MD WALK-FILES
   s" stale-status-lint: " SS-OUT SS-BAD @ SS-U. s"  finding(s)" SS-OUT SS-NL
   SS-BAD @ 0 > IF 1 throw THEN ;

epoch-seconds DATE-SECONDS-DAY / SS-TODAY-DAYS !
STALE-STATUS-LINT
