\ stale-status-lint.f - enforce STATUS.md as the only live self-check count.
\ Load after tools/lint/lib.f and tools/fs.f. Run with bin/hb.

0 set-check

$20000 constant SS-FILE-CAP
32 constant SS-NUM-CAP
16 constant SS-DATE-CAP

10 constant SS-LF
45 constant SS-DASH
47 constant SS-SLASH
48 constant SS-ZERO
58 constant SS-COLON

create SS-FILE-BUF SS-FILE-CAP allot
create SS-NUM-BUF SS-NUM-CAP allot
create SS-TODAY-BUF SS-DATE-CAP allot
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
variable SS-Y
variable SS-M
variable SS-D
variable SS-Z
variable SS-ERA
variable SS-DOE
variable SS-YOE
variable SS-DOY
variable SS-MP

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

: SS-CIVIL! {: days :} ( days -- )
   days 719468 + SS-Z !
   SS-Z @ 146097 / SS-ERA !
   SS-Z @ SS-ERA @ 146097 * - SS-DOE !
   SS-DOE @  SS-DOE @ 1460 / -  SS-DOE @ 36524 / +  SS-DOE @ 146096 / -  365 / SS-YOE !
   SS-YOE @ SS-ERA @ 400 * + SS-Y !
   SS-DOE @  365 SS-YOE @ *  SS-YOE @ 4 / +  SS-YOE @ 100 / -  - SS-DOY !
   5 SS-DOY @ * 2 + 153 / SS-MP !
   SS-DOY @  153 SS-MP @ * 2 + 5 /  - 1 + SS-D !
   SS-MP @ 10 < IF SS-MP @ 3 + ELSE SS-MP @ 9 - THEN SS-M !
   SS-M @ 2 <= IF SS-Y @ 1+ SS-Y ! THEN ;

: SS-DATE-C! {: c pos :} ( c pos -- )
   c SS-TODAY-BUF pos + c! ;

: SS-DATE-N! {: n width pos :} ( n width pos -- )
   n SS-RUN !
   width 1- SS-SCAN-X !
   begin SS-SCAN-X @ 0 >= while
      SS-RUN @ 10 mod SS-ZERO +  pos SS-SCAN-X @ + SS-DATE-C!
      SS-RUN @ 10 / SS-RUN !
      SS-SCAN-X @ 1- SS-SCAN-X !
   repeat ;

: SS-TODAY-FROM-EPOCH ( -- a u )
   SS-Y @ 4 0 SS-DATE-N!
   SS-DASH 4 SS-DATE-C!
   SS-M @ 2 5 SS-DATE-N!
   SS-DASH 7 SS-DATE-C!
   SS-D @ 2 8 SS-DATE-N!
   SS-TODAY-BUF 10 ;

: SS-TODAY$ ( -- a u )
   s" STALE_STATUS_TODAY" GETENV dup 0 > IF exit THEN
   2drop SS-TODAY-FROM-EPOCH ;

: SS-BAD+ ( -- )
   SS-BAD @ 1+ SS-BAD ! ;

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

epoch-seconds 86400 / SS-CIVIL!
STALE-STATUS-LINT
