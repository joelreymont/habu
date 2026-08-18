\ native-recorder.f - the two ceilings a recording unit is opened with, and what
\ they admit now that neither of them is a number this tree picked.
\ One concern: src/compiler/native/migrate.f TEXT-CAP and TAPE-ROOM.
\
\ WHAT THIS SUITE HAS TO SHOW.
\
\   1. That the byte ceiling is the ENGINE's and not the recorder's opinion. The
\      cases below build their sources against BODYBUF-CAP, the engine's own
\      body-capture buffer, and not against a literal: a source of exactly that
\      many bytes is recorded and compiled, and one byte more is refused by name.
\      Both halves fail if the recorder ever picks its own number again - a
\      smaller one reds the first case, a larger one reds the second - so what is
\      pinned is the DERIVATION and not the value.
\
\   2. That the tape follows the source. A body of two hundred and fifty-two
\      tokens is past the fixed hundred and twenty-eight rows a unit used to be
\      opened with, and its tokens are as dense as a token stream can be - two
\      bytes each, one of spelling and the space after it - so it is also the
\      case that holds the derivation tight: a tape of one row per two source
\      bytes has exactly enough room for it and nothing spare.
\
\   3. That a real definition the recorder used to refuse compiles and ANSWERS.
\      lib/date.f DAYS>YMD is 851 bytes of source, one of the 151 definitions of
\      src and lib the 512-byte ceiling kept unmeasured on 2026-08-12, and it is
\      the one the dot named. Its body text is migrated inside its own package,
\      where its private scratch cells resolve, and the migrated word is run
\      against the engine's own compilation of the real one on pinned days.
\
\ WHAT IT DELIBERATELY DOES NOT SHOW. That every long definition compiles. The
\ recorder is no longer the thing that answers first, and what answers instead is
\ downstream and named: the elaborator holds a body's tokens in a table of 256
\ (src/compiler/native/elaborate.f TMAX, E-NELAB-BLOCK), so a body past that is
\ refused there. That ceiling is the next one to derive and it belongs to its own
\ file, not to this one.

require lib/test.f
require lib/date.f
require src/compiler/native/migrate.f

package NRC-TEST

private

\ ---- building one source ------------------------------------------------------
\ Its own appender: these sources reach eight kilobytes and the shared string
\ builder holds one.
$4000 constant B-CAP

here CELL 1- and CELL swap - CELL 1- and allot
create B-BUF B-CAP allot
variable B-U

: B-RESET ( -- )
   0 B-U ! ;

: B+ ( ptr u8 n -- ) {: a:ptr u:n :}
   B-U @ u + B-CAP > if E-STR-CAPACITY throw then
   a  B-BUF B-U @ +  u STR-LEN BYTE-COPY-LEN
   B-U @ u + B-U ! ;

: B-C ( n -- ) {: c:n :}
   B-U @ 1+ B-CAP > if E-STR-CAPACITY throw then
   c B-BUF B-U @ + c!
   B-U @ 1+ B-U ! ;

: B$ ( -- ptr u8 n )
   B-BUF B-U @ ;

\ Pad with a byte that is not a delimiter until the source is exactly n bytes
\ long. The padding sits inside a parenthesised comment, so it is bytes of the
\ definition and of the engine's capture and NOT tokens of the tape: what the
\ length cases vary is length and nothing else.
: PAD-TO ( n -- ) {: n:n :}
   begin B-U @ n < while
      112 B-C
   repeat ;

\ ---- the length fixtures ------------------------------------------------------
\ A definition of exactly n bytes whose body is two tokens. The suffix is closed
\ first so the padding can be counted against the whole.
7 constant TAIL-U                    \ ` ) 1+ ;`

: LEN-SRC ( n -- ptr u8 n ) {: n:n :}
   B-RESET
   s" : NRC-LONG ( n -- n ) ( " B+
   n TAIL-U - PAD-TO
   s"  ) 1+ ;" B+
   B$ ;

variable LEN-N

: LEN-GO ( -- )
   LEN-N @ LEN-SRC NMIGRATE:MEASURE-HELD ;

: LEN-TRY ( n -- n ) {: n:n :}
   n LEN-N !
   [: LEN-GO ;] catch ;

\ ---- the density fixture ------------------------------------------------------
\ `1 + 1 - ` is four tokens in eight bytes, which is the densest a token stream
\ can be: a token is at least one byte of spelling and the space after it. Sixty
\ three of them is 252 tokens - past the 128 rows a unit used to be opened with,
\ and inside the elaborator's own 256-token table - in about 530 bytes, past the
\ 512 bytes the recorder used to take.
63 constant DENSE-STEPS

: DENSE-SRC ( -- ptr u8 n )
   B-RESET
   s" : NRC-DENSE ( n -- n ) " B+
   DENSE-STEPS 0 ?do  s" 1 + 1 - " B+  loop
   s" ;" B+
   B$ ;

: DENSE-GO ( -- )
   DENSE-SRC NMIGRATE:MEASURE-HELD ;

\ ---- the real definition ------------------------------------------------------
\ lib/date.f DAYS>YMD, line for line, under a name of its own. Nothing else about
\ it is changed: the same scratch cells, the same constants, the same two guards
\ at the end.
: L ( ptr u8 n -- )
   B+ s"  " B+ ;

: YMD-SRC ( -- ptr u8 n )
   B-RESET
   s" : NRC-YMD ( n -- n n n ) {: days:n :}" L
   s" days DATE-UNIX-EPOCH-DAY + DATE-Z !" L
   s" DATE-Z @ DATE-DAYS-ERA / DATE-ERA !" L
   s" DATE-Z @ DATE-ERA @ DATE-DAYS-ERA * - DATE-DOE !" L
   s" DATE-DOE @  DATE-DOE @ DATE-YOE-LEAP-CORR / -  DATE-DOE @ DATE-YOE-CENTURY-CORR / +  DATE-DOE @ DATE-LAST-DAY-ERA / -  DATE-DAYS-YEAR / DATE-YOE !" L
   s" DATE-YOE @ DATE-ERA @ DATE-ERA-YEARS * + DATE-Y !" L
   s" DATE-DOE @  DATE-DAYS-YEAR DATE-YOE @ *  DATE-YOE @ DATE-LEAP-YEARS / +  DATE-YOE @ DATE-CENTURY-YEARS / -  - DATE-DOY !" L
   s" DATE-MP-DIVISOR DATE-DOY @ * DATE-MP-BIAS + DATE-MP-SCALE / DATE-MP !" L
   s" DATE-DOY @  DATE-MP-SCALE DATE-MP @ * DATE-MP-BIAS + DATE-MP-DIVISOR /  - 1 + DATE-D !" L
   s" DATE-MP @ DATE-MP-LIMIT < IF DATE-MP @ DATE-MAR-BIAS + ELSE DATE-MP @ DATE-JAN-FEB-BIAS - THEN DATE-M !" L
   s" DATE-M @ DATE-FEB <= IF DATE-Y @ 1+ DATE-Y ! THEN" L
   s" DATE-Y @ DATE-M @ DATE-D @ ;" B+
   B$ ;

public

\ Migrated where its names resolve. It is called from inside `package DATE
\ public` below, which is why it is a public word of this package rather than a
\ private one - and it is called BEFORE the cases are compiled, because a case
\ that calls the migrated word has to be compiled against a word that exists.
: MIGRATE-YMD ( -- )
   YMD-SRC NMIGRATE:DEFINE ;

;package

package DATE
public
NRC-TEST:MIGRATE-YMD
;package

package NRC-TEST

private

\ ---- the cases ----------------------------------------------------------------
\ The engine's own body capture is what the recorder's ceiling is, so the cases
\ name that constant rather than the number it happens to hold today.
: CEILING-CASE ( -- )
   s" a source of exactly the engine's capture size is recorded and compiled" T-LABEL
   BODYBUF-CAP LEN-TRY 0 T=

   s" and one byte more is refused by the recorder, by name" T-LABEL
   BODYBUF-CAP 1+ LEN-TRY E-NMIGRATE-TEXT T=

   s" the fixture really is at the ceiling and not near it" T-LABEL
   BODYBUF-CAP LEN-SRC nip BODYBUF-CAP T=
   BODYBUF-CAP 1+ LEN-SRC nip BODYBUF-CAP 1+ T=

   s" and the ceiling really is past the 512 bytes it used to be" T-LABEL
   1024 LEN-TRY 0 T=
   4096 LEN-TRY 0 T= ;

: DENSE-CASE ( -- )
   s" the densest token stream past the old fixed tape compiles" T-LABEL
   [: DENSE-GO ;] catch 0 T=

   s" and it is past both numbers a unit used to be opened with" T-LABEL
   DENSE-SRC nip 512 > TTRUE
   DENSE-STEPS 4 * 128 > TTRUE

   \ One row per two source bytes against a stream of two-byte tokens plus the
   \ name row: the rows to spare are what the definition's own prefix and closer
   \ pay for, a dozen and no more. A tape sized any other way moves this number.
   s" with a derived tape that has room for it and barely any spare" T-LABEL
   DENSE-SRC nip 2 /  DENSE-STEPS 4 * 1+ -  {: spare:n :}
   spare 0 > TTRUE
   spare 16 < TTRUE ;

\ The migrated body against the engine's own compilation of the real one. Every
\ day is answered as three values, so the two are compared field by field: a
\ chain that got the month right and the year wrong would pass a comparison of
\ any one of them.
: YMD-AT ( n -- ) {: d:n :}
   d DATE:DAYS>YMD {: ey:n em:n ed:n :}
   d DATE:NRC-YMD {: cy:n cm:n cd:n :}
   cy ey T=  cm em T=  cd ed T= ;

: YMD-CASE ( -- )
   s" the 851-byte definition the recorder used to refuse answers the engine" T-LABEL
   0 YMD-AT
   1 YMD-AT
   -1 YMD-AT
   59 YMD-AT
   60 YMD-AT
   365 YMD-AT
   -365 YMD-AT
   11322 YMD-AT
   -25567 YMD-AT
   20609 YMD-AT

   s" and its source really is the length that was refused" T-LABEL
   YMD-SRC nip 512 > TTRUE
   YMD-SRC nip BODYBUF-CAP < TTRUE ;

public

: RUN ( -- )
   CEILING-CASE
   DENSE-CASE
   YMD-CASE ;

;package

T-RESET
NRC-TEST:RUN
T-REPORT
