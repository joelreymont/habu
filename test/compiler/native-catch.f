\ native-catch.f - `catch` through the native chain, against the engine's own.
\ One concern: catching a quotation.
\
\ WHAT THE CHECKER HAS TO PUBLISH AND WHY A UNIT ASSERTION IS NOT ENOUGH ON ITS
\ OWN. `catch` has no declared effect: its rows are built where it stands, out of
\ the quotation in hand and the live stack under it, so the window - how many
\ stack cells the caught body may disturb - exists only at the site. The checker
\ records it against the TOKEN the catch stands on, and the chain asks about the
\ tape row it is elaborating; the claim that those two ordinals are the same
\ ordinal is what everything else here rests on. So it is measured twice: the
\ cases below read the recorded windows back at hand-counted ordinals, and the
\ differential cases run the compiled code. A keying that is off by one answers
\ some other site's window, which the first set catches as a wrong number and the
\ second as a wrong ANSWER.
\
\ THE TWO SHAPES THAT WOULD PASS A CARELESS FIXTURE ARE BOTH HERE. A definition
\ whose catch stands after a STRING LITERAL is the one that separates "count the
\ tokens" from "count the reports": the reader spends a literal's payload rather
\ than tokenising it, so a producer that counted the payload's words would put
\ every later site one or more rows out. And a definition holding TWO catches
\ with DIFFERENT windows is the one that separates a keyed table from a latch:
\ with a latch both sites answer whichever was recorded last, and every fixture
\ whose two sites happen to agree passes anyway.
\
\ THE ENGINE'S ANSWER IS THE SPEC, AND THE INTERESTING PART OF IT IS THAT `catch`
\ RESTORES THE DEPTH AND NEVER THE CONTENTS. A caught body that drops the cell
\ under it and pushes another leaves THAT value where the caller's was, and the
\ caller gets it back - so `7 [: drop 5 throw ;] catch` answers 5 under the code
\ and not 7. A chain that kept the window in a register across the call would
\ answer 7: correct-looking, and wrong in exactly the way no shape assertion can
\ see. Every differential below is run against the engine's own compilation of
\ the same text.

require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require src/compiler/native/migrate.f

package NCA-TEST

private

18 constant REGS

\ Compiling a body without publishing anything, so a refusal can be measured with
\ nothing left behind on the way out.
: MEASURE-AT ( ptr u8 n n n -- )
   REGS NMIGRATE:MEASURE-HELD ;

\ ---- reading the windows a recorded definition's catch sites took -------------
\ The chain's own reader, asked about the tape the migration above just recorded.
\ Reading it after the unit closed is the point: that is when the elaborator reads
\ it, and the table is kept for exactly that reason.
: WIN-IN ( n -- n )
   NDICT:CATCH-CELLS drop ;

: WIN-OUT ( n -- n )
   NDICT:CATCH-CELLS nip ;

\ How many of the first `n` tape rows carry a recorded window. A case that only
\ checked the sites it expected would pass against a table that had recorded a
\ site on every token, so the count is asserted too.
: SITES ( n -- n )
   0 swap 0 ?do
      i NDICT:CATCH-CELLS drop NDICT:CATCH-NONE <> if 1 + then
   loop ;

\ ---- the fixture sources -----------------------------------------------------
\ Each one is followed by its token numbering, because the assertions name
\ ordinals and a reader has to be able to check them: the definition's NAME is
\ row 0 - it is the one token the outer interpreter reads before the parser
\ switches to compiling - the signature is not a token at all, and the closing
\ `;` is gone before the checker sees anything.

\ 0 NCA-W1  1 [:  2 drop  3 5  4 throw  5 ;]  6 catch
: SRC-DEAD ( -- )
   s" : NCA-W1 ( n -- n n ) [: drop 5 throw ;] catch ;" 1 2 MEASURE-AT ;

\ 0 NCA-W2  1 [:  2 1+  3 ;]  4 catch
: SRC-LIVE ( -- )
   s" : NCA-W2 ( n -- n n ) [: 1+ ;] catch ;" 1 2 MEASURE-AT ;

\ 0 NCA-W0  1 [:  2 1  3 2  4 3  5 throw  6 ;]  7 catch
: SRC-EMPTY ( -- )
   s" : NCA-W0 ( n -- n n ) [: 1 2 3 throw ;] catch ;" 1 2 MEASURE-AT ;

\ 0 NCA-WS  1 "hi"  2 2drop  3 [:  4 1+  5 ;]  6 catch
: SRC-STRING ( -- )
   S\" : NCA-WS ( n -- n n ) s\q hi\q 2drop [: 1+ ;] catch ;" 1 2 MEASURE-AT ;

\ 0 NCA-WW  1 [:  2 1+  3 swap  4 1+  5 swap  6 ;]  7 catch
\ 8 drop  9 [:  10 1+  11 ;]  12 catch
: SRC-TWO ( -- )
   s" : NCA-WW ( n n -- n n n ) [: 1+ swap 1+ swap ;] catch drop [: 1+ ;] catch ;"
   2 3 MEASURE-AT ;

\ ---- a definition with more catch sites than the checker records --------------
\ The ceiling is the checker's (CWIN-MAX beside RSCATCH). This fixture has to
\ EXCEED it, so the count is one more than that constant: if the ceiling moves,
\ the last site starts being recorded and the case below goes red, which is the
\ signal to move this number too. It is built rather than written out because
\ seventeen copies of one phrase is a line no reader can count.
17 constant CAP-SITES                \ one more than the checker's CWIN-MAX
4 constant SITE-TOKENS               \ [: ;] catch drop
512 constant CAP-BUF-CAP

here CELL 1- and CELL swap - CELL 1- and allot
create CAP-BUF CAP-BUF-CAP allot
variable CAP-U

: CAP+ ( ptr u8 n -- ) {: a:ptr u:n :}   \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   CAP-U @ u + CAP-BUF-CAP > if E-STR-CAPACITY throw then
   a  CAP-BUF CAP-U @ +  u STR-LEN BYTE-COPY-LEN
   CAP-U @ u + CAP-U ! ;

: CAP-BUILD ( -- ptr u8 n )
   0 CAP-U !
   s" : NCA-CAP ( n -- n ) " CAP+
   CAP-SITES 0 ?do
      s" [: ;] catch drop " CAP+
   loop
   s" ;" CAP+
   CAP-BUF CAP-U @ ;

\ Site k's `catch` is at row 1 + k*SITE-TOKENS + 2: the name is row 0 and each
\ site spends its opener and closer before its own token.
: CAP-SITE-TOK ( n -- n )
   SITE-TOKENS * 3 + ;

: SRC-CAP ( -- )
   CAP-BUILD 1 1 MEASURE-AT ;

\ Running one fixture and swallowing whatever the chain made of it. The recorded
\ windows are the subject here, and they are recorded by the SCAN - which has
\ happened either way, because the chain only ever runs after it.
: SCANNED ( -- )
   [: SRC-DEAD ;] catch drop ;

public

\ ---- the export --------------------------------------------------------------
: DEAD-CASE ( -- )
   s" a caught body that never returns publishes its window and no output" T-LABEL
   SCANNED
   6 WIN-IN 1 T=
   6 WIN-OUT NDICT:CATCH-NONE T=
   8 SITES 1 T= ;

: LIVE-CASE ( -- )
   s" a caught body that returns publishes both, and they are the same width" T-LABEL
   [: SRC-LIVE ;] catch drop
   4 WIN-IN 1 T=
   4 WIN-OUT 1 T=
   8 SITES 1 T= ;

: EMPTY-CASE ( -- )
   s" a body that takes nothing has a window of zero, not of the stack under it" T-LABEL
   [: SRC-EMPTY ;] catch drop
   7 WIN-IN 0 T=
   7 WIN-OUT NDICT:CATCH-NONE T=
   9 SITES 1 T= ;

: STRING-CASE ( -- )
   s" a string literal before the catch is one row, so the site keeps its ordinal" T-LABEL
   [: SRC-STRING ;] catch drop
   6 WIN-IN 1 T=
   6 WIN-OUT 1 T=
   \ and nothing was recorded on the rows a payload-counting producer would have
   \ pushed the site onto
   5 WIN-IN NDICT:CATCH-NONE T=
   7 WIN-IN NDICT:CATCH-NONE T=
   9 SITES 1 T= ;

: TWO-CASE ( -- )
   s" two catches in one definition keep their own windows" T-LABEL
   [: SRC-TWO ;] catch drop
   7 WIN-IN 2 T=
   12 WIN-IN 1 T=
   16 SITES 2 T= ;

: CAP-CASE ( -- )
   s" past the ceiling a site is not recorded, and answers absent" T-LABEL
   [: SRC-CAP ;] catch drop
   0 CAP-SITE-TOK WIN-IN 0 T=
   CAP-SITES 2 - CAP-SITE-TOK WIN-IN 0 T=
   CAP-SITES 1 - CAP-SITE-TOK WIN-IN NDICT:CATCH-NONE T=
   CAP-SITES 1 - CAP-SITE-TOK WIN-OUT NDICT:CATCH-NONE T=
   CAP-SITES SITE-TOKENS * 4 + SITES  CAP-SITES 1 -  T= ;

: NO-UNIT-CASE ( -- )
   s" a token of no recorded definition answers absent, in both halves" T-LABEL
   [: SRC-LIVE ;] catch drop
   0 WIN-IN NDICT:CATCH-NONE T=
   1 WIN-IN NDICT:CATCH-NONE T=
   99 WIN-IN NDICT:CATCH-NONE T=
   99 WIN-OUT NDICT:CATCH-NONE T= ;

: RUN ( -- )
   DEAD-CASE
   LIVE-CASE
   EMPTY-CASE
   STRING-CASE
   TWO-CASE
   CAP-CASE
   NO-UNIT-CASE ;

;package

T-RESET
NCA-TEST:RUN
T-REPORT
