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
require tools/codegen-compare-core.f

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
\
\ AND THE CEILING IS REACHABLE THROUGH THE REAL ENTRY, which is why the case can
\ measure the refusal rather than assert it in prose: seventeen sites is
\ sixty-nine tokens in about three hundred and thirty bytes, and the recorder
\ sizes a unit's tape from those bytes (src/compiler/native/migrate.f TAPE-ROOM,
\ one row per two source bytes), so the tape has room to spare and what answers
\ first is the checker's table and not a capacity of the recorder's.
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
   s" past the ceiling a site is not recorded, and the chain refuses it by name" T-LABEL
   [: SRC-CAP ;] E-NELAB-QUOT TTHROWSQ
   NELAB:REFUSED-ROW  CAP-SITES 1 - CAP-SITE-TOK  T=
   NELAB:REFUSED$ s" catch" T$=

   s" and every site below the ceiling was recorded" T-LABEL
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

\ ---- the engine's compilation: the reference ---------------------------------
\ Every body here is STRAIGHT-LINE, and that is a ceiling of the quotation path
\ rather than of `catch`: a quotation body holding any control structure is
\ refused by the IR verifier (E-IR-VERIFY-SUCCARG), measured on the parent binary
\ through the pre-existing route a body reaches - an argument a callee declares -
\ so it predates catch and is pinned as its own case below. What the bodies here
\ CALL may branch as much as it likes, which is how the throwing cases are
\ written.
package NCA-FIXTURE

public

: NCA-OK1 ( n -- n )
   1+ ;

\ Drops the cell it was handed, puts another in its place and throws. It is the
\ whole point of the differential: the caller gets the SECOND value back under
\ the throw code, because a caught throw restores the stack's depth and leaves
\ its contents alone. A chain that kept the window in a register answers the
\ value the site started with.
: NCA-CLOB ( n -- n )
   drop 5 dup 3 > if 9 throw then ;

\ A throw from two frames below the catch.
: NCA-DEEP2 ( n -- n )
   dup 3 > if 11 throw then ;

: NCA-DEEP1 ( n -- n )
   NCA-DEEP2 ;

\ Leaves MORE cells than it took before it throws, so the cells above the window
\ are written and the one IN the window is written with the first of them. The
\ caller sees that first value, which no reading of "the stack is restored" other
\ than the depth-only one predicts.
: NCA-WIDE ( n -- n )
   drop 1 2 3 4 dup 3 > if 7 throw then drop drop drop ;

: NCA-D1 ( n -- n n )
   [: NCA-CLOB ;] catch ;

: NCA-D2 ( n -- n n )
   [: NCA-OK1 ;] catch ;

\ The production shape: the code into a local, then a decision on it. Twenty of
\ the forty catch sites in src and lib are written this way. Its body calls
\ nothing, which used to be forced - a body that calls under a definition with a
\ locals group was a quotation-path ceiling - and now is only what this case
\ happens to measure: the calling twin of the same shape is a differential of its
\ own in test/compiler/native-quot-scope.f.
: NCA-D4 ( n -- n n )
   [: 1+ ;] catch {: rc:n :}
   rc 0 <> if 77 else 0 then ;

: NCA-D5 ( n -- n )
   3 0 ?do [: NCA-OK1 ;] catch drop loop ;

: NCA-D6 ( n -- n n )
   [: NCA-DEEP1 ;] catch ;

: NCA-D7 ( n -- n n )
   [: NCA-WIDE ;] catch ;

\ Two catches in one definition whose windows are two different widths, and the
\ shape a latched export would compile wrongly: the first takes two cells and the
\ second one.
: NCA-D8 ( n n -- n n n )
   [: 1+ swap 1+ swap ;] catch drop [: 1+ ;] catch ;

\ A string literal before the catch. The reader spends a literal's payload rather
\ than tokenising it, so this is the definition whose site would move if the
\ recorded window were filed against anything but the reader's own report count.
: NCA-D9 ( n -- n n )
   s" hi" 2drop [: NCA-OK1 ;] catch ;

\ A value PARKED on the return stack across the catch, on both paths. The catch
\ site is an ordinary bare call and a parked value crosses one exactly as a live
\ data value does - src/compiler/native/elaborate.f R-OPERANDS+ and R-RESULTS@
\ are inside CALL-OPERANDS+ and CALL-CLOSE, which is the staging DO-CATCH goes
\ through - so what these two measure is that the seam really is the ordinary one
\ and not a special case: the parked 42 comes back whether the body threw or not,
\ while the window cell under it answers 5 on the throwing path and 8 on the
\ other.
: NCA-PT ( n -- n n n )
   42 >r [: NCA-CLOB ;] catch r> ;

: NCA-PN ( n -- n n n )
   42 >r [: NCA-OK1 ;] catch r> ;

\ ---- the shapes that were once refused, compiled by the engine ----------------
\ Each is here so the cases below can measure what the ENGINE answers for the
\ very text the chain declined: a refusal is only interesting beside a working
\ program, and an acceptance that replaced one is only interesting beside the
\ same. Two of the three now compile through the chain as well, and their cases
\ hold the two compilations against each other; the body that never returns is
\ the one that is still refused.
: NCA-NR ( n -- n n )
   [: drop 5 throw ;] catch ;

: NCA-BC ( n -- n n )
   [: dup 3 > if 1+ then ;] catch ;

: NCA-BL ( n -- n n )
   [: NCA-OK1 ;] catch {: rc:n :}
   rc 0 <> if 77 else 0 then ;

;package

\ ---- the chain's compilation: the subject ------------------------------------
\ The same texts, character for character but for the fixture suffix on each
\ name, compiled through the production migration entry.
package NCA-MIGRATED

private

18 constant REGS

: CALLEE1 ( ptr u8 n ptr u8 n -- )   \ the spelling the source writes, and the word it denotes
   CODEGEN-COMPARE:CODE-ENTRY 1 1 NMIGRATE:CALLEE ;

: D1 ( -- )
   s" NCA-CLOB" s" NCA-FIXTURE:NCA-CLOB" CALLEE1
   s" : NCA-D1-N ( n -- n n ) [: NCA-CLOB ;] catch ;"
   1 2 REGS NMIGRATE:DEFINE-CALLING ;

: D2 ( -- )
   s" NCA-OK1" s" NCA-FIXTURE:NCA-OK1" CALLEE1
   s" : NCA-D2-N ( n -- n n ) [: NCA-OK1 ;] catch ;"
   1 2 REGS NMIGRATE:DEFINE-CALLING ;

: D4 ( -- )
   s" : NCA-D4-N ( n -- n n ) [: 1+ ;] catch {: rc:n :} rc 0 <> if 77 else 0 then ;"
   1 2 REGS NMIGRATE:DEFINE ;

: D5 ( -- )
   s" NCA-OK1" s" NCA-FIXTURE:NCA-OK1" CALLEE1
   s" : NCA-D5-N ( n -- n ) 3 0 ?do [: NCA-OK1 ;] catch drop loop ;"
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

: D6 ( -- )
   s" NCA-DEEP1" s" NCA-FIXTURE:NCA-DEEP1" CALLEE1
   s" : NCA-D6-N ( n -- n n ) [: NCA-DEEP1 ;] catch ;"
   1 2 REGS NMIGRATE:DEFINE-CALLING ;

: D7 ( -- )
   s" NCA-WIDE" s" NCA-FIXTURE:NCA-WIDE" CALLEE1
   s" : NCA-D7-N ( n -- n n ) [: NCA-WIDE ;] catch ;"
   1 2 REGS NMIGRATE:DEFINE-CALLING ;

: D8 ( -- )
   s" : NCA-D8-N ( n n -- n n n ) [: 1+ swap 1+ swap ;] catch drop [: 1+ ;] catch ;"
   2 3 REGS NMIGRATE:DEFINE ;

: D9 ( -- )
   s" NCA-OK1" s" NCA-FIXTURE:NCA-OK1" CALLEE1
   S\" : NCA-D9-N ( n -- n n ) s\q hi\q 2drop [: NCA-OK1 ;] catch ;"
   1 2 REGS NMIGRATE:DEFINE-CALLING ;

: PT ( -- )
   s" NCA-CLOB" s" NCA-FIXTURE:NCA-CLOB" CALLEE1
   s" : NCA-PT-N ( n -- n n n ) 42 >r [: NCA-CLOB ;] catch r> ;"
   1 3 REGS NMIGRATE:DEFINE-CALLING ;

: PN ( -- )
   s" NCA-OK1" s" NCA-FIXTURE:NCA-OK1" CALLEE1
   s" : NCA-PN-N ( n -- n n n ) 42 >r [: NCA-OK1 ;] catch r> ;"
   1 3 REGS NMIGRATE:DEFINE-CALLING ;

\ The caught body holding a control structure of its own, which used to be the
\ ceiling this file's last refusal case pinned. It calls nothing, so it goes
\ through the entry that stages no callee.
: BC ( -- )
   s" : NCA-BC-N ( n -- n n ) [: dup 3 > if 1+ then ;] catch ;"
   1 2 REGS NMIGRATE:DEFINE ;

public

: RUN ( -- )
   D1 D2 D4 D5 D6 D7 D8 D9 PT PN BC ;

;package

package NCA-FIXTURE
public

NCA-MIGRATED:RUN

;package

\ ---- the differentials -------------------------------------------------------
package NCA-DIFF

private

18 constant REGS

: MEASURE-AT ( ptr u8 n n n -- )
   REGS NMIGRATE:MEASURE-HELD ;

\ A migration that stages a callee cannot be measured without publishing - there
\ is no held entry that takes a staged list - so the two halves of the calling
\ case go through the publishing entry under names of their own.
: DEFINE-AT ( ptr u8 n n n -- )
   REGS NMIGRATE:DEFINE-CALLING ;

\ The spelling is the QUALIFIED one, because these two migrations run with this
\ suite's own package open rather than the fixture's: the source they compile is
\ evaluated in the scope this file is in, so a bare tail would resolve to nothing.
: CALLEE-OK1 ( -- )
   s" NCA-FIXTURE:NCA-OK1" s" NCA-FIXTURE:NCA-OK1"
   CODEGEN-COMPARE:CODE-ENTRY 1 1 NMIGRATE:CALLEE ;

\ THE TWO ANSWERS ARE BOUND BEFORE EITHER IS COMPARED, because a catch site
\ leaves TWO cells and a bare pair of comparators over four stack cells would
\ hold each answer against ITSELF: the top two values are the second call's
\ under and code, not one value from each call. Naming them is what makes each
\ assertion a comparison between the engine's answer and the chain's.
: D1= ( n -- ) {: v:n :}
   v NCA-FIXTURE:NCA-D1   v NCA-FIXTURE:NCA-D1-N
   {: eu:n er:n cu:n cr:n :}
   er cr T=  eu cu T= ;

: D2= ( n -- ) {: v:n :}
   v NCA-FIXTURE:NCA-D2   v NCA-FIXTURE:NCA-D2-N
   {: eu:n er:n cu:n cr:n :}
   er cr T=  eu cu T= ;

: D4= ( n -- ) {: v:n :}
   v NCA-FIXTURE:NCA-D4   v NCA-FIXTURE:NCA-D4-N
   {: eu:n er:n cu:n cr:n :}
   er cr T=  eu cu T= ;

: D5= ( n -- ) {: v:n :}
   v NCA-FIXTURE:NCA-D5   v NCA-FIXTURE:NCA-D5-N   T= ;

: D6= ( n -- ) {: v:n :}
   v NCA-FIXTURE:NCA-D6   v NCA-FIXTURE:NCA-D6-N
   {: eu:n er:n cu:n cr:n :}
   er cr T=  eu cu T= ;

: D7= ( n -- ) {: v:n :}
   v NCA-FIXTURE:NCA-D7   v NCA-FIXTURE:NCA-D7-N
   {: eu:n er:n cu:n cr:n :}
   er cr T=  eu cu T= ;

: D8= ( n n -- ) {: a:n b:n :}
   a b NCA-FIXTURE:NCA-D8  a b NCA-FIXTURE:NCA-D8-N
   {: e1:n e2:n e3:n c1:n c2:n c3:n :}
   e1 c1 T=  e2 c2 T=  e3 c3 T= ;

: D9= ( n -- ) {: v:n :}
   v NCA-FIXTURE:NCA-D9   v NCA-FIXTURE:NCA-D9-N
   {: eu:n er:n cu:n cr:n :}
   er cr T=  eu cu T= ;

: PT= ( n -- ) {: v:n :}
   v NCA-FIXTURE:NCA-PT   v NCA-FIXTURE:NCA-PT-N
   {: eu:n er:n ep:n cu:n cr:n cp:n :}
   ep cp T=  er cr T=  eu cu T= ;

: PN= ( n -- ) {: v:n :}
   v NCA-FIXTURE:NCA-PN   v NCA-FIXTURE:NCA-PN-N
   {: eu:n er:n ep:n cu:n cr:n cp:n :}
   ep cp T=  er cr T=  eu cu T= ;

: BC= ( n -- ) {: v:n :}
   v NCA-FIXTURE:NCA-BC   v NCA-FIXTURE:NCA-BC-N
   {: eu:n er:n cu:n cr:n :}
   er cr T=  eu cu T= ;

public

\ THE ONE ANSWER THAT DECIDES THIS WHOLE LANE. `7 NCA-D1` is 9 over 5, not 9 over
\ 7: the caught body dropped the caller's cell, wrote another in its slot and
\ threw, and what the engine restores is the DEPTH. The differential holds the
\ chain to that, and the number a chain that cached the window would answer - 7 -
\ is the value the site started with, which no shape assertion would ever notice.
: CONTENTS-CASE ( -- )
   s" a caught throw restores the depth and not the contents" T-LABEL
   7 NCA-FIXTURE:NCA-D1 {: eu:n er:n :}
   er 9 T=  eu 5 T=
   7 D1=  0 D1=  100 D1= ;

: NORMAL-CASE ( -- )
   s" the path where nothing throws answers the engine too" T-LABEL
   7 NCA-FIXTURE:NCA-D2 {: eu:n er:n :}
   er 0 T=  eu 8 T=
   7 D2=  0 D2=  -3 D2= ;

: LOCAL-CASE ( -- )
   s" the code into a local and a decision on it: the production shape" T-LABEL
   7 D4=  0 D4=  -3 D4= ;

: LOOP-CASE ( -- )
   s" a catch inside a counted loop, once per turn" T-LABEL
   7 NCA-FIXTURE:NCA-D5 10 T=
   7 D5=  0 D5=  -3 D5= ;

: DEEP-CASE ( -- )
   s" a throw from two frames below the catch, and the same body not throwing" T-LABEL
   7 NCA-FIXTURE:NCA-D6 {: du:n dr:n :}
   dr 11 T=  du 7 T=
   7 D6=  2 D6=  4 D6=  -1 D6= ;

: WIDEN-CASE ( -- )
   s" a body that leaves more cells than it took before it throws" T-LABEL
   7 NCA-FIXTURE:NCA-D7 {: wu:n wr:n :}
   wr 7 T=  wu 1 T=
   7 D7=  0 D7= ;

: TWO-WINDOW-CASE ( -- )
   s" two catches with two different windows, in one definition" T-LABEL
   5 6 D8=  0 0 D8=  -2 9 D8= ;

: STRING-SITE-CASE ( -- )
   s" a catch after a string literal keeps its own window" T-LABEL
   7 D9=  0 D9= ;

: PARKED-CASE ( -- )
   s" a value parked across the catch comes back, on both paths" T-LABEL
   7 NCA-FIXTURE:NCA-PT {: tu:n tr:n tp:n :}
   tp 42 T=  tr 9 T=  tu 5 T=
   7 NCA-FIXTURE:NCA-PN {: nu:n nr:n np:n :}
   np 42 T=  nr 0 T=  nu 8 T=
   7 PT=  0 PT=  -3 PT=
   7 PN=  0 PN=  -3 PN= ;

\ ---- what the chain still refuses, and whose refusal each one is -------------
\ Each pair is the same shape twice: the offending one and a twin without the
\ offence, so what the refusal is about is the shape and not something else in
\ the line. The engine compiles and runs every one of them, which the first line
\ of each case measures - so both refusals are the chain's alone.
: NORET-BODY-CASE ( -- )
   s" the engine runs a caught body that never returns" T-LABEL
   7 NCA-FIXTURE:NCA-NR {: nu:n nr:n :}
   nr 5 T=  nu 5 T=

   s" and the chain refuses it, while its returning twin compiles" T-LABEL
   [: s" : NCA-NR1 ( n -- n n ) [: drop 5 throw ;] catch ;" 1 2 MEASURE-AT ;]
   E-NELAB-QUOT TTHROWSQ
   [: s" : NCA-NR2 ( n -- n n ) [: 1+ ;] catch ;" 1 2 MEASURE-AT ;]
   0 TTHROWSQ ;

\ THE THIRD CEILING IS GONE TOO, and this case is what it left behind. A
\ quotation body holding any control structure used to be refused - through this
\ route and through the older one, an argument a callee declares - and it was
\ never `catch`'s ceiling but the body's.
\
\ WHAT THE REFUSAL WAS. A block names itself by an ordinal in the MODULE's block
\ table, which is what a successor carries, and by an ordinal in its OWN
\ function, which is what the passes index by; a body is a second function, so it
\ was the first thing in the tree whose two ordinals differed. The refusal
\ started as the freeze verifier's and depended on the ENCLOSING routine's block
\ count (E-IR-VERIFY-SUCCARG under a straight-line definition, E-IR-VERIFY-DOM
\ under one holding an `if`); the elaborator then named a body's successors in
\ the module's own table, which made it one refusal - the register allocator's -
\ and the machine passes then learnt the same subtraction the selector already
\ made (src/compiler/native/regalloc.f B-BASE!, and its two siblings in
\ regalloc-verify.f and emit.f).
\
\ WHAT IS LEFT HERE IS THE ANSWER. The straight-line twin is kept beside it,
\ because a body with no control structure names no successor at all and is the
\ row that would still pass if the subtraction were wrong.
\ test/compiler/native-quot-scope.f measures the branching body under every
\ enclosing shape and through both routes.
: BODY-CONTROL-CASE ( -- )
   s" a caught body holding a control structure, against the engine" T-LABEL
   7 NCA-FIXTURE:NCA-BC {: cu:n cr:n :}
   cr 0 T=  cu 8 T=
   7 BC=  3 BC=  4 BC=  0 BC=  -5 BC=

   s" and both it and its straight-line twin compile" T-LABEL
   [: s" : NCA-BC1 ( n -- n n ) [: dup 3 > if 1+ then ;] catch ;" 1 2 MEASURE-AT ;]
   0 TTHROWSQ
   [: s" : NCA-BC2 ( n -- n n ) [: 1+ ;] catch ;" 1 2 MEASURE-AT ;]
   0 TTHROWSQ ;

\ THE SECOND CEILING IS GONE, and this case is what it left behind: the same two
\ texts, now both compiled. A quotation body that CALLS, with a locals group in
\ the definition around it, used to be refused as an operand naming a value of
\ another function - the body was built with the enclosing routine's local scope
\ still open, so its call carried the enclosing routine's local values. The body
\ is now built with no local scope at all, which is what a quotation has (dot
\ habu-let-a-calling-7578eaaa). What the compiled shape ANSWERS is measured in
\ test/compiler/native-quot-scope.f, against the engine, on both paths; this case
\ keeps the acceptance beside the refusal it replaced.
: BODY-CALL-LOCALS-CASE ( -- )
   s" the engine runs a caught calling body under a definition with locals" T-LABEL
   7 NCA-FIXTURE:NCA-BL {: bu:n br:n :}
   br 0 T=  bu 8 T=

   s" and the chain compiles it now, with the group and without it" T-LABEL
   [: CALLEE-OK1
      s" : NCA-BL1 ( n -- n n ) [: NCA-FIXTURE:NCA-OK1 ;] catch {: rc:n :} rc 0 <> if 77 else 0 then ;"
      1 2 DEFINE-AT ;]
   0 TTHROWSQ
   [: CALLEE-OK1
      s" : NCA-BL2 ( n -- n n ) [: NCA-FIXTURE:NCA-OK1 ;] catch ;"
      1 2 DEFINE-AT ;]
   0 TTHROWSQ ;

: RUN ( -- )
   CONTENTS-CASE
   NORMAL-CASE
   LOCAL-CASE
   LOOP-CASE
   DEEP-CASE
   WIDEN-CASE
   TWO-WINDOW-CASE
   STRING-SITE-CASE
   PARKED-CASE
   NORET-BODY-CASE
   BODY-CONTROL-CASE
   BODY-CALL-LOCALS-CASE ;

;package

T-RESET
NCA-TEST:RUN
NCA-DIFF:RUN
T-REPORT
