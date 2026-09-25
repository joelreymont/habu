\ native-catch.f - production `catch` compilation.
\ Tier 1 first: the `catch` lowering asserted below is the optimizing
\ compiler's (48 rows fail at the default tier).
1 set-tier

require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require lib/adt/option.f
require test/checker-assert.f
require src/compiler/native/compiler.f

package NCA-TEST

public

TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

private

\ ---- reading the windows a recorded definition's catch sites took -------------
\ The compiler's reader, asked about the tape the compilation above just recorded.
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

\ The body touches its window cell and leaves it where it was, because a body
\ that overwrites the cell it was handed hands back a `stale<n>` the definition's
\ declared output refuses (test/catch-stale-suite.f). What this fixture is for is
\ the recorded window of a body that never returns, and that is unchanged.
\ 0 NCA-W1  1 [:  2 dup  3 drop  4 5  5 throw  6 ;]  7 catch
: SRC-DEAD ( -- )
   s" : NCA-W1 ( n -- n n ) [: dup drop 5 throw ;] catch ;" NCA-TEST:EV ;

\ 0 NCA-W2  1 [:  2 1+  3 ;]  4 catch
: SRC-LIVE ( -- )
   s" : NCA-W2 ( n -- n n ) [: 1+ ;] catch ;" NCA-TEST:EV ;

\ 0 NCA-W0  1 [:  2 1  3 2  4 3  5 throw  6 ;]  7 catch
: SRC-EMPTY ( -- )
   s" : NCA-W0 ( n -- n n ) [: 1 2 3 throw ;] catch ;" NCA-TEST:EV ;

\ 0 NCA-WS  1 "hi"  2 2drop  3 [:  4 1+  5 ;]  6 catch
: SRC-STRING ( -- )
   S\" : NCA-WS ( n -- n n ) s\q hi\q 2drop [: 1+ ;] catch ;" NCA-TEST:EV ;

\ 0 NCA-WW  1 [:  2 1+  3 swap  4 1+  5 swap  6 ;]  7 catch
\ 8 drop  9 [:  10 1+  11 ;]  12 catch
: SRC-TWO ( -- )
   s" : NCA-WW ( n n -- n n n ) [: 1+ swap 1+ swap ;] catch drop [: 1+ ;] catch ;"
   NCA-TEST:EV ;

\ A definition crossing the initial metadata allocation must record every site.
17 constant CAP-SITES
4 constant SITE-TOKENS               \ [: ;] catch drop
512 constant CAP-BUF-CAP

here CELL 1- and CELL swap - CELL 1- and allot
create CAP-BUF CAP-BUF-CAP allot
variable CAP-U

: CAP+ ( ptr u8 n -- ) {: a:ptr u:n :}
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
   CAP-BUILD NCA-TEST:EV ;

\ Running one fixture and swallowing whatever compilation made of it. The recorded
\ windows are the subject here, and they are recorded by the SCAN - which has
\ happened either way, because lowering only runs after it.
: SCANNED ( -- )
   [: SRC-DEAD ;] 0 TTHROWSQ ;

public

\ ---- the export --------------------------------------------------------------
: DEAD-CASE ( -- )
   s" a caught body that never returns publishes its window and no output" T-LABEL
   SCANNED
   7 WIN-IN 1 T=
   7 WIN-OUT NDICT:CATCH-NONE T=
   9 SITES 1 T= ;

: LIVE-CASE ( -- )
   s" a caught body that returns publishes both, and they are the same width" T-LABEL
   [: SRC-LIVE ;] 0 TTHROWSQ
   4 WIN-IN 1 T=
   4 WIN-OUT 1 T=
   8 SITES 1 T= ;

: EMPTY-CASE ( -- )
   s" a body that takes nothing has a window of zero, not of the stack under it" T-LABEL
   [: SRC-EMPTY ;] 0 TTHROWSQ
   7 WIN-IN 0 T=
   7 WIN-OUT NDICT:CATCH-NONE T=
   9 SITES 1 T= ;

: STRING-CASE ( -- )
   s" a string literal before the catch is one row, so the site keeps its ordinal" T-LABEL
   [: SRC-STRING ;] 0 TTHROWSQ
   6 WIN-IN 1 T=
   6 WIN-OUT 1 T=
   \ and nothing was recorded on the rows a payload-counting producer would have
   \ pushed the site onto
   5 WIN-IN NDICT:CATCH-NONE T=
   7 WIN-IN NDICT:CATCH-NONE T=
   9 SITES 1 T= ;

: TWO-CASE ( -- )
   s" two catches in one definition keep their own windows" T-LABEL
   [: SRC-TWO ;] 0 TTHROWSQ
   7 WIN-IN 2 T=
   12 WIN-IN 1 T=
   16 SITES 2 T= ;

: CAP-CASE ( -- )
   s" quotation metadata grows with the definition" T-LABEL
   [: SRC-CAP ;] 0 TTHROWSQ
   CAP-SITES 0 ?do
      i CAP-SITE-TOK WIN-IN 0 T=
      i CAP-SITE-TOK WIN-OUT 0 T=
   loop
   CAP-SITES SITE-TOKENS * 4 + SITES CAP-SITES T= ;

: NO-UNIT-CASE ( -- )
   s" a token of no recorded definition answers absent, in both halves" T-LABEL
   [: s" : NCA-NONE ( n -- n ) 1+ ;" NCA-TEST:EV ;] 0 TTHROWSQ
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

\ ---- the production programs under test --------------------------------------
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
\ whole point of the production test: the caller gets the SECOND value back under
\ the throw code, because a caught throw restores the stack's depth and leaves
\ its contents alone. A compiler that kept the window in a register answers the
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

\ The contents measurement is the one thing a quotation LITERAL can no longer
\ express: a checked program may not READ the window cell a throwing body may
\ have written - it comes back `stale<n>` (test/catch-stale-suite.f). An
\ exceptional edge is not part of a quotation's TYPE, so `['] W catch` keeps the
\ window typed and the measurement below stays exact. The callee-evidence lane
\ (dot c2923193) is what would give the literal the same proof.
: NCA-D1 ( n -- n n )
   ['] NCA-CLOB catch ;

: NCA-D2 ( n -- n n )
   [: NCA-OK1 ;] catch ;

\ The production shape: the code into a local, then a decision on it. Twenty of
\ the forty catch sites in src and lib are written this way. Its body calls
\ nothing, which used to be forced - a body that calls under a definition with a
\ locals group was a quotation-path ceiling - and now is only what this case
\ happens to measure: the calling twin of the same shape is a production test of its
\ own in test/compiler/native-quot-scope.f.
: NCA-D4 ( n -- n n )
   [: 1+ ;] catch {: rc:n :}
   rc 0 <> if 77 else 0 then ;

: NCA-D5 ( n -- n )
   3 0 ?do [: NCA-OK1 ;] catch drop loop ;

\ A callee that throws counts as overwriting every input it declared, whether or
\ not it writes one: it may rewrite them all before it throws. NCA-DEEP1 leaves
\ the cell alone and NCA-WIDE clobbers it, and under a quotation literal both
\ windows come back stale - so these two assert the code the throw carried out of
\ two and three frames down, and drop the cell.
: NCA-D6 ( n -- n )
   [: NCA-DEEP1 ;] catch nip ;

: NCA-D7 ( n -- n )
   [: NCA-WIDE ;] catch nip ;

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
\ and not a special case: the parked 42 comes back whether the body threw or not.
\ The parked cell is BELOW the caught body's return window, which is empty, so
\ the depth restore never reaches it and it stays typed; the DATA window cell of
\ the throwing twin comes back stale and is dropped, while the non-throwing twin
\ keeps it and still answers 8.
: NCA-PT ( n -- n n )
   42 >r [: NCA-CLOB ;] catch nip r> ;

: NCA-PN ( n -- n n n )
   42 >r [: NCA-OK1 ;] catch r> ;

\ ---- quotation-body cases -----------------------------------------------------
: NCA-BC ( n -- n n )
   [: dup 3 > if 1+ then ;] catch ;

: NCA-BL ( n -- n n )
   [: NCA-OK1 ;] catch {: rc:n :}
   rc 0 <> if 77 else 0 then ;

\ The caught window contains a family with a multicell payload. A different
\ multicell value stays below it, outside the window.
PRODUCT point 0
   FIELD x n
   FIELD y n
;PRODUCT

: SOME-POINT ( -- option<point> )
   17 25 NCA--FIXTURE-POINT:MAKE OPTION:SOME ;

: BUNDLE-BODY ( option<point> -- option<point> )
   drop SOME-POINT ;

: BUNDLE-THROW ( option<point> -- option<point> )
   BUNDLE-BODY -79 throw ;

: BUNDLE-ID ( option<point> -- option<point> ) ;

: NO-POINT ( -- option<point> ) OPTION:NONE ;

: READ-BUNDLES ( point option<point> -- n n )
   MATCH option
      none OF 0 ENDOF
      some OF NCA--FIXTURE-POINT:UNMAKE + ENDOF
   ;MATCH
   >r NCA--FIXTURE-POINT:UNMAKE + r> ;

: CATCH-BUNDLE
   ( point option<point> [ option<point> -- option<point> ] -- n n n )
   catch {: rc:n :}
   MATCH option
      none OF 0 ENDOF
      some OF NCA--FIXTURE-POINT:UNMAKE + ENDOF
   ;MATCH
   >r NCA--FIXTURE-POINT:UNMAKE + r> rc ;

: CATCH-EMPTY ( point option<point> -- n n n )
   [: ;] catch {: rc:n :}
   READ-BUNDLES rc ;

\ The contents measurement of a MULTICELL window is what a quotation LITERAL
\ cannot express: the literal route brings the window back as one
\ `stale<option<point<>>>` (test/catch-stale-suite.f). `['] W catch` takes W's
\ OWN throw-edge evidence (the tick route), so the window is typed exactly when
\ every throw path of W left it where it was: BUNDLE-KEPT builds a bundle and
\ drops it before throwing, leaving the caller's option in place, and the
\ measurement below reads it back through the throw path. BUNDLE-THROW, which
\ replaces the option, stales it on this route too (CATCH-STALE-DROP).
: BUNDLE-KEPT ( option<point> -- option<point> )
   SOME-POINT drop -79 throw ;

: CATCH-DEAD ( point option<point> -- n n n )
   ['] BUNDLE-KEPT catch {: rc:n :}
   READ-BUNDLES rc ;

: CATCH-EMPTY-THROW ( point option<point> -- n n n )
   [: -80 throw ;] catch {: rc:n :}
   READ-BUNDLES rc ;

\ What the literal route DOES express: a stale bundle is one value of the width
\ the elaborator gives it. One `drop` removes the whole group, and `nip` takes
\ the scalar out from under it - the checker's side of both rows is
\ test/catch-stale-suite.f (B1, B7).
: CATCH-STALE-DROP ( point option<point> -- n )
   [: BUNDLE-THROW ;] catch {: rc:n :}
   drop drop rc ;

: CATCH-STALE-NIP ( n option<point> -- n )
   [: BUNDLE-THROW ;] catch {: rc:n :}
   nip drop rc ;

\ OPEN keeps an archive beside option<document>, catches a reader that can
\ genuinely throw after consuming the option, cleans up, then matches it.
NEWTYPE document 0
CAST: >DOCUMENT ( n -- document )
CAST: DOCUMENT>N ( document -- n )

variable CLOSE-COUNT
variable CLOSED-ARCHIVE
81 constant E-DOC-READER

: RESET-CLOSE ( -- ) 0 CLOSE-COUNT ! -1 CLOSED-ARCHIVE ! ;
: CLOSE-COUNT@ ( -- n ) CLOSE-COUNT @ ;
: CLOSED-ARCHIVE@ ( -- n ) CLOSED-ARCHIVE @ ;
: CLOSE-ARCHIVE ( n -- ) CLOSED-ARCHIVE ! 1 CLOSE-COUNT +! ;

: BUILD-DOCUMENT ( n -- n )
   dup 2 = if drop E-DOC-READER throw then
   drop 42 ;

: NEW-DOCUMENT ( n -- document )
   [: BUILD-DOCUMENT ;] catch {: code:n :}
   code 0<> if drop code throw then
   >DOCUMENT ;

: READ-DOCUMENT ( n option<document> -- n option<document> )
   drop {: archive:n :}
   archive 0= if OPTION:NONE else archive NEW-DOCUMENT OPTION:SOME then
   archive swap ;

: NO-DOCUMENT ( -- option<document> ) OPTION:NONE ;

: OPEN-DOCUMENT ( n -- document )
   {: archive:n :}
   archive NO-DOCUMENT [: READ-DOCUMENT ;] catch {: code:n :}
   nip archive CLOSE-ARCHIVE
   code 0<> if drop code throw then
   MATCH option
      none OF -99 throw ENDOF
      some OF ENDOF
   ;MATCH ;

82 constant E-SCALAR-READER
variable SCALAR-FAIL

: SCALAR-NEW ( -- n )
   SCALAR-FAIL @ 0<> if E-SCALAR-READER throw then
   64 ;

: SCALAR-READER ( n -- n )
   drop SCALAR-NEW ;

: SCALAR-LITERAL ( n -- n )
   dup 0< if 1 else 0 then SCALAR-FAIL !
   [: SCALAR-READER ;] catch {: code:n :}
   code 0<> if drop code throw then ;

: SCALAR-TICK ( n -- n )
   dup 0< if 1 else 0 then SCALAR-FAIL !
   ['] SCALAR-READER catch {: code:n :}
   code 0<> if drop code throw then ;

\ The result is bound before proof, while the code goes through a duplicate,
\ a swap and the return stack. Only the zero arm gives the result a typed use.
: SCALAR-TRANSPORT ( n -- n n )
   dup 0< if 1 else 0 then SCALAR-FAIL !
   [: SCALAR-READER ;] catch dup >r swap {: code:n result :}
   code 0= {: success:bool :}
   success if result {: value:n :} value r> else -1 r> then ;

: SCALAR-ELSE ( n -- n )
   dup 0< if 1 else 0 then SCALAR-FAIL !
   [: SCALAR-READER ;] catch {: code:n :}
   code 0= if
      1 1 = if 1+ else 2 + then
   else drop -5 then ;

\ An input below the caught temporary stays intact when a guarded result is
\ dropped and the enclosing word later throws. Both inner outcomes preserve it.
variable PRESERVE-FAIL
create PRESERVE-BUF 1 allot

: PRESERVE-INNER ( n -- n )
   drop PRESERVE-FAIL @ 0<> if -71 throw then 5 ;

: PRESERVE-OUTER ( ptr u8 -- ptr u8 )
   0 [: PRESERVE-INNER ;] catch {: code:n :}
   code 0<> if drop code throw then
   drop -72 throw ;

: PRESERVE-CATCH ( ptr u8 -- ptr u8 n )
   [: PRESERVE-OUTER ;] catch ;

: PRESERVED ( n -- bool n )
   PRESERVE-FAIL !
   PRESERVE-BUF PRESERVE-CATCH {: p:ptr code:n :}
   p PRESERVE-BUF = code ;

;package

package NCA-RUN

private

public

\ THE ONE ANSWER THAT DECIDES THIS WHOLE LANE. `7 NCA-D1` is 9 over 5, not 9 over
\ 7: the caught body dropped the caller's cell, wrote another in its slot and
\ threw, and what catch restores is the DEPTH. The production test holds the
\ compiler to that, and the number cached window contents would answer - 7 -
\ is the value the site started with, which no shape assertion would ever notice.
: CONTENTS-CASE ( -- )
   s" a caught throw restores the depth and not the contents" T-LABEL
   7 NCA-FIXTURE:NCA-D1 {: eu:n er:n :}
   er 9 T=  eu 5 T= ;

: NORMAL-CASE ( -- )
   s" the path where nothing throws returns the body result" T-LABEL
   7 NCA-FIXTURE:NCA-D2 {: eu:n er:n :}
   er 0 T=  eu 8 T= ;

: LOCAL-CASE ( -- )
   s" a catch result bound to a local drives the following branch" T-LABEL
   7 NCA-FIXTURE:NCA-D4 {: v:n rc:n :}
   v 8 T=  rc 0 T=
   -3 NCA-FIXTURE:NCA-D4 {: v:n rc:n :}
   v -2 T=  rc 0 T= ;

: LOOP-CASE ( -- )
   s" a catch inside a counted loop, once per turn" T-LABEL
   7 NCA-FIXTURE:NCA-D5 10 T= ;

: DEEP-CASE ( -- )
   s" a throw from two frames below the catch" T-LABEL
   7 NCA-FIXTURE:NCA-D6 11 T= ;

: WIDEN-CASE ( -- )
   s" a body that leaves more cells than it took before it throws" T-LABEL
   7 NCA-FIXTURE:NCA-D7 7 T= ;

: TWO-WINDOW-CASE ( -- )
   s" two catches with different windows keep separate stack shapes" T-LABEL
   5 6 NCA-FIXTURE:NCA-D8 {: a:n b:n rc:n :}
   a 6 T=  b 8 T=  rc 0 T=
   -2 9 NCA-FIXTURE:NCA-D8 {: a:n b:n rc:n :}
   a -1 T=  b 11 T=  rc 0 T= ;

: STRING-SITE-CASE ( -- )
   s" a catch after a string literal keeps its window and result" T-LABEL
   7 NCA-FIXTURE:NCA-D9 {: v:n rc:n :}
   v 8 T=  rc 0 T=
   0 NCA-FIXTURE:NCA-D9 {: v:n rc:n :}
   v 1 T=  rc 0 T= ;

: PARKED-CASE ( -- )
   s" a value parked across the catch comes back, on both paths" T-LABEL
   7 NCA-FIXTURE:NCA-PT {: tr:n tp:n :}
   tp 42 T=  tr 9 T=
   7 NCA-FIXTURE:NCA-PN {: nu:n nr:n np:n :}
   np 42 T=  nr 0 T=  nu 8 T= ;

: BUNDLE-CASE ( -- )
   s" catch preserves family and multicell boundaries on both paths" T-LABEL
   3 5 NCA--FIXTURE-POINT:MAKE NCA-FIXTURE:NO-POINT
   [: NCA-FIXTURE:BUNDLE-BODY ;] NCA-FIXTURE:CATCH-BUNDLE
   0 T= 42 T= 8 T=
   3 5 NCA--FIXTURE-POINT:MAKE NCA-FIXTURE:NO-POINT
   NCA-FIXTURE:CATCH-DEAD
   -79 T= 0 T= 8 T=
   3 5 NCA--FIXTURE-POINT:MAKE NCA-FIXTURE:NO-POINT
   [: NCA-FIXTURE:BUNDLE-ID ;] NCA-FIXTURE:CATCH-BUNDLE
   0 T= 0 T= 8 T=
   s" a stale multicell window is ONE value: one drop moves it, nip moves it" T-LABEL
   3 5 NCA--FIXTURE-POINT:MAKE NCA-FIXTURE:SOME-POINT
   NCA-FIXTURE:CATCH-STALE-DROP -79 T=
   8 NCA-FIXTURE:SOME-POINT NCA-FIXTURE:CATCH-STALE-NIP -79 T=
   s" a zero-window catch leaves both bundles intact" T-LABEL
   3 5 NCA--FIXTURE-POINT:MAKE NCA-FIXTURE:SOME-POINT
   NCA-FIXTURE:CATCH-EMPTY 0 T= 42 T= 8 T=
   3 5 NCA--FIXTURE-POINT:MAKE NCA-FIXTURE:SOME-POINT
   NCA-FIXTURE:CATCH-EMPTY-THROW -80 T= 42 T= 8 T=
   s" an OPEN-shaped caught reader retains its option through cleanup" T-LABEL
   NCA-FIXTURE:RESET-CLOSE
   1 NCA-FIXTURE:OPEN-DOCUMENT NCA-FIXTURE:DOCUMENT>N 42 T=
   NCA-FIXTURE:CLOSE-COUNT@ 1 T=
   NCA-FIXTURE:CLOSED-ARCHIVE@ 1 T=
   NCA-FIXTURE:RESET-CLOSE
   [: 2 NCA-FIXTURE:OPEN-DOCUMENT drop ;] NCA-FIXTURE:E-DOC-READER TTHROWSQ
   NCA-FIXTURE:CLOSE-COUNT@ 1 T=
   NCA-FIXTURE:CLOSED-ARCHIVE@ 2 T=
   NCA-FIXTURE:RESET-CLOSE
   [: 0 NCA-FIXTURE:OPEN-DOCUMENT drop ;] -99 TTHROWSQ
   NCA-FIXTURE:CLOSE-COUNT@ 1 T=
   NCA-FIXTURE:CLOSED-ARCHIVE@ 0 T=
   s" catch requires the same returned types, even at equal cell widths" T-LABEL
   s" SAME-ROW ( NCA-FIXTURE:document -- NCA-FIXTURE:document n ) [: ;] catch"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CHANGED-ROW ( NCA-FIXTURE:document -- NCA-FIXTURE:document n ) [: NCA-FIXTURE:DOCUMENT>N ;] catch"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: SUCCESS-CASE ( -- )
   s" literal and named scalar readers expose only the guarded normal result" T-LABEL
   1 NCA-FIXTURE:SCALAR-LITERAL 64 T=
   [: -1 NCA-FIXTURE:SCALAR-LITERAL drop ;]
      NCA-FIXTURE:E-SCALAR-READER TTHROWSQ
   1 NCA-FIXTURE:SCALAR-TICK 64 T=
   [: -1 NCA-FIXTURE:SCALAR-TICK drop ;]
      NCA-FIXTURE:E-SCALAR-READER TTHROWSQ
   s" status transport, zero arm and live ELSE keep their own proof" T-LABEL
   1 NCA-FIXTURE:SCALAR-TRANSPORT 0 T= 64 T=
   -1 NCA-FIXTURE:SCALAR-TRANSPORT NCA-FIXTURE:E-SCALAR-READER T= -1 T=
   1 NCA-FIXTURE:SCALAR-ELSE 65 T=
   -1 NCA-FIXTURE:SCALAR-ELSE -5 T=
   s" a separate input stays intact across guard, drop and later throw" T-LABEL
   0 NCA-FIXTURE:PRESERVED -72 T= TTRUE
   1 NCA-FIXTURE:PRESERVED -71 T= TTRUE ;

\ ---- what production compilation still refuses -------------------------------
: NORET-BODY-CASE ( -- )
   s" catch compiles both throwing and returning bodies" T-LABEL
   [: s" : NCA-NR1 ( n -- n n ) [: dup drop 5 throw ;] catch ;" NCA-TEST:EV ;]
   0 TTHROWSQ
   [: s" : NCA-NR2 ( n -- n n ) [: 1+ ;] catch ;" NCA-TEST:EV ;]
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
   s" a caught body holding a control structure" T-LABEL
   7 NCA-FIXTURE:NCA-BC {: cu:n cr:n :}
   cr 0 T=  cu 8 T=

   s" and both it and its straight-line twin compile" T-LABEL
   [: s" : NCA-BC1 ( n -- n n ) [: dup 3 > if 1+ then ;] catch ;" NCA-TEST:EV ;]
   0 TTHROWSQ
   [: s" : NCA-BC2 ( n -- n n ) [: 1+ ;] catch ;" NCA-TEST:EV ;]
   0 TTHROWSQ ;

\ THE SECOND CEILING IS GONE, and this case is what it left behind: the same two
\ texts, now accepted. A quotation body that CALLS, with a locals group in
\ the definition around it, used to be refused as an operand naming a value of
\ another function - the body was built with the enclosing routine's local scope
\ still open, so its call carried the enclosing routine's local values. The body
\ is now built with no local scope at all, which is what a quotation has (dot
\ habu-let-a-calling-7578eaaa). What the compiled shape ANSWERS is measured in
\ test/compiler/native-quot-scope.f on both paths; this case
\ keeps the acceptance beside the refusal it replaced.
: BODY-CALL-LOCALS-CASE ( -- )
   s" a caught calling body runs under a definition with locals" T-LABEL
   7 NCA-FIXTURE:NCA-BL {: bu:n br:n :}
   br 0 T=  bu 8 T=

   s" and production compilation accepts it with the group and without it" T-LABEL
   [: s" : NCA-BL1 ( n -- n n ) [: NCA-FIXTURE:NCA-OK1 ;] catch {: rc:n :} rc 0 <> if 77 else 0 then ;"
      NCA-TEST:EV ;]
   0 TTHROWSQ
   [: s" : NCA-BL2 ( n -- n n ) [: NCA-FIXTURE:NCA-OK1 ;] catch ;"
      NCA-TEST:EV ;]
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
   BUNDLE-CASE
   SUCCESS-CASE
   NORET-BODY-CASE
   BODY-CONTROL-CASE
   BODY-CALL-LOCALS-CASE ;

;package

T-RESET
NCA-TEST:RUN
NCA-RUN:RUN
T-REPORT
