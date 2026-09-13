\ prefix-mark-test.f - the recorded core-prefix boundary is where it claims.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f test/prefix-mark-test.f
\
\ The end of src/core/lower-cert-seal.f identifies its final dictionary record,
\ captures the include registry's row count and asks the checker to record every
\ mark a scope of its own would carry, keeping that record's width. src/habu/
\ prefix-rewind.f returns every generated engine source to them. A mark that is
\ merely PLAUSIBLE is the dangerous failure: the build truncates to whatever it
\ says and then compiles a payload against whatever survived.
\
\ The subject is the running engine's own boot, not a replay: these are the
\ marks this process's runtime carries, read through the package's public readers
\ exactly as the rewind reads them. Exact record identity matters after AOT
\ restore: the captured host's numeric dictionary index changes when the target
\ registers the window after its own engine records.
\
\ WHAT THE FIRST VERSION OF THAT CAPTURE DID. Its readers were spelled `NDICT@`
\ and `UEND@`. Habu folds case, so inside `package PREFIX-MARK` the tail
\ `NDICT@` IS the engine's `ndict@`, and the capture line `ndict@ ND !` read its
\ own empty accessor: the mark recorded 0, which a build would have taken as
\ "truncate everything". Nothing complained. LOWER-BOUND below is the case that
\ catches it, which is why it is not a decorative sanity check.

require lib/errors.f
require lib/string.f
require lib/test.f

package PREFIX-MARK-TEST

private

\ The last definition src/core/lower-cert-seal.f makes before it takes the mark,
\ so the boundary is not a free number: it is the count of records that existed
\ when that file finished.
: LAST-BEFORE-MARK$ ( -- ptr u8 n )
   s" PREFIX-MARK:CURSORS" ;

: LOWER-BOUND ( -- )
   PREFIX-MARK:DICT 0 > TTRUE
   PREFIX-MARK:CURSORS 0 > TTRUE ;

\ The core prefix ends BEFORE the boot finishes: the stdlib, script-argv.f,
\ internal-mark.f and top-row.f all load after it, so a mark equal to the live
\ count would mean the capture ran at the wrong moment - or never, leaving a
\ later writer's value behind.
\
\ Only the dictionary gets a live upper bound here. The boundary width has no
\ reachable one: the record it is the width OF belongs to CHECKER-BOUND, which
\ sits below the lower-cert seal and answers CHECKER-RESOLVES? with 0 from out
\ here - the same wall that makes src/habu/prefix-rewind.f reach the seam through
\ a trusted row. Nonzero, above, is what this side can state, and it is exactly
\ the clause tools/build-fixpoint.f requires of a host.
: UPPER-BOUND ( -- )
   PREFIX-MARK:DICT ndict@ < TTRUE
   s" CHECKER-BOUND:CURSORS" CHECKER-RESOLVES? TFALSE ;

\ The include registry's cursor. The boot records the engine's own provided
\ files at the END of the cold prefix - after this file - so a mark equal to the
\ live count would mean the capture ran after them, and the rewind that reads it
\ would leave a build claiming to provide the very files it had just discarded.
\ The ENGINE-PROVIDES? case beside it is what stops the comparison being
\ vacuous: the rows past the mark exist, and they are those files.
: REGISTRY-CURSOR ( -- )
   PREFIX-MARK:REQ 0 >= TTRUE
   PREFIX-MARK:REQ REQUIRE-REG:COUNT < TTRUE
   s" src/core/checker.f" ENGINE-PROVIDES? TTRUE ;

\ Three quantities. A dictionary count, an include
\ registry row count and the checker boundary's width are different quantities,
\ and a reader copy-pasted onto another's variable would make two of them answer
\ the same number. The registry cursor is zero at the mark - the engine records
\ its own provided files after this file - so it is compared against the two that
\ are not, which is the pairing that can actually catch the copy.
: THREE-CELLS ( -- )
   PREFIX-MARK:DICT PREFIX-MARK:CURSORS = TFALSE
   PREFIX-MARK:REQ PREFIX-MARK:CURSORS = TFALSE
   PREFIX-MARK:REQ PREFIX-MARK:DICT = TFALSE ;

\ The mark names the exact boundary after lower-cert-seal.f's final word and
\ before the cold stdlib. `CHECKER-RESOLVES?` is
\ the same query tools/build-fixpoint.f BF-WATERMARK? uses to refuse a host that
\ has no mark at all, so this also pins that the probe's subject exists - and
\ the bogus tails beside it prove the probe discriminates rather than answering
\ true for anything that merely looks qualified.
: BOUNDARY-WORD ( -- )
   LAST-BEFORE-MARK$ CHECKER-RESOLVES? TTRUE
   LAST-BEFORE-MARK$ XREF-FIND-INDEX 1+ PREFIX-MARK:DICT T=
   s" true" 0 XREF-FIND-WL-INDEX PREFIX-MARK:DICT >= TTRUE
   s" PREFIX-MARK:NO-SUCH-TAIL" CHECKER-RESOLVES? TFALSE
   s" NOT-A-PACKAGE:NO-SUCH-TAIL" CHECKER-RESOLVES? TFALSE ;

public

: TEST ( -- )
   s" prefix mark lower bound" T-LABEL LOWER-BOUND
   s" prefix mark upper bound" T-LABEL UPPER-BOUND
   s" prefix mark three cells" T-LABEL THREE-CELLS
   s" prefix mark registry cursor" T-LABEL REGISTRY-CURSOR
   s" prefix mark boundary word" T-LABEL BOUNDARY-WORD
   T-REPORT ;

;package

T-RESET
PREFIX-MARK-TEST:TEST
