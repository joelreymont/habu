\ outer-interpret.f - the interpret loop written in Habu, src/habu/outer.f
\ OUTER:INTERPRET, against the engine's own, through the real load path.
\
\ Each case is a file a child engine loads twice:
\   --load <prelude> test/outer-loop-on.f <case>   the Habu loop reads the case
\   --load <prelude> <case>                        the engine's evaluate reads it
\ test/outer-loop-on.f binds the loaded-bytes seam SOURCE-ROOT:INCLUDE-INTERPRET
\ (src/core/include.f) to OUTER:INTERPRET. The two runs must end with the same
\ rc, stdout and stderr, and each case states the rc and output that show what
\ it exercised, so two runs failing alike do not pass.
\
\ The prelude defines with keywords (`:`, `TRUSTED:`, `SUMTYPE`), which the Habu
\ loop does not read yet, so it loads before the switch and a case holds only
\ numbers, comments and words.
\
\ Two checks run in a forked copy of this process instead. Ambiguity needs two
\ usings open around the token, and `using` is a keyword: the fork opens both
\ and calls OUTER:INTERPRET, against a fork whose `evaluate` reads the same
\ text. And the seam: a fork binds it to a counting spy, and a loaded file must
\ arrive there.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require test/gate-common.f
require src/habu/outer.f

\ ---- one tail public in two packages: ambiguous under both usings ---------------
package OUTER-INTERPRET-FXA
public
: OI-TWIN ( -- n ) 1 ;
;package

package OUTER-INTERPRET-FXB
public
: OI-TWIN ( -- n ) 2 ;
;package

package OUTER-INTERPRET-TEST

$0A constant NEWLINE
$5C constant BACKSLASH

variable CASES

\ ---- the files --------------------------------------------------------------------
create PRELUDE-BUF FS-PATH-CAP allot
variable PRELUDE-U
create CASE-BUF FS-PATH-CAP allot
variable CASE-U
create NESTED-BUF FS-PATH-CAP allot
variable NESTED-U

: PRELUDE$ ( -- ptr u8 n )
   PRELUDE-BUF PRELUDE-U @ ;

: CASE$ ( -- ptr u8 n )
   CASE-BUF CASE-U @ ;

: NESTED$ ( -- ptr u8 n )
   NESTED-BUF NESTED-U @ ;

\ The source buffer's text, written to the file at path.
: SRC>FILE ( ptr u8 n -- )
   GE-SRC-BUF GE-SRC-U @ WRITE-ALL ;

\ Words the cases call: a certified word of two inputs, a trusted one that
\ drops a cell it does not declare, an immediate word, a wide one, a word
\ spelled as an out-of-range number, a top-row hook that logs each event and
\ one that drops two cells more than its event carries.
: PRELUDE ( -- )
   GE-SRC-RESET
   s" : OI-TWO ( n n -- ) 2drop ;" GE-SRC-LINE
   s" TRUSTED: OI-UF ( -- ) drop ;" GE-SRC-LINE
   s" : OI-IMM ( -- ) ; immediate" GE-SRC-LINE
   s" SUMTYPE oiwide 2" GE-SRC-LINE
   s"   VARIANT ok a ;VARIANT" GE-SRC-LINE
   s"   VARIANT err b ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" TRUSTED: OI-WIDE ( -- oiwide<n,n> ) 7 9 ;" GE-SRC-LINE
   s" : 18446744073709551616 ( -- n ) 1 ;" GE-SRC-LINE
   s" : OI-LOG ( ptr u8 n n n -- ) {: a:ptr u:n c:n f:n :} c . f . a u type cr ;" GE-SRC-LINE
   s" TRUSTED: OI-HOOK-ON ( -- ) ['] OI-LOG set-top-check ;" GE-SRC-LINE
   s" TRUSTED: OI-SINK ( ptr u8 n n n -- ) 2drop 2drop 2drop ;" GE-SRC-LINE
   s" TRUSTED: OI-SINK-ON ( -- ) ['] OI-SINK set-top-check ;" GE-SRC-LINE
   s" oi-prelude.f" PRELUDE-BUF GT-PATH PRELUDE-U !
   PRELUDE$ SRC>FILE ;

\ ---- the two runs of a case --------------------------------------------------------
create WANT-OUT GT-OUT-CAP allot
variable WANT-OUT-U
create WANT-ERR GT-ERR-CAP allot
variable WANT-ERR-U
variable WANT-RC

: WANT-OUT$ ( -- ptr u8 n )
   WANT-OUT WANT-OUT-U @ ;

: WANT-ERR$ ( -- ptr u8 n )
   WANT-ERR WANT-ERR-U @ ;

\ The engine's run, kept while the Habu loop's runs.
: KEEP ( -- )
   GT-RC@ WANT-RC !
   GT-OUT$ {: oa:ptr ou:n :}  oa WANT-OUT ou BYTE-COPY  ou WANT-OUT-U !
   GT-ERR$ {: ea:ptr eu:n :}  ea WANT-ERR eu BYTE-COPY  eu WANT-ERR-U ! ;

: MISMATCH ( ptr u8 n ptr u8 n -- ) {: why:ptr whyu:n label:ptr labelu:n :}
   why whyu type cr
   s" engine rc: " type WANT-RC @ FMT:.INT cr
   s" engine stdout:" type cr WANT-OUT$ type
   s" engine stderr:" type cr WANT-ERR$ type
   label labelu GE-FAIL ;

\ The Habu loop's run against the kept engine run.
: SAME ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GT-RC@ WANT-RC @ <> if s" rc differs from the engine's" label labelu MISMATCH then
   GT-OUT$ WANT-OUT$ STR= 0= if s" stdout differs from the engine's" label labelu MISMATCH then
   GT-ERR$ WANT-ERR$ STR= 0= if s" stderr differs from the engine's" label labelu MISMATCH then ;

: RUN ( bool -- ) {: habu:bool :}
   GE-HB-RESET
   s" --load" GE-ARG+
   PRELUDE$ GE-ARG+
   habu if s" test/outer-loop-on.f" GE-ARG+ then
   CASE$ GE-ARG+
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV ;

\ The source buffer as the case file named, run by the engine, then by the
\ Habu loop, which must agree.
: BOTH ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu CASE-BUF GT-PATH CASE-U !
   CASE$ SRC>FILE
   false RUN KEEP
   true RUN
   name nameu SAME
   1 CASES +! ;

\ ---- the cases -----------------------------------------------------------------------
: NUMBERS ( -- )
   GE-SRC-RESET
   s" 1 2 + . $2A . -7 . -$10 . 1.5 . -2.25 . 0 . depth ." GE-SRC-LINE
   s" oi-numbers.f" BOTH
   s" numbers" GE-EXPECT-OK
   S\" 42\n-7\n-16\n" s" numbers" GE-EXPECT-OUT-HAS ;

\ A backslash comment, a `(` comment within a line and across one, and a `(`
\ that the input ends inside.
: COMMENTS ( -- )
   GE-SRC-RESET
   s" 1 " GE-SRC+ BACKSLASH GE-SRC-C s"  2 3" GE-SRC-LINE
   s" ( 4 ) 5 ( 6" GE-SRC-LINE
   s" 7 ) . . depth . (" GE-SRC+
   s" oi-comments.f" BOTH
   s" comments" GE-EXPECT-OK
   S\" 5\n1\n0\n" s" comments" GE-EXPECT-OUT ;

\ A backslash that is the input's last byte.
: LINE-COMMENT-AT-END ( -- )
   GE-SRC-RESET
   s" 1 . " GE-SRC+ BACKSLASH GE-SRC-C
   s" oi-line-comment-end.f" BOTH
   s" line comment at the end" GE-EXPECT-OK
   S\" 1\n" s" line comment at the end" GE-EXPECT-OUT ;

\ Only a whole one-byte token opens a comment.
: NOT-A-COMMENT ( -- )
   GE-SRC-RESET
   s" (x) 1 ." GE-SRC-LINE
   s" oi-not-a-comment.f" BOTH
   70 s" not a comment" GE-EXPECT-RC
   S\" E-UNDEFINED: (x)\n" s" not a comment" GE-EXPECT-ERR ;

: BLANK-INPUT ( -- )
   GE-SRC-RESET
   GE-SRC-LF s"  " GE-SRC+ 9 GE-SRC-C GE-SRC-LF
   s" oi-blank.f" BOTH
   s" blank input" GE-EXPECT-OK
   s" blank input" GE-EXPECT-SILENT ;

: UNDEFINED ( -- )
   GE-SRC-RESET
   s" 1 . OI-NOPE 2 ." GE-SRC-LINE
   s" oi-undefined.f" BOTH
   70 s" undefined" GE-EXPECT-RC
   S\" 1\n" s" undefined" GE-EXPECT-OUT
   S\" E-UNDEFINED: OI-NOPE\n" s" undefined" GE-EXPECT-ERR ;

\ Out of range, a number's spelling is undefined though a word has that name.
: OUT-OF-RANGE ( -- )
   GE-SRC-RESET
   s" 18446744073709551616" GE-SRC-LINE
   s" oi-range.f" BOTH
   70 s" out of range" GE-EXPECT-RC
   S\" E-UNDEFINED: 18446744073709551616\n" s" out of range" GE-EXPECT-ERR ;

: UNDERFLOW ( -- )
   GE-SRC-RESET
   s" OI-UF" GE-SRC-LINE
   s" oi-underflow.f" BOTH
   70 s" underflow" GE-EXPECT-RC
   S\" E-UNDERFLOW: OI-UF\n" s" underflow" GE-EXPECT-ERR ;

: UNDERDEPTH ( -- )
   GE-SRC-RESET
   s" 1 OI-TWO" GE-SRC-LINE
   s" oi-underdepth.f" BOTH
   70 s" underdepth" GE-EXPECT-RC
   S\" hb: interpret stack underdepth: OI-TWO\n" s" underdepth" GE-EXPECT-ERR ;

\ DEFER-UNSET is an engine-prefix word with no checker-known effect, so the
\ seal marks it DNAME-INT (src/core/internal-mark.f).
: INTERNAL ( -- )
   GE-SRC-RESET
   s" DEFER-UNSET" GE-SRC-LINE
   s" oi-internal.f" BOTH
   70 s" internal" GE-EXPECT-RC
   S\" hb: internal engine word: DEFER-UNSET\n" s" internal" GE-EXPECT-ERR ;

: WIDE ( -- )
   GE-SRC-RESET
   s" OI-WIDE" GE-SRC-LINE
   s" oi-wide.f" BOTH
   70 s" wide" GE-EXPECT-RC
   S\" hb: interpret-mode layout value: OI-WIDE\n" s" wide" GE-EXPECT-ERR ;

\ Each event logs its class, flags and token on three lines: numbers after the
\ push, words before they run, and set-top-check's own event is the last.
: HOOK-WINDOW ( -- )
   GE-SRC-RESET
   s" OI-HOOK-ON 42 1 OI-TWO $10 drop OI-IMM depth drop 0 set-top-check 5 ." GE-SRC-LINE
   s" oi-hook.f" BOTH
   s" hook window" GE-EXPECT-OK
   S\" 1\n0\n42\n" s" hook window" GE-EXPECT-OUT-HAS
   S\" 6\n513\nOI-TWO\n" s" hook window" GE-EXPECT-OUT-HAS
   S\" 6\n3\nOI-IMM\n" s" hook window" GE-EXPECT-OUT-HAS
   S\" 6\n257\nset-top-check\n5\n" s" hook window" GE-EXPECT-OUT-HAS ;

\ The floor holds after the hook too: the sink takes the pushed number and one
\ cell below it, so the number's token is refused.
: HOOK-UNDERFLOW ( -- )
   GE-SRC-RESET
   s" OI-SINK-ON 5" GE-SRC-LINE
   s" oi-hook-underflow.f" BOTH
   70 s" hook underflow" GE-EXPECT-RC
   S\" E-UNDERFLOW: 5\n" s" hook underflow" GE-EXPECT-ERR ;

\ A file the case includes is read by the same loop, and the case's own input
\ resumes after it.
: NESTED ( -- )
   GE-SRC-RESET
   s" 3" GE-SRC-LINE
   s" oi-nested.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   GE-SRC-RESET
   s" 1 include oi-nested.f 2 . . . depth ." GE-SRC-LINE
   s" oi-include.f" BOTH
   s" nested include" GE-EXPECT-OK
   S\" 2\n3\n1\n0\n" s" nested include" GE-EXPECT-OUT ;

\ ---- in a forked copy of this process -----------------------------------------------
create TWIN-BUF 16 allot

\ Two empty lines, then the ambiguous token: the refusal names line 3.
: TWIN$ ( -- ptr u8 n )
   s" OI-TWIN" {: a:ptr u:n :}
   NEWLINE TWIN-BUF c!
   NEWLINE TWIN-BUF 1 + c!
   a TWIN-BUF 2 + u BYTE-COPY
   TWIN-BUF u 2 + ;

variable SPY-N

: SPY ( ptr u8 n -- )
   1 SPY-N +!
   OUTER:INTERPRET ;

public

: OI-AMBIG ( -- )
   TWIN$ OUTER:INTERPRET ;

: SPY-ON ( -- )
   [: SPY ;] is SOURCE-ROOT:INCLUDE-INTERPRET ;

: SPY-N. ( -- )
   SPY-N @ . ;

private

: AMBIGUITY ( -- )
   GE-SRC-RESET
   s" using OUTER-INTERPRET-FXA using OUTER-INTERPRET-FXB" GE-SRC-LINE
   GE-SRC-LF
   s" OI-TWIN" GE-SRC+
   GE-EVAL-FORK-CAPTURE KEEP
   GE-SRC-RESET
   s" using OUTER-INTERPRET-FXA using OUTER-INTERPRET-FXB" GE-SRC-LINE
   s" OUTER-INTERPRET-TEST:OI-AMBIG" GE-SRC+
   GE-EVAL-FORK-CAPTURE
   s" ambiguity" SAME
   ENGINE-ERROR:USING-AMBIGUOUS s" ambiguity" GE-EXPECT-RC
   s" hb: ambiguous bare word resolves in multiple used packages: OI-TWIN at "
   s" ambiguity" GE-EXPECT-ERR-HAS
   S\" outer-interpret.f:3\n" s" ambiguity" GE-EXPECT-ERR-HAS
   1 CASES +! ;

\ A file loaded after the seam is rebound arrives at the new binding.
: SEAM ( -- )
   GE-SRC-RESET
   s" 7 ." GE-SRC-LINE
   s" oi-seam.f" CASE-BUF GT-PATH CASE-U !
   CASE$ SRC>FILE
   GE-SRC-RESET
   s" OUTER-INTERPRET-TEST:SPY-ON " GE-SRC+
   CASE$ GE-SRC-S"
   s"  included OUTER-INTERPRET-TEST:SPY-N." GE-SRC-LINE
   GE-EVAL-FORK-CAPTURE
   s" seam" GE-EXPECT-OK
   S\" 7\n1\n" s" seam" GE-EXPECT-OUT ;

: MAIN ( -- )
   0 CASES !
   s" habu-outer-interpret" GT-START
   PRELUDE
   NUMBERS
   COMMENTS
   LINE-COMMENT-AT-END
   NOT-A-COMMENT
   BLANK-INPUT
   UNDEFINED
   OUT-OF-RANGE
   UNDERFLOW
   UNDERDEPTH
   INTERNAL
   WIDE
   HOOK-WINDOW
   HOOK-UNDERFLOW
   NESTED
   AMBIGUITY
   SEAM
   GT-CLEANUP
   s" outer-interpret: " type CASES @ FMT:.INT
   s"  cases agree with the engine's loop" type cr ;

MAIN

;package
