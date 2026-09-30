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
\ numbers, comments, the literal keywords (`s"`, `c"`, `."`, their escaped
\ forms, `char` and `'`) and words.
\
\ Three checks run in a forked copy of this process instead. Ambiguity, and a
\ tick that only a used package answers, need usings open around the token,
\ and `using` is a keyword: the fork opens them and calls OUTER:INTERPRET,
\ against a fork whose `evaluate` reads the same text. And the seam: a fork
\ binds it to a counting spy, and a loaded file must arrive there.

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
$7E constant TILDE

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

\ Source text in which each `~` stands for a double quote, which a string in
\ this file cannot hold.
: Q+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do
      a i + c@ {: c:n :}
      c TILDE = if GE-DQ else c then GE-SRC-C
   loop ;

: QLINE ( ptr u8 n -- )
   Q+ GE-SRC-LF ;

\ Words the cases call: a certified word of two inputs, a trusted one that
\ drops a cell it does not declare, an immediate word, a wide one, a word
\ spelled as an out-of-range number, a top-row hook that logs each event and
\ one that drops two cells more than its event carries, a word that prints a
\ string's bytes in decimal, and a package public.
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
   s" : OI-BYTES ( ptr u8 n -- ) {: a:ptr u:n :} u 0 ?do a i + c@ . loop ;" GE-SRC-LINE
   s" package OI-PKG public : OI-SEVEN ( -- n ) 7 ; ;package" GE-SRC-LINE
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

\ ---- the literal keywords --------------------------------------------------------------
\ Each string form, some spelled in upper case; every escape; the data space
\ each keeps (a `."` none, a `.\"` its decoded bytes); an empty text; a keyword
\ ended by a newline; a text across lines; and a counted string of 255 bytes,
\ one of them from 1020 bytes of escapes.
: LITERALS ( -- )
   GE-SRC-RESET
   s" s~ hi~ type cr" QLINE
   s" S~ Hi~ type cr" QLINE
   s" c~ hey~ count type cr" QLINE
   s" .~ dot~ cr" QLINE
   s" C\~ e\x42~ count type cr" QLINE
   s" .\~ x\ny~ cr" QLINE
   s" s\~ \a\b\e\f\l\n\r\t\v\z\q\~\\\x41\X4a\x4F~ OI-BYTES" QLINE
   s" here s~ abc~ 2drop here swap - ." QLINE
   s" here c~ abc~ drop here swap - ." QLINE
   s" here .~ ~ here swap - ." QLINE
   s" here .\~ q~ here swap - ." QLINE
   s" s~ ~ nip ." QLINE
   s" s~" QLINE
   s" x~ type cr" QLINE
   s" s~ two" QLINE
   s" lines~ type cr" QLINE
   s" c~ " Q+
   255 [char] z GE-SRC-REPEAT-C s" ~ c@ ." QLINE
   s" c\~ " Q+
   255 0 ?do s" \x41" GE-SRC+ loop s" ~ c@ ." QLINE
   s" depth ." GE-SRC-LINE
   s" oi-literals.f" BOTH
   s" literals" GE-EXPECT-OK
   S\" hi\nHi\nhey\ndot\neB\nx\ny\n7\n8\n27\n12\n10\n10\n13\n9\n11\n0\n34\n34\n92\n65\n74\n79\n3\n4\n0\nq1\n0\nx\ntwo\nlines\n255\n255\n0\n"
   s" literals" GE-EXPECT-OUT ;

\ char pushes its operand's first byte. ' pushes an xt with no depth gate, a
\ name no word has is a quiet miss, an edge colon leaves a name bare, and an
\ unsealed qualifier resolves.
: TICK-AND-CHAR ( -- )
   GE-SRC-RESET
   s" char Abc . CHAR z ." GE-SRC-LINE
   s" 1 2 ' OI-TWO execute depth ." GE-SRC-LINE
   s" ' OI-TWO drop depth ." GE-SRC-LINE
   s" ' OI-NOPE depth ." GE-SRC-LINE
   s" ' engine-error: depth ." GE-SRC-LINE
   s" ' OI-PKG:OI-SEVEN execute ." GE-SRC-LINE
   s" oi-tick-char.f" BOTH
   s" tick and char" GE-EXPECT-OK
   S\" 65\n122\n0\n0\n0\n0\n7\n" s" tick and char" GE-EXPECT-OUT ;

\ A refusal through the engine's compile-die tail: its rc, its message, and
\ the case file's line the refusal names.
: DIED-AT ( n ptr u8 n ptr u8 n -- ) {: rc:n msg:ptr msgu:n at:ptr atu:n :}
   rc CASE$ GE-EXPECT-RC
   msg msgu CASE$ GE-EXPECT-ERR-HAS
   at atu CASE$ GE-EXPECT-ERR-HAS ;

\ No closing quote: the refusal names the keyword's line, not the end's.
: UNTERMINATED ( -- )
   GE-SRC-RESET
   s" 1 ." GE-SRC-LINE
   s" 2 s~ never" QLINE
   s" closed" GE-SRC-LINE
   s" oi-unterminated.f" BOTH
   S\" 1\n" CASE$ GE-EXPECT-OUT
   74 s" hb: bad string literal at " S\" oi-unterminated.f:2\n" DIED-AT ;

\ A bad escape past a newline in the text is refused at the keyword's line.
: BAD-ESCAPE ( -- )
   GE-SRC-RESET
   s" 1 ." GE-SRC-LINE
   s" s\~ a" QLINE
   s" \m~" QLINE
   s" oi-bad-escape.f" BOTH
   74 s" hb: bad string literal at " S\" oi-bad-escape.f:2\n" DIED-AT ;

: BAD-HEX ( -- )
   GE-SRC-RESET
   s" 1 ." GE-SRC-LINE
   s" s\~ \x4g~" QLINE
   s" oi-bad-hex.f" BOTH
   74 s" hb: bad string literal at " S\" oi-bad-hex.f:2\n" DIED-AT ;

\ A backslash that is the input's last byte escapes nothing.
: ESCAPE-AT-END ( -- )
   GE-SRC-RESET
   s" 1 ." GE-SRC-LINE
   s" s\~ ab\" Q+
   s" oi-escape-end.f" BOTH
   74 s" hb: bad string literal at " S\" oi-escape-end.f:2\n" DIED-AT ;

\ 256 bytes across a newline. c" checks the length once INP has passed the
\ closing quote, c\" before, so the two name different lines.
: COUNTED-TOO-LONG ( -- )
   GE-SRC-RESET
   s" c~ " Q+ 200 [char] x GE-SRC-REPEAT-C GE-SRC-LF
   55 [char] y GE-SRC-REPEAT-C s" ~" QLINE
   s" oi-counted-long.f" BOTH
   76 s" hb: counted string too long (max 255) at " S\" oi-counted-long.f:2\n" DIED-AT ;

: ESCAPED-TOO-LONG ( -- )
   GE-SRC-RESET
   s" c\~ " Q+ 200 [char] x GE-SRC-REPEAT-C GE-SRC-LF
   55 [char] y GE-SRC-REPEAT-C s" ~" QLINE
   s" oi-escaped-long.f" BOTH
   76 s" hb: counted string too long (max 255) at " S\" oi-escaped-long.f:1\n" DIED-AT ;

\ A reader keyword at the end of the input: the refusal spells the keyword in
\ lowercase and names the end's line.
: CHAR-NO-NAME ( -- )
   GE-SRC-RESET
   s" 1 ." GE-SRC-LINE
   s" CHAR" GE-SRC-LINE
   s" oi-char-no-name.f" BOTH
   74 s" hb: reader keyword needs a name: char at " S\" oi-char-no-name.f:3\n" DIED-AT ;

: TICK-NO-NAME ( -- )
   GE-SRC-RESET
   s" 1 ." GE-SRC-LINE
   s" '" GE-SRC-LINE
   GE-SRC-LF
   s" oi-tick-no-name.f" BOTH
   74 s" hb: reader keyword needs a name: ' at " S\" oi-tick-no-name.f:4\n" DIED-AT ;

\ ' of a token qualified by a sealed package ends the process, the token its
\ whole diagnostic. Every package the engine seals is asked once, in assorted
\ case, and a second colon does not spare the token. The exit is fail-closed:
\ the exit hook the program armed, `cr` ($2818 is src/habu/layout.f
\ EXIT-HOOK-CELL), does not run.
: SEALED ( ptr u8 n -- ) {: t:ptr u:n :}
   GE-SRC-RESET
   s" ' cr data-base $2818 + !" GE-SRC-LINE
   s" ' " GE-SRC+ t u GE-SRC-LINE
   s" oi-sealed.f" BOTH
   ENGINE-ERROR:SEAL-PACKAGE t u GE-EXPECT-RC
   t u t u GE-EXPECT-ERR ;

: SEALED-PACKAGES ( -- )
   s" tfam:x" SEALED
   s" TYPE:x" SEALED
   s" Match:x" SEALED
   s" checker-cert:x:y" SEALED
   s" lower-cert:x" SEALED
   s" LOWER-CERT-HOOK:x" SEALED
   s" engine-error:x" SEALED ;

: TICK-WIDE ( -- )
   GE-SRC-RESET
   s" ' OI-WIDE" GE-SRC-LINE
   s" oi-tick-wide.f" BOTH
   70 s" tick wide" GE-EXPECT-RC
   S\" hb: interpret-mode layout value: OI-WIDE\n" s" tick wide" GE-EXPECT-ERR ;

: TICK-INTERNAL ( -- )
   GE-SRC-RESET
   s" ' DEFER-UNSET" GE-SRC-LINE
   s" oi-tick-internal.f" BOTH
   70 s" tick internal" GE-EXPECT-RC
   S\" hb: internal engine word: DEFER-UNSET\n" s" tick internal" GE-EXPECT-ERR ;

\ The literal events, test/top-row-hook-test.f's window: each logs its class,
\ flags and token. A string's token is its keyword, a char's and a tick's the
\ operand, and a tick's flags are the word's. `."`, `.\"` and a missed tick
\ log nothing.
: LITERAL-HOOK ( -- )
   GE-SRC-RESET
   s" OI-HOOK-ON s~ hi~ 2drop c~ hey~ drop char A drop ' OI-TWO drop" QLINE
   s" S\~ e~ 2drop .~ x~ .\~ y~ ' OI-NOPE 0 set-top-check" QLINE
   s" oi-literal-hook.f" BOTH
   s" literal hook" GE-EXPECT-OK
   S\" 2\n0\ns\q\n6\n513\n2drop\n3\n0\nc\q\n6\n257\ndrop\n4\n0\nA\n6\n257\ndrop\n5\n513\nOI-TWO\n6\n257\ndrop\n2\n0\nS\\\q\n6\n513\n2drop\nxy1\n0\n0\n6\n257\nset-top-check\n"
   s" literal hook" GE-EXPECT-OUT ;

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

\ A tick that only a used package answers: the fork's `evaluate` pushes the
\ text for OUTER:INTERPRET to read.
: TICK-USED ( -- )
   GE-SRC-RESET
   s" using OUTER-INTERPRET-FXA ' OI-TWIN execute ." GE-SRC-LINE
   GE-EVAL-FORK-CAPTURE KEEP
   GE-SRC-RESET
   s" using OUTER-INTERPRET-FXA " GE-SRC+
   s" ' OI-TWIN execute ." GE-SRC-S"
   s"  OUTER:INTERPRET" GE-SRC-LINE
   GE-EVAL-FORK-CAPTURE
   s" tick through a using" SAME
   s" tick through a using" GE-EXPECT-OK
   S\" 1\n" s" tick through a using" GE-EXPECT-OUT
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
   LITERALS
   TICK-AND-CHAR
   UNTERMINATED
   BAD-ESCAPE
   BAD-HEX
   ESCAPE-AT-END
   COUNTED-TOO-LONG
   ESCAPED-TOO-LONG
   CHAR-NO-NAME
   TICK-NO-NAME
   SEALED-PACKAGES
   TICK-WIDE
   TICK-INTERNAL
   LITERAL-HOOK
   AMBIGUITY
   TICK-USED
   SEAM
   GT-CLEANUP
   s" outer-interpret: " type CASES @ FMT:.INT
   s"  cases agree with the engine's loop" type cr ;

MAIN

;package
