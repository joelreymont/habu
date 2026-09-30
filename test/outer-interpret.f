\ outer-interpret.f - the interpret loop written in Habu, src/habu/interpret.f
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
\ forms, `char` and `'`), the package keywords (`package`, `public`,
\ `private`, `;package`, `using`, `;using` and `export`) and words.
\
\ One case is the Habu loop's alone: `export` of an internal word, which the
\ engine's own `export` publishes without its DNAME-INT mark. And one check
\ runs in a forked copy of this process instead, the seam: a fork binds it to
\ a counting spy, and a loaded file must arrive there.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require test/gate-common.f
require src/habu/interpret.f

package OUTER-INTERPRET-TEST

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
\ The spin prelude's path; SPIN-U is 0 while a case runs without it.
create SPIN-BUF FS-PATH-CAP allot
variable SPIN-U

: PRELUDE$ ( -- ptr u8 n )
   PRELUDE-BUF PRELUDE-U @ ;

: CASE$ ( -- ptr u8 n )
   CASE-BUF CASE-U @ ;

: NESTED$ ( -- ptr u8 n )
   NESTED-BUF NESTED-U @ ;

: SPIN$ ( -- ptr u8 n )
   SPIN-BUF SPIN-U @ ;

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
\ string's bytes in decimal, a package with a private word and a public one
\ that a global twin shadows, two packages whose publics share a tail, a
\ namespace a qualified definition made (it has no private wordlist), an
\ integer constant, and a word that fills the dictionary (the name index goes
\ first, as test/engine-writers.f EW-DICT-FULL drops it).
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
   s" package OI-PKG : OI-SECRET ( -- n ) 5 ; public : OI-SEVEN ( -- n ) 7 ; ;package" GE-SRC-LINE
   s" : OI-SEVEN ( -- n ) 8 ;" GE-SRC-LINE
   s" package OI-FXA public : OI-TWIN ( -- n ) 1 ; : OI-LONG-NAMED-TWIN ( -- n ) 4 ; ;package" GE-SRC-LINE
   s" package OI-FXB public : OI-TWIN ( -- n ) 2 ; ;package" GE-SRC-LINE
   s" : OI-QUAL:OI-Q ( -- n ) 3 ;" GE-SRC-LINE
   s" 5 constant OI-FIVE" GE-SRC-LINE
   s" TRUSTED: OI-DICT-FULL ( -- ) 0 data-base HIDXP-CELL + ! DICT-CAP ndict! ;" GE-SRC-LINE
   s" oi-prelude.f" PRELUDE-BUF GT-PATH PRELUDE-U !
   PRELUDE$ SRC>FILE ;

\ A second prelude for the cases that need a live task: OI-SPIN starts one that
\ spins until the process ends. A task this prelude started would end the load
\ of test/outer-loop-on.f, whose `package` mutates the dictionary, so each such
\ case starts its own.
: SPIN-PRELUDE ( -- )
   GE-SRC-RESET
   s" require lib/task.f" GE-SRC-LINE
   s" : OI-SPIN-BODY ( -- ) begin TASK:PAUSE false until ;" GE-SRC-LINE
   s" TASK:MIN-STACK TASK:TASK OI-SPINNER" GE-SRC-LINE
   s" : OI-SPIN ( -- ) ['] OI-SPIN-BODY OI-SPINNER TASK:ACTIVATE ;" GE-SRC-LINE
   s" oi-spin-prelude.f" SPIN-BUF GT-PATH SPIN-U !
   SPIN$ SRC>FILE ;

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
   SPIN-U @ 0<> if SPIN$ GE-ARG+ then
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

\ The source buffer as the case file named, run by the Habu loop alone.
: HABU ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu CASE-BUF GT-PATH CASE-U !
   CASE$ SRC>FILE
   true RUN ;

\ One line as the case file named, run by both loops.
: LINE-CASE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   GE-SRC-RESET
   src srcu GE-SRC-LINE
   name nameu BOTH ;

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

\ Top-level `char` operands whose bytes together pass the definition-body
\ capture (layout.f BODYBUF-CAP): no definition is open, so none is captured.
: CHAR-PAST-BODY-CAP ( -- )
   GE-SRC-RESET
   BODYBUF-CAP 8 / 1 + 0 ?do s" char ABCDEFGH drop" GE-SRC-LINE loop
   s" char Q . depth ." GE-SRC-LINE
   s" oi-char-past-cap.f" BOTH
   s" char past the body capture" GE-EXPECT-OK
   S\" 81\n0\n" s" char past the body capture" GE-EXPECT-OUT ;

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
\ the exit hook the program armed, `cr` (in src/habu/layout.f EXIT-HOOK-CELL),
\ does not run.
: SEALED ( ptr u8 n -- ) {: t:ptr u:n :}
   GE-SRC-RESET
   s" ' cr data-base EXIT-HOOK-CELL + !" GE-SRC-LINE
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

\ ---- the package keywords --------------------------------------------------------------
\ A package opens on its private wordlist, where its private words resolve
\ bare, then its public ones; `public` and `private` switch between them, and
\ after `;package` a bare tail is the global wordlist's again.
: PACKAGE-SCOPE ( -- )
   GE-SRC-RESET
   s" package OI-X public ;package 1 ." GE-SRC-LINE
   s" package OI-PKG OI-SECRET . OI-SEVEN . public OI-SEVEN . private OI-SECRET . ;package OI-SEVEN ." GE-SRC-LINE
   s" oi-package-scope.f" BOTH
   s" package scope" GE-EXPECT-OK
   S\" 1\n5\n7\n7\n5\n8\n" s" package scope" GE-EXPECT-OUT ;

\ A new package takes two fresh wordlists, public then private, and opens on
\ the private one; the global wordlist, 0, is current after `;package`. A
\ namespace a qualified definition made has no private wordlist, and opening
\ it takes the next one. The two routes load different files before the case,
\ so a wordlist's id differs between them and only differences are printed:
\ `wordlist` answers the id after the last one taken.
: PACKAGE-WORDLISTS ( -- )
   GE-SRC-RESET
   s" package OI-NEW get-current public get-current - . private get-current ;package wordlist swap - . get-current ." GE-SRC-LINE
   s" package OI-QUAL OI-Q . get-current ;package wordlist swap - ." GE-SRC-LINE
   s" oi-package-wordlists.f" BOTH
   s" package wordlists" GE-EXPECT-OK
   S\" 1\n1\n0\n3\n1\n" s" package wordlists" GE-EXPECT-OUT ;

\ `package` needs a name, no package open and no colon in the name; `public`,
\ `private` and `;package` need an open package. Each refusal names the token
\ it read: the keyword, or the name.
: PACKAGE-REFUSALS ( -- )
   GE-SRC-RESET
   s" 1 ." GE-SRC-LINE
   s" package" GE-SRC+
   s" oi-package-no-name.f" BOTH
   S\" 1\n" CASE$ GE-EXPECT-OUT
   74 s" package at " S\" oi-package-no-name.f:2\n" DIED-AT
   s" package OI-A package OI-B" s" oi-package-nested.f" LINE-CASE
   75 s" package at " S\" oi-package-nested.f:1\n" DIED-AT
   s" package OI-A:B" s" oi-package-colon.f" LINE-CASE
   75 s" OI-A:B at " S\" oi-package-colon.f:1\n" DIED-AT
   s" public" s" oi-public-outside.f" LINE-CASE
   75 s" public at " S\" oi-public-outside.f:1\n" DIED-AT
   s" private" s" oi-private-outside.f" LINE-CASE
   75 s" private at " S\" oi-private-outside.f:1\n" DIED-AT
   s" ;package" s" oi-end-package-outside.f" LINE-CASE
   75 s" ;package at " S\" oi-end-package-outside.f:1\n" DIED-AT ;

\ The package keywords raise no top-row event: only the number, `.`, `0` and
\ set-top-check log.
: PACKAGE-HOOK ( -- )
   GE-SRC-RESET
   s" OI-HOOK-ON package OI-PKG public private ;package using OI-FXA ;using 5 . 0 set-top-check" GE-SRC-LINE
   s" oi-package-hook.f" BOTH
   s" package hook" GE-EXPECT-OK
   S\" 1\n0\n5\n6\n257\n.\n5\n1\n0\n0\n6\n257\nset-top-check\n" s" package hook" GE-EXPECT-OUT ;

\ `using` makes a package's publics bare until `;using`.
: USING-SCOPE ( -- )
   s" using OI-FXA OI-TWIN . ;using OI-TWIN ." s" oi-using.f" LINE-CASE
   70 s" using" GE-EXPECT-RC
   S\" 1\n" s" using" GE-EXPECT-OUT
   S\" E-UNDEFINED: OI-TWIN\n" s" using" GE-EXPECT-ERR ;

\ A using opened in a package ends at its `;package`.
: USING-IN-PACKAGE ( -- )
   s" package OI-T using OI-FXA OI-TWIN . ;package OI-TWIN" s" oi-using-in-package.f" LINE-CASE
   70 s" using in a package" GE-EXPECT-RC
   S\" 1\n" s" using in a package" GE-EXPECT-OUT
   S\" E-UNDEFINED: OI-TWIN\n" s" using in a package" GE-EXPECT-ERR ;

\ Usings are file-local: one a nested include opens ends with it, and so does
\ one open when the include throws.
: USING-FILE-LOCAL ( -- )
   GE-SRC-RESET
   s" using OI-FXA" GE-SRC-LINE
   s" oi-nested-using.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   s" include oi-nested-using.f OI-TWIN" s" oi-include-using.f" LINE-CASE
   70 s" using ends with its file" GE-EXPECT-RC
   S\" E-UNDEFINED: OI-TWIN\n" s" using ends with its file" GE-EXPECT-ERR
   GE-SRC-RESET
   s" using OI-FXA OI-NOPE" GE-SRC-LINE
   s" oi-nested-using-throw.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   GE-SRC-RESET
   s" s~ oi-nested-using-throw.f~ ' included catch . OI-TWIN" QLINE
   s" oi-include-using-throw.f" BOTH
   70 s" using ends with a throw" GE-EXPECT-RC
   S\" 70\n" s" using ends with a throw" GE-EXPECT-OUT
   S\" E-UNDEFINED: OI-NOPE\nE-UNDEFINED: OI-TWIN\n" s" using ends with a throw" GE-EXPECT-ERR ;

\ `using` needs a name with no colon that names a package, and at most
\ USE-MAX usings open; `;using` needs one open.
: USING-REFUSALS ( -- )
   s" using" s" oi-using-no-name.f" LINE-CASE
   ENGINE-ERROR:USING-NO-NAME s" hb: using: missing package name at " S\" oi-using-no-name.f:2\n" DIED-AT
   s" using OI-A:B" s" oi-using-colon.f" LINE-CASE
   ENGINE-ERROR:USING-BAD-NAME s" hb: using: package name must not contain ':': OI-A:B at " S\" oi-using-colon.f:1\n" DIED-AT
   s" using OI-NOPE" s" oi-using-unknown.f" LINE-CASE
   ENGINE-ERROR:USING-UNKNOWN s" hb: using: unknown package: OI-NOPE at " S\" oi-using-unknown.f:1\n" DIED-AT
   GE-SRC-RESET
   USE-MAX 0 ?do s" using OI-PKG" GE-SRC-LINE loop
   s" using OI-FXA" GE-SRC-LINE
   s" oi-using-overflow.f" BOTH
   ENGINE-ERROR:USING-OVERFLOW s" hb: using: too many concurrent usings: OI-FXA at " S\" oi-using-overflow.f:17\n" DIED-AT
   s" ;using" s" oi-using-unbalanced.f" LINE-CASE
   ENGINE-ERROR:USING-UNBALANCED s" hb: ;using without an open using at " S\" oi-using-unbalanced.f:1\n" DIED-AT ;

\ A tail two used packages export is ambiguous: the refusal names its line.
: AMBIGUITY ( -- )
   GE-SRC-RESET
   s" using OI-FXA using OI-FXB" GE-SRC-LINE
   GE-SRC-LF
   s" OI-TWIN" GE-SRC+
   s" oi-ambiguity.f" BOTH
   ENGINE-ERROR:USING-AMBIGUOUS
   s" hb: ambiguous bare word resolves in multiple used packages: OI-TWIN at "
   S\" oi-ambiguity.f:3\n" DIED-AT ;

\ A tick that only a used package answers.
: TICK-USED ( -- )
   s" using OI-FXA ' OI-TWIN execute ." s" oi-tick-used.f" LINE-CASE
   s" tick through a using" GE-EXPECT-OK
   S\" 1\n" s" tick through a using" GE-EXPECT-OUT ;

\ The checker resolves a definition's bare names through the same usings, so
\ it finds a used public that shadows a global tail only when `using` told it
\ the package's name.
: USING-SHADOW ( -- )
   GE-SRC-RESET
   s" using OI-PKG s~ : OI-SH ( -- n ) OI-SEVEN ;~ evaluate" QLINE
   s" oi-using-shadow.f" BOTH
   67 s" using shadow" GE-EXPECT-RC
   s" E-USING-SHADOW-GLOBAL" s" using shadow" GE-EXPECT-ERR-HAS ;

\ At top level `export` consumes its name and does nothing else; with no name
\ it refuses, naming itself.
: EXPORT-TOP-LEVEL ( -- )
   GE-SRC-RESET
   s" export OI-NOPE 1 ." GE-SRC-LINE
   s" export" GE-SRC+
   s" oi-export-top.f" BOTH
   S\" 1\n" CASE$ GE-EXPECT-OUT
   74 s" export at " S\" oi-export-top.f:2\n" DIED-AT ;

\ In a package `export` publishes an existing word under its tail: the same
\ code with the immediate, certified-input and wide bits, which the hook's
\ flags and the wide refusal show. A global word, a constant, an engine
\ constant and another package's public all export.
: EXPORT-ALIASES ( -- )
   GE-SRC-RESET
   s" package OI-EX public export OI-TWO export OI-IMM export OI-FIVE export USE-MAX export OI-FXA:OI-TWIN export OI-WIDE ;package" GE-SRC-LINE
   s" OI-HOOK-ON 1 2 OI-EX:OI-TWO OI-EX:OI-IMM 0 set-top-check" GE-SRC-LINE
   s" OI-EX:OI-FIVE . OI-EX:USE-MAX . OI-EX:OI-TWIN . depth ." GE-SRC-LINE
   s" OI-EX:OI-WIDE" GE-SRC-LINE
   s" oi-export-aliases.f" BOTH
   70 s" export aliases" GE-EXPECT-RC
   S\" 6\n513\nOI-EX:OI-TWO\n6\n3\nOI-EX:OI-IMM\n" s" export aliases" GE-EXPECT-OUT-HAS
   S\" 5\n16\n1\n0\n" s" export aliases" GE-EXPECT-OUT-HAS
   S\" hb: interpret-mode layout value: OI-EX:OI-WIDE\n" s" export aliases" GE-EXPECT-ERR ;

\ The source is looked up in the open package and the global wordlist, never
\ in a used public; its tail must be new to the current wordlist, the refusal
\ naming the operand as spelled; and `export` needs a name.
: EXPORT-REFUSALS ( -- )
   s" package OI-EX public using OI-FXA export OI-TWIN" s" oi-export-undefined.f" LINE-CASE
   70 s" OI-TWIN at " S\" oi-export-undefined.f:1\n" DIED-AT
   s" package OI-PKG public export OI-FXA:OI-TWIN ;package OI-PKG:OI-TWIN . package OI-PKG public export oi-fxa:oi-twin"
   s" oi-export-duplicate.f" LINE-CASE
   S\" 1\n" CASE$ GE-EXPECT-OUT
   78 s" duplicate definition: oi-fxa:oi-twin at " S\" oi-export-duplicate.f:1\n" DIED-AT
   GE-SRC-RESET
   s" package OI-EX public" GE-SRC-LINE
   s" export" GE-SRC+
   s" oi-export-no-name.f" BOTH
   74 s" export at " S\" oi-export-no-name.f:2\n" DIED-AT ;

\ An internal word has no checker-known effect, and the engine's `export`
\ publishes an alias of one without the DNAME-INT mark that keeps it behind a
\ TRUSTED: boundary. The Habu loop refuses it as its interpret gate does.
: EXPORT-INTERNAL ( -- )
   GE-SRC-RESET
   s" package OI-EX public export DEFER-UNSET" GE-SRC-LINE
   s" oi-export-internal.f" HABU
   70 s" export internal" GE-EXPECT-RC
   S\" hb: internal engine word: DEFER-UNSET\n" s" export internal" GE-EXPECT-ERR ;

\ With the dictionary full a new package refuses, naming what DEF-TKA and
\ DEF-TKL hold, which `package` never writes: here the operand of the `export`
\ before it. A long name that would reach the code ceiling refuses too;
\ `export` names the tail it would store.
: CAPACITY ( -- )
   s" package OI-PKG public export OI-TWO ;package OI-DICT-FULL package OI-NEW" s" oi-dictionary-full.f" LINE-CASE
   77 s" hb: dictionary full at: OI-TWO at " S\" oi-dictionary-full.f:1\n" DIED-AT
   s" dbase@ REGION + $4000 - 8 - cp! package A-LONG-NAMESPACE-ROW" s" oi-package-code-full.f" LINE-CASE
   76 s" hb: code space full at: A-LONG-NAMESPACE-ROW at " S\" oi-package-code-full.f:1\n" DIED-AT
   s" package OI-EX public dbase@ REGION + $4000 - 8 - cp! export OI-FXA:OI-LONG-NAMED-TWIN" s" oi-export-code-full.f" LINE-CASE
   76 s" hb: code space full at: OI-LONG-NAMED-TWIN at " S\" oi-export-code-full.f:1\n" DIED-AT ;

\ `package` of a sealed package's name, or of a package whose public wordlist
\ is protected, ends the process, the name its whole diagnostic; so does
\ `export` into a protected wordlist, naming its guard and the word. The exit
\ is fail-closed: the exit hook the program armed, `cr`, does not run.
: ARMED ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   GE-SRC-RESET
   s" ' cr data-base EXIT-HOOK-CELL + !" GE-SRC-LINE
   src srcu GE-SRC-LINE
   name nameu BOTH
   ENGINE-ERROR:SEAL-PACKAGE CASE$ GE-EXPECT-RC ;

: PROTECTED-PACKAGES ( -- )
   s" package ENGINE-ERROR" s" oi-sealed-package.f" ARMED
   s" ENGINE-ERROR" CASE$ GE-EXPECT-ERR
   s" package OI-EX public get-current prot-wid-add ;package package OI-EX" s" oi-protected-package.f" ARMED
   s" OI-EX" CASE$ GE-EXPECT-ERR
   s" package OI-EX public get-current prot-wid-add export OI-TWO" s" oi-protected-export.f" ARMED
   S\" hb: cannot publish into protected word: OI-TWO\n" CASE$ GE-EXPECT-ERR ;

\ ---- with a task live -----------------------------------------------------------------
\ A literal that keeps its text in data space exits $4F with no output while a
\ task is live, as `allot` does, after its own refusals: a counted string too
\ long still refuses first. `."` keeps nothing and runs.
: SPUN ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   GE-SRC-RESET
   s" OI-SPIN " GE-SRC+  src srcu QLINE
   name nameu BOTH ;

: TASK-LIVE ( -- )
   SPIN-PRELUDE
   s" s~ hi~ type cr" s" oi-live-str.f" SPUN
   79 CASE$ GE-EXPECT-RC  CASE$ GE-EXPECT-SILENT
   s" c~ hi~ count type cr" s" oi-live-cstr.f" SPUN
   79 CASE$ GE-EXPECT-RC  CASE$ GE-EXPECT-SILENT
   s" s\~ h\x69~ type cr" s" oi-live-esc-str.f" SPUN
   79 CASE$ GE-EXPECT-RC  CASE$ GE-EXPECT-SILENT
   s" c\~ h\x69~ count type cr" s" oi-live-esc-cstr.f" SPUN
   79 CASE$ GE-EXPECT-RC  CASE$ GE-EXPECT-SILENT
   s" .\~ h\x69~ cr" s" oi-live-esc-dot.f" SPUN
   79 CASE$ GE-EXPECT-RC  CASE$ GE-EXPECT-SILENT
   s" .~ ok~ cr" s" oi-live-dot.f" SPUN
   CASE$ GE-EXPECT-OK
   S\" ok\n" CASE$ GE-EXPECT-OUT
   GE-SRC-RESET
   s" OI-SPIN c~ " Q+ 256 [char] x GE-SRC-REPEAT-C s" ~" QLINE
   s" oi-live-too-long.f" BOTH
   76 s" hb: counted string too long (max 255) at " S\" oi-live-too-long.f:1\n" DIED-AT
   0 SPIN-U ! ;

\ A package keyword exits 79 while a task is live, the keyword its whole
\ diagnostic, before it reads anything.
: LIVE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n kw:ptr kwu:n name:ptr nameu:n :}
   src srcu name nameu LINE-CASE
   79 CASE$ GE-EXPECT-RC
   kw kwu CASE$ GE-EXPECT-ERR ;

: TASK-LIVE-KEYWORDS ( -- )
   SPIN-PRELUDE
   s" OI-SPIN package OI-T" s" package" s" oi-live-package.f" LIVE
   s" package OI-T OI-SPIN public" s" public" s" oi-live-public.f" LIVE
   s" package OI-T OI-SPIN private" s" private" s" oi-live-private.f" LIVE
   s" package OI-T OI-SPIN ;package" s" ;package" s" oi-live-end-package.f" LIVE
   s" OI-SPIN using OI-FXA" s" using" s" oi-live-using.f" LIVE
   s" using OI-FXA OI-SPIN ;using" s" ;using" s" oi-live-end-using.f" LIVE
   s" package OI-T public OI-SPIN export OI-TWO" s" export" s" oi-live-export.f" LIVE
   0 SPIN-U ! ;

\ ---- in a forked copy of this process -----------------------------------------------

variable SPY-N

: SPY ( ptr u8 n -- )
   1 SPY-N +!
   OUTER:INTERPRET ;

public

: SPY-ON ( -- )
   [: SPY ;] is SOURCE-ROOT:INCLUDE-INTERPRET ;

: SPY-N. ( -- )
   SPY-N @ . ;

private

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
   CHAR-PAST-BODY-CAP
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
   PACKAGE-SCOPE
   PACKAGE-WORDLISTS
   PACKAGE-REFUSALS
   PACKAGE-HOOK
   USING-SCOPE
   USING-IN-PACKAGE
   USING-FILE-LOCAL
   USING-REFUSALS
   AMBIGUITY
   TICK-USED
   USING-SHADOW
   EXPORT-TOP-LEVEL
   EXPORT-ALIASES
   EXPORT-REFUSALS
   EXPORT-INTERNAL
   CAPACITY
   PROTECTED-PACKAGES
   TASK-LIVE
   TASK-LIVE-KEYWORDS
   SEAM
   GT-CLEANUP
   s" outer-interpret: " type CASES @ FMT:.INT
   s"  cases agree with the engine's loop" type cr ;

MAIN

;package
