\ outer-interpret.f - OUTER:INTERPRET (src/habu/interpret.f), the interpret
\ loop written in Habu, against the engine's own through the real load path.
\ A child engine loads each case file twice, `--load <prelude>
\ test/outer-loop-on.f <case>` (the Habu loop, installed through
\ SOURCE-ROOT:USE-INTERPRETER) and `--load <prelude> <case>`: rc, stdout and
\ stderr must match, and each case states the rc and output it exercises.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require test/gate-common.f
require src/habu/interpret.f

package OUTER-INTERPRET-TEST

$5C constant BACKSLASH
$7E constant TILDE

variable CASES

\ ---- the files --------------------------------------------------------------------
create PRELUDE-BUF PATH-CAP allot
variable PRELUDE-U
create CASE-BUF PATH-CAP allot
variable CASE-U
create NESTED-BUF PATH-CAP allot
variable NESTED-U
\ The spin prelude's path; SPIN-U is 0 while a case runs without it.
create SPIN-BUF PATH-CAP allot
variable SPIN-U

: PRELUDE$ ( -- ptr u8 n )
   PRELUDE-BUF PRELUDE-U @ ;

: CASE$ ( -- ptr u8 n )
   CASE-BUF CASE-U @ ;

: NESTED$ ( -- ptr u8 n )
   NESTED-BUF NESTED-U @ ;

: SPIN$ ( -- ptr u8 n )
   SPIN-BUF SPIN-U @ ;

: SRC>FILE ( ptr u8 n -- )
   GE-SRC-BUF GE-SRC-U @ FS:WRITE-ALL ;

\ Source text in which each `~` stands for a double quote, which a string in
\ this file cannot hold.
: Q+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do
      a i + c@ {: c:n :}
      c TILDE = if GE-DQ else c then GE-SRC-C
   loop ;

: QLINE ( ptr u8 n -- )
   Q+ GE-SRC-LF ;

\ Words the cases call. OI-DICT-FULL clears the name index pointer before it
\ fills the dictionary. OI-SHOW, a neutral immediate, runs OI-SHOWN so a body
\ prints its pending definition where it names OI-SHOW: an exit hook runs only
\ after the input's end refused the definition and rolled it back. OI-ENDED.
\ prints the cells a definition's end clears, or'd together: 0 when all are.
\ OI-DX's clause name is longer than DNAME-INL bytes. OI-XT>N reads an xt as its
\ address for OI-ARM's exit hook (src/habu/layout.f EXIT-HOOK-CELL): an
\ undeclared cell holds no xt, and the cast introduces no pointer or xt, so it
\ needs no private section. A case catches a parked include or evaluate as
\ `s" text" OI-PARK ' OI-EVAL-PARKED catch`, which leaves only the code.
: PRELUDE ( -- )
   GE-SRC-RESET
   s" : OI-TWO ( n n -- ) 2drop ;" GE-SRC-LINE
   s" TRUSTED: OI-UF ( -- ) drop ;" GE-SRC-LINE
   s" : OI-IMM ( -- ) ; immediate" GE-SRC-LINE
   s" SUMTYPE oiwide<a,b>" GE-SRC-LINE
   s"   VARIANT ok a ;VARIANT" GE-SRC-LINE
   s"   VARIANT err b ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" TRUSTED: OI-WIDE ( -- oiwide<n,n> ) 7 9 ;" GE-SRC-LINE
   s" 1 constant 18446744073709551616" GE-SRC-LINE
   s" : OI-LOG ( ptr u8 n n n -- ) {: a:ptr u:n c:n f:n :} c . f . a u type cr ;" GE-SRC-LINE
   s" TRUSTED: OI-HOOK-ON ( -- ) ['] OI-LOG set-top-check ;" GE-SRC-LINE
   s" TRUSTED: OI-SINK ( n n n n n n -- ) 2drop 2drop 2drop ;" GE-SRC-LINE
   s" TRUSTED: OI-SINK-ON ( -- ) ['] OI-SINK set-top-check ;" GE-SRC-LINE
   s" : OI-BYTES ( ptr u8 n -- ) {: a:ptr u:n :} u 0 ?do a i + c@ . loop ;" GE-SRC-LINE
   s" package OI-PKG : OI-SECRET ( -- n ) 5 ; public : OI-SEVEN ( -- n ) 7 ; ;package" GE-SRC-LINE
   s" : OI-SEVEN ( -- n ) 8 ;" GE-SRC-LINE
   s" package OI-FXA public : OI-TWIN ( -- n ) 1 ; : OI-LONG-NAMED-TWIN ( -- n ) 4 ; ;package" GE-SRC-LINE
   s" package OI-FXB public : OI-TWIN ( -- n ) 2 ; ;package" GE-SRC-LINE
   s" package OI-C2 public : c2-invoke ( -- n ) 4 ; ;package" GE-SRC-LINE
   s" : OI-QUAL:OI-Q ( -- n ) 3 ;" GE-SRC-LINE
   s" 5 constant OI-FIVE" GE-SRC-LINE
   s" TRUSTED: OI-DICT-FULL ( -- ) 0 data-base HIDXP-CELL + ! DICT-CAP ndict! ;" GE-SRC-LINE
   s" variable OI-WANT" GE-SRC-LINE
   s" : OI-B. ( bool -- ) if 1 else 0 then . ;" GE-SRC-LINE
   s" : OI-CELL@ ( n -- n ) data-base + @ ;" GE-SRC-LINE
   s" : OI-PEND ( -- ptr n ) PEND-CELL OI-CELL@ XREF-ADDR>REC ;" GE-SRC-LINE
   s" : OI-REC. ( ptr n -- ) {: r:ptr :} r XREF-NAME$ type cr r XREF-FLAGS ." GE-SRC+
   s"  r XREF-WORDLIST OI-WANT @ = OI-B. ;" GE-SRC-LINE
   s" : OI-BODY. ( -- ) data-base BODYBUF-OFF + BYTE-VIEW BODYLEN-CELL OI-CELL@ type cr ;" GE-SRC-LINE
   s" : OI-STATE. ( -- ) TSIG-U-CELL OI-CELL@ . TRUSTED-CELL OI-CELL@ . NCOMP-DISPATCH:DEF-TIER-CELL OI-CELL@ . ;" GE-SRC-LINE
   s" : OI-DUMP ( -- ) OI-PEND OI-REC. OI-BODY. OI-STATE. ;" GE-SRC-LINE
   s" TRUSTED: OI-DICT-ALMOST ( -- ) 0 data-base HIDXP-CELL + ! DICT-CAP 1 - ndict! ;" GE-SRC-LINE
   s" package OI-DX : OI-MAKER-WORD ( n -- ) create , does> ( -- n ) @ 1 + ; ;package" GE-SRC-LINE
   s" : OI-CLAUSE. ( n -- ) {: k:n :} k XREF-DOES-COMPANION? dup OI-B. if k 1+ XREF-REC XREF-NAME$ type cr then ;" GE-SRC-LINE
   s" : OI-DX. ( -- ) s~ OI-DX:OI-MAKER-WORD~ XREF-FIND-INDEX OI-CLAUSE. ;" QLINE
   s" package OI-DH public : OI-HELD;does ( -- n ) 3 ; private : OI-HELD ( n -- ) create , does> ( -- n ) @ ; ;package" GE-SRC-LINE
   s" : OI-HERE ( -- n ) here BYTE-VIEW data-base BYTE-VIEW - ;" GE-SRC-LINE
   s" variable OI-AT" GE-SRC-LINE
   s" : OI-MARK ( -- ) OI-HERE OI-AT ! ;" GE-SRC-LINE
   s" : OI-CSIG$ ( -- ptr u8 n ) data-base TCSIG-A-CELL CELL / ptr-field @ TCSIG-U-CELL OI-CELL@ ;" GE-SRC-LINE
   s" : OI-DOES. ( -- ) OI-BODY. DOESB-CELL OI-CELL@ . OI-CSIG$ type cr OI-HERE OI-AT @ - . ;" GE-SRC-LINE
   s" : OI-ROOM ( -- n ) DATA-SIZE PROF-CNT-BYTES - OI-HERE - ;" GE-SRC-LINE
   s" TRUSTED: OI-ALIAS ( ptr u8 n -- ) ndict@ 1- get-current alias-record ;" GE-SRC-LINE
   s" : OI-OR@ ( n n -- n ) OI-CELL@ or ;" GE-SRC-LINE
   s" : OI-ENDED. ( -- ) PEND-CELL OI-CELL@ TSIG-A-CELL OI-OR@ TSIG-U-CELL OI-OR@" GE-SRC-LINE
   s"   TCSIG-A-CELL OI-OR@ TCSIG-U-CELL OI-OR@ DOESB-CELL OI-OR@ CLAUSE-CELL OI-OR@ TRUSTED-CELL OI-OR@" GE-SRC-LINE
   s"   NCOMP-DISPATCH:DEF-TIER-CELL OI-OR@ . ;" GE-SRC-LINE
   s" TYPED-VARIABLE OI-SHOWN [ -- ]" GE-SRC-LINE
   s" : OI-SHOW ( -- ) OI-SHOWN @ execute ; immediate  s~ OI-SHOW~ 0 parse-imm" QLINE
   s" : OI-CR ( -- ) cr ;" GE-SRC-LINE
   s" CAST: OI-XT>N ( [ R -- S ] -- n )" GE-SRC-LINE
   s" : OI-ARM ( [ -- ] -- ) OI-XT>N data-base EXIT-HOOK-CELL + ! ;" GE-SRC-LINE
   s" TYPED-VARIABLE OI-PARKED ptr u8" GE-SRC-LINE
   s" variable OI-PARKED-U" GE-SRC-LINE
   s" : OI-PARK ( ptr u8 n -- ) OI-PARKED-U ! OI-PARKED ! ;" GE-SRC-LINE
   s" : OI-INC-PARKED ( -- ) OI-PARKED @ OI-PARKED-U @ included ;" GE-SRC-LINE
   s" TRUSTED: OI-EVAL-PARKED ( -- ) OI-PARKED @ OI-PARKED-U @ evaluate ;" GE-SRC-LINE
   s" : OI-EVC-PARKED ( -- ) OI-PARKED @ OI-PARKED-U @ evaluate-closed ;" GE-SRC-LINE
   s" oi-prelude.f" PRELUDE-BUF GT-PATH PRELUDE-U !
   PRELUDE$ SRC>FILE ;

\ A second prelude for the cases that need a live task: OI-SPIN starts one that
\ spins until the process ends. A task this prelude started would end the load
\ of test/outer-loop-on.f, whose `package` mutates the dictionary, so each such
\ case starts its own.
: SPIN-PRELUDE ( -- )
   GE-SRC-RESET
   s" require src/lib/task.f" GE-SRC-LINE
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

\ The engine's run, kept while the Habu loop's runs. A run that ran out of time
\ fails under label with its capture (GE-RC@).
: KEEP ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GE-RC@ WANT-RC !
   GT-OUT$ {: oa:ptr ou:n :}  oa WANT-OUT ou BYTE-COPY  ou WANT-OUT-U !
   GT-ERR$ {: ea:ptr eu:n :}  ea WANT-ERR eu BYTE-COPY  eu WANT-ERR-U ! ;

: MISMATCH ( ptr u8 n ptr u8 n -- ) {: why:ptr whyu:n label:ptr labelu:n :}
   why whyu type cr
   s" engine rc: " type WANT-RC @ FMT:.INT cr
   s" engine stdout:" type cr WANT-OUT$ type
   s" engine stderr:" type cr WANT-ERR$ type
   label labelu GE-FAIL ;

: SAME ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GE-RC@ WANT-RC @ <> if s" rc differs from the engine's" label labelu MISMATCH then
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
   false RUN name nameu KEEP
   true RUN
   name nameu SAME
   1 CASES +! ;

\ The source buffer as the case file named, run by the Habu loop alone.
: HABU ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu CASE-BUF GT-PATH CASE-U !
   CASE$ SRC>FILE
   true RUN ;

: LINE-CASE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   GE-SRC-RESET
   src srcu GE-SRC-LINE
   name nameu BOTH ;

\ A located refusal's message and the file and line it names. The path before
\ the file name is the temporary root, which may resolve through a link.
: ERR-AT ( ptr u8 n ptr u8 n -- ) {: msg:ptr msgu:n at:ptr atu:n :}
   msg msgu CASE$ GE-EXPECT-ERR-HAS
   at atu CASE$ GE-EXPECT-ERR-HAS ;

\ ---- the cases -----------------------------------------------------------------------
\ The prelude defines with keywords the Habu loop does not read (`SUMTYPE`), so
\ it loads before the switch. A case holds only numbers, comments, the literal
\ keywords (`s"`, `c"`, `."`, their escaped forms, `char`, `'`), the package
\ keywords (`package`, `public`, `private`, `;package`, `using`, `;using`,
\ `export`), words, `create`, `variable`, `constant`, `cast:`, `immediate`, and
\ definitions (`:`, `kernel:`, `trusted:`) that end at `;` or refuse, at the
\ latest where the input ends inside them; other keywords go through evaluate,
\ which the engine's loop reads. Tier-0 cases are the Habu loop's alone: the
\ engine's loop compiles their definitions and the Habu loop refuses them.
: NUMBERS ( -- )
   GE-SRC-RESET
   s" require src/lib/ieee754.f" GE-SRC-LINE
   s" 1 2 + . $2A . -7 . -$10 . 1.5 IEEE754:F64>BITS . -2.25 IEEE754:F64>BITS . 0 . depth ." GE-SRC-LINE
   s" oi-numbers.f" BOTH
   s" numbers" GE-EXPECT-OK
   S\" 42\n-7\n-16\n" s" numbers" GE-EXPECT-OUT-HAS ;

\ A backslash comment, and a `(` comment within a line and across one.
: COMMENTS ( -- )
   GE-SRC-RESET
   s" 1 " GE-SRC+ BACKSLASH GE-SRC-C s"  2 3" GE-SRC-LINE
   s" ( 4 ) 5 ( 6" GE-SRC-LINE
   s" 7 ) . . depth ." GE-SRC+
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
   s" E-UNDEFINED: (x) at " S\" oi-not-a-comment.f:1\n" ERR-AT ;

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
   s" E-UNDEFINED: OI-NOPE at " S\" oi-undefined.f:1\n" ERR-AT ;

\ Out of range, a number's spelling is undefined though a word has that name.
: OUT-OF-RANGE ( -- )
   GE-SRC-RESET
   s" 18446744073709551616" GE-SRC-LINE
   s" oi-range.f" BOTH
   70 s" out of range" GE-EXPECT-RC
   s" E-UNDEFINED: 18446744073709551616 at " S\" oi-range.f:1\n" ERR-AT ;

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


\ A top-level step is judged as a body call is. xref-retarget is an engine
\ primitive whose one row is package NPUB's private one, so the step sees no
\ row: it is refused by name with the checker's rc, and the word never runs.
: ROWLESS-STEP ( -- )
   GE-SRC-RESET
   s" 1 . 0 0 0 xref-retarget 2 ." GE-SRC-LINE
   s" oi-rowless-step.f" BOTH
   70 s" rowless step" GE-EXPECT-RC
   S\" 1\n" s" rowless step" GE-EXPECT-OUT
   s" undefined word 'xref-retarget'" CASE$ GE-EXPECT-ERR-HAS ;


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
\ resumes after it with the cell below the include intact. The included file
\ prints its value, which it may not leave (NESTED-CLOSED).
: NESTED ( -- )
   GE-SRC-RESET
   s" 3 ." GE-SRC-LINE
   s" oi-nested.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   GE-SRC-RESET
   s" 1 include oi-nested.f 2 . . depth ." GE-SRC-LINE
   s" oi-include.f" BOTH
   s" nested include" GE-EXPECT-OK
   S\" 3\n2\n1\n0\n" s" nested include" GE-EXPECT-OUT ;

\ An included file is a closed program under either loop: its floor is the
\ includer's depth, so its `drop` is refused before it takes the cell below
\ the include, and a value it leaves is refused E-EVAL-RESIDUE (-3804). Each
\ refusal is caught, and the includer's 7 is intact.
: NESTED-CLOSED ( -- )
   GE-SRC-RESET
   s" drop" GE-SRC-LINE
   s" oi-nested-drop.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   GE-SRC-RESET
   s" 7 s~ oi-nested-drop.f~ OI-PARK ' OI-INC-PARKED catch . . depth ." QLINE
   s" oi-include-drop.f" BOTH
   s" include floor" GE-EXPECT-OK
   S\" 70\n7\n0\n" s" include floor" GE-EXPECT-OUT
   S\" hb: interpret stack underdepth: drop\n" s" include floor" GE-EXPECT-ERR
   GE-SRC-RESET
   s" 3" GE-SRC-LINE
   s" oi-nested-left.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   GE-SRC-RESET
   s" 7 s~ oi-nested-left.f~ OI-PARK ' OI-INC-PARKED catch . . depth ." QLINE
   s" oi-include-left.f" BOTH
   s" include residue" GE-EXPECT-OK
   S\" -3804\n7\n0\n" s" include residue" GE-EXPECT-OUT ;

\ A file that fails takes back the require rows it added under either loop:
\ its nested require reloads when the includer requires it again (1 twice).
: REQUIRE-ROLLBACK ( -- )
   GE-SRC-RESET
   s" 1 ." GE-SRC-LINE
   s" oi-rr-nested.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   GE-SRC-RESET
   s" require oi-rr-nested.f" GE-SRC-LINE
   s" OI-RR-NOSUCH" GE-SRC-LINE
   s" oi-rr-outer.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   GE-SRC-RESET
   s" s~ oi-rr-outer.f~ OI-PARK ' OI-INC-PARKED catch ." QLINE
   s" s~ oi-rr-nested.f~ required 9 ." QLINE
   s" oi-require-rollback.f" BOTH
   s" require rollback" GE-EXPECT-OK
   S\" 1\n70\n1\n9\n" s" require rollback" GE-EXPECT-OUT
   s" E-UNDEFINED: OI-RR-NOSUCH at " S\" oi-rr-outer.f:2\n" ERR-AT ;

\ Nested plain evaluate buffers resume their callers' inputs. A caught inner
\ throw leaves both readers active; a throw caught by the outer buffer rolls
\ back the definition the inner buffer published; one that escapes both
\ reaches the file's catch without running either following token.
: EVALUATE-NESTED ( -- )
   GE-SRC-RESET
   s" 1 set-tier 0 set-check TRUSTED: OI-AT-INNER ( -- ) s~ 3 depth .~ evaluate ;" QLINE
   s" TRUSTED: OI-AT-OUTER ( -- ) s~ 2 OI-AT-INNER 4 . . . .~ evaluate ;" QLINE
   s" 1 OI-AT-OUTER depth . 5 ." GE-SRC-LINE
   s" oi-evaluate-nested.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 3\n4\n3\n2\n1\n0\n5\n" CASE$ GE-EXPECT-OUT
   GE-SRC-RESET
   s" 1 set-tier : OI-T42 ( -- ) 42 throw ;" GE-SRC-LINE
   s" TRUSTED: OI-AT-CATCH ( -- ) s~ ' OI-T42 catch . 6 .~ evaluate ;" QLINE
   s" TRUSTED: OI-AT-OUTER ( -- ) s~ OI-AT-CATCH 7 .~ evaluate ;" QLINE
   s" OI-AT-OUTER 8 ." GE-SRC-LINE
   s" oi-evaluate-inner-catch.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 42\n6\n7\n8\n" CASE$ GE-EXPECT-OUT
   GE-SRC-RESET
   s" 1 set-tier : OI-T42 ( -- ) 42 throw ;" GE-SRC-LINE
   s" TRUSTED: OI-AT-THROW ( -- ) s~ : OI-GONE ( -- n ) 1 ; OI-T42 9 .~ evaluate ;" QLINE
   s" s~ OI-AT-THROW~ OI-PARK ' OI-EVAL-PARKED catch ." QLINE
   s" s~ OI-GONE~ OI-PARK ' OI-EVAL-PARKED catch . 10 ." QLINE
   s" s~ 11 .~ evaluate" QLINE
   s" oi-evaluate-outer-catch.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 42\n70\n10\n11\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: OI-GONE at " S\" oi-evaluate-outer-catch.f:1\n" ERR-AT
   GE-SRC-RESET
   s" 1 set-tier : OI-T42 ( -- ) 42 throw ;" GE-SRC-LINE
   s" TRUSTED: OI-AT-THROW ( -- ) s~ OI-T42 12 .~ evaluate ;" QLINE
   s" s~ OI-AT-THROW 13 .~ OI-PARK ' OI-EVAL-PARKED catch . 14 ." QLINE
   s" oi-evaluate-past-both.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 42\n14\n" CASE$ GE-EXPECT-OUT ;

\ A checked word enters evaluate-closed under a plain evaluate. The closed
\ text can print, but cannot consume its caller's 7 or leave its own 3.
\ A rejected open definition is rolled back, allowing the name to be reused.
: EVALUATE-CLOSED ( -- )
   GE-SRC-RESET
   s" 1 set-tier : OI-CHK-EVC ( -- ) s~ 2 .~ evaluate-closed ;" QLINE
   s" s~ OI-CHK-EVC 1 .~ evaluate 3 ." QLINE
   s" oi-evaluate-closed-nested.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 2\n1\n3\n" CASE$ GE-EXPECT-OUT
   GE-SRC-RESET
   s" 7 s~ drop~ OI-PARK ' OI-EVC-PARKED catch . . depth ." QLINE
   s" oi-evaluate-closed-floor.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 70\n7\n0\n" CASE$ GE-EXPECT-OUT
   S\" hb: interpret stack underdepth: drop\n" CASE$ GE-EXPECT-ERR
   GE-SRC-RESET
   s" 7 s~ 3~ OI-PARK ' OI-EVC-PARKED catch . . depth ." QLINE
   s" oi-evaluate-closed-residue.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" -3804\n7\n0\n" CASE$ GE-EXPECT-OUT
   GE-SRC-RESET
   s" 1 set-tier s~ : OI-OPEN ( -- n ) 42~ OI-PARK ' OI-EVC-PARKED catch ." QLINE
   s" : OI-OPEN ( -- n ) 5 ; OI-OPEN ." GE-SRC-LINE
   s" oi-evaluate-closed-head.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 74\n5\n" CASE$ GE-EXPECT-OUT
   s" hb: source ended inside definition: OI-OPEN at " CASE$ GE-EXPECT-ERR-HAS ;

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

\ char pushes its operand's first byte. ' pushes an xt with no depth gate, and
\ an unsealed qualifier resolves.
: TICK-AND-CHAR ( -- )
   GE-SRC-RESET
   s" char Abc . CHAR z ." GE-SRC-LINE
   s" 1 2 ' OI-TWO execute depth ." GE-SRC-LINE
   s" ' OI-TWO drop depth ." GE-SRC-LINE
   s" ' OI-PKG:OI-SEVEN execute ." GE-SRC-LINE
   s" oi-tick-char.f" BOTH
   s" tick and char" GE-EXPECT-OK
   S\" 65\n122\n0\n0\n7\n" s" tick and char" GE-EXPECT-OUT ;

\ ' of a name no word has is undefined, as the name run is, and nothing after it
\ runs. An edge colon leaves the name bare, so no seal guard answers it.
\ Under evaluate the refusal is the catchable reject, which the engine's loop
\ reads; in a file the case includes under catch it is the reading loop's own:
\ the file stops at the tick, and the includer gets 70 above its 3 and the
\ path's two cells and reads on.
: TICK-UNDEFINED ( -- )
   GE-SRC-RESET
   s" 1 . ' OI-NOPE 2 ." GE-SRC-LINE
   s" oi-tick-undefined.f" BOTH
   70 s" tick undefined" GE-EXPECT-RC
   S\" 1\n" s" tick undefined" GE-EXPECT-OUT
   s" E-UNDEFINED: OI-NOPE at " S\" oi-tick-undefined.f:1\n" ERR-AT
   s" ' engine-error:" s" oi-tick-edge-colon.f" LINE-CASE
   70 s" tick edge colon" GE-EXPECT-RC
   s" E-UNDEFINED: engine-error: at " S\" oi-tick-edge-colon.f:1\n" ERR-AT
   GE-SRC-RESET
   s" s~ ' OI-NOPE~ OI-PARK ' OI-EVAL-PARKED catch . depth ." QLINE
   s" oi-tick-undefined-caught.f" BOTH
   s" tick undefined caught" GE-EXPECT-OK
   S\" 70\n0\n" s" tick undefined caught" GE-EXPECT-OUT
   s" E-UNDEFINED: OI-NOPE at " S\" oi-tick-undefined-caught.f:1\n" ERR-AT
   GE-SRC-RESET
   s" 1 . ' OI-NOPE 2 ." GE-SRC-LINE
   s" oi-nested-tick.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   GE-SRC-RESET
   s" 3 s~ oi-nested-tick.f~ OI-PARK ' OI-INC-PARKED catch . depth . 4 . drop" QLINE
   s" oi-tick-undefined-included.f" BOTH
   s" tick undefined included" GE-EXPECT-OK
   S\" 1\n70\n1\n4\n" s" tick undefined included" GE-EXPECT-OUT
   s" E-UNDEFINED: OI-NOPE at " S\" oi-nested-tick.f:1\n" ERR-AT ;

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
   msg msgu at atu ERR-AT ;

\ ARM can put CP eight bytes below its code ceiling to test name allocation.
\ Intel seals that boundary, so the same source refuses the CP write first.
: CODE-DIED-AT ( n ptr u8 n ptr u8 n -- )
   HB-TARGET-LINUX-X86-64? if
      2drop 2drop drop
      83 CASE$ GE-EXPECT-RC
      s" " CASE$ GE-EXPECT-OUT
      s" " CASE$ GE-EXPECT-ERR
      exit
   then
   DIED-AT ;

\ A `(` comment the input ends inside is refused at the `(`'s line.
: COMMENT-AT-END ( -- )
   GE-SRC-RESET
   s" 1 ." GE-SRC-LINE
   s" ( 2" GE-SRC-LINE
   s" 3 ." GE-SRC+
   s" oi-comment-end.f" BOTH
   S\" 1\n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside a ( comment at " S\" oi-comment-end.f:2\n" DIED-AT ;

\ A fail-closed exit that names a token: stderr is that token and a newline.
: ERR-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   SB-RESET  a u SB-APPEND  GE-SB-LF
   SB$ CASE$ GE-EXPECT-ERR ;

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
\ whole diagnostic line. Every package the engine seals is asked once, in
\ assorted case, and a second colon does not spare the token. The exit is
\ fail-closed: the exit hook the program armed, `cr` (in src/habu/layout.f
\ EXIT-HOOK-CELL), does not run.
: SEALED ( ptr u8 n -- ) {: t:ptr u:n :}
   GE-SRC-RESET
   s" ' OI-CR OI-ARM" GE-SRC-LINE
   s" ' " GE-SRC+ t u GE-SRC-LINE
   s" oi-sealed.f" BOTH
   ENGINE-ERROR:SEAL-PACKAGE t u GE-EXPECT-RC
   t u ERR-LINE ;

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

\ The engine's tick keeps trusted C2 call primitives behind the checker owner,
\ even though an ordinary source interpreter can resolve their dictionary rows.
\ A different word with the same tail remains an ordinary tick.
: TICK-TRUSTED ( -- )
   s" ' c2-invoke drop" s" oi-tick-trusted.f" LINE-CASE
   70 s" tick trusted" GE-EXPECT-RC
   S\" hb: trusted-only tick: c2-invoke\n" s" tick trusted" GE-EXPECT-ERR
   GE-SRC-RESET
   s" ' OI-C2:c2-invoke execute ." GE-SRC-LINE
   s" oi-tick-other-c2.f" BOTH
   s" tick other c2" GE-EXPECT-OK
   S\" 4\n" s" tick other c2" GE-EXPECT-OUT ;

\ The complete C2 image owns this exact scope entry. Its dictionary row is
\ internal as well, so the earlier interpret gate supplies the diagnostic.
: TICK-C2-SCOPE ( -- )
   s" ' C2-MEM:WITH-MUT drop" s" oi-tick-c2-scope.f" LINE-CASE
   70 s" tick c2 scope" GE-EXPECT-RC
   S\" hb: internal engine word: C2-MEM:WITH-MUT\n" s" tick c2 scope" GE-EXPECT-ERR ;

\ The literal events, test/top-row-hook-test.f's window: each logs its class,
\ flags and token. A string's token is its keyword, a char's and a tick's the
\ operand, and a tick's flags are the word's. `."` and `.\"` log nothing.
: LITERAL-HOOK ( -- )
   GE-SRC-RESET
   s" OI-HOOK-ON s~ hi~ 2drop c~ hey~ drop char A drop ' OI-TWO drop" QLINE
   s" S\~ e~ 2drop .~ x~ .\~ y~ 0 set-top-check" QLINE
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
   s" E-UNDEFINED: OI-TWIN at " S\" oi-using.f:1\n" ERR-AT ;

\ A using opened in a package ends at its `;package`.
: USING-IN-PACKAGE ( -- )
   s" package OI-T using OI-FXA OI-TWIN . ;package OI-TWIN" s" oi-using-in-package.f" LINE-CASE
   70 s" using in a package" GE-EXPECT-RC
   S\" 1\n" s" using in a package" GE-EXPECT-OUT
   s" E-UNDEFINED: OI-TWIN at " S\" oi-using-in-package.f:1\n" ERR-AT ;

\ Usings are file-local: one a nested include opens ends with it, and so does
\ one open when the include throws.
: USING-FILE-LOCAL ( -- )
   GE-SRC-RESET
   s" using OI-FXA" GE-SRC-LINE
   s" oi-nested-using.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   s" include oi-nested-using.f OI-TWIN" s" oi-include-using.f" LINE-CASE
   70 s" using ends with its file" GE-EXPECT-RC
   s" E-UNDEFINED: OI-TWIN at " S\" oi-include-using.f:1\n" ERR-AT
   GE-SRC-RESET
   s" using OI-FXA OI-NOPE" GE-SRC-LINE
   s" oi-nested-using-throw.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   GE-SRC-RESET
   s" s~ oi-nested-using-throw.f~ OI-PARK ' OI-INC-PARKED catch . OI-TWIN" QLINE
   s" oi-include-using-throw.f" BOTH
   70 s" using ends with a throw" GE-EXPECT-RC
   S\" 70\n" s" using ends with a throw" GE-EXPECT-OUT
   s" E-UNDEFINED: OI-NOPE at " S\" oi-nested-using-throw.f:1\nE-UNDEFINED: OI-TWIN at " ERR-AT
   S\" oi-include-using-throw.f:1\n" CASE$ GE-EXPECT-ERR-HAS ;

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
\ the package's name. The refusal it renders exits 70, as every refusal does.
: USING-SHADOW ( -- )
   GE-SRC-RESET
   s" using OI-PKG s~ : OI-SH ( -- n ) OI-SEVEN ;~ evaluate" QLINE
   s" oi-using-shadow.f" BOTH
   70 s" using shadow" GE-EXPECT-RC
   s" E-USING-SHADOW-GLOBAL" s" using shadow" GE-EXPECT-ERR-HAS ;

\ The interpreter refuses the same tail by name, as a word and as a tick's
\ operand (the global answers 8, OI-PKG's public 7). Under `evaluate` the
\ refusal is a throw the caller catches, and the qualified name and the read
\ after `;using` are unchanged.
: TOP-SHADOW ( -- )
   s" using OI-PKG OI-SEVEN ." s" oi-top-shadow.f" LINE-CASE
   ENGINE-ERROR:USING-SHADOW-GLOBAL
   s" hb: bare word a global and a used package both export: OI-SEVEN at "
   S\" oi-top-shadow.f:1\n" DIED-AT
   s" using OI-PKG ' OI-SEVEN" s" oi-tick-shadow.f" LINE-CASE
   ENGINE-ERROR:USING-SHADOW-GLOBAL
   s" hb: bare word a global and a used package both export: OI-SEVEN at "
   S\" oi-tick-shadow.f:1\n" DIED-AT
   GE-SRC-RESET
   s" using OI-PKG s~ OI-SEVEN~ OI-PARK ' OI-EVAL-PARKED catch . OI-PKG:OI-SEVEN . ;using OI-SEVEN ." QLINE
   s" oi-shadow-caught.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 105\n7\n8\n" CASE$ GE-EXPECT-OUT
   s" hb: bare word a global and a used package both export: OI-SEVEN at " CASE$ GE-EXPECT-ERR-HAS ;

\ At tier 1 a package defines a tail that the global OI-SEVEN and the used
\ OI-PKG:OI-SEVEN both export: the name a definition makes is no reference, so
\ the using refuses neither a colon definition (15) nor a definer (6). A bare
\ reference to the tail in a body is still refused, the definition's own name
\ included (7141), and a private twin of another width still refuses the
\ public (7145): there the tail binds the private word.
: USING-OWN-NAME ( -- )
   GE-SRC-RESET
   s" 1 set-tier package OI-Q using OI-PKG : OI-SEVEN ( n -- n ) 3 * ; 5 OI-SEVEN . ;using ;package" GE-SRC-LINE
   s" package OI-QD using OI-PKG : OI-SEVEN ( n -- ) create , does> ( -- n ) @ ;" GE-SRC-LINE
   s" 6 OI-SEVEN OI-SIX OI-SIX . ;using ;package" GE-SRC-LINE
   s" package OI-QR using OI-PKG s~ : OI-SEVEN ( -- n ) OI-SEVEN ;~ OI-PARK ' OI-EVAL-PARKED catch . ;using ;package" QLINE
   s" package OI-QA using OI-PKG : OI-SEVEN ( n n -- n ) + ; public" GE-SRC-LINE
   s" s~ : OI-SEVEN ( n -- n ) 1 + ;~ OI-PARK ' OI-EVAL-PARKED catch . ;using ;package" QLINE
   s" oi-using-own-name.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 15\n6\n7141\n7145\n" CASE$ GE-EXPECT-OUT
   s" E-USING-SHADOW-GLOBAL habu: bare 'OI-SEVEN'" CASE$ GE-EXPECT-ERR-HAS
   s" E-SHADOWED-ARITY habu: public 'oi-qa:oi-seven'" CASE$ GE-EXPECT-ERR-HAS ;

\ `;package` restores the using depth its package opened at, so inside a
\ package `;using` closes only a using the package opened. One opened before
\ `package` is refused by name: closed, it came back at `;package` and the
\ definition after it read OI-PKG:OI-SEVEN against the global OI-SEVEN
\ (E-USING-SHADOW-GLOBAL). A package's own using, and one opened before
\ `package` and closed after `;package`, are unchanged. In an included file the
\ refusal is a throw the includer catches, and the file's using ends with it.
\ A package an included file leaves open keeps none of the file's usings: the
\ includer's own `;using` in it closes, and `;package` reopens nothing.
: USING-ACROSS-PACKAGE ( -- )
   GE-SRC-RESET
   s" using OI-PKG package OI-T ;using ;package s~ : OI-SH ( -- n ) OI-SEVEN ;~ evaluate" QLINE
   s" oi-using-outer.f" BOTH
   ENGINE-ERROR:USING-OUTER s" hb: ;using would close a using opened outside the package at " S\" oi-using-outer.f:1\n" DIED-AT
   s" package OI-T using OI-FXA OI-TWIN . ;using OI-TWIN" s" oi-using-own.f" LINE-CASE
   70 CASE$ GE-EXPECT-RC
   S\" 1\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: OI-TWIN at " S\" oi-using-own.f:1\n" ERR-AT
   s" using OI-FXA package OI-T OI-TWIN . ;package OI-TWIN . ;using OI-TWIN" s" oi-using-around.f" LINE-CASE
   70 CASE$ GE-EXPECT-RC
   S\" 1\n1\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: OI-TWIN at " S\" oi-using-around.f:1\n" ERR-AT
   GE-SRC-RESET
   s" using OI-FXA package OI-T ;using" GE-SRC-LINE
   s" oi-nested-using-outer.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   GE-SRC-RESET
   s" s~ oi-nested-using-outer.f~ OI-PARK ' OI-INC-PARKED catch . OI-TWIN" QLINE
   s" oi-include-using-outer.f" BOTH
   70 CASE$ GE-EXPECT-RC
   S\" 104\n" CASE$ GE-EXPECT-OUT
   s" hb: ;using would close a using opened outside the package at " CASE$ GE-EXPECT-ERR-HAS
   S\" oi-nested-using-outer.f:1\nE-UNDEFINED: OI-TWIN at " S\" oi-include-using-outer.f:1\n" ERR-AT
   GE-SRC-RESET
   s" using OI-FXA package OI-T" GE-SRC-LINE
   s" oi-nested-package-open.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   s" include oi-nested-package-open.f using OI-FXB OI-TWIN . ;using ;package OI-TWIN ." s" oi-include-package-open.f" LINE-CASE
   70 CASE$ GE-EXPECT-RC
   S\" 2\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: OI-TWIN at " S\" oi-include-package-open.f:1\n" ERR-AT ;

\ ---- the package scope after a caught throw ----------------------------------------
\ A throw puts back the package scope its buffer entered with on both loops:
\ the open package, the using depth and the package's using floor, and the
\ checker's package mirror follows before the handler runs. OI-VERIFY:SCOPE
\ replays a source through the verifier, which refuses E-PKG-CONTEXT (7136)
\ when the mirror and the engine name different packages; OI-VERIFY:FLOOR
\ replays a using the package opened and closes it, which the replay judges
\ against the engine's floor. Both are defined through `evaluate` because the
\ Habu loop cannot end a definition, and the verifier loads in the second
\ prelude slot, before the loop switches, for the same reason.
: PKG-VERIFY-PRELUDE ( -- )
   GE-SRC-RESET
   s" require src/habu/verify-source.f" GE-SRC-LINE
   s" oi-verify-prelude.f" SPIN-BUF GT-PATH SPIN-U !
   SPIN$ SRC>FILE ;

: PKG-VS-LINE ( -- )
   s" package OI-VERIFY public" GE-SRC-LINE
   s" S\~ : SCOPE ( -- n ) [: s\~ 1 drop\~ VERIFY:SOURCE-BUF ;] catch ;~ evaluate" QLINE
   s" S\~ : FLOOR ( -- n ) [: s\~ using OI-FXA ;using\~ VERIFY:SOURCE-BUF ;] catch ;~ evaluate" QLINE
   s" ;package" GE-SRC-LINE ;

: PKG-NESTED ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   GE-SRC-RESET
   src srcu GE-SRC-LINE
   name nameu NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE ;

\ Each case prints the include's 70 and the verifier's 0, then shows the
\ restored scope: a file that closed OI-Q left it open, one that opened OI-T
\ left none, and an engine evaluate inside the Habu loop recovers the same
\ way. The floor case closes OI-P, opens a using and OI-R, and throws: OI-P's
\ own using closes, and `;package` reopens nothing, so OI-TWIN is undefined.
: PACKAGE-RECOVERY ( -- )
   s" package OI-Q" s" oi-nested-pkg-open.f" PKG-NESTED
   s" ;package OI-NOPE" s" oi-nested-pkg-close-throw.f" PKG-NESTED
   s" package OI-T OI-NOPE" s" oi-nested-pkg-throw.f" PKG-NESTED
   s" ;package using OI-FXB package OI-R OI-NOPE" s" oi-nested-floor-throw.f" PKG-NESTED
   PKG-VERIFY-PRELUDE
   GE-SRC-RESET
   PKG-VS-LINE
   s" include oi-nested-pkg-open.f" GE-SRC-LINE
   s" s~ oi-nested-pkg-close-throw.f~ OI-PARK ' OI-INC-PARKED catch . OI-VERIFY:SCOPE . ;package 5 ." QLINE
   s" oi-pkg-reopen.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 70\n0\n5\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: OI-NOPE at " S\" oi-nested-pkg-close-throw.f:1\n" ERR-AT
   GE-SRC-RESET
   PKG-VS-LINE
   s" s~ oi-nested-pkg-throw.f~ OI-PARK ' OI-INC-PARKED catch . OI-VERIFY:SCOPE . package OI-V ;package 5 ." QLINE
   s" oi-pkg-reclose.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 70\n0\n5\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: OI-NOPE at " S\" oi-nested-pkg-throw.f:1\n" ERR-AT
   GE-SRC-RESET
   PKG-VS-LINE
   s" s~ package OI-Q~ evaluate s~ ;package OI-NOPE~ OI-PARK ' OI-EVAL-PARKED catch . OI-VERIFY:SCOPE . ;package 5 ." QLINE
   s" oi-pkg-evaluate.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 70\n0\n5\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: OI-NOPE at " S\" oi-pkg-evaluate.f:1\n" ERR-AT
   GE-SRC-RESET
   PKG-VS-LINE
   s" package OI-P" GE-SRC-LINE
   s" s~ oi-nested-floor-throw.f~ OI-PARK ' OI-INC-PARKED catch . OI-VERIFY:FLOOR . using OI-FXA ;using ;package OI-TWIN" QLINE
   s" oi-pkg-floor.f" BOTH
   70 CASE$ GE-EXPECT-RC
   S\" 70\n0\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: OI-NOPE at " S\" oi-nested-floor-throw.f:1\nE-UNDEFINED: OI-TWIN at " ERR-AT
   S\" oi-pkg-floor.f:6\n" CASE$ GE-EXPECT-ERR-HAS
   0 SPIN-U ! ;

\ A throw puts the package scope back with a task live, as the engine's
\ recovery does: the file closes OI-T, starts a task and throws 42, and the
\ includer catches 42 with OI-T's private word in scope again.
: PACKAGE-RECOVERY-LIVE ( -- )
   s" ;package OI-SPIN 42 throw" s" oi-nested-live-throw.f" PKG-NESTED
   SPIN-PRELUDE
   GE-SRC-RESET
   s" package OI-T s~ : OI-TP ( -- n ) 6 ;~ evaluate" QLINE
   s" s~ oi-nested-live-throw.f~ OI-PARK ' OI-INC-PARKED catch . OI-TP ." QLINE
   s" oi-pkg-live-throw.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 42\n6\n" CASE$ GE-EXPECT-OUT
   0 SPIN-U ! ;

\ ---- a file closes only the usings it opens -------------------------------------------
\ A load file is a using scope on both loops. A file's `;using` that would close
\ a using its includer opened is refused by name, and the includer that catches
\ it still has OI-FXA. A file that closes its includer's package ends the
\ usings opened in it, and the includer gets back the depth `;package`
\ restored: the file's OI-FXB ends with the file and OI-FXA, opened before the
\ package, stays.
: USING-INCLUDER ( -- )
   s" ;using using OI-FXB" s" oi-nested-using-includer.f" PKG-NESTED
   GE-SRC-RESET
   s" using OI-FXA s~ oi-nested-using-includer.f~ OI-PARK ' OI-INC-PARKED catch . OI-TWIN ." QLINE
   s" oi-using-includer.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 104\n1\n" CASE$ GE-EXPECT-OUT
   s" hb: ;using would close a using opened outside the file at " CASE$ GE-EXPECT-ERR-HAS
   S\" oi-nested-using-includer.f:1\n" CASE$ GE-EXPECT-ERR-HAS
   s" ;package using OI-FXB" s" oi-nested-close-includer.f" PKG-NESTED
   s" using OI-FXA package OI-P using OI-PKG include oi-nested-close-includer.f OI-TWIN . ;using OI-TWIN"
   s" oi-close-includer.f" LINE-CASE
   70 CASE$ GE-EXPECT-RC
   S\" 1\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: OI-TWIN at " S\" oi-close-includer.f:1\n" ERR-AT ;

\ A throw puts back the includer's used publics on both loops. The file closes
\ OI-P, opens OI-FXN (whose OI-TWIN takes an input) in OI-FXA's slot and
\ throws; both loops then read OI-FXA's OI-TWIN.
: USING-SLOT-THROW ( -- )
   s" ;package using OI-FXN OI-NOPE" s" oi-nested-slot-throw.f" PKG-NESTED
   GE-SRC-RESET
   s" s~ package OI-FXN public : OI-TWIN ( n -- n ) 100 + ; ;package~ evaluate" QLINE
   s" package OI-P using OI-FXA s~ oi-nested-slot-throw.f~ OI-PARK ' OI-INC-PARKED catch . OI-TWIN ." QLINE
   s" s~ : OI-SLOT ( -- n ) OI-TWIN ;~ evaluate OI-SLOT . ;package" QLINE
   s" oi-using-slot-throw.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 70\n1\n1\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: OI-NOPE at " S\" oi-nested-slot-throw.f:1\n" ERR-AT ;

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

\ An internal word has no checker-known effect, and an alias of one would not
\ carry the DNAME-INT mark that keeps it behind a TRUSTED: boundary, so both
\ loops refuse it as their interpret gates do.
: EXPORT-INTERNAL ( -- )
   GE-SRC-RESET
   s" package OI-EX public export DEFER-UNSET" GE-SRC-LINE
   s" oi-export-internal.f" BOTH
   70 s" export internal" GE-EXPECT-RC
   S\" hb: internal engine word: DEFER-UNSET\n" s" export internal" GE-EXPECT-ERR ;

\ A does> definer's export carries the definer's clause into the slot above
\ its own, under the clause's name, which a name past DNAME-INL bytes puts at
\ CP. The clause's slot, its name in the current wordlist and that name's room
\ are refused as the export's own are, the dictionary refusal naming the
\ operand and the other two the clause.
: EXPORT-DEFINER ( -- )
   s" package OI-DX public export OI-MAKER-WORD ;package OI-DX." s" oi-export-definer.f" LINE-CASE
   s" export definer" GE-EXPECT-OK
   S\" 1\nOI-MAKER-WORD;does\n" s" export definer" GE-EXPECT-OUT
   s" package OI-DH public export OI-HELD" s" oi-export-clause-held.f" LINE-CASE
   78 s" duplicate definition: OI-HELD;does at " S\" oi-export-clause-held.f:1\n" DIED-AT
   s" package OI-DX public OI-DICT-ALMOST export OI-MAKER-WORD" s" oi-export-clause-slot.f" LINE-CASE
   77 s" hb: dictionary full at: OI-MAKER-WORD at " S\" oi-export-clause-slot.f:1\n" DIED-AT
   s" package OI-DX public dbase@ REGION + $4000 - 8 - cp! export OI-MAKER-WORD"
   s" oi-export-clause-code.f" LINE-CASE
   76 s" hb: code space full at: OI-MAKER-WORD;does at " S\" oi-export-clause-code.f:1\n" CODE-DIED-AT ;

\ With the dictionary full a new package refuses, naming what DEF-TKA and
\ DEF-TKL hold, which `package` never writes: here the operand of the `export`
\ before it. A long name that would reach the code ceiling refuses too;
\ `export` names the tail it would store.
: CAPACITY ( -- )
   s" package OI-PKG public export OI-TWO ;package OI-DICT-FULL package OI-NEW" s" oi-dictionary-full.f" LINE-CASE
   77 s" hb: dictionary full at: OI-TWO at " S\" oi-dictionary-full.f:1\n" DIED-AT
   s" dbase@ REGION + $4000 - 8 - cp! package A-LONG-NAMESPACE-ROW" s" oi-package-code-full.f" LINE-CASE
   76 s" hb: code space full at: A-LONG-NAMESPACE-ROW at " S\" oi-package-code-full.f:1\n" CODE-DIED-AT
   s" package OI-EX public dbase@ REGION + $4000 - 8 - cp! export OI-FXA:OI-LONG-NAMED-TWIN" s" oi-export-code-full.f" LINE-CASE
   76 s" hb: code space full at: OI-LONG-NAMED-TWIN at " S\" oi-export-code-full.f:1\n" CODE-DIED-AT ;

\ `package` of a sealed package's name, or of a package whose public wordlist
\ is protected, ends the process, the name its whole diagnostic line; so does
\ `export` into a protected wordlist, naming its guard and the word. The exit
\ is fail-closed: the exit hook the program armed, `cr`, does not run.
: ARMED ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   GE-SRC-RESET
   s" ' OI-CR OI-ARM" GE-SRC-LINE
   src srcu GE-SRC-LINE
   name nameu BOTH
   ENGINE-ERROR:SEAL-PACKAGE CASE$ GE-EXPECT-RC ;

: PROTECTED-PACKAGES ( -- )
   s" package ENGINE-ERROR" s" oi-sealed-package.f" ARMED
   s" ENGINE-ERROR" ERR-LINE
   s" package OI-EX public get-current prot-wid-add ;package package OI-EX" s" oi-protected-package.f" ARMED
   s" OI-EX" ERR-LINE
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
\ diagnostic line, before it reads anything.
: LIVE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n kw:ptr kwu:n name:ptr nameu:n :}
   src srcu name nameu LINE-CASE
   79 CASE$ GE-EXPECT-RC
   kw kwu ERR-LINE ;

: TASK-LIVE-KEYWORDS ( -- )
   SPIN-PRELUDE
   s" OI-SPIN package OI-T" s" package" s" oi-live-package.f" LIVE
   s" package OI-T OI-SPIN public" s" public" s" oi-live-public.f" LIVE
   s" package OI-T OI-SPIN private" s" private" s" oi-live-private.f" LIVE
   s" package OI-T OI-SPIN ;package" s" ;package" s" oi-live-end-package.f" LIVE
   s" OI-SPIN using OI-FXA" s" using" s" oi-live-using.f" LIVE
   s" using OI-FXA OI-SPIN ;using" s" ;using" s" oi-live-end-using.f" LIVE
   s" package OI-T public OI-SPIN export OI-TWO" s" export" s" oi-live-export.f" LIVE
   s" OI-SPIN create OI-T" s" create" s" oi-live-create.f" LIVE
   s" OI-SPIN variable OI-T" s" variable" s" oi-live-variable.f" LIVE
   0 SPIN-U ! ;

\ ---- the definition heads -------------------------------------------------------
\ A head refuses through the engine's tails, in the engine's order, or leaves
\ its definition pending: nothing compiles until `;`. A case selects tier 1
\ first; the Habu loop refuses tier 0, whose bodies the engine's loop compiles
\ as it reads them.
: HEAD ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   GE-SRC-RESET s" 1 set-tier " GE-SRC+
   src srcu GE-SRC-LINE
   name nameu BOTH ;

\ `:` at the end of the input, the definition's name missing; `kernel:` is its
\ synonym and `trusted:` a reader keyword.
: HEAD-NO-NAME ( -- )
   GE-SRC-RESET
   s" 1 set-tier 1 ." GE-SRC-LINE
   s" :" GE-SRC+
   s" oi-colon-no-name.f" BOTH
   S\" 1\n" CASE$ GE-EXPECT-OUT
   74 s" hb: : missing definition name after  at " S\" oi-colon-no-name.f:2\n" DIED-AT
   s" KERNEL:" s" oi-kernel-no-name.f" HEAD
   74 s" hb: : missing definition name after  at " S\" oi-kernel-no-name.f:2\n" DIED-AT
   GE-SRC-RESET
   s" 1 set-tier TRUSTED:" GE-SRC-LINE
   s" oi-trusted-no-name.f" BOTH
   74 s" hb: reader keyword needs a name: trusted: at " S\" oi-trusted-no-name.f:2\n" DIED-AT ;

\ OI-INC is a neutral immediate that includes the nested file.
: INC-LINE ( -- )
   s" s\~ TRUSTED: OI-INC ( -- ) s\~ " Q+  NESTED$ GE-SRC+
   s" \~ included ; immediate~ evaluate  s~ OI-INC~ 0 parse-imm" QLINE ;

\ At tier 0 a head and a body token refuse. A body token reaches the Habu loop
\ at tier 0 only in a file included while the engine's loop holds a head open,
\ as an immediate word in an evaluated head does. An x86-64 engine boots at
\ tier 1 and has no tier 0, so there the same sources read as tier-1 heads.
: HEAD-TIER-0 ( -- )
   GE-SRC-RESET
   s" 1 ." GE-SRC-LINE
   s" : OI-T0" GE-SRC-LINE
   s" oi-tier-0-head.f" HABU
   S\" 1\n" CASE$ GE-EXPECT-OUT
   HB-TARGET-LINUX-X86-64? if
      74 s" hb: source ended inside definition: OI-T0 at " S\" oi-tier-0-head.f:2\n" DIED-AT
   else
      76 s" hb: tier 0 is not in the Habu loop: : at " S\" oi-tier-0-head.f:2\n" DIED-AT
   then
   s" 1 2" s" oi-tier-0-nested.f" PKG-NESTED
   GE-SRC-RESET
   INC-LINE
   s" s~ : OI-T0 ( -- ) OI-INC~ evaluate" QLINE
   s" oi-tier-0-body.f" HABU
   HB-TARGET-LINUX-X86-64? if
      74 s" hb: source ended inside definition: OI-T0 at " S\" oi-tier-0-body.f:1\n" DIED-AT
   else
      76 s" hb: tier 0 is not in the Habu loop: 1 at " S\" oi-tier-0-nested.f:1\n" DIED-AT
   then ;

\ A head exits 79 while a task is live, the keyword its whole diagnostic.
: HEAD-TASK-LIVE ( -- )
   SPIN-PRELUDE
   s" 1 set-tier OI-SPIN : OI-T" s" :" s" oi-live-colon.f" LIVE
   s" 1 set-tier OI-SPIN Kernel: OI-T" s" Kernel:" s" oi-live-kernel.f" LIVE
   s" 1 set-tier OI-SPIN TRUSTED: OI-T ( -- )" s" TRUSTED:" s" oi-live-trusted.f" LIVE
   0 SPIN-U ! ;

\ CP at the code ceiling and a full dictionary refuse first, naming the
\ keyword. A qualifier's new namespace row can take the last record slot, and
\ the refusal then names the token; a long name or namespace that would reach
\ the ceiling names itself.
: HEAD-CAPACITY ( -- )
   s" dbase@ REGION + $4000 - cp! : OI-T" s" oi-head-code-full.f" HEAD
   76 s" hb: code space full at: : at " S\" oi-head-code-full.f:1\n" DIED-AT
   s" OI-DICT-FULL kernel: OI-T" s" oi-head-dictionary-full.f" HEAD
   77 s" hb: dictionary full at: kernel: at " S\" oi-head-dictionary-full.f:1\n" DIED-AT
   s" 0 data-base HIDXP-CELL + ! DICT-CAP 1 - ndict! : OI-NEWNS:OI-T" s" oi-head-namespace-last.f" HEAD
   77 s" hb: dictionary full at: OI-NEWNS:OI-T at " S\" oi-head-namespace-last.f:1\n" DIED-AT
   s" dbase@ REGION + $4000 - 8 - cp! : OI-PKG:A-LONG-DEFINITION-NAME" s" oi-head-name-full.f" HEAD
   76 s" hb: code space full at: A-LONG-DEFINITION-NAME at " S\" oi-head-name-full.f:1\n" CODE-DIED-AT
   s" dbase@ REGION + $4000 - 8 - cp! : A-LONG-NAMESPACE-ROW:X" s" oi-head-namespace-full.f" HEAD
   76 s" hb: code space full at: A-LONG-NAMESPACE-ROW at " S\" oi-head-namespace-full.f:1\n" CODE-DIED-AT ;

\ A second colon in a qualified name refuses, and so does a tail the target
\ wordlist holds in any case, naming the token as spelled.
: HEAD-NAME-REFUSALS ( -- )
   s" : OI-A:B:C" s" oi-head-two-colons.f" HEAD
   75 s" OI-A:B:C at " S\" oi-head-two-colons.f:1\n" DIED-AT
   s" : oi-two" s" oi-head-duplicate.f" HEAD
   78 s" duplicate definition: oi-two at " S\" oi-head-duplicate.f:1\n" DIED-AT
   s" : oi-pkg:OI-SEVEN" s" oi-head-qualified-duplicate.f" HEAD
   78 s" duplicate definition: oi-pkg:OI-SEVEN at " S\" oi-head-qualified-duplicate.f:1\n" DIED-AT ;

\ A tail the compiler reads as a keyword in a body refuses, the tail named.
: HEAD-KEYWORD-WALL ( -- )
   s" : If" s" oi-head-if.f" HEAD
   70 s" hb: compile keyword cannot be a definition name: If at " S\" oi-head-if.f:1\n" DIED-AT
   s" trusted: OI-PKG:then ( -- )" s" oi-head-qualified-then.f" HEAD
   70 s" hb: compile keyword cannot be a definition name: then at " S\" oi-head-qualified-then.f:1\n" DIED-AT ;

\ `trusted:` needs a signature, opened and closed; the refusal names the
\ definition at the name's line. An input that ends before the signature
\ closes ends inside the definition.
: HEAD-TRUSTED-SIGNATURE ( -- )
   s" TRUSTED: OI-T 1 2" s" oi-head-no-signature.f" HEAD
   76 s" OI-T at " S\" oi-head-no-signature.f:1\n" DIED-AT
   GE-SRC-RESET
   s" 1 set-tier trusted: OI-T" GE-SRC-LINE
   s" ( n -- n" GE-SRC+
   s" oi-head-open-signature.f" BOTH
   74 s" hb: source ended inside definition: OI-T at "
   S\" oi-head-open-signature.f:1\n" DIED-AT
   s" TRUSTED: OI-T" s" oi-head-ended-signature.f" HEAD
   74 s" hb: source ended inside definition: OI-T at "
   S\" oi-head-ended-signature.f:1\n" DIED-AT ;

\ A head into a sealed package's name, or into a protected wordlist, and one
\ with no native compiler installed end the process without the exit hook.
: HEAD-FAIL-CLOSED ( -- )
   s" 1 set-tier : tfam:x" s" oi-head-sealed.f" ARMED
   s" tfam:x" ERR-LINE
   s" 1 set-tier package OI-EX public get-current prot-wid-add ;package : OI-EX:OI-T" s" oi-head-protected.f" ARMED
   S\" hb: cannot publish into protected word: OI-EX:OI-T\n" CASE$ GE-EXPECT-ERR
   s" 1 set-tier package OI-EX public get-current prot-wid-add : OI-T" s" oi-head-protected-current.f" ARMED
   S\" hb: cannot publish into protected word: OI-T\n" CASE$ GE-EXPECT-ERR
   GE-SRC-RESET
   s" ' OI-CR OI-ARM" GE-SRC-LINE
   s" 1 set-tier 0 data-base NCOMP-DISPATCH:XT-CELL + ! : OI-T ( -- )" GE-SRC-LINE
   s" oi-head-dispatch-unset.f" BOTH
   ENGINE-ERROR:AOT-SEED CASE$ GE-EXPECT-RC
   S\" hb: native compiler dispatch unset\n" CASE$ GE-EXPECT-ERR ;

\ A pending definition, dumped by OI-DUMP where its body names OI-SHOW, with
\ OI-WANT set to the wordlist the head must pick. The input then ends inside
\ the definition, and both loops refuse it there, naming it as its head
\ captured it and locating it at the name's line, the case's third.
: PENDING ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: want:ptr wantu:n src:ptr srcu:n name:ptr nameu:n :}
   GE-SRC-RESET
   s" 1 set-tier ' OI-DUMP OI-SHOWN !" GE-SRC-LINE
   want wantu GE-SRC+ s"  OI-WANT !" GE-SRC-LINE
   src srcu GE-SRC+
   name nameu BOTH ;

\ The current wordlist takes a bare name. A comment is not captured, blanks
\ between tokens collapse to one space, and the signature is the inside of the
\ parentheses.
: PENDING-BARE ( -- )
   s" get-current" S\" : OI-FOO ( n -- n ) ( a comment ) dup  1\n+ OI-SHOW" s" oi-pending-bare.f" PENDING
   S\" OI-FOO\n6\n1\nOI-FOO ( n -- n ) dup 1 + OI-SHOW \n8\n0\n1\n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-FOO at " S\" oi-pending-bare.f:3\n" DIED-AT ;

\ A prelude package's public wordlist takes its qualified tail. The long name
\ is copied to CP; `trusted:` sets the trusted cell, and its
\ signature need not be a whole token.
: PENDING-QUALIFIED ( -- )
   s" package OI-PKG public get-current ;package"
   s" TRUSTED: OI-PKG:A-LONG-DEFINITION-NAME (n -- ptr u8 n) drop OI-SHOW"
   s" oi-pending-qualified.f" PENDING
   S\" A-LONG-DEFINITION-NAME\n2305843009213693974\n1\nOI-PKG:A-LONG-DEFINITION-NAME (n -- ptr u8 n) drop OI-SHOW \n13\n1\n1\n"
   CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-PKG:A-LONG-DEFINITION-NAME at "
   S\" oi-pending-qualified.f:3\n" DIED-AT ;

\ An unknown qualifier makes a namespace row, whose public wordlist is the next
\ one. A signature the input ends inside runs to the end, where no OI-SHOW can
\ follow it.
: PENDING-NEW-NAMESPACE ( -- )
   s" WIDN-CELL OI-CELL@" s" : OI-NEWNS:OI-BAR ( n -- n ) OI-SHOW" s" oi-pending-namespace.f" PENDING
   S\" OI-BAR\n6\n1\nOI-NEWNS:OI-BAR ( n -- n ) OI-SHOW \n8\n0\n1\n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-NEWNS:OI-BAR at "
   S\" oi-pending-namespace.f:3\n" DIED-AT
   s" WIDN-CELL OI-CELL@" s" : OI-NEWNS:OI-BAR ( n -- n" s" oi-pending-signature.f" PENDING
   s" " CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-NEWNS:OI-BAR at "
   S\" oi-pending-signature.f:3\n" DIED-AT ;

\ A colon at either edge leaves the name bare.
: PENDING-EDGE-COLONS ( -- )
   s" get-current" s" : :OI-EDGE OI-SHOW" s" oi-pending-leading.f" PENDING
   S\" :OI-EDGE\n8\n1\n:OI-EDGE OI-SHOW \n0\n0\n1\n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: :OI-EDGE at " S\" oi-pending-leading.f:3\n" DIED-AT
   s" get-current" s" kernel: OI-EDGE: dup OI-SHOW" s" oi-pending-trailing.f" PENDING
   S\" OI-EDGE:\n8\n1\nOI-EDGE: dup OI-SHOW \n0\n0\n1\n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-EDGE: at " S\" oi-pending-trailing.f:3\n" DIED-AT ;

\ CP to the start of a code unit (PROT-PAGE-MAX), so what the case compiles
\ next lies in the unit its head then opens.
: UNIT-START ( -- )
   s" cp@ PROT-PAGE-MAX 1 - + PROT-PAGE-MAX negate and cp!" GE-SRC-LINE ;

\ An exit hook compiled in the unit a pending head holds open runs at exit,
\ after the input ending inside the head's definition is refused. The head is
\ each loop's own at tier 1, then the engine's at tier 0 through evaluate,
\ whose refusal is at its string's line.
: PENDING-HOOK ( -- )
   GE-SRC-RESET
   UNIT-START
   s" s~ : OI-HK ( -- ) 7 . ;~ evaluate ' OI-HK OI-ARM" QLINE
   s" 1 set-tier : OI-X ( -- ) 1" GE-SRC-LINE
   s" oi-pending-hook.f" BOTH
   S\" 7\n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-X at " S\" oi-pending-hook.f:3\n" DIED-AT
   GE-SRC-RESET
   UNIT-START
   s" s~ : OI-HK ( -- ) 7 . ;~ evaluate ' OI-HK OI-ARM" QLINE
   s" s~ : OI-X ( -- ) 1~ evaluate" QLINE
   s" oi-pending-hook-jit.f" BOTH
   S\" 7\n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-X at " S\" oi-pending-hook-jit.f:1\n" DIED-AT ;

\ An evaluate in a running word that leaves a head pending is refused at its
\ own end, at its string's line: the word runs no further.
: EVALUATE-HEAD ( -- )
   GE-SRC-RESET
   UNIT-START
   s" s\~ TRUSTED: OI-SPLIT ( -- ) s\~ : OI-Y ( n -- n )\~ evaluate 5 . s\~ 1 + ;\~ evaluate ;~ evaluate" QLINE
   s" OI-SPLIT 41 OI-Y ." GE-SRC-LINE
   s" oi-evaluate-head.f" BOTH
   s" " CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-Y at " S\" oi-evaluate-head.f:1\n" DIED-AT ;

\ A file an immediate word includes while its definition is pending ends inside
\ that definition without having opened it, and the file that included it ends
\ clean once its evaluate closes the definition: each source's end is judged
\ against the definition pending as that source began, not as one it read did.
: INCLUDE-UNDER-HEAD ( -- )
   s" \ opens nothing" s" oi-under-head.f" PKG-NESTED
   GE-SRC-RESET
   INC-LINE
   s" s~ : OI-UNDER ( -- ) OI-INC ;~ evaluate  1 ." QLINE
   s" oi-include-under-head.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 1\n" CASE$ GE-EXPECT-OUT ;

\ A `;` in a file an immediate word includes while its definition is pending
\ is refused at that `;`: the file did not open the definition.
: INCLUDE-CLOSES-HEAD ( -- )
   S\" \\ closes what it did not open\n;" s" oi-closes-head.f" PKG-NESTED
   GE-SRC-RESET
   INC-LINE
   s" 1 set-tier : OI-UNDER ( -- ) OI-INC" GE-SRC-LINE
   s" oi-include-closes-head.f" BOTH
   s" " CASE$ GE-EXPECT-OUT
   74 s" hb: source closed a definition it did not open: OI-UNDER at "
   S\" oi-closes-head.f:2\n" DIED-AT ;

\ A buffer the Habu loop reads under catch that ends inside the definition it
\ opened gives the definition up with its refusal, as an evaluate's does: the
\ caller goes on with nothing pending, defines anew and reuses the name. The
\ buffer goes to OUTER:INTERPRET directly, since an include under either loop
\ runs inside an evaluate frame, whose unwind would give it up anyway. OI-INT-PARKED
\ interprets the parked buffer, taking and leaving nothing; a case defines it
\ after its require, as the prelude loads before src/habu/interpret.f does, and
\ at tier 1, the only tier the Habu loop defines at.
: CAUGHT-HEAD ( -- )
   GE-SRC-RESET
   s" require src/habu/interpret.f 1 set-tier : OI-INT-PARKED ( -- ) OI-PARKED @ OI-PARKED-U @ OUTER:INTERPRET ;" GE-SRC-LINE
   s" s~ : OI-HELD ( -- )~ OI-PARK ' OI-INT-PARKED catch . 1 ." QLINE
   s" : OI-NEW ( -- ) 5 . ; OI-NEW : OI-HELD ( -- ) 6 . ; OI-HELD" GE-SRC-LINE
   s" oi-caught-head.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 74\n1\n5\n6\n" CASE$ GE-EXPECT-OUT
   s" hb: source ended inside definition: OI-HELD at " CASE$ GE-EXPECT-ERR-HAS
   S\" oi-caught-head.f:1\n" CASE$ GE-EXPECT-ERR-HAS ;

\ The loop compiled at tier 1: in the engine's run the require is the first
\ load of src/habu/interpret.f, whose literal and hook quotations each reach
\ only a cast. The Habu loop's run loaded the file before the case, so there
\ the require is a no-op.
: LOOP-TIER-1 ( -- )
   GE-SRC-RESET
   s" 1 set-tier require src/habu/interpret.f" GE-SRC-LINE
   s" s~ 1 2 + .~ OUTER:INTERPRET" QLINE
   s" oi-loop-tier-1.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 3\n" CASE$ GE-EXPECT-OUT ;

\ An immediate word's evaluate compiles into the definition it runs in, at
\ tier 0 and tier 1, and the definition goes on after it. The engine's loop
\ reads the head through evaluate, and at tier 1 the loop under test reads it
\ too, whose body runs the immediate.
: IMMEDIATE-EVALUATE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: tier:ptr tieru:n head:ptr headu:n name:ptr nameu:n :}
   GE-SRC-RESET
   tier tieru GE-SRC-LINE
   s" s\~ TRUSTED: OI-M ( -- ) s\~ 1 +\~ evaluate ; immediate~ evaluate  s~ OI-M~ 0 parse-imm" QLINE
   UNIT-START
   s" s~ : OI-G ( n -- n ) 2 + ;~ evaluate" QLINE
   head headu QLINE
   s" 40 OI-F ." GE-SRC-LINE
   name nameu BOTH
   CASE$ GE-EXPECT-OK
   S\" 43\n" CASE$ GE-EXPECT-OUT ;

\ An immediate word whose evaluate ends the definition it runs in is refused at
\ that `;`, whichever loop read the head: a definition closes in the source that
\ opened it.
: IMMEDIATE-SEMI ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: tier:ptr tieru:n head:ptr headu:n name:ptr nameu:n :}
   GE-SRC-RESET
   tier tieru GE-SRC-LINE
   s" s\~ TRUSTED: OI-M ( -- ) s\~ 1 + ;\~ evaluate ; immediate~ evaluate  s~ OI-M~ 0 parse-imm" QLINE
   UNIT-START
   s" s~ : OI-G ( n -- n ) 2 + ;~ evaluate" QLINE
   head headu QLINE
   name nameu BOTH
   s" " CASE$ GE-EXPECT-OUT ;

: IMMEDIATES ( -- )
   HB-TARGET-LINUX-X86-64? 0= if
      s" 0 set-tier" s" s~ : OI-F ( n -- n ) OI-M OI-G ;~ evaluate"
      s" oi-immediate-evaluate.f" IMMEDIATE-EVALUATE
   then
   s" 1 set-tier" s" s~ : OI-F ( n -- n ) OI-M OI-G ;~ evaluate"
   s" oi-immediate-evaluate-tier-1.f" IMMEDIATE-EVALUATE
   s" 1 set-tier" s" : OI-F ( n -- n ) OI-M OI-G ;"
   s" oi-immediate-evaluate-loop.f" IMMEDIATE-EVALUATE
   HB-TARGET-LINUX-X86-64? 0= if
      s" 0 set-tier" s" s~ : OI-F ( n -- n ) OI-M 40 OI-G OI-F .~ evaluate"
      s" oi-immediate-semi.f" IMMEDIATE-SEMI
      74 s" hb: source closed a definition it did not open: OI-F at " S\" oi-immediate-semi.f:1\n" DIED-AT
   then
   s" 1 set-tier" s" s~ : OI-F ( n -- n ) OI-M 40 OI-G OI-F .~ evaluate"
   s" oi-immediate-semi-tier-1.f" IMMEDIATE-SEMI
   74 s" hb: source closed a definition it did not open: OI-F at "
   S\" oi-immediate-semi-tier-1.f:1\n" DIED-AT
   s" 1 set-tier" s" : OI-F ( n -- n ) OI-M 40 OI-G OI-F ."
   s" oi-immediate-semi-loop.f" IMMEDIATE-SEMI
   74 s" hb: source closed a definition it did not open: OI-F at "
   S\" oi-immediate-semi-loop.f:1\n" DIED-AT ;

\ A body past BODYBUF-CAP refuses, naming the definition and the size the
\ capture needed.
: BODY-FULL ( -- )
   GE-SRC-RESET
   s" 1 set-tier : OI-BIG ( -- )" GE-SRC-LINE
   1000 0 ?do s" 1234567" GE-SRC-LINE loop
   s" oi-body-full.f" BOTH
   71 s" hb: definition body text full at 8000 bytes: OI-BIG needs 8006 at "
   S\" oi-body-full.f:1000\n" DIED-AT ;

\ ---- the string keywords in a body ------------------------------------------------
\ A string keyword's text joins the body whole, from one byte past the keyword
\ to its closing quote, as written: a blank run, a `(`, a `\`, a `;` and a
\ newline stay in it, and so do its escapes. The keyword is matched with its
\ A-Z folded.
: BODY-STRINGS ( -- )
   s" get-current"
   S\" : OI-STR ( -- ) S\q a  ( b ) \\ c\q c\q ;\q .\q e\nf\q s\\\q \\q\\x41\\\q\q C\\\q \\n\q .\\\q g\\\\\q dup OI-SHOW"
   s" oi-body-strings.f" PENDING
   S\" OI-STR\n6\n1\nOI-STR ( -- ) S\q a  ( b ) \\ c\q c\q ;\q .\q e\nf\q s\\\q \\q\\x41\\\q\q C\\\q \\n\q .\\\q g\\\\\q dup OI-SHOW \n4\n0\n1\n"
   CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-STR at " S\" oi-body-strings.f:3\n" DIED-AT ;

\ A quotation's body is the definition's: a string in it holding `;]` and `(`
\ does not end it.
: BODY-QUOTATION ( -- )
   s" get-current" S\" : OI-Q ( -- ) [: s\q ;] ( x\q type ;] execute OI-SHOW" s" oi-body-quotation.f" PENDING
   S\" OI-Q\n4\n1\nOI-Q ( -- ) [: s\q ;] ( x\q type ;] execute OI-SHOW \n4\n0\n1\n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-Q at " S\" oi-body-quotation.f:3\n" DIED-AT ;

\ `[:` outside a definition is no keyword of either loop: no word has the name.
: TOP-LEVEL-QUOTATION ( -- )
   s" 1 . [: 2 . ;] execute" s" oi-top-quotation.f" LINE-CASE
   70 CASE$ GE-EXPECT-RC
   S\" 1\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: [: at " S\" oi-top-quotation.f:1\n" ERR-AT ;

\ A string in a body with no closing quote, or with a bad escape, refuses as
\ the top-level keyword does, at the keyword's line. A counted string's length
\ is not checked until `;` compiles it: the input ends inside its definition
\ first.
: BODY-STRING-REFUSALS ( -- )
   GE-SRC-RESET
   s" 1 set-tier : OI-T ( -- ) 1 s~ never" QLINE
   s" closed" GE-SRC-LINE
   s" oi-body-unterminated.f" BOTH
   74 s" hb: bad string literal at " S\" oi-body-unterminated.f:1\n" DIED-AT
   GE-SRC-RESET
   s" 1 set-tier : OI-T ( -- )" GE-SRC-LINE
   s" .\~ a" QLINE
   s" \m~" QLINE
   s" oi-body-bad-escape.f" BOTH
   74 s" hb: bad string literal at " S\" oi-body-bad-escape.f:2\n" DIED-AT
   GE-SRC-RESET
   s" 1 set-tier : OI-T ( -- ) c~ " Q+ 256 [char] z GE-SRC-REPEAT-C s" ~" QLINE
   s" oi-body-counted-long.f" BOTH
   74 s" hb: source ended inside definition: OI-T at " S\" oi-body-counted-long.f:1\n" DIED-AT ;

\ A string the body cannot hold refuses as a token does, naming the definition
\ and what the capture needed, at the closing quote's line. Its text counts as
\ written, blanks, newline and escapes whole. One that fills the body to
\ BODYBUF-CAP is taken, and the token after it refuses.
: BODY-STRING-FULL ( -- )
   GE-SRC-RESET
   s" 1 set-tier : OI-BIG ( -- ) s~    " Q+ 7978 [char] x GE-SRC-REPEAT-C s" ~" QLINE
   s" y" GE-SRC-LINE
   s" oi-body-string-fills.f" BOTH
   71 s" hb: definition body text full at 8000 bytes: OI-BIG needs 8002 at "
   S\" oi-body-string-fills.f:2\n" DIED-AT
   GE-SRC-RESET
   s" 1 set-tier : OI-BIG ( -- ) s~    " QLINE 7978 [char] x GE-SRC-REPEAT-C s" ~" QLINE
   s" oi-body-string-full.f" BOTH
   71 s" hb: definition body text full at 8000 bytes: OI-BIG needs 8001 at "
   S\" oi-body-string-full.f:2\n" DIED-AT
   GE-SRC-RESET
   s" 1 set-tier : OI-BIG ( -- ) s\~    " Q+ 1994 0 ?do s" \x41" GE-SRC+ loop
   s" xx~" QLINE
   s" oi-body-escaped-full.f" BOTH
   71 s" hb: definition body text full at 8000 bytes: OI-BIG needs 8001 at "
   S\" oi-body-escaped-full.f:1\n" DIED-AT ;

\ ---- the immediates in a body -------------------------------------------------------
\ OI-N is a stack-neutral parsing immediate (parse-imm) that prints 9.
: NEUTRAL-LINE ( -- )
   s" s~ : OI-N ( -- ) 9 . ; immediate~ evaluate  s~ OI-N~ 0 parse-imm" QLINE ;

\ A neutral immediate runs as the body reads it, after its capture, and reads
\ the input after it; one the checker does not call neutral waits in the
\ capture for `;`. OI-N lies in the unit the head holds open, so it runs with
\ the code window closed.
: BODY-IMMEDIATES ( -- )
   GE-SRC-RESET
   s" 1 set-tier ' OI-BODY. OI-SHOWN !" GE-SRC-LINE
   UNIT-START
   NEUTRAL-LINE
   s" s~ : OI-SKIP ( -- ) parse-name 2drop ; immediate~ evaluate  s~ OI-SKIP~ 1 parse-imm" QLINE
   s" s~ : OI-P ( -- ) 8 . ; immediate~ evaluate" QLINE
   s" : OI-X ( -- ) OI-N OI-SKIP skipped OI-P OI-IMM 1 OI-SHOW" GE-SRC-LINE
   s" oi-body-immediates.f" BOTH
   S\" 9\nOI-X ( -- ) OI-N OI-SKIP OI-P OI-IMM 1 OI-SHOW \n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-X at " S\" oi-body-immediates.f:6\n" DIED-AT ;

\ The immediate is looked up as the engine's LFIND looks it up: in the open
\ package and the global wordlist, and NAME:tail, but not in a used package,
\ so a bare tail only used packages hold stays in the capture. Two used
\ packages hold OI-UN, so a lookup that asked them would refuse it as
\ ambiguous.
: BODY-IMMEDIATE-SCOPE ( -- )
   GE-SRC-RESET
   s" 1 set-tier ' OI-BODY. OI-SHOWN !" GE-SRC-LINE
   s" s~ package OI-UP public : OI-UN ( -- ) 9 . ; immediate : OI-UQ ( -- ) 3 . ; immediate ;package~ evaluate" QLINE
   s" s~ OI-UP:OI-UN~ 0 parse-imm  s~ OI-UP:OI-UQ~ 0 parse-imm" QLINE
   s" s~ package OI-UP2 public : OI-UN ( -- ) 8 . ; immediate ;package~ evaluate" QLINE
   s" s~ package OI-PP : OI-PN ( -- ) 4 . ; immediate ;package~ evaluate" QLINE
   s" using OI-UP using OI-UP2 package OI-PP s~ OI-PN~ 0 parse-imm" QLINE
   s" : OI-X ( -- ) OI-UN OI-UP:OI-UQ OI-PN OI-SHOW" GE-SRC-LINE
   s" oi-body-immediate-scope.f" BOTH
   S\" 3\n4\nOI-X ( -- ) OI-UN OI-UP:OI-UQ OI-PN OI-SHOW \n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-X at " S\" oi-body-immediate-scope.f:7\n" DIED-AT ;

\ An armed checker's preflight gets the body so far, the immediate's token and
\ the trusted cell before the immediate runs; this one refuses a trusted body.
: PREFLIGHT-CASE ( ptr u8 n ptr u8 n -- ) {: head:ptr headu:n name:ptr nameu:n :}
   GE-SRC-RESET
   NEUTRAL-LINE
   s" s~ : OI-PF ( ptr u8 n ptr u8 n bool -- ) {: b:ptr bu:n t:ptr tu:n f:bool :}" Q+
   s"  b bu type cr t tu type cr f OI-B. f if 75 throw then ;~ evaluate" QLINE
   s" s~ : OI-HK ( ptr u8 n -- n ) 2drop -1 ;~ evaluate" QLINE
   s" 0 set-check ' OI-PF set-preflight ' OI-HK set-check" GE-SRC-LINE
   head headu GE-SRC-LINE
   name nameu BOTH ;

: BODY-IMMEDIATE-PREFLIGHT ( -- )
   s" 1 set-tier : OI-X ( -- ) 5 OI-N" s" oi-body-preflight.f" PREFLIGHT-CASE
   S\" OI-X ( -- ) 5 OI-N \nOI-N\n0\n9\n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-X at " S\" oi-body-preflight.f:5\n" DIED-AT
   s" 1 set-tier trusted: OI-X ( -- ) 5 OI-N" s" oi-body-preflight-trusted.f" PREFLIGHT-CASE
   75 CASE$ GE-EXPECT-RC
   S\" OI-X ( -- ) 5 OI-N \nOI-N\n1\n" CASE$ GE-EXPECT-OUT ;

\ A checker armed with no preflight refuses the immediate, and so does the
\ floor when the immediate takes a cell it was not given.
: BODY-IMMEDIATE-REFUSALS ( -- )
   GE-SRC-RESET
   NEUTRAL-LINE
   s" s~ : OI-HK ( ptr u8 n -- n ) 2drop -1 ;~ evaluate" QLINE
   s" 0 set-check ' OI-HK set-check" GE-SRC-LINE
   s" 1 . 1 set-tier : OI-T ( -- ) OI-N 2 ." GE-SRC-LINE
   s" oi-body-preflight-missing.f" BOTH
   70 CASE$ GE-EXPECT-RC
   S\" 1\n" CASE$ GE-EXPECT-OUT
   S\" hb: compile preflight hook missing\n" CASE$ GE-EXPECT-ERR
   HB-TARGET-LINUX-X86-64? 0= if
      GE-SRC-RESET
      s" s~ TRUSTED: OI-UFI ( -- ) drop ; immediate~ evaluate  s~ OI-UFI~ 0 parse-imm" QLINE
      s" 1 . 1 set-tier : OI-T ( -- ) OI-UFI 2 ." GE-SRC-LINE
      s" oi-body-immediate-underflow.f" BOTH
      70 CASE$ GE-EXPECT-RC
      S\" 1\n" CASE$ GE-EXPECT-OUT
      S\" E-UNDERFLOW: OI-UFI\n" CASE$ GE-EXPECT-ERR
   then ;

\ At tier 0 the body refuses before an immediate in it runs, in a file included
\ under the engine's head as HEAD-TIER-0's.
: BODY-IMMEDIATE-TIER-0 ( -- )
   s" OI-N 2" s" oi-body-imm-tier-0-nested.f" PKG-NESTED
   GE-SRC-RESET
   NEUTRAL-LINE
   INC-LINE
   s" 1 ." GE-SRC-LINE
   s" s~ : OI-T0 ( -- ) OI-INC~ evaluate" QLINE
   s" oi-body-immediate-tier-0.f" HABU
   HB-TARGET-LINUX-X86-64? if
      S\" 1\n9\n" CASE$ GE-EXPECT-OUT
      74 s" hb: source ended inside definition: OI-T0 at " S\" oi-body-immediate-tier-0.f:1\n" DIED-AT
   else
      S\" 1\n" CASE$ GE-EXPECT-OUT
      76 s" hb: tier 0 is not in the Habu loop: OI-N at " S\" oi-body-imm-tier-0-nested.f:1\n" DIED-AT
   then ;

\ ---- `does>` in a body --------------------------------------------------------------
\ `does>` is matched with its A-Z folded and joins the capture, and DOESB takes
\ the capture's length with it. The signature after it, past blanks and a
\ newline, joins no capture: its inside, blanks and all, is copied to here for
\ TCSIG. A comment after the signature is a comment.
: DOES-SPLIT ( -- )
   GE-SRC-RESET
   s" 1 set-tier ' OI-DOES. OI-SHOWN ! OI-MARK" GE-SRC-LINE
   s" : OI-K ( n -- ) create , DOES>   " GE-SRC-LINE
   s" ( -- n ) ( a comment ) @ OI-SHOW" GE-SRC-LINE
   s" oi-does-split.f" BOTH
   S\" OI-K ( n -- ) create , DOES> @ OI-SHOW \n29\n -- n \n6\n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-K at " S\" oi-does-split.f:2\n" DIED-AT ;

\ A definer the loop under test reads compiles at `;`: the word it creates runs
\ its clause, and a checked caller is held to the created signature.
: DOES-DEFINER ( -- )
   GE-SRC-RESET
   s" 1 set-tier : OI-K ( n -- ) create , does> ( -- n ) @ 1 + ;" GE-SRC-LINE
   s" 5 OI-K OI-FV OI-FV . s~ : OI-USE ( -- n ) OI-FV ;~ evaluate OI-USE ." QLINE
   s" oi-does-definer.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 6\n6\n" CASE$ GE-EXPECT-OUT
   GE-SRC-RESET
   s" 1 set-tier : OI-K ( n -- ) create , does> ( -- ptr u8 ) @ ;" GE-SRC-LINE
   s" 5 OI-K OI-FV s~ : OI-USE ( -- n ) OI-FV ;~ evaluate" QLINE
   s" oi-does-effect.f" BOTH
   70 CASE$ GE-EXPECT-RC
   s" habu: in oi-use: at 'OI-FV' expected: n actual: ptr u8" CASE$ GE-EXPECT-ERR-HAS ;

\ A second `does>` refuses naming `does>`, and a signature that is missing or
\ open names the token as spelled, each at the line the cursor is on.
: DOES-REFUSALS ( -- )
   GE-SRC-RESET
   s" 1 set-tier : OI-K ( n -- ) create , does> ( -- n ) @" GE-SRC-LINE
   s" DOES> ( -- )" GE-SRC-LINE
   s" oi-does-twice.f" BOTH
   70 s" does> at " S\" oi-does-twice.f:2\n" DIED-AT
   s" : OI-K ( n -- ) create , Does> @" s" oi-does-no-signature.f" HEAD
   76 s" Does> at " S\" oi-does-no-signature.f:1\n" DIED-AT
   GE-SRC-RESET
   s" 1 set-tier : OI-K ( n -- ) create , does>" GE-SRC-LINE
   s" ( -- n" GE-SRC+
   s" oi-does-open-signature.f" BOTH
   76 s" does> at " S\" oi-does-open-signature.f:1\n" DIED-AT ;

\ The signature's copy may take the data space to its ceiling; one byte more
\ refuses as allot does.
: DOES-DATA-FULL ( -- )
   s" OI-ROOM 6 - allot : OI-K ( n -- ) create , does> ( -- n ) @" s" oi-does-data-fits.f" HEAD
   s" " CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-K at " S\" oi-does-data-fits.f:1\n" DIED-AT
   s" OI-ROOM 5 - allot : OI-K ( n -- ) create , does> ( -- n ) @" s" oi-does-data-full.f" HEAD
   76 s" hb: data space out of range: DP " S\" oi-does-data-full.f:1\n" DIED-AT ;

\ LFIND finds a word before the engine reads its keywords: one spelled `does>`
\ that is not immediate is called, the body not split and the comment after it
\ a comment, while an immediate one the checker does not call neutral leaves
\ `does>` the keyword.
: CALLED ( ptr u8 n ptr u8 n -- ) {: def:ptr defu:n name:ptr nameu:n :}
   GE-SRC-RESET
   s" 1 set-tier ' OI-DOES. OI-SHOWN !" GE-SRC-LINE
   def defu QLINE
   s" s~ does>~ OI-ALIAS OI-MARK" QLINE
   s" : OI-X ( -- ) does> ( a comment ) 1 OI-SHOW" GE-SRC-LINE
   name nameu BOTH ;

: DOES-CALLED ( -- )
   s" s~ : OI-DZ ( -- ) 7 . ;~ evaluate" s" oi-does-called.f" CALLED
   S\" OI-X ( -- ) does> 1 OI-SHOW \n0\n\n0\n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-X at " S\" oi-does-called.f:4\n" DIED-AT
   s" s~ : OI-DZ ( -- ) 7 . ; immediate~ evaluate" s" oi-does-immediate.f" CALLED
   S\" OI-X ( -- ) does> 1 OI-SHOW \n18\n a comment \n11\n" CASE$ GE-EXPECT-OUT
   74 s" hb: source ended inside definition: OI-X at " S\" oi-does-immediate.f:4\n" DIED-AT ;

\ ---- `;` ---------------------------------------------------------------------------
\ `;` compiles the body the loop captured and ends the definition, a body over
\ two lines, a `trusted:` one, one in a package's public wordlist and a
\ `does>` definer alike: each word runs, and after each `;` the cells the end
\ clears are 0. `;` leaves the tier at 1.
: SEMI ( -- )
   GE-SRC-RESET
   s" 1 set-tier : OI-X ( n -- n )" GE-SRC-LINE
   s"   1 + ;" GE-SRC-LINE
   s" 41 OI-X . OI-ENDED. tier@ ." GE-SRC-LINE
   s" trusted: OI-T ( -- n ) 5 ; OI-T . OI-ENDED." GE-SRC-LINE
   s" package OI-P public : OI-Y ( -- n ) 3 ; ;package OI-P:OI-Y ." GE-SRC-LINE
   s" : OI-DEF ( n -- ) create , does> ( -- n ) @ ; OI-ENDED. 7 OI-DEF OI-K OI-K ." GE-SRC-LINE
   s" oi-semi.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 42\n0\n1\n5\n0\n3\n0\n7\n" CASE$ GE-EXPECT-OUT ;

\ With no definition open `;` is no keyword, and undefined. A body the checker
\ refuses ends the load at its `;`, before the word after it runs.
: SEMI-REFUSALS ( -- )
   s" 1 . ;" s" oi-semi-none.f" HEAD
   70 CASE$ GE-EXPECT-RC
   S\" 1\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: ; at " S\" oi-semi-none.f:1\n" ERR-AT
   s" : OI-BAD ( -- n ) ; 7 ." s" oi-semi-unchecked.f" HEAD
   70 CASE$ GE-EXPECT-RC
   s" " CASE$ GE-EXPECT-OUT
   s" ncomp: cannot compile OI-BAD" CASE$ GE-EXPECT-ERR-HAS ;

\ A body immediate that clears the compiler entry leaves `;` none to call: the
\ process ends as at the head, without the exit hook.
: SEMI-DISPATCH-UNSET ( -- )
   GE-SRC-RESET
   s" s~ TRUSTED: OI-UNSET ( -- ) 0 data-base NCOMP-DISPATCH:XT-CELL + ! ; immediate~ evaluate" QLINE
   s" s~ OI-UNSET~ 0 parse-imm ' OI-CR OI-ARM" QLINE
   s" 1 set-tier : OI-T ( -- ) OI-UNSET ;" GE-SRC-LINE
   s" oi-semi-dispatch-unset.f" BOTH
   ENGINE-ERROR:AOT-SEED CASE$ GE-EXPECT-RC
   s" " CASE$ GE-EXPECT-OUT
   S\" hb: native compiler dispatch unset\n" CASE$ GE-EXPECT-ERR ;

\ At tier 0 `;` refuses as every body token does: the engine's loop ends a
\ tier 0 body.
: SEMI-TIER-0 ( -- )
   s" ;" s" oi-semi-tier-0-nested.f" PKG-NESTED
   GE-SRC-RESET
   INC-LINE
   s" s~ : OI-T0 ( -- ) OI-INC~ evaluate" QLINE
   s" oi-semi-tier-0.f" HABU
   HB-TARGET-LINUX-X86-64? if
      74 s" hb: source closed a definition it did not open: OI-T0 at " S\" oi-semi-tier-0-nested.f:1\n" DIED-AT
   else
      76 s" hb: tier 0 is not in the Habu loop: ; at " S\" oi-semi-tier-0-nested.f:1\n" DIED-AT
   then ;

\ ---- `create`, `variable` and `constant` -------------------------------------------
\ A created word pushes its cell, and a checked word reads it through the
\ effect the checker was given; a variable's cell is its own.
: CREATED ( -- )
   s" 1 set-tier variable OI-V variable OI-W 5 OI-V ! 6 OI-W ! : OI-F ( -- n ) OI-V @ OI-W @ + ; OI-F . OI-V @ ."
   s" oi-variable.f" LINE-CASE
   CASE$ GE-EXPECT-OK
   S\" 11\n5\n" CASE$ GE-EXPECT-OUT
   s" 1 set-tier create OI-B 16 allot 7 OI-B c! 9 OI-B 15 + c! : OI-G ( -- n ) OI-B c@ OI-B 15 + c@ + ; OI-G ."
   s" oi-create.f" LINE-CASE
   CASE$ GE-EXPECT-OK
   S\" 16\n" CASE$ GE-EXPECT-OUT ;

\ With the input at its end the refusal names `create`, for `variable` too.
: CREATED-NO-NAME ( -- )
   GE-SRC-RESET
   s" 1 ." GE-SRC-LINE
   s" create" GE-SRC+
   s" oi-create-no-name.f" BOTH
   S\" 1\n" CASE$ GE-EXPECT-OUT
   74 s" hb: reader keyword needs a name: create at " S\" oi-create-no-name.f:2\n" DIED-AT
   GE-SRC-RESET
   s" 1 ." GE-SRC-LINE
   s" variable" GE-SRC+
   s" oi-variable-no-name.f" BOTH
   S\" 1\n" CASE$ GE-EXPECT-OUT
   74 s" hb: reader keyword needs a name: create at " S\" oi-variable-no-name.f:2\n" DIED-AT ;

\ A full dictionary refuses after the name, naming the word, not the keyword.
: CREATED-DICT-FULL ( -- )
   GE-SRC-RESET
   s" OI-DICT-FULL create OI-T" GE-SRC-LINE
   s" oi-create-dictionary-full.f" BOTH
   77 s" hb: dictionary full at: OI-T at " S\" oi-create-dictionary-full.f:1\n" DIED-AT ;

\ The def hook gets the name and `create`, for `variable` too, and the word
\ publishes whatever its verdict.
: CREATED-HOOK ( -- )
   GE-SRC-RESET
   s" s~ : OI-HK ( ptr u8 n -- n ) type cr 0 ;~ evaluate" QLINE
   s" 0 set-check ' OI-HK set-check" GE-SRC-LINE
   s" create OI-B variable OI-V 5 OI-V ! OI-V @ ." GE-SRC-LINE
   s" oi-create-hook.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" OI-B create \nOI-V create \n5\n" CASE$ GE-EXPECT-OUT ;

\ A constant is the record a later `does>` patches, as a variable is: the
\ clause acts on the constant, not on the variable before it. An export of the
\ constant made before the patch shares its body, so a tier 1 mention of the
\ alias compiled after the patch calls the clause. So does one of OI-KH's
\ export, which `ndict!` hid while the patch ran and raised back after it. The
\ clause reads OI-AV, stored after the mentions compile: a mention folded as
\ the constant's fixed value would print the 1 the clause gave when it was
\ folded, not 9.
: CONSTANT-DOES ( -- )
   GE-SRC-RESET
   s" 1 set-tier variable OI-AV variable OI-LO variable OI-HI 3 constant OI-KC" GE-SRC-LINE
   s" package OI-KP public export OI-KC ;package" GE-SRC-LINE
   s" : OI-BEHAVE ( -- ) does> ( -- n ) drop OI-AV @ ;" GE-SRC-LINE
   s" ndict@ OI-LO ! package OI-KH public export OI-KC ;package ndict@ OI-HI !" GE-SRC-LINE
   s" OI-LO @ ndict! OI-BEHAVE 1 OI-AV ! OI-HI @ ndict!" GE-SRC-LINE
   s" : OI-U1 ( -- n ) OI-KP:OI-KC ; : OI-UH ( -- n ) OI-KH:OI-KC ;" GE-SRC-LINE
   s" 9 OI-AV ! OI-KC . OI-U1 . OI-UH ." GE-SRC-LINE
   s" oi-constant-does.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" 9\n9\n9\n" CASE$ GE-EXPECT-OUT ;

\ A constant refused after its record opened is no record a later `does>`
\ patches: the clause acts on the variable before it. Each loop is held to
\ the output alone: the engine names the refusal at the file's floor
\ (`interpret stack underdepth`), the Habu loop's constant E-UNDERFLOW. The
\ case runs unchecked: the typed top level refuses a caught `included` there,
\ and the checker still types the patched variable as its cell.
: CONSTANT-REFUSED-DOES ( -- )
   GE-SRC-RESET
   s" constant OI-KC" GE-SRC-LINE
   s" oi-nested-constant.f" NESTED-BUF GT-PATH NESTED-U !
   NESTED$ SRC>FILE
   GE-SRC-RESET
   s" 1 set-tier 0 set-check variable OI-AV s~ oi-nested-constant.f~ ' included catch . 2drop" QLINE
   s" : OI-BEHAVE ( -- ) does> ( -- n ) drop 9 ;" GE-SRC-LINE
   s" OI-BEHAVE OI-AV ." GE-SRC-LINE
   s" oi-constant-refused-does.f" CASE-BUF GT-PATH CASE-U !
   CASE$ SRC>FILE
   false RUN  CASE$ GE-EXPECT-OK  S\" 70\n9\n" CASE$ GE-EXPECT-OUT
   true RUN   CASE$ GE-EXPECT-OK  S\" 70\n9\n" CASE$ GE-EXPECT-OUT
   1 CASES +! ;

\ ---- `immediate` and `cast:` ---------------------------------------------------------
\ `immediate` marks the newest record, as spelled in any case, and after a
\ does> definer that is its clause, not the definer.
: MARKED ( -- )
   s" 1 set-tier : OI-A ( -- ) ; IMMEDIATE parse-name OI-A tok-imm? ." s" oi-immediate.f" LINE-CASE
   CASE$ GE-EXPECT-OK
   S\" 2\n" CASE$ GE-EXPECT-OUT
   s" 1 set-tier : OI-K ( n -- ) create , does> ( -- n ) @ ; immediate parse-name OI-K tok-imm? . parse-name OI-K;does tok-imm? ."
   s" oi-immediate-does.f" LINE-CASE
   CASE$ GE-EXPECT-OK
   S\" 0\n2\n" CASE$ GE-EXPECT-OUT ;

\ A cast is the identity, called or executed, at any tier, into a qualifier's
\ public wordlist or a namespace the qualifier makes, its long name past the
\ record. With the hook cell empty its record carries DKIND:CAST alone; with
\ the checker's hook also the registrar's minimum input arity, so a bare call
\ on an empty stack is refused.
: CAST-DECLARED ( -- )
   s" 1 set-tier 0 set-check cast: OI-X ( n -- n ) 1 OI-X . 2 ' OI-X execute . ndict@ 1- XREF-REC XREF-FLAGS 48 rshift ."
   s" oi-cast.f" LINE-CASE
   CASE$ GE-EXPECT-OK
   S\" 1\n2\n12\n" CASE$ GE-EXPECT-OUT
   s" 1 set-tier cast: OI-X ( n -- n ) 1 OI-X . ndict@ 1- XREF-REC XREF-FLAGS 48 rshift . OI-X"
   s" oi-cast-hooked.f" LINE-CASE
   70 CASE$ GE-EXPECT-RC
   S\" 1\n28\n" CASE$ GE-EXPECT-OUT
   S\" hb: interpret stack underdepth: OI-X\n" CASE$ GE-EXPECT-ERR
   s" 0 set-tier cast: OI-X ( n -- n ) 3 OI-X ." s" oi-cast-tier-0.f" LINE-CASE
   CASE$ GE-EXPECT-OK
   S\" 3\n" CASE$ GE-EXPECT-OUT
   s" 1 set-tier package OI-CP public ;package cast: OI-CP:OI-Y ( n -- n ) 4 OI-CP:OI-Y . cast: OI-NS:OI-Q ( n -- n ) 5 OI-NS:OI-Q ."
   s" oi-cast-qualified.f" LINE-CASE
   CASE$ GE-EXPECT-OK
   S\" 4\n5\n" CASE$ GE-EXPECT-OUT
   s" 1 set-tier cast: A-LONG-CAST-NAME-PAST-INLINE ( n -- n ) 6 A-LONG-CAST-NAME-PAST-INLINE ."
   s" oi-cast-long-name.f" LINE-CASE
   CASE$ GE-EXPECT-OK
   S\" 6\n" CASE$ GE-EXPECT-OUT ;

\ A cast in a package's private section mints a pointer, the checked word
\ after it calls it, and it is no public word.
: CAST-PRIVATE ( -- )
   GE-SRC-RESET
   s" 1 set-tier ndict@ package OI-CX private" GE-SRC-LINE
   s" cast: >OI-V ( n -- ptr [ -- ] )" GE-SRC-LINE
   s" : OI-F ( n -- ptr [ -- ] ) >OI-V ;" GE-SRC-LINE
   s" public ;package ndict@ swap - . ndict@ 1- XREF-REC XREF-NAME$ type cr" GE-SRC-LINE
   s" ' OI-CX:>OI-V" GE-SRC-LINE
   s" oi-cast-private.f" BOTH
   70 CASE$ GE-EXPECT-RC
   S\" 3\nOI-F\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: OI-CX:>OI-V at " S\" oi-cast-private.f:5\n" ERR-AT ;

\ `cast:` refuses in the engine's order: a full code space or dictionary,
\ naming the keyword, then a missing name, at the line the input ends on past
\ the keyword's, then a signature missing, unclosed or not opened, naming the
\ name at its line, then the qualifier's refusals. The signature is passed
\ before those, so they name the line it ends on, and a duplicate name with a
\ bad signature refuses the signature.
: CAST-REFUSALS ( -- )
   s" dbase@ REGION + $4000 - cp! cast: OI-X ( n -- n )" s" oi-cast-code-full.f" HEAD
   76 s" hb: code space full at: cast: at " S\" oi-cast-code-full.f:1\n" DIED-AT
   s" OI-DICT-FULL cast: OI-X ( n -- n )" s" oi-cast-dictionary-full.f" HEAD
   77 s" hb: dictionary full at: cast: at " S\" oi-cast-dictionary-full.f:1\n" DIED-AT
   GE-SRC-RESET
   s" 1 set-tier 1 ." GE-SRC-LINE
   s" CAST:" GE-SRC-LINE
   GE-SRC-LF
   GE-SRC-LF
   s" oi-cast-no-name.f" BOTH
   S\" 1\n" CASE$ GE-EXPECT-OUT
   74 s" hb: cast: missing name after CAST: at " S\" oi-cast-no-name.f:5\n" DIED-AT
   GE-SRC-RESET
   s" 1 set-tier 1 . cast: OI-X" GE-SRC-LINE
   s" 2 ." GE-SRC-LINE
   s" oi-cast-no-signature.f" BOTH
   S\" 1\n" CASE$ GE-EXPECT-OUT
   76 s" OI-X at " S\" oi-cast-no-signature.f:1\n" DIED-AT
   GE-SRC-RESET
   s" 1 set-tier cast: OI-X ( n -- n" GE-SRC-LINE
   s" 2 ." GE-SRC-LINE
   s" oi-cast-open-signature.f" BOTH
   76 s" OI-X at " S\" oi-cast-open-signature.f:1\n" DIED-AT
   GE-SRC-RESET
   s" 1 set-tier cast: OI-X" GE-SRC+
   s" oi-cast-ended.f" BOTH
   76 s" OI-X at " S\" oi-cast-ended.f:1\n" DIED-AT
   s" cast: OI-TWO n -- n ) 2 ." s" oi-cast-bad-signature.f" HEAD
   76 s" OI-TWO at " S\" oi-cast-bad-signature.f:1\n" DIED-AT
   s" cast: if ( n -- n )" s" oi-cast-if.f" HEAD
   70 s" hb: compile keyword cannot be a definition name: if at " S\" oi-cast-if.f:1\n" DIED-AT
   s" cast: oi-two ( n -- n )" s" oi-cast-duplicate.f" HEAD
   78 s" duplicate definition: oi-two at " S\" oi-cast-duplicate.f:1\n" DIED-AT
   GE-SRC-RESET
   s" 1 set-tier cast: OI-TWO" GE-SRC-LINE
   s" ( n -- n )" GE-SRC-LINE
   s" oi-cast-duplicate-next-line.f" BOTH
   78 s" duplicate definition: OI-TWO at " S\" oi-cast-duplicate-next-line.f:2\n" DIED-AT
   s" dbase@ REGION + $4000 - 8 - cp! cast: A-LONG-CAST-NAME-PAST-INLINE ( n -- n )" s" oi-cast-name-full.f" HEAD
   76 s" hb: code space full at: A-LONG-CAST-NAME-PAST-INLINE at " S\" oi-cast-name-full.f:1\n" CODE-DIED-AT ;

\ The checker's registrar refuses a mint outside a private section and a
\ retype that changes the arity with a catchable throw, before the record is
\ counted. Uncaught, the load ends with the exit hook run; caught, the name is
\ absent and nothing stays pending.
: CAST-REGISTRAR ( -- )
   GE-SRC-RESET
   s" ' OI-CR OI-ARM" GE-SRC-LINE
   s" 1 set-tier cast: OI-X ( n -- ptr u8 )" GE-SRC-LINE
   s" oi-cast-mint.f" BOTH
   67 CASE$ GE-EXPECT-RC
   S\" \n" CASE$ GE-EXPECT-OUT
   S\" hb: uncaught throw code 7147\n" CASE$ GE-EXPECT-ERR
   GE-SRC-RESET
   s" require src/habu/interpret.f 1 set-tier : OI-INT-PARKED ( -- ) OI-PARKED @ OI-PARKED-U @ OUTER:INTERPRET ;" GE-SRC-LINE
   s" s~ cast: OI-Y ( n n -- n )~ OI-PARK ' OI-INT-PARKED catch . depth ." QLINE
   s" : OI-NEW ( -- ) 5 . ; OI-NEW" GE-SRC-LINE
   s" ' OI-Y" GE-SRC-LINE
   s" oi-cast-caught.f" BOTH
   70 CASE$ GE-EXPECT-RC
   S\" 7129\n0\n5\n" CASE$ GE-EXPECT-OUT
   s" E-UNDEFINED: OI-Y at " S\" oi-cast-caught.f:4\n" ERR-AT ;

\ The def hook never sees a cast: it gets only the created word after it.
: CAST-HOOK ( -- )
   GE-SRC-RESET
   s" s~ : OI-HK ( ptr u8 n -- n ) type cr 0 ;~ evaluate" QLINE
   s" 0 set-check ' OI-HK set-check" GE-SRC-LINE
   s" cast: OI-X ( n -- n ) create OI-G 5 OI-X ." GE-SRC-LINE
   s" oi-cast-hook.f" BOTH
   CASE$ GE-EXPECT-OK
   S\" OI-G create \n5\n" CASE$ GE-EXPECT-OUT ;

\ While a task is live `cast:` exits 79, the keyword its whole diagnostic, and
\ `immediate`, which checks nothing, marks.
: CAST-LIVE ( -- )
   SPIN-PRELUDE
   s" 1 set-tier OI-SPIN CAST: OI-X ( n -- n )" s" CAST:" s" oi-live-cast.f" LIVE
   s" 1 set-tier : OI-A ( -- ) ; OI-SPIN immediate parse-name OI-A tok-imm? ." s" oi-live-immediate.f" LINE-CASE
   CASE$ GE-EXPECT-OK
   S\" 2\n" CASE$ GE-EXPECT-OUT
   0 SPIN-U ! ;

\ ---- in a forked copy of this process -----------------------------------------------
\ The fork binds the seam to a counting spy that a loaded file must reach.

variable SPY-N

: SPY ( ptr u8 n n n -- )
   1 SPY-N +!
   OUTER:INTERPRET-RANGE ;

public

: SPY-ON ( -- )
   [: SPY ;] SOURCE-ROOT:USE-INTERPRETER ;

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
   COMMENT-AT-END
   LINE-COMMENT-AT-END
   NOT-A-COMMENT
   BLANK-INPUT
   UNDEFINED
   OUT-OF-RANGE
   UNDERFLOW
   UNDERDEPTH
   INTERNAL
   ROWLESS-STEP
   WIDE
   HOOK-WINDOW
   HOOK-UNDERFLOW
   NESTED
   NESTED-CLOSED
   REQUIRE-ROLLBACK
   EVALUATE-NESTED
   EVALUATE-CLOSED
   LITERALS
   TICK-AND-CHAR
   TICK-UNDEFINED
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
   TICK-TRUSTED
   TICK-C2-SCOPE
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
   TOP-SHADOW
   USING-OWN-NAME
   USING-ACROSS-PACKAGE
   PACKAGE-RECOVERY
   PACKAGE-RECOVERY-LIVE
   USING-INCLUDER
   USING-SLOT-THROW
   EXPORT-TOP-LEVEL
   EXPORT-ALIASES
   EXPORT-REFUSALS
   EXPORT-INTERNAL
   EXPORT-DEFINER
   CAPACITY
   PROTECTED-PACKAGES
   TASK-LIVE
   TASK-LIVE-KEYWORDS
   SEAM
   HEAD-NO-NAME
   HEAD-TIER-0
   HEAD-TASK-LIVE
   HEAD-CAPACITY
   HEAD-NAME-REFUSALS
   HEAD-KEYWORD-WALL
   HEAD-TRUSTED-SIGNATURE
   HEAD-FAIL-CLOSED
   PENDING-BARE
   PENDING-QUALIFIED
   PENDING-NEW-NAMESPACE
   PENDING-EDGE-COLONS
   PENDING-HOOK
   EVALUATE-HEAD
   INCLUDE-UNDER-HEAD
   INCLUDE-CLOSES-HEAD
   CAUGHT-HEAD
   LOOP-TIER-1
   IMMEDIATES
   BODY-FULL
   BODY-STRINGS
   BODY-QUOTATION
   TOP-LEVEL-QUOTATION
   BODY-STRING-REFUSALS
   BODY-STRING-FULL
   BODY-IMMEDIATES
   BODY-IMMEDIATE-SCOPE
   BODY-IMMEDIATE-PREFLIGHT
   BODY-IMMEDIATE-REFUSALS
   BODY-IMMEDIATE-TIER-0
   DOES-SPLIT
   DOES-DEFINER
   DOES-REFUSALS
   DOES-DATA-FULL
   DOES-CALLED
   SEMI
   SEMI-REFUSALS
   SEMI-DISPATCH-UNSET
   SEMI-TIER-0
   CREATED
   CREATED-NO-NAME
   CREATED-DICT-FULL
   CREATED-HOOK
   CONSTANT-DOES
   CONSTANT-REFUSED-DOES
   MARKED
   CAST-DECLARED
   CAST-PRIVATE
   CAST-REFUSALS
   CAST-REGISTRAR
   CAST-HOOK
   CAST-LIVE
   GT-CLEANUP
   s" outer-interpret: " type CASES @ FMT:.INT
   s"  cases agree with the engine's loop" type cr ;

MAIN

;package
