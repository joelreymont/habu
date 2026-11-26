\ diag-position-test.f - a checker packet locates its token in the checked file,
\ with LF ending a line; a packet in a required file names and locates there.
\ Semantic checks call LSP-CHECK's public words in process; the verifier and run
\ stages use HABU_UNDER_TEST when set, else this engine.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test/assert.f
require src/lib/test/ending.f
require src/lib/test/runner.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/json.f
require tools/gate-json-assert-core.f
require tools/check-core.f
require src/lib/json/read.f
require src/lib/ffi-abi.f

package DIAG-POSITION-TEST

PATH-CAP 1+ 4 * constant CAP

create FX CAP allot
create EMPTY 1 allot
create BASE PATH-CAP allot
create FX-P CAP allot
create DEP CAP allot
create DEP-P PATH-CAP allot
create DIR-P PATH-CAP allot
create CANON FS:PATHZ-CAP allot
4200 constant LONG-NAME-BYTES
create LONG-NAME LONG-NAME-BYTES allot

variable BASE-U
variable FX-U
variable FX-PU
variable DEP-U
variable DEP-PU
variable RC

PROCESS-SYMBOLS
FUNCTION: MKFIFO-CALL mkfifo ( ^u8 n -- i32 ) ;FUNCTION

: BASE$ ( -- str )
   BASE BASE-U @ >str ;

: FX$ ( -- str )
   FX FX-U @ >str ;

: FX-PATH$ ( -- str )
   FX-P FX-PU @ >str ;

: DEP$ ( -- str )
   DEP DEP-U @ >str ;

: DEP-PATH$ ( -- str )
   DEP-P DEP-PU @ >str ;

: ROOT! ( -- )
   FS:CLEANUP-RESET
   s" habu-diag-position" FS:MKTEMP-DIR SOURCE-ROOT:CANONICAL drop
   {: root:str :}
   root STR:DATA BASE root STR:LEN BYTE-COPY
   root STR:LEN BASE-U !
   BASE$ FS:CLEANUP-TREE+ ;

: LF+ ( -- ) 10 SB-APPEND-C ;
: CR+ ( -- ) 13 SB-APPEND-C ;
: TAB+ ( -- ) 9 SB-APPEND-C ;

\ Writes the string builder's text as fixture NAME and keeps a copy for the
\ packets' byte spans.
: FIXTURE! ( str -- )
   {: name:str :}
   BASE$ name FX-P FS:JOIN-PATH FX-PU !
   SB$
   {: text:str :}
   text STR:DATA FX text STR:LEN BYTE-COPY
   text STR:LEN FX-U !
   FX-PATH$ FX$ FS:WRITE-ALL ;

\ The bytes of s end the fixture text, built in place for a fixture longer
\ than the string builder holds.
: FX+ ( str -- ) {: s:str :}
   FX-U @ s STR:LEN + CAP > IF E-STR-CAPACITY throw THEN
   s STR:DATA FX FX-U @ + s STR:LEN BYTE-COPY
   FX-U @ s STR:LEN + FX-U ! ;

\ PATH with every link resolved: the checker names a required file so, and
\ --verify-only its subject too.
: CANON$ ( str -- str )
   FS:>PATHZ CANON FS:PATHZ-CAP SPAN:MAKE realpath {: n:n :}
   n 0 <= IF E-FS-PATH throw THEN
   CANON n >str ;

\ The fixture becomes the required file later packets name.
: DEP! ( -- )
   FX DEP FX-U @ BYTE-COPY
   FX-U @ DEP-U !
   FX-PATH$ CANON$ {: path:str :}
   path STR:DATA DEP-P path STR:LEN BYTE-COPY
   path STR:LEN DEP-PU ! ;

: STORE! ( str len len PROCESS:ending -- )
   {: src:str outu:len erru:len oc :}
   outu LEN>N GT-OUT-U !
   erru LEN>N GT-ERR-U !
   oc MATCH PROCESS:ending
     exited OF RC ! ENDOF
     signaled OF drop -1 RC ! ENDOF
     timeout OF src GT-OUT$ GT-ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

\ LSP-CHECK:RUN throws its nonzero code on, as for the CLI's exit status, with
\ the same stdout and stderr writes captured in this process.
: CHECK-RUN ( -- )
   LSP-CHECK:RUN ;

\ Runs the check LSP-CHECK holds and asserts its status is WANT.
: CHECK-CAPTURE ( n -- ) {: want:n :}
   [: CHECK-RUN ;] BASE$ [: GT-OUT-ROOM ;] [: GT-ERR-ROOM ;] GT-CAPTURE-ACTION
   {: outu:len erru:len rc:n :}
   outu LEN>N GT-OUT-U !
   erru LEN>N GT-ERR-U !
   rc want T=
   GT-ERR$ GJA-SPLIT-LINES ;

: CHECK-EXIT ( str n -- )
   {: flag:str want:n :}
   LSP-CHECK:RESET
   flag STR:LEN 0 > IF flag STR:DATA 2 + flag STR:LEN 2 - >str LSP-CHECK:OPT THEN
   s" json-errors" LSP-CHECK:OPT
   FX-PATH$ LSP-CHECK:FILE
   want CHECK-CAPTURE ;

: CHECK ( str -- )
   70 CHECK-EXIT ;

: FIELD-MISSING ( str -- )
   {: key:str :}
   T-NEXT
   T-FAIL
   s" assert: missing JSON field " type key type cr ;

: FIELD-NOT ( str str -- )
   {: key:str kind:str :}
   T-NEXT
   T-FAIL
   s" assert: JSON field " type key type s"  is not " type kind type cr ;


: STR-FIELD= ( SPAN:span<u8> str str -- )
   {: root:SPAN:span<u8> key:str want:str :}
   root key GJA-FIELD? 0= IF drop key FIELD-MISSING EXIT THEN
   {: v:SPAN:span<u8> :}
   v GJA-KIND JSON:T-STR <> IF key s" a string" FIELD-NOT EXIT THEN
   v GJA-STRING$ want T$= ;


: INT-FIELD= ( SPAN:span<u8> str n -- )
   {: root:SPAN:span<u8> key:str want:n :}
   root key GJA-FIELD? 0= IF drop key FIELD-MISSING EXIT THEN
   {: v:SPAN:span<u8> :}
   v GJA-KIND JSON:T-INT <> IF key s" an integer" FIELD-NOT EXIT THEN
   v GJA-OPEN JSON:READ-INT {: got:n :} JSON:READ-CLOSE
   got want T= ;


: FIELD-ABSENT ( SPAN:span<u8> str -- )
   {: root:SPAN:span<u8> key:str :}
   root key GJA-FIELD? 0= IF drop T-NEXT EXIT THEN
   drop
   T-NEXT
   T-FAIL
   s" assert: unexpected JSON field " type key type cr ;


: FIELD= ( n str str -- )
   {: k:n key:str want:str :}
   GJA-LINE# @ k > dup TTRUE 0= IF EXIT THEN
   k GJA-LINE$ GJA-PARSE key want STR-FIELD= ;

: FIELD-HAS ( n str str -- )
   {: k:n key:str want:str :}
   GJA-LINE# @ k > dup TTRUE 0= IF EXIT THEN
   k GJA-LINE$ GJA-PARSE key GJA-FIELD? 0= IF drop key FIELD-MISSING EXIT THEN
   {: v:SPAN:span<u8> :}
   v GJA-KIND JSON:T-STR <> IF key s" a string" FIELD-NOT EXIT THEN
   v GJA-STRING$ want STR:CONTAINS? TTRUE ;

: NOTE= ( n str str -- )
   {: k:n key:str want:str :}
   GJA-LINE# @ k > dup TTRUE 0= IF EXIT THEN
   k GJA-LINE$ GJA-PARSE s" notes" GJA-FIELD? 0= IF drop s" notes" FIELD-MISSING EXIT THEN
   key want STR-FIELD= ;

: PLACED-IN ( n str n n n n str str -- )
   {: k:n sp:str line:n col:n bs:n be:n path:str text:str :}
   GJA-LINE# @ k > dup TTRUE 0= IF EXIT THEN
   k GJA-LINE$ GJA-PARSE
   {: root:SPAN:span<u8> :}
   root s" file" path STR-FIELD=
   root s" line" line INT-FIELD=
   root s" column" col INT-FIELD=
   root s" byte_start" bs INT-FIELD=
   root s" byte_end" be INT-FIELD=
   bs 0 >=  be bs >= and  be text STR:LEN <= and IF
      text STR:DATA bs + be bs - >str sp T$=
   ELSE
      T-NEXT s" expected span lies outside the fixture" T-ASSERT-DETAIL
   THEN ;

: AT-IN ( n str n n n n str str -- )
   {: k:n tok:str line:n col:n bs:n be:n path:str text:str :}
   k tok line col bs be path text PLACED-IN
   k s" token" tok FIELD= ;

: AT ( n str n n n n -- )
   FX-PATH$ FX$ AT-IN ;

: DEP-AT ( n str n n n n -- )
   DEP-PATH$ DEP$ AT-IN ;

: TEST-LINES ( -- )
   s" a body on the line after its name" T-LABEL
   SB-RESET
   s" : H5 ( n -- n )" SB-APPEND LF+
   s"    drop ;" SB-APPEND LF+
   s" lines.f" FIXTURE!
   s" " CHECK
   0 s" drop" 2 4 19 23 AT ;

: TEST-TAB ( -- )
   s" a tab counts one column" T-LABEL
   SB-RESET
   s" : T1 ( n -- n )" SB-APPEND LF+
   TAB+ s"   drop ;" SB-APPEND LF+
   s" tab.f" FIXTURE!
   s" " CHECK
   0 s" drop" 2 4 19 23 AT ;

: TEST-CRLF ( -- )
   s" CR LF ends a line at its LF" T-LABEL
   SB-RESET
   s" : C1 ( n -- n )" SB-APPEND CR+ LF+
   s"    drop ;" SB-APPEND CR+ LF+
   s" crlf.f" FIXTURE!
   s" " CHECK
   0 s" drop" 2 4 20 24 AT ;

: TEST-SPACES ( -- )
   s" runs of spaces between tokens" T-LABEL
   SB-RESET
   s" :   W1    ( -- )      1     NOPE-W1   drop ;" SB-APPEND LF+
   s" spaces.f" FIXTURE!
   s" " CHECK
   0 s" NOPE-W1" 1 29 28 35 AT ;

: TEST-COMMENTS ( -- )
   s" paren and backslash comments in the body" T-LABEL
   SB-RESET
   s" : K1 ( n -- n ) ( a comment ) " SB-APPEND
   92 SB-APPEND-C s"  line comment" SB-APPEND LF+
   s"   1 + " SB-APPEND 92 SB-APPEND-C s"  another" SB-APPEND LF+
   s"   ( more ) NOPE-K1 ;" SB-APPEND LF+
   s" comments.f" FIXTURE!
   s" " CHECK
   0 s" NOPE-K1" 3 12 72 79 AT ;

: TEST-STRING ( -- )
   s" a string with runs of spaces before the token" T-LABEL
   SB-RESET
   s" : Q1 ( -- ) s" SB-APPEND 34 SB-APPEND-C
   s"   two  spaces " SB-APPEND 34 SB-APPEND-C s"  2drop" SB-APPEND LF+
   s"    NOPE-Q1 ;" SB-APPEND LF+
   s" string.f" FIXTURE!
   s" " CHECK
   0 s" NOPE-Q1" 2 4 39 46 AT ;

: TEST-EMOJI-STRING ( -- )
   s" a multibyte character inside a string" T-LABEL
   SB-RESET
   s" : H4 ( n -- n ) s" SB-APPEND 34 SB-APPEND-C
   s"  " SB-APPEND $F0 SB-APPEND-C $9F SB-APPEND-C $98 SB-APPEND-C $80 SB-APPEND-C
   34 SB-APPEND-C s"  2drop drop ;" SB-APPEND LF+
   s" emoji.f" FIXTURE!
   s" " CHECK
   0 s" drop" 1 32 31 35 AT ;

\ Line 1 is a comment holding two- and four-byte characters; line 2 has a
\ two-byte character in a string and in the rejected token itself.
: TEST-UTF8 ( -- )
   s" multibyte characters before and inside the token" T-LABEL
   SB-RESET
   92 SB-APPEND-C s"  caf" SB-APPEND $C3 SB-APPEND-C $A9 SB-APPEND-C
   s"  " SB-APPEND $F0 SB-APPEND-C $9F SB-APPEND-C $98 SB-APPEND-C $80 SB-APPEND-C LF+
   s" : U8 ( -- ) s" SB-APPEND 34 SB-APPEND-C
   s"  " SB-APPEND $C3 SB-APPEND-C $A9 SB-APPEND-C s" t" SB-APPEND
   $C3 SB-APPEND-C $A9 SB-APPEND-C 34 SB-APPEND-C
   s"  2drop NOPE-" SB-APPEND $C3 SB-APPEND-C $A9 SB-APPEND-C s"  ;" SB-APPEND LF+
   s" utf8.f" FIXTURE!
   s" " CHECK
   SB-RESET s" NOPE-" SB-APPEND $C3 SB-APPEND-C $A9 SB-APPEND-C
   0 SB$ 2 29 41 48 AT ;

: TEST-PAREN-EMOJI ( -- )
   s" a multibyte character in a comment before the name" T-LABEL
   SB-RESET
   s" ( " SB-APPEND $F0 SB-APPEND-C $9F SB-APPEND-C $98 SB-APPEND-C $80 SB-APPEND-C
   s"  ) : H ( n -- n ) drop ;" SB-APPEND LF+
   s" paren-emoji.f" FIXTURE!
   s" " CHECK
   0 s" drop" 1 25 24 28 AT ;

: TEST-SIGNATURE-LINE ( -- )
   s" a signature on the line after its name" T-LABEL
   SB-RESET
   s" : S3" SB-APPEND LF+
   s"    ( n -- mystery ) ;" SB-APPEND LF+
   s" signature.f" FIXTURE!
   s" " CHECK
   0 s" mystery" 2 11 15 22 AT ;

: TEST-LATER ( -- )
   s" a definition after two that certify" T-LABEL
   SB-RESET
   s" : OK1 ( n -- n ) 1 + ;" SB-APPEND LF+ LF+
   s" : OK2 ( n -- n )" SB-APPEND LF+
   s"    2 * ;" SB-APPEND LF+
   s" : LATE ( -- )" SB-APPEND LF+
   s"    s" SB-APPEND 34 SB-APPEND-C s"  a b" SB-APPEND 34 SB-APPEND-C
   s"  2drop" SB-APPEND LF+
   s"    NOPE-LATE ;" SB-APPEND LF+
   s" later.f" FIXTURE!
   s" " CHECK
   0 s" NOPE-LATE" 7 4 84 93 AT ;

\ Packet 0 of the last run is the run stage's rejection CODE, placed at its
\ token in the file that holds it as a definition's is.
: TOP-AT ( str str n n n n str str -- )
   {: c:str tok:str line:n col:n bs:n be:n path:str text:str :}
   0 s" code" c FIELD=
   0 tok line col bs be path text AT-IN ;

: TEST-TOP-LINE ( -- )
   s" a top-level rejection after a definition is placed at its token" T-LABEL
   SB-RESET
   s" variable V" SB-APPEND LF+
   s" : F ( -- ) ;" SB-APPEND LF+ LF+ LF+
   s" here V !" SB-APPEND LF+
   s" top-line.f" FIXTURE!
   s" " CHECK
   s" E-RAW-CELL-PTR" s" !" 5 8 33 34 FX-PATH$ FX$ TOP-AT ;

\ The run stage labels the subject, and the label names that file alone: a
\ top-level rejection in a file the subject requires names the required file and
\ its own place.
: TEST-TOP-DEP ( -- )
   s" a top-level rejection in a required file names that file" T-LABEL
   SB-RESET
   s" variable DV" SB-APPEND LF+
   s" 1 drop" SB-APPEND LF+ LF+ LF+ LF+ LF+
   s" here DV !" SB-APPEND LF+
   s" top-dep.f" FIXTURE!
   DEP!
   SB-RESET
   s" require top-dep.f" SB-APPEND LF+
   s" : F ( -- ) ;" SB-APPEND LF+
   s" top-subject.f" FIXTURE!
   s" " CHECK
   s" E-RAW-CELL-PTR" s" !" 7 9 31 32 DEP-PATH$ DEP$ TOP-AT ;

: TEST-TOP-MISMATCH ( -- )
   s" a top-level mismatch has a null word" T-LABEL
   SB-RESET
   s" 1 0= 1 +" SB-APPEND LF+
   s" top-mismatch.f" FIXTURE!
   s" " CHECK
   s" E-MISMATCH" s" +" 1 8 7 8 FX-PATH$ FX$ TOP-AT ;

: TEST-TOP-PRIVATE ( -- )
   s" a top-level call of a primitive whose rows are private is undefined" T-LABEL
   SB-RESET
   s" 0 execute-floor drop" SB-APPEND LF+
   s" top-private.f" FIXTURE!
   s" " CHECK
   s" E-UNDEFINED" s" execute-floor" 1 3 2 15
   FX-PATH$ FX$ TOP-AT
   0 s" message" s" cannot find word `execute-floor`" FIELD= ;

: TEST-TOP-PRIVATE-TICK ( -- )
   s" a top-level tick of a primitive whose rows are private is undefined to the verifier" T-LABEL
   SB-RESET
   s" ' patch32 drop" SB-APPEND LF+
   s" top-private-tick.f" FIXTURE!
   s" --verify-only" CHECK
   s" E-UNDEFINED" s" patch32" 1 3 2 9
   FX-PATH$ FX$ TOP-AT ;

\ The public F moves other cells than its private twin: the rejection names the
\ private F as written, at the public definition's name.
: SHADOWED-FIXTURE ( -- )
   SB-RESET
   s" package SAT-A" SB-APPEND LF+
   s" : F ( n n -- n ) + ;" SB-APPEND LF+
   s" public" SB-APPEND LF+ LF+
   s" :   F ( n -- n )" SB-APPEND LF+
   s"    1 + ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" shadowed.f" FIXTURE! ;

: SHADOWED-AT ( str -- )
   {: path:str :}
   0 s" code" s" E-SHADOWED-ARITY" FIELD=
   0 s" message" s" private `F`" FIELD-HAS
   0 s" F" 5 5 47 48 path FX$ AT-IN ;

: TEST-SHADOWED-ARITY ( -- )
   s" a public whose private twin moves other cells" T-LABEL
   SHADOWED-FIXTURE
   s" " 70 CHECK-EXIT
   FX-PATH$ SHADOWED-AT
   s" --all-errors" 70 CHECK-EXIT
   FX-PATH$ SHADOWED-AT
   s" --verify-only" CHECK
   GJA-LINE# @ 1 T=
   FX-PATH$ CANON$ SHADOWED-AT ;

\ The name of the word `;FUNCTION` builds lies in no source.
: SHADOWED-FN-AT ( str -- )
   {: path:str :}
   0 s" code" s" E-SHADOWED-ARITY" FIELD=
   0 s" message" s" public `SFN:GP` has a different stack effect" FIELD-HAS
   0 s" word" s" GP" NOTE=
   0 s" ;FUNCTION" 7 1 104 113 path FX$ AT-IN ;

: TEST-SHADOWED-FUNCTION ( -- )
   s" a FUNCTION: word whose private twin moves other cells" T-LABEL
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND LF+
   s" package SFN" SB-APPEND LF+
   s" : GP ( -- ) ;" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" PROCESS-SYMBOLS" SB-APPEND LF+
   s" FUNCTION: GP getpid ( -- n )" SB-APPEND LF+
   s" ;FUNCTION" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" shadowed-fn.f" FIXTURE!
   s" " 70 CHECK-EXIT
   FX-PATH$ SHADOWED-FN-AT
   s" --verify-only" CHECK
   GJA-LINE# @ 1 T=
   FX-PATH$ CANON$ SHADOWED-FN-AT ;

\ A definer's rejection under --all-errors does not stop the definitions' check.
: TEST-ALL-RESERVED ( -- )
   s" a reserved definition name, then an undefined word, under --all-errors" T-LABEL
   SB-RESET
   s" : if ( -- ) ;" SB-APPEND LF+
   s" : foo ( -- ) undefined-word ;" SB-APPEND LF+
   s" all-reserved.f" FIXTURE!
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=
   0 s" code" s" E-RESERVED-DEFINITION" FIELD=
   0 s" message" s" `if` is a keyword and cannot name a definition" FIELD=
   0 s" if" 1 3 2 4 AT
   1 s" code" s" E-UNDEFINED" FIELD=
   1 s" message" s" cannot find word `undefined-word`" FIELD=
   1 s" undefined-word" 2 14 27 41 AT ;

\ Under --all-errors a form discovery rejects is reported where the composition
\ meets it, after the records before it.
: TEST-ALL-ORDER ( -- )
   s" an undefined word, then a retired loader word, under --all-errors" T-LABEL
   SB-RESET
   s" : RT-ONE ( -- ) nosuchword ;" SB-APPEND LF+
   s" undefine required" SB-APPEND LF+
   S\" s\" absent.f\" required" SB-APPEND LF+
   s" all-retire.f" FIXTURE!
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=
   0 s" code" s" E-UNDEFINED" FIELD=
   0 s" nosuchword" 1 17 16 26 AT
   1 s" code" s" E-LOADER-FORM" FIELD=
   1 s" required" 3 14 60 68 AT ;

\ Without --all-errors the pre-pass composition writes this record, which the
\ load does not give.
: TEST-DYN-PLACE ( -- )
   s" a dynamic required, checked alone" T-LABEL
   SB-RESET
   S\" : RT-P ( -- str ) s\" absent.f\" ;" SB-APPEND LF+
   s" RT-P required" SB-APPEND LF+
   s" dyn-place.f" FIXTURE!
   s" " CHECK
   GJA-LINE# @ 1 T=
   0 s" code" s" E-LOADER-FORM" FIELD=
   0 s" required" 2 6 38 46 AT ;

\ WIDE's inferred effect nests 33 binders, more than an effect text holds: the
\ checker certifies it and warns, at its name, that the effect is not recorded.
: NOT-RECORDED-TEXT ( -- )
   SB-RESET
   s" defer V-MK ( -- a )" SB-APPEND LF+
   s" : V-UN ( forall<p,[ a -- ]> -- a ) drop V-MK ;" SB-APPEND LF+
   s" :   WIDE" SB-APPEND LF+
   s"    V-UN V-UN V-UN V-UN V-UN V-UN V-UN V-UN V-UN V-UN V-UN" SB-APPEND LF+
   s"    V-UN V-UN V-UN V-UN V-UN V-UN V-UN V-UN V-UN V-UN V-UN" SB-APPEND LF+
   s"    V-UN V-UN V-UN V-UN V-UN V-UN V-UN V-UN V-UN V-UN V-UN ;" SB-APPEND LF+ ;

: NOT-RECORDED ( str -- )
   {: path:str :}
   0 s" code" s" W-EFFECT-NOT-RECORDED" FIELD=
   0 s" message" s" `WIDE`" FIELD-HAS
   0 s" WIDE" 3 5 71 75 path FX$ AT-IN
   1 begin dup GJA-LINE# @ < while
      dup GJA-LINE$ s" W-EFFECT-NOT-RECORDED" STR:CONTAINS? TFALSE
      1 +
   repeat drop ;

\ Check writes the warning once, placed in its file: the run on a pass, the
\ check before it when no run follows, as under --verify-only.
: TEST-NOT-RECORDED ( -- )
   s" an inferred effect not recorded" T-LABEL
   NOT-RECORDED-TEXT
   s" not-recorded.f" FIXTURE!
   s" " 0 CHECK-EXIT
   FX-PATH$ NOT-RECORDED
   s" --all-errors" 0 CHECK-EXIT
   FX-PATH$ NOT-RECORDED
   s" --verify-only" 0 CHECK-EXIT
   FX-PATH$ CANON$ NOT-RECORDED
   NOT-RECORDED-TEXT
   s" -1 throw" SB-APPEND LF+
   s" not-recorded-throw.f" FIXTURE!
   s" " EXIT-STATUS:UNCAUGHT CHECK-EXIT
   FX-PATH$ NOT-RECORDED
   s" --all-errors" EXIT-STATUS:UNCAUGHT CHECK-EXIT
   FX-PATH$ NOT-RECORDED ;

\ --verify-only reads a stored type on across lines while a `<` or `[` is
\ open, as the load does.
: TEST-STORED-SPAN ( -- )
   s" a stored type spelled across lines" T-LABEL
   SB-RESET
   s" NEWTYPE span-pair option<" SB-APPEND LF+
   s"   n>" SB-APPEND LF+
   s" TYPED-VARIABLE SPAN-SLOT [ n --" SB-APPEND LF+
   s"   n ]" SB-APPEND LF+
   s" stored-span.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT ;

\ The rejection's notes: an immediate in a body has the effect ( -- ), and
\ this one's is ( n -- ).
: IMM-EFFECT-NOTES ( -- )
   0 s" expected" s" ( -- )" NOTE=
   0 s" found" s" ( n -- )" NOTE= ;

\ --verify-only runs no definition, yet reads a source word made immediate as
\ the load does: a body that calls it is rejected at the call.
: TEST-SOURCE-IMMEDIATE ( -- )
   s" a source word made immediate" T-LABEL
   SB-RESET
   s" : W1 ( n -- ) drop ; immediate" SB-APPEND LF+
   s" : USE ( -- n ) W1 73 ;" SB-APPEND LF+
   s" source-immediate.f" FIXTURE!
   s" " CHECK
   0 s" code" s" E-UNMODELED-IMMEDIATE" FIELD=
   0 s" W1" 2 16 46 48 AT
   IMM-EFFECT-NOTES
   s" --verify-only" CHECK
   0 s" code" s" E-UNMODELED-IMMEDIATE" FIELD=
   0 s" W1" 2 16 46 48 FX-PATH$ CANON$ FX$ AT-IN
   IMM-EFFECT-NOTES
   \ An `immediate` after a SUMTYPE marks nothing: W0 stays an ordinary call.
   s" an immediate after a SUMTYPE" T-LABEL
   SB-RESET
   s" : W0 ( -- ) ;" SB-APPEND LF+
   s" SUMTYPE fold VARIANT plain ;VARIANT ;SUMTYPE" SB-APPEND LF+
   s" immediate" SB-APPEND LF+
   s" : USE ( -- n ) W0 73 ;" SB-APPEND LF+
   s" sum-immediate.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   \ An `immediate` after `undefine W` marks nothing, so the new W is ordinary.
   s" an immediate after an undefine" T-LABEL
   SB-RESET
   s" : W ( -- n ) 5 ;" SB-APPEND LF+
   s" undefine W" SB-APPEND LF+
   s" immediate" SB-APPEND LF+
   s" : W ( -- n ) 6 ;" SB-APPEND LF+
   s" : USE ( -- n ) W ;" SB-APPEND LF+
   s" USE ." SB-APPEND LF+
   s" undefine-immediate.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   \ `undefine` ends the immediacy of the word it retires.
   s" a word made immediate, then undefined" T-LABEL
   SB-RESET
   s" : W ( -- ) 7 . ;" SB-APPEND LF+
   s" immediate" SB-APPEND LF+
   s" undefine W" SB-APPEND LF+
   s" : W ( -- n ) 6 ;" SB-APPEND LF+
   s" : USE ( -- n ) W ;" SB-APPEND LF+
   s" USE ." SB-APPEND LF+
   s" undefined-immediate.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT ;

\ --verify-only rejects an `undefine` of a word the package scope does not
\ define, as the load does.
: TEST-UNDEFINE-ABSENT ( -- )
   s" an undefine of a word outside the package scope" T-LABEL
   SB-RESET
   s" : W ( -- n ) 1 ;" SB-APPEND LF+
   s" package P" SB-APPEND LF+
   s" undefine W" SB-APPEND LF+
   s" : G ( -- n ) W ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" undefine-absent.f" FIXTURE!
   s" --verify-only" CHECK
   0 s" code" s" E-UNDEFINED" FIELD=
   0 s" message" s" is not defined here, so `undefine` cannot retire it" FIELD-HAS
   0 s" W" 3 10 36 37 FX-PATH$ CANON$ FX$ AT-IN ;

\ --verify-only leaves an `undefine` of a word rendered text defined to the
\ run, which retires it.
: TEST-UNDEFINE-RENDERED ( -- )
   s" an undefine of a word an evaluate defined" T-LABEL
   SB-RESET
   s\" s\" : W ( -- n ) 1 ;\" evaluate" SB-APPEND LF+
   s" undefine W" SB-APPEND LF+
   s" undefine-rendered.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   0 s" code" s" W-CHECK-DEFERRED" FIELD=
   GJA-LINE# @ 2 T=
   1 s" code" s" W-CHECK-DEFERRED" FIELD=
   1 s" message" s" rendered source may change what it does" FIELD-HAS
   1 s" W" 2 10 39 40 FX-PATH$ CANON$ FX$ AT-IN ;

\ A quotation that reads a local, stored where it outlives the local, differs
\ from its slot only in the locals it reads, which no rendered row shows: the
\ E-LOCAL-ESCAPE writes no expected or found rows that would read the same.
: TEST-LOCAL-ESCAPE-ROWS ( -- )
   s" a stored quotation that reads a local" T-LABEL
   SB-RESET
   s" TYPED-VARIABLE V [ -- ]" SB-APPEND LF+
   s" : RUN ( [ -- ] -- ) V ! ;" SB-APPEND LF+
   s" : PUT ( n -- ) {: s :} [: s drop ;] RUN ;" SB-APPEND LF+
   s" local-escape-rows.f" FIXTURE!
   s" --verify-only" CHECK
   0 s" code" s" E-LOCAL-ESCAPE" FIELD=
   0 s" RUN" 3 37 86 89 AT
   GJA-LINE# @ 0 > dup TTRUE 0= IF EXIT THEN
   0 GJA-LINE$ GJA-PARSE s" notes" GJA-FIELD? 0= IF drop EXIT THEN
   {: notes:SPAN:span<u8> :}
   notes s" expected" FIELD-ABSENT
   notes s" found" FIELD-ABSENT ;

\ --verify-only fails a body whose trusted-only tick it meets under unknown
\ order: it stops there.
: TEST-TICK-STOPPED ( -- )
   s" a trusted-only tick under unknown order" T-LABEL
   SB-RESET
   s" : IM ( -- ) ; immediate" SB-APPEND LF+
   s" : F ( -- ) IM ['] ffi-call drop ;" SB-APPEND LF+
   s" tick-stopped.f" FIXTURE!
   s" --verify-only" 1 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" code" s" W-CHECK-STOPPED" FIELD=
   0 s" ffi-call" 2 19 42 50 FX-PATH$ CANON$ FX$ AT-IN ;

\ A program's word of a target predicate's spelling is no engine answer to
\ --verify-only: its `[if]` is left to the run, as any other flag is.
: TEST-PROGRAM-TARGET-WORD ( -- )
   s" a package's word of a target predicate's spelling" T-LABEL
   SB-RESET
   s" package P" SB-APPEND LF+
   s" : HB-TARGET-LINUX? ( -- bool ) false ;" SB-APPEND LF+
   s" HB-TARGET-LINUX? [if] require target-absent.f [then]" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" package-target.f" FIXTURE!
   s" --verify-only" 1 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" code" s" W-CHECK-STOPPED" FIELD=
   0 s" [if]" 3 18 66 70 FX-PATH$ CANON$ FX$ AT-IN
   s" a target predicate undefined and redefined" T-LABEL
   SB-RESET
   s" undefine HB-TARGET-LINUX?" SB-APPEND LF+
   s" : HB-TARGET-LINUX? ( -- bool ) false ;" SB-APPEND LF+
   s" HB-TARGET-LINUX? 0= [if] require target-absent.f [then]" SB-APPEND LF+
   s" redefined-target.f" FIXTURE!
   s" --verify-only" 1 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" code" s" W-CHECK-STOPPED" FIELD=
   0 s" [if]" 3 21 85 89 FX-PATH$ CANON$ FX$ AT-IN ;

\ The fixture's one rejection, E-PARSES-ROW at TOK, under the check and under
\ --verify-only.
: ROW-REJECTED ( str n n n n -- )
   {: tok:str line:n col:n bs:n be:n :}
   s" " CHECK
   GJA-LINE# @ 1 T=
   0 s" code" s" E-PARSES-ROW" FIELD=
   0 tok line col bs be AT
   s" --verify-only" CHECK
   GJA-LINE# @ 1 T=
   0 s" code" s" E-PARSES-ROW" FIELD=
   0 tok line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

: TEST-PARSES-REJECT ( -- )
   s" a parses: row naming an undefined word" T-LABEL
   SB-RESET
   s" parses: NOPE 1" SB-APPEND LF+
   s" parses-undefined.f" FIXTURE!
   s" NOPE" 1 9 8 12 ROW-REJECTED
   s" a parses: row with a negative count" T-LABEL
   SB-RESET
   s" parses: W -1" SB-APPEND LF+
   s" parses-count.f" FIXTURE!
   s" W" 1 9 8 9 ROW-REJECTED ;

\ A rejected `generates:` row is the verifier's own packet.
: TEST-GENERATES ( -- )
   s" a generates: row naming no word" T-LABEL
   SB-RESET
   s" : OK1 ( n -- n )" SB-APPEND LF+
   s"    1 + ;" SB-APPEND LF+
   92 SB-APPEND-C s"  a row for a word nothing defines" SB-APPEND LF+
   s" generates:  NOWHERE ( -- n )" SB-APPEND LF+
   s" generates.f" FIXTURE!
   s" " CHECK
   GJA-LINE# @ 1 T=
   0 s" NOWHERE" 4 13 73 80 AT
   s" --all-errors" CHECK
   GJA-LINE# @ 1 T=
   0 s" NOWHERE" 4 13 73 80 AT
   s" --verify-only" CHECK
   0 s" NOWHERE" 4 13 73 80 FX-PATH$ CANON$ FX$ AT-IN ;

\ A row on a word that reads no name after it is rejected: kept, it makes the
\ pre-pass take the token after the call of GEN as the made word.
: TEST-GENERATES-READS-NONE ( -- )
   s" a generates: row on a word that reads no name" T-LABEL
   SB-RESET
   s" : GEN ( -- ) s" SB-APPEND 34 SB-APPEND-C
   s"  : MADE ( -- ) ;" SB-APPEND 34 SB-APPEND-C s"  evaluate-closed ;" SB-APPEND LF+
   s" generates: GEN ( -- )" SB-APPEND LF+
   s" GEN" SB-APPEND LF+
   s" MADE" SB-APPEND LF+
   s" generates-reads-none.f" FIXTURE!
   s" " CHECK
   GJA-LINE# @ 1 T=
   0 s" code" s" E-GENERATES-ROW" FIELD=
   0 s" GEN" 2 12 62 65 AT
   s" --all-errors" CHECK
   0 s" code" s" E-GENERATES-ROW" FIELD=
   0 s" GEN" 2 12 62 65 AT
   s" --verify-only" CHECK
   0 s" code" s" E-GENERATES-ROW" FIELD=
   0 s" GEN" 2 12 62 65 FX-PATH$ CANON$ FX$ AT-IN ;

\ compose-base.f certifies. It is longer than the files that require it and its
\ lines are short, so a packet located in its bytes by mistake lands on another
\ line and column than its own.
: BASE-FIXTURE ( -- )
   SB-RESET
   s" : BASE-ONE ( n -- n )" SB-APPEND LF+
   7 0 ?do s"    1 +" SB-APPEND LF+ loop
   s"    1 + ;" SB-APPEND LF+
   s" compose-base.f" FIXTURE! ;

: COMPOSE-FIXTURE ( -- )
   BASE-FIXTURE
   SB-RESET
   s" require compose-base.f" SB-APPEND LF+ LF+
   s" : DEP-BAD ( n -- n )" SB-APPEND LF+
   s"    drop ;" SB-APPEND LF+
   s" compose-dep.f" FIXTURE!
   DEP!
   SB-RESET
   s" require compose-dep.f" SB-APPEND LF+
   s" : PARENT-BAD ( n -- n )" SB-APPEND LF+
   s"    drop ;" SB-APPEND LF+
   s" compose.f" FIXTURE! ;

: TEST-COMPOSE-DEP ( -- )
   s" a packet in a required file" T-LABEL
   COMPOSE-FIXTURE
   s" " CHECK
   GJA-LINE# @ 1 T=
   0 s" drop" 4 4 48 52 DEP-AT ;

: TEST-COMPOSE-ALL ( -- )
   s" --all-errors past a required file's rejection to the subject's" T-LABEL
   COMPOSE-FIXTURE
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=
   0 s" drop" 4 4 48 52 DEP-AT
   1 s" drop" 3 4 49 53 AT ;

: TEST-COMPOSE-VERIFY ( -- )
   s" --verify-only, the check a language server makes" T-LABEL
   COMPOSE-FIXTURE
   s" --verify-only" CHECK
   GJA-LINE# @ 2 T=
   0 s" drop" 4 4 48 52 DEP-AT
   1 s" drop" 3 4 49 53 FX-PATH$ CANON$ FX$ AT-IN ;

: TEST-DECLARATION ( -- )
   s" a declaration packet" T-LABEL
   SB-RESET
   s" SUMTYPE bad-sum" SB-APPEND LF+
   s"    VARIANT Up" SB-APPEND LF+
   s" ;SUMTYPE" SB-APPEND LF+
   s" declaration.f" FIXTURE!
   s" " CHECK
   0 s" Up" 2 12 27 29 AT ;

\ A SUMTYPE in a private section defines HUE-RED for its variant red.
: HUE-FIXTURE ( str -- )
   {: tail:str :}
   SB-RESET
   s" package HUES" SB-APPEND LF+
   s" : HUE-RED ( -- n ) 1 ;" SB-APPEND LF+
   s" SUMTYPE hue VARIANT red ;VARIANT VARIANT blue ;VARIANT ;SUMTYPE"
   SB-APPEND LF+
   tail STR:LEN 0 > IF tail SB-APPEND LF+ THEN
   s" ;package" SB-APPEND LF+
   s" sum-generated.f" FIXTURE! ;

\ A STRUCTURE in a private section defines CKDPROD-MAKE.
: CKDPROD-FIXTURE ( str -- )
   {: tail:str :}
   SB-RESET
   s" package CKDPRODS" SB-APPEND LF+
   s" : CKDPROD-MAKE ( n -- n ) 1 + ;" SB-APPEND LF+
   s" STRUCTURE ckdprod FIELD x n ;STRUCTURE" SB-APPEND LF+
   tail STR:LEN 0 > IF tail SB-APPEND LF+ THEN
   s" ;package" SB-APPEND LF+
   s" prod-generated.f" FIXTURE! ;

\ The generated name lies in no source text: the record is placed at the
\ family's name and names the word in its `word` note.
: GENERATED-AT ( str -- )
   {: ws:str :}
   0 s" code" s" E-BAD-DECLARATION" FIELD=
   0 s" label" s" generated declaration already defined" FIELD=
   0 s" word" ws NOTE= ;

\ The rejected family registers no type for a later use.
: TEST-GENERATED-NAME ( -- )
   s" a generated name already defined" T-LABEL
   s" " HUE-FIXTURE
   s" " CHECK
   GJA-LINE# @ 1 T=  s" HUE-RED" GENERATED-AT  0 s" hue" 3 9 44 47 AT
   s" : HUE-USE ( hue -- ) drop ;" HUE-FIXTURE
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=  s" HUE-RED" GENERATED-AT  0 s" hue" 3 9 44 47 AT
   1 s" code" s" E-UNKNOWN-SIGNATURE-TYPE" FIELD=
   1 s" hue" 4 13 112 115 AT
   s" a structure's generated name already defined" T-LABEL
   s" " CKDPROD-FIXTURE
   s" " CHECK
   GJA-LINE# @ 1 T=  s" CKDPROD-MAKE" GENERATED-AT  0 s" ckdprod" 3 11 59 66 AT
   s" : CKDPROD-USE ( ckdprod -- ) drop ;" CKDPROD-FIXTURE
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=  s" CKDPROD-MAKE" GENERATED-AT  0 s" ckdprod" 3 11 59 66 AT
   1 s" code" s" E-UNKNOWN-SIGNATURE-TYPE" FIELD=
   1 s" ckdprod" 4 17 104 111 AT ;

\ A malformed parameter list is reported at its complete header.
: TEST-NEWTYPE ( -- )
   s" a NEWTYPE packet at its family name" T-LABEL
   SB-RESET
   s" : N1 ( -- ) ;" SB-APPEND LF+
   s" NEWTYPE plain<>" SB-APPEND LF+
   s" newtype.f" FIXTURE!
   s" " CHECK
   0 s" plain<>" 2 9 22 29 AT ;

: TEST-ENUM ( -- )
   s" an ENUM declaration packet" T-LABEL
   SB-RESET
   s" ENUM colour" SB-APPEND LF+
   s"    red red ;ENUM" SB-APPEND LF+
   s" enum.f" FIXTURE!
   s" " CHECK
   0 s" red" 2 8 19 22 AT ;

\ A close-stage fault names the family, read before the body.
: TEST-ENUM-CLOSE ( -- )
   s" an ENUM packet at its family name" T-LABEL
   SB-RESET
   s" ENUM shade light dark ;ENUM" SB-APPEND LF+
   s" ENUM colour" SB-APPEND LF+
   s" ;ENUM" SB-APPEND LF+
   s" enum-close.f" FIXTURE!
   s" " CHECK
   0 s" colour" 2 6 33 39 AT ;

: TEST-STRUCTURE ( -- )
   s" a STRUCTURE declaration packet" T-LABEL
   SB-RESET
   s" STRUCTURE pair" SB-APPEND LF+
   s"    FIELD size n" SB-APPEND LF+
   s"    FIELD size n" SB-APPEND LF+
   s" ;STRUCTURE" SB-APPEND LF+
   s" structure.f" FIXTURE!
   s" " CHECK
   0 s" size" 3 10 40 44 AT ;

: LINE-FIXTURE ( str str -- )
   {: text:str name:str :}
   SB-RESET
   text SB-APPEND LF+
   name FIXTURE! ;

: REJECTED-AS ( str -- )
   {: c:str :}
   GJA-LINE# @ 1 T=
   0 s" code" c FIELD= ;

\ The composition check gives one packet, a rejection. Plain preverification
\ uses this same composition verifier; REJECTED-MODES keeps representative
\ checks across the mode boundary.
: REJECTED-AT ( str str n n n n -- )
   {: c:str tok:str line:n col:n bs:n be:n :}
   s" --all-errors" CHECK
   c REJECTED-AS
   0 tok line col bs be AT ;

: REJECTED-MODES-RC ( str str n n n n n -- )
   {: c:str tok:str line:n col:n bs:n be:n rc:n :}
   s" " rc CHECK-EXIT
   c REJECTED-AS
   0 tok line col bs be AT
   s" --all-errors" rc CHECK-EXIT
   c REJECTED-AS
   0 tok line col bs be AT
   s" --verify-only" rc CHECK-EXIT
   c REJECTED-AS
   0 tok line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

: REJECTED-MODES ( str str n n n n -- )
   70 REJECTED-MODES-RC ;

\ A named file that will not open is one record, which no source places: its
\ message carries the reason the open failed.
: TEST-ALL-UNREADABLE ( -- )
   s" a named file that will not open, under --all-errors" T-LABEL
   SB-RESET  s" 1 drop" SB-APPEND LF+
   s" all-locked.f" FIXTURE!
   FX-PATH$ 0 FS:CHMOD-MODE
   [: s" --all-errors" 74 CHECK-EXIT ;]
   [: FX-PATH$ $1A4 FS:CHMOD-MODE ;] finally
   s" E-UNREADABLE-SOURCE" REJECTED-AS
   SB-RESET  s" cannot read `" SB-APPEND  FX-PATH$ CANON$ SB-APPEND
   s" `: permission denied" SB-APPEND
   0 s" message" SB$ FIELD=
   SB-RESET  s" if the file is yours, make it readable with `chmod u+r '" SB-APPEND
   FX-PATH$ CANON$ SB-APPEND  s" '`; else ask its owner to let you read it" SB-APPEND
   0 s" help" SB$ FIELD=
   GJA-LINE# @ 0 > dup TTRUE 0= IF EXIT THEN
   0 GJA-LINE$ GJA-PARSE s" notes" FIELD-ABSENT ;

\ Runs q while the file DEP! kept will not open.
: LOCKED ( [ -- ] -- ) {: q :}
   DEP-PATH$ 0 FS:CHMOD-MODE
   q [: DEP-PATH$ $1A4 FS:CHMOD-MODE ;] finally ;

\ No suite host is Wasm, so these arms are skipped and a check meets nothing in
\ them, as the load does.
: TEST-ALL-DEAD-ARM ( -- )
   s" a missing file only a skipped arm requires, under --all-errors" T-LABEL
   SB-RESET
   s" : DA-ONE ( -- ) nosuchone ;" SB-APPEND LF+
   s" HB-TARGET-WASM? [if] require deadarm-absent.f [then]" SB-APPEND LF+
   s" : DA-TWO ( -- ) nosuchtwo ;" SB-APPEND LF+
   s" all-dead-arm.f" FIXTURE!
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=
   0 s" code" s" E-UNDEFINED" FIELD=
   0 s" nosuchone" 1 17 16 25 AT
   1 s" code" s" E-UNDEFINED" FIELD=
   1 s" nosuchtwo" 3 17 97 106 AT
   SB-RESET  s" 1 drop" SB-APPEND LF+
   s" dead-locked.f" FIXTURE!
   DEP!
   s" a skipped arm's missing file before an unreadable one, under --all-errors" T-LABEL
   SB-RESET
   s" : R1 ( -- ) nosuchone ;" SB-APPEND LF+
   s" HB-TARGET-WASM? [if] require dead-io-absent.f [then]" SB-APPEND LF+
   s" require dead-locked.f" SB-APPEND LF+
   s" : R2 ( -- ) nosuchtwo ;" SB-APPEND LF+
   s" dead-io.f" FIXTURE!
   [: s" " CHECK ;] LOCKED
   s" E-UNDEFINED" REJECTED-AS
   0 s" nosuchone" 1 13 12 21 AT
   [: s" --all-errors" 74 CHECK-EXIT ;] LOCKED
   GJA-LINE# @ 2 T=
   0 s" code" s" E-UNDEFINED" FIELD=
   0 s" nosuchone" 1 13 12 21 AT
   1 s" code" s" E-UNREADABLE-SOURCE" FIELD=
   1 s" dead-locked.f" 3 9 85 98 AT
   s" an unclosed locals group in a skipped arm, under --all-errors" T-LABEL
   SB-RESET
   s" : G1 ( -- ) nosuchone ;" SB-APPEND LF+
   s" HB-TARGET-WASM? [if] : G3 ( -- ) {: a:n [then]" SB-APPEND LF+
   s" : G2 ( -- ) nosuchtwo ;" SB-APPEND LF+
   s" dead-locals.f" FIXTURE!
   s" " CHECK
   s" E-UNDEFINED" REJECTED-AS
   0 s" nosuchone" 1 13 12 21 AT
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=
   0 s" code" s" E-UNDEFINED" FIELD=
   0 s" nosuchone" 1 13 12 21 AT
   1 s" code" s" E-UNDEFINED" FIELD=
   1 s" nosuchtwo" 3 13 83 92 AT ;

\ A word ARENA derives that is taken in the other section is the duplicate,
\ rejected at the declared name.
: TEST-ARENA ( -- )
   s" an ARENA whose word is taken in the private section" T-LABEL
   SB-RESET
   s" package P" SB-APPEND LF+
   s" : X-NEW-DISPOSE ( -- ) ;" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" ARENA X n" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" arena-taken.f" FIXTURE!
   s" " 70 CHECK-EXIT
   s" E-DUPLICATE-DEFINITION" REJECTED-AS  0 s" X" 4 7 48 49 AT
   s" --all-errors" CHECK
   s" E-DUPLICATE-DEFINITION" REJECTED-AS  0 s" X" 4 7 48 49 AT
   s" --verify-only" CHECK
   s" E-DUPLICATE-DEFINITION" REJECTED-AS
   0 s" X" 4 7 48 49 FX-PATH$ CANON$ FX$ AT-IN
   s" an ARENA whose element type may hold a linear value" T-LABEL
   SB-RESET
   s" package P" SB-APPEND LF+
   s" DEFLINEAR lin" SB-APPEND LF+
   s" ARENA X [ lin -- ]" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" arena-linear.f" FIXTURE!
   s" E-BAD-STORAGE" s" [ lin -- ]" 3 9 32 42 REJECTED-MODES
   s" an ARENA whose element type hands out an atom" T-LABEL
   SB-RESET
   s" package P" SB-APPEND LF+
   s" ARENA X [ -- xt ]" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" arena-atom.f" FIXTURE!
   s" E-BAD-STORAGE" s" [ -- xt ]" 2 9 18 27 REJECTED-MODES ;

\ A storage type that does not parse is rejected at the token the parse stopped
\ on, or on the type where the parse ran off its end.
: TEST-STORAGE-MALFORMED ( -- )
   s" a storage type that does not parse is rejected at the token the parse stopped on" T-LABEL
   SB-RESET
   s" package P" SB-APPEND LF+
   s" TYPED-VARIABLE XB [ n n ]" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" typed-var-malformed.f" FIXTURE!
   s" E-BAD-STORAGE" s" ]" 2 25 34 35 REJECTED-MODES
   0 s" label" s" the type does not parse here" FIELD=
   s" a bare `^` storage type is rejected at the `^`" T-LABEL
   SB-RESET
   s" package P" SB-APPEND LF+
   s" TYPED-VARIABLE XB ^" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" typed-var-bare-ptr.f" FIXTURE!
   s" E-BAD-STORAGE" s" ^" 2 19 28 29 REJECTED-MODES
   0 s" label" s" `^` with no element type" FIELD= ;

\ Under --verify-only the declaration pre-pass alone rejects the fixture; the
\ record's label says why.
: PREPASS-REJECTED ( str str str n n n n -- )
   {: c:str why:str tok:str line:n col:n bs:n be:n :}
   s" --verify-only" CHECK
   c REJECTED-AS
   0 s" label" why FIELD=
   0 tok line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

: TEST-PREPASS-NOMINAL ( -- )
   s" a reserved DEFLINEAR name under --verify-only" T-LABEL
   s" DEFLINEAR n" s" deflinear-reserved.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" already a type or reserved" s" n" 1 11 10 11 PREPASS-REJECTED
   s" a keyword DEFLINEAR name under --verify-only" T-LABEL
   s" DEFLINEAR if" s" deflinear-keyword.f" LINE-FIXTURE
   s" E-RESERVED-DEFINITION" s" reserved keyword" s" if" 1 11 10 12 PREPASS-REJECTED
   GJA-LINE# @ 1 T=
   s" VALUE-RECORD if x n END-VALUE-RECORD" s" vrec-keyword.f" LINE-FIXTURE
   s" E-RESERVED-DEFINITION" s" reserved keyword" s" if" 1 14 13 15 PREPASS-REJECTED
   GJA-LINE# @ 1 T=
   s" a reserved DEFTYPE name under --verify-only, at the name it folds" T-LABEL
   s" DEFTYPE N" s" deftype-reserved.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" reserved name" s" N" 1 9 8 9 PREPASS-REJECTED
   0 s" message" s" `n`" FIELD-HAS
   s" a DEFTYPE name with parameters under --verify-only, and no converters after it" T-LABEL
   s" DEFTYPE PTR<a>" s" deftype-ptr-params.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" reserved name" s" PTR" 1 9 8 11 PREPASS-REJECTED
   s" an upper-case NEWTYPE name with parameters, at its bare name" T-LABEL
   s" NEWTYPE Foo<a>" s" newtype-case-params.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" Foo" 1 9 8 11 REJECTED-MODES
   s" a NEWTYPE header with parameters and no name, at the header" T-LABEL
   s" NEWTYPE <a>" s" newtype-params-no-name.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" <a>" 1 9 8 11 REJECTED-MODES
   s" a duplicate VALUE-RECORD field under --verify-only" T-LABEL
   s" VALUE-RECORD vr x n x n END-VALUE-RECORD" s" value-record-dup.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" declared again" s" x" 1 21 20 21 PREPASS-REJECTED
   s" a duplicate VALUE-RECORD field under --all-errors" T-LABEL
   s" --all-errors" CHECK
   s" E-BAD-DECLARATION" REJECTED-AS
   0 s" x" 1 21 20 21 AT
   0 s" label" s" declared again" FIELD=
   s" a duplicate VALUE-RECORD field, then an undefined word, under --all-errors" T-LABEL
   SB-RESET
   s" VALUE-RECORD vr x n x n END-VALUE-RECORD" SB-APPEND LF+
   s" : VR-USE ( -- ) zork ;" SB-APPEND LF+
   s" value-record-dup-then.f" FIXTURE!
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=
   0 s" code" s" E-BAD-DECLARATION" FIELD=
   0 s" x" 1 21 20 21 AT
   1 s" code" s" E-UNDEFINED" FIELD=
   1 s" zork" 2 17 57 61 AT ;

: TEST-PREPASS-OPEN ( -- )
   s" an unclosed VALUE-RECORD under --verify-only" T-LABEL
   s" VALUE-RECORD vr x n" s" vrec-open.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" source ends first" s" vr" 1 14 13 15 PREPASS-REJECTED
   s" an unclosed SUMTYPE under --verify-only" T-LABEL
   s" SUMTYPE shape" s" sum-open.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" missing ;SUMTYPE" s" shape" 1 9 8 13 PREPASS-REJECTED
   s" an unclosed ENUM under --verify-only" T-LABEL
   s" ENUM color red green" s" enum-open.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" missing ;ENUM" s" color" 1 6 5 10 PREPASS-REJECTED
   s" an unclosed STRUCTURE under --verify-only" T-LABEL
   s" STRUCTURE rec FIELD id n" s" struct-open.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" missing ;STRUCTURE" s" rec" 1 11 10 13 PREPASS-REJECTED ;

\ After GEN's call may have made types, the check leaves the missing widget to
\ the run; packet 0 is the deferral at that call.
: TEST-SUM-PAYLOAD-DEFER ( -- )
   s" a SUMTYPE payload's deferral at the parameter inside its token" T-LABEL
   SB-RESET
   s" : GEN ( -- ) s" SB-APPEND 34 SB-APPEND-C
   s"  : MADE ( -- ) ;" SB-APPEND 34 SB-APPEND-C s"  evaluate-closed ;" SB-APPEND LF+
   s" GEN" SB-APPEND LF+
   s" SUMTYPE boxy<x> VARIANT full x ;VARIANT ;SUMTYPE" SB-APPEND LF+
   s" SUMTYPE s15" SB-APPEND LF+
   s"   VARIANT one n ;VARIANT" SB-APPEND LF+
   s"   VARIANT two boxy<widget> ;VARIANT" SB-APPEND LF+
   s" ;SUMTYPE" SB-APPEND LF+
   s" sum-payload-defer.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 2 T=
   1 s" code" s" W-CHECK-DEFERRED" FIELD=
   1 s" widget" 6 20 160 166 FX-PATH$ CANON$ FX$ AT-IN ;

: TWO-LINE-FIXTURE ( str str str -- )
   {: one:str two:str name:str :}
   SB-RESET
   one SB-APPEND LF+
   two SB-APPEND LF+
   name FIXTURE! ;

\ A VALUE-RECORD reads its fields with `parse-name`, so a `\` or `(` there is the token it rejects.
: TEST-PREPASS-COMMENT ( -- )
   s" a comment holding a SUMTYPE's terminator" T-LABEL
   \ A commented `;SUMTYPE` closes nothing: the sum runs to the input's end.
   s" SUMTYPE shape VARIANT dot ;VARIANT \ ;SUMTYPE" s" sum-comment-end.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" shape" 1 9 8 13 REJECTED-MODES
   0 s" label" s" missing ;SUMTYPE" FIELD=
   s" a comment in a closed VALUE-RECORD" T-LABEL
   s" VALUE-RECORD vr x n \ note" s" END-VALUE-RECORD" s" vrec-comment.f"
      TWO-LINE-FIXTURE
   s" E-BAD-DECLARATION" s" \" 1 21 20 21 REJECTED-MODES
   0 s" label" s" a comment, not a name" FIELD=
   s" a closed paren comment in a VALUE-RECORD" T-LABEL
   s" VALUE-RECORD vr x n ( note )" s" END-VALUE-RECORD" s" vrec-paren.f"
      TWO-LINE-FIXTURE
   s" E-BAD-DECLARATION" s" (" 1 21 20 21 REJECTED-MODES
   0 s" label" s" a signature, not a name" FIELD= ;

: TEST-PREPASS-NAME-COMMENT ( -- )
   s" a comment before an ENUM's name" T-LABEL
   s" ENUM \ note" s" colr red green ;ENUM" s" enum-name-comment.f"
      TWO-LINE-FIXTURE
   s" E-BAD-DECLARATION" s" \" 1 6 5 6 REJECTED-MODES
   0 s" label" s" name must be a lowercase family tail" FIELD=
   s" a comment as a typed variable's name" T-LABEL
   s" TYPED-VARIABLE \ n" s" tvar-name-comment.f" LINE-FIXTURE
   s" E-MISSING-NAME" s" \" 1 16 15 16 REJECTED-MODES
   0 s" label" s" a comment, not a name" FIELD= ;

\ The load compiles a body before it checks the definition at `;`, so a body
\ word it cannot compile, undefined or a local it cannot bind, rejects a
\ definition whose signature does not parse, at that word; any other error in
\ the body leaves the rejection to the signature, at its type.
: TEST-BAD-SIGNATURE ( -- )
   s" a signature that does not parse and an undefined word" T-LABEL
   s" : X ( n -- zz ) NOPE ;" s" badsig-undefined.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 17 16 20 REJECTED-MODES
   s" that signature alone" T-LABEL
   s" : X ( n -- zz ) 1 ;" s" badsig.f" LINE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 1 12 11 13 REJECTED-MODES
   s" that undefined word alone" T-LABEL
   s" : X ( n -- n ) NOPE ;" s" undefined.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 16 15 19 REJECTED-AT
   s" that signature and an unsafe word" T-LABEL
   s" : X ( n -- zz ) evaluate ;" s" badsig-unsafe.f" LINE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 1 12 11 13 REJECTED-AT
   s" that signature and a quotation local" T-LABEL
   s" : X ( n -- zz ) [: {: y:n :} ;] drop 1 ;" s" badsig-local.f" LINE-FIXTURE
   s" E-BAD-LOCAL-SHAPE" s" {:" 1 20 19 21 REJECTED-MODES ;

\ The load binds `--` and `|` in a locals group as locals; the checker rejects
\ the definition at that token with the reason, and a body reference to it
\ keeps that rejection rather than naming an undefined word.
: TEST-LOCAL-GROUP ( -- )
   s" -- in a locals group" T-LABEL
   s" : X ( n -- n ) {: a -- b :} a ;" s" local-dash.f" LINE-FIXTURE
   s" E-BAD-LOCAL-SHAPE" s" --" 1 21 20 22 REJECTED-MODES
   0 s" message" s" a locals group names only locals; the signature declares the outputs" FIELD=
   s" | in a locals group" T-LABEL
   s" : X ( n n -- n ) {: a | :} a ;" s" local-bar.f" LINE-FIXTURE
   s" E-BAD-LOCAL-SHAPE" s" |" 1 23 22 23 REJECTED-MODES
   0 s" message" s" a locals group names only locals; Habu has no uninitialised locals" FIELD=
   s" a body reference to -- from a locals group" T-LABEL
   s" : X ( n -- n ) {: a -- b :} -- ;" s" local-dash-ref.f" LINE-FIXTURE
   s" E-BAD-LOCAL-SHAPE" s" --" 1 21 20 22 REJECTED-MODES
   s" a body reference to | from a locals group" T-LABEL
   s" : X ( n n -- n ) {: a | :} | ;" s" local-bar-ref.f" LINE-FIXTURE
   s" E-BAD-LOCAL-SHAPE" s" |" 1 23 22 23 REJECTED-MODES ;

: RECORD-PLACED ( n str str str n n n n str -- )
   {: k:n code:str tok:str sp:str line:n col:n bs:n be:n path:str :}
   k s" code" code FIELD=
   k s" token" tok FIELD=
   k sp line col bs be path FX$ PLACED-IN ;

: STORED-PLACED ( str str n n n n str -- )
   {: word:str tok:str line:n col:n bs:n be:n path:str :}
   0 s" E-BAD-STORED-SIGNATURE" tok tok line col bs be path RECORD-PLACED
   0 s" message" word FIELD-HAS ;

\ STORED-PLACED for each check of the fixture, plain, --all-errors and
\ --verify-only: each gives the rejection alone, never followed by its statement's
\ throw.
: STORED-AT ( str str n n n n -- )
   {: word:str tok:str line:n col:n bs:n be:n :}
   s" " CHECK
   GJA-LINE# @ 1 T=
   word tok line col bs be FX-PATH$ STORED-PLACED
   s" --all-errors" CHECK
   GJA-LINE# @ 1 T=
   word tok line col bs be FX-PATH$ STORED-PLACED
   s" --verify-only" CHECK
   GJA-LINE# @ 1 T=
   word tok line col bs be FX-PATH$ CANON$ STORED-PLACED ;

\ A stored signature that does not parse is rejected at the type it cannot
\ read, or at the name its declaration reads when the signature holds no type,
\ and the record names the word as written.
: TEST-STORED-SIGNATURE ( -- )
   s" a TRUSTED: definition" T-LABEL
   s" TRUSTED: X ( n -- zz ) 1 ;" s" stored-trusted.f" LINE-FIXTURE
   s" `X`" s" zz" 1 19 18 20 STORED-AT
   s" a defer" T-LABEL
   s" defer W ( n -- zz )" s" stored-defer.f" LINE-FIXTURE
   s" `W`" s" zz" 1 16 15 17 STORED-AT
   s" a defer after a definition with the same signature text" T-LABEL
   SB-RESET
   s" package A" SB-APPEND LF+
   s" DEFTYPE zz" SB-APPEND LF+
   s" : X ( zz -- zz ) ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" defer W ( zz -- zz )" SB-APPEND LF+
   s" stored-same-text.f" FIXTURE!
   s" `W`" s" zz" 5 11 59 61 STORED-AT
   s" a cast" T-LABEL
   s" cast: >X ( ^ -- n )" s" stored-cast.f" LINE-FIXTURE
   s" `>X`" s" ^" 1 12 11 12 STORED-AT ;

: TEST-REPLACED-SIGNATURE ( -- )
   s" a TRUSTED: definition replacing an undefined word" T-LABEL
   SB-RESET
   s" : X ( -- ) ;" SB-APPEND LF+
   s" undefine X" SB-APPEND LF+
   s" TRUSTED: X ( -- zz ) ;" SB-APPEND LF+
   s" replaced-trusted.f" FIXTURE!
   s" `X`" s" zz" 3 17 40 42 STORED-AT
   s" a defer replacing an undefined word" T-LABEL
   SB-RESET
   s" : X ( -- ) ;" SB-APPEND LF+
   s" undefine X" SB-APPEND LF+
   s" defer X ( -- zz )" SB-APPEND LF+
   s" replaced-defer.f" FIXTURE!
   s" `X`" s" zz" 3 14 37 39 STORED-AT
   s" a TRUSTED: definition in a package, under a global's tail" T-LABEL
   SB-RESET
   s" : X ( -- ) ;" SB-APPEND LF+
   s" package Q public" SB-APPEND LF+
   s" TRUSTED: X ( -- zz ) ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" replaced-same-tail.f" FIXTURE!
   s" `X`" s" zz" 3 17 46 48 STORED-AT ;

\ A `:` definition's binder is stored with its signature when CHECK leaves the
\ body unjudged: deferred to the run, the file checks clean; rejected, the next
\ rejection under --all-errors keeps its own record.
: TEST-DEFINITION-BINDER ( -- )
   s" a binder in a definition deferred to the run" T-LABEL
   SB-RESET
   s\" s\" : HELPER-B ( -- ) ;\" evaluate-closed" SB-APPEND LF+
   s" : SCHEME-B ( forall<p,[ n -- n ]> -- ) drop HELPER-B ;" SB-APPEND LF+
   s" : CALL-B ( -- ) [: 1 + ;] SCHEME-B ;" SB-APPEND LF+
   s" binder-deferred.f" FIXTURE!
   s" " 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" a binder in a rejected definition, then another rejection" T-LABEL
   SB-RESET
   s" : SCHEME-C ( forall<p,[ n -- n ]> -- ) drop drop ;" SB-APPEND LF+
   s" : OTHER-C ( -- ) drop ;" SB-APPEND LF+
   s" binder-rejected.f" FIXTURE!
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=
   0 s" code" s" E-INPUT-UNDERFLOW" FIELD=
   0 s" drop" 1 45 44 48 AT
   1 s" code" s" E-INPUT-UNDERFLOW" FIELD=
   1 s" drop" 2 18 68 72 AT ;

: RECORD-AT ( str str str n n n n n n -- )
   {: code:str tok:str sp:str line:n col:n bs:n be:n count:n lead:n :}
   s" " CHECK
   GJA-LINE# @ count T=
   0 code tok sp line col bs be FX-PATH$ RECORD-PLACED
   s" --all-errors" CHECK
   GJA-LINE# @ count T=
   0 code tok sp line col bs be FX-PATH$ RECORD-PLACED
   s" --verify-only" CHECK
   GJA-LINE# @ count lead + T=
   lead code tok sp line col bs be FX-PATH$ CANON$ RECORD-PLACED ;

\ RECORD-AT for E-BAD-QUALIFIED-RECORD: each check gives the record alone, at
\ the name as written.
: MALFORMED-AT ( str n n n n -- )
   {: tok:str line:n col:n bs:n be:n :}
   s" E-BAD-QUALIFIED-RECORD" tok tok line col bs be 1 0 RECORD-AT ;

\ A colon definition CHECK rejects: the plain check stops at that rejection, and
\ --all-errors and --verify-only follow it with the record the rejected
\ definition asks for its malformed name.
: REJECTED-MALFORMED ( -- )
   s" : P:Q:R ( -- ) NOPE ;" s" malformed-rejected.f" LINE-FIXTURE
   s" " CHECK
   s" E-UNDEFINED" REJECTED-AS
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=
   1 s" E-BAD-QUALIFIED-RECORD" s" P:Q:R" s" P:Q:R" 1 3 2 7 FX-PATH$ RECORD-PLACED
   s" --verify-only" CHECK
   GJA-LINE# @ 2 T=
   1 s" E-BAD-QUALIFIED-RECORD" s" P:Q:R" s" P:Q:R" 1 3 2 7 FX-PATH$ CANON$ RECORD-PLACED ;

\ A record asked for a malformed qualified name is rejected at the name its
\ declaration reads, whether the record is keyed by that name's fold (a `defer`,
\ a `variable`), by the name as read (`undefine`) or by the name CHECK copied (a
\ colon definition, certified, rejected or deferred to the run). --verify-only defers the `evaluate` that renders MADE.
: TEST-MALFORMED-RECORD ( -- )
   s" a malformed defer" T-LABEL
   s" defer DIAG:MAL:NAME ( -- )" s" malformed-defer.f" LINE-FIXTURE
   s" DIAG:MAL:NAME" 1 7 6 19 MALFORMED-AT
   s" a malformed variable" T-LABEL
   s" variable P:Q:R" s" malformed-variable.f" LINE-FIXTURE
   s" P:Q:R" 1 10 9 14 MALFORMED-AT
   s" a malformed undefine" T-LABEL
   SB-RESET
   s" : R ( -- ) ;" SB-APPEND LF+
   s" undefine P:Q:R" SB-APPEND LF+
   s" malformed-undefine.f" FIXTURE!
   s" P:Q:R" 2 10 22 27 MALFORMED-AT
   s" a malformed colon definition" T-LABEL
   s" : P:Q:R ( -- ) ;" s" malformed-colon.f" LINE-FIXTURE
   s" P:Q:R" 1 3 2 7 MALFORMED-AT
   s" a malformed colon definition CHECK rejects" T-LABEL
   REJECTED-MALFORMED
   s" a malformed colon definition deferred to the run" T-LABEL
   SB-RESET
   s\" s\" : MADE ( -- ) ;\" evaluate" SB-APPEND LF+
   s" : P:Q:R ( -- ) MADE ;" SB-APPEND LF+
   s" malformed-deferred.f" FIXTURE!
   s" E-BAD-QUALIFIED-RECORD" s" P:Q:R" s" P:Q:R" 2 3 31 36 1 1 RECORD-AT
   0 s" code" s" W-CHECK-DEFERRED" FIELD=
   0 s" evaluate" 1 21 20 28 FX-PATH$ CANON$ FX$ AT-IN ;

\ The same order without a signature fault: a body word the load cannot
\ compile rejects the definition at that word before any check made at `;`, and
\ otherwise the first token whose check fails does, its verdict included.
: TEST-REJECTION-ORDER ( -- )
   s" an undefined word before a trust-boundary word" T-LABEL
   s" : X ( n -- n ) NOPE evaluate ;" s" undef-unsafe.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 16 15 19 REJECTED-AT
   s" an undefined word after a trust-boundary word" T-LABEL
   s" : X ( n -- n ) evaluate NOPE ;" s" unsafe-undef.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 25 24 28 REJECTED-AT
   s" a malformed qualified call after a trust-boundary word" T-LABEL
   s" : X ( n -- n ) evaluate A:B:C ;" s" unsafe-qual.f" LINE-FIXTURE
   s" E-BAD-QUALIFIED" s" A:B:C" 1 25 24 29 REJECTED-AT
   s" an unknown construct variant after a trust-boundary word" T-LABEL
   s" SUMTYPE rf VARIANT ok n ;VARIANT ;SUMTYPE : X ( n -- rf ) evaluate construct rf nope ;"
      s" unsafe-variant.f" LINE-FIXTURE
   s" E-CONSTRUCT-UNKNOWN-VARIANT" s" nope" 1 81 80 84 REJECTED-AT
   s" a mismatch before a rejected locals group token" T-LABEL
   s" : X ( n -- n ) dup c@ {: a -- b :} a ;" s" mismatch-local.f" LINE-FIXTURE
   s" E-MISMATCH" s" c@" 1 20 19 21 REJECTED-AT
   s" a mismatch before a quotation local" T-LABEL
   s" : X ( n -- n ) dup c@ [: {: y:n :} ;] drop 1 ;" s" mismatch-qlocal.f" LINE-FIXTURE
   s" E-BAD-LOCAL-SHAPE" s" {:" 1 26 25 27 REJECTED-AT ;

\ A read below the return-stack cells pushed here is the return row's to repair,
\ and its record says so.
: READS-BELOW ( bool -- ) {: quoted:bool :}
   quoted IF
      0 s" message" s" return stack: the quotation reads below the return-stack cells it pushed" FIELD=
   ELSE
      0 s" message" s" return stack: this reads below the return-stack cells pushed here" FIELD=
   THEN ;

\ A read below the return-stack cells pushed here is rejected at the read, not
\ at the last token walked: the measure is taken before the read's unify binds
\ the open return base.
: TEST-REPAIR-FOLLOWS ( -- )
   s" a mismatch before an unbalanced return row" T-LABEL
   s" : X ( n -- ) >r 1 c@ ;" s" mismatch-rstack.f" LINE-FIXTURE
   s" E-MISMATCH" s" c@" 1 19 18 20 REJECTED-AT
   s" a >r whose cell does not fit, as dup over that cell" T-LABEL
   SB-RESET
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE" SB-APPEND LF+
   s" : X ( rvbox<a> -- ) >r r> drop ;" SB-APPEND LF+
   s" rvbox-tor.f" FIXTURE!
   s" E-MISMATCH" s" >r" 2 21 60 62 REJECTED-AT
   0 s" expected" s" a" NOTE=
   0 s" found" s" rvbox<b>" NOTE=
   0 s" family" s" rvbox" NOTE=
   s" a 2>r whose pair does not fit, as 2dup over that pair" T-LABEL
   SB-RESET
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE" SB-APPEND LF+
   s" : X ( n rvbox<a> -- ) 2>r 2r> 2drop ;" SB-APPEND LF+
   s" rvbox-2tor.f" FIXTURE!
   s" E-MISMATCH" s" 2>r" 2 23 62 65 REJECTED-AT
   0 s" expected" s" c a" NOTE=
   0 s" found" s" n rvbox<b>" NOTE=
   0 s" family" s" rvbox" NOTE=
   s" that unbalanced return row alone" T-LABEL
   s" : X ( n -- ) >r ;" s" rstack.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" >r" 1 14 13 15 REJECTED-AT
   0 s" message" s" return stack: the return row does not balance at exit" FIELD=
   s" an r> below the return cells pushed here" T-LABEL
   s" : X ( -- ) r> drop ;" s" rread.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r>" 1 12 11 13 REJECTED-AT  false READS-BELOW
   s" an r@ below them" T-LABEL
   s" : X ( -- ) r@ drop ;" s" rfetch.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r@" 1 12 11 13 REJECTED-AT  false READS-BELOW
   s" a 2r> below them" T-LABEL
   s" : X ( -- ) 2r> 2drop ;" s" r2read.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" 2r>" 1 12 11 14 REJECTED-AT  false READS-BELOW
   s" a 2r@ below them" T-LABEL
   s" : X ( -- ) 2r@ 2drop ;" s" r2fetch.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" 2r@" 1 12 11 14 REJECTED-AT  false READS-BELOW
   s" an r> whose cell goes back with >r" T-LABEL
   s" : X ( -- ) r> >r ;" s" rround.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r>" 1 12 11 13 REJECTED-AT  false READS-BELOW
   s" an r> in the else arm, whose if arm pushed its own" T-LABEL
   s" : X ( bool -- ) if 1 >r r> drop else r> drop then ;" s" relse.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r>" 1 38 37 39 REJECTED-AT  false READS-BELOW
   s" an r> in a quotation literal" T-LABEL
   s" : X ( -- ) [: r> drop ;] drop ;" s" rquot.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r>" 1 15 14 16 REJECTED-AT  true READS-BELOW
   s" an r@ in a do loop, whose cells are not on the return row" T-LABEL
   s" : X ( n n -- ) do r@ drop loop ;" s" rloopf.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r@" 1 19 18 20 REJECTED-AT  false READS-BELOW
   s" an r> in a do loop" T-LABEL
   s" : X ( n n -- ) do r> drop loop ;" s" rloopr.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r>" 1 19 18 20 REJECTED-AT  false READS-BELOW
   s" an r> below a return row the signature names" T-LABEL
   s" : X ( | U -- | U ) r> drop ;" s" rnamed.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r>" 1 20 19 21 REJECTED-AT  false READS-BELOW
   s" a 2r> that moves a record whole, one cell short" T-LABEL
   SB-RESET
   s" STRUCTURE pr2 FIELD x n FIELD y n ;STRUCTURE" SB-APPEND LF+
   s" : X ( pr2 -- ) >r 2r> drop drop ;" SB-APPEND LF+
   s" rwide.f" FIXTURE!
   s" E-RETURN-STACK" s" 2r>" 2 19 63 66 REJECTED-AT  false READS-BELOW ;

\ A return cell whose type changes on one path is rejected where the paths
\ meet, naming both return rows as they stood before they met.
: RETURN-MEET ( str str -- ) {: exp:str act:str :}
   0 s" expected" exp NOTE=
   0 s" found" act NOTE= ;
: TEST-RETURN-MEET ( -- )
   s" an until pass that changes a return cell's type" T-LABEL
   s" : W ( n -- ) 0 >r begin r> drop true >r 1 - dup 0= until drop r> drop ;"
      s" rmeet-until.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" until" 1 52 51 56 REJECTED-AT
   0 s" message" s" return stack: the loop's back edge changes the return cells from n to bool" FIELD=
   0 s" help" s" keep each return cell's type on every pass, or take it back with `r>` before the loop ends" FIELD=
   s" n" s" bool" RETURN-MEET
   s" an again pass that changes it" T-LABEL
   s" : W ( n -- ) 0 >r begin r> drop true >r again ;" s" rmeet-again.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" again" 1 41 40 45 REJECTED-AT
   0 s" message" s" return stack: the loop's back edge changes the return cells from n to bool" FIELD=
   s" n" s" bool" RETURN-MEET
   s" a while repeat pass that changes it" T-LABEL
   s" : W ( n -- ) 0 >r begin r> drop true >r 1 - dup 0= while repeat drop r> drop ;"
      s" rmeet-repeat.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" repeat" 1 58 57 63 REJECTED-AT
   0 s" message" s" return stack: the loop's back edge changes the return cells from n to bool" FIELD=
   s" n" s" bool" RETURN-MEET
   s" a ?do loop pass that changes it" T-LABEL
   s" : W ( n -- ) 0 >r 3 0 ?do r> drop true >r loop drop r> drop ;" s" rmeet-loop.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" loop" 1 43 42 46 REJECTED-AT
   0 s" message" s" return stack: the loop's back edge changes the return cells from n to bool" FIELD=
   s" n" s" bool" RETURN-MEET
   s" an if arm that changes it" T-LABEL
   s" : W ( n -- ) 0 >r 0= if r> drop true >r then r> drop ;" s" rmeet-then.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" then" 1 41 40 44 REJECTED-AT
   0 s" message" s" return stack: the paths that meet here leave different return cells, bool and n" FIELD=
   s" bool" s" n" RETURN-MEET
   s" an exit that leaves a changed return cell" T-LABEL
   s" : W ( n -- ) 0 >r 0= if r> drop true >r exit then r> drop ;" s" rmeet-exit.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" ;" 1 59 58 59 REJECTED-AT
   0 s" message" s" return stack: the paths that meet here leave different return cells, nothing and bool" FIELD=
   s" (nothing)" s" bool" RETURN-MEET
   s" an exit from a definer's head that leaves a changed return cell" T-LABEL
   s" : D ( n -- ) create 0 >r 0= if r> drop true >r exit then r> drop does> ( -- ) drop ;"
      s" rmeet-exit-does.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" does>" 1 66 65 70 REJECTED-AT
   0 s" message" s" return stack: the paths that meet here leave different return cells, nothing and bool" FIELD=
   s" (nothing)" s" bool" RETURN-MEET
   s" a body that leaves a return cell the signature does not declare" T-LABEL
   s" : X ( n | U -- | U ) >r ;" s" rmeet-decl.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" >r" 1 22 21 23 REJECTED-AT
   0 s" message" s" return stack: the signature declares nothing on the return stack at exit; the body leaves n" FIELD=
   s" (nothing)" s" n" RETURN-MEET ;

\ A stack word over an open layout is rejected by the step path's mismatch,
\ whatever else is among its operands: a proof or an expanded layout beside it
\ does not send it to the transport path, which has no reason to give.
: TEST-OPEN-LAYOUT-XPORT ( -- )
   s" an open layout under swap" T-LABEL
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE : X ( rvbox<a> n -- n rvbox<a> ) swap ;"
      s" open-layout.f" LINE-FIXTURE
   s" E-MISMATCH" s" swap" 1 74 73 77 REJECTED-AT
   s" an open layout beside a proof under swap" T-LABEL
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE : X ( rvbox<a> -- n rvbox<a> ) [: ;] catch swap ;"
      s" open-layout-proof.f" LINE-FIXTURE
   s" E-MISMATCH" s" swap" 1 84 83 87 REJECTED-AT
   s" an open layout beside an expanded layout under swap" T-LABEL
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE STRUCTURE pr2 FIELD x n FIELD y n ;STRUCTURE : X ( rvbox<a> pr2 -- pr2 rvbox<a> ) swap ;"
      s" open-layout-pr2.f" LINE-FIXTURE
   s" E-MISMATCH" s" swap" 1 123 122 126 REJECTED-AT
   s" a stale open layout under drop, a non-linear family first" T-LABEL
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE STRUCTURE pr2 FIELD x n FIELD y n ;STRUCTURE : ID ( rvbox<a> -- rvbox<a> ) ; : X ( pr2 rvbox<a> bool -- pr2 ) [: ( rvbox<a> bool -- rvbox<a> bool ) >r ID r> dup if 1 throw then ;] catch drop drop drop ;"
      s" open-layout-stale-pr2.f" LINE-FIXTURE
   s" E-STALE-READ" s" drop" 1 237 236 240 REJECTED-AT
   0 s" family" s" rvbox" NOTE=
   s" a stale open layout in a locals capture" T-LABEL
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE STRUCTURE pr2 FIELD x n FIELD y n ;STRUCTURE : ID ( rvbox<a> -- rvbox<a> ) ; : X ( pr2 rvbox<a> bool -- pr2 ) [: ( rvbox<a> bool -- rvbox<a> bool ) >r ID r> dup if 1 throw then ;] catch drop drop {: x :} ;"
      s" open-layout-stale-local.f" LINE-FIXTURE
   s" E-STALE-READ" s" :}" 1 242 241 243 REJECTED-AT ;


: LIT-FIXTURE ( str -- )
   {: text:str :}
   SB-RESET
   s" : U8C ( u8 -- ) drop ;" SB-APPEND LF+
   s" : U16C ( u16 -- ) drop ;" SB-APPEND LF+
   s" : U32C ( u32 -- ) drop ;" SB-APPEND LF+
   s" : I64C ( i64 -- ) drop ;" SB-APPEND LF+
   s" : NC ( n -- ) drop ;" SB-APPEND LF+
   s" : EQ2 ( a a -- ) 2drop ;" SB-APPEND LF+
   s" : K ( a b [ a b -- ] -- ) execute ;" SB-APPEND LF+
   text SB-APPEND LF+
   s" literal.f" FIXTURE! ;


: LIT-REJECTED ( str str -- )
   {: text:str why:str :}
   text T-LABEL
   text LIT-FIXTURE
   s" " CHECK
   s" E-MISMATCH" REJECTED-AS
   0 s" message" why FIELD= ;


: LIT-ACCEPTED ( str -- )
   {: text:str :}
   text T-LABEL
   text LIT-FIXTURE
   s" " 0 CHECK-EXIT ;


\ A number literal in a body passes into a fixed-width input or output only when
\ it fits; its fact rides stack words, the return stack, a local and equal joins.
: TEST-LITERAL-REJECTED ( -- )
   s" : X ( -- ) -1 U8C ;" s" literal `-1` does not fit `u8` (`0`..`255`)" LIT-REJECTED
   s" : X ( -- ) 256 U8C ;" s" literal `256` does not fit `u8` (`0`..`255`)" LIT-REJECTED
   s" : X ( -- ) 65536 U16C ;"
   s" literal `65536` does not fit `u16` (`0`..`65535`)" LIT-REJECTED
   s" : X ( -- ) 4294967296 U32C ;"
   s" literal `4294967296` does not fit `u32` (`0`..`4294967295`)" LIT-REJECTED
   s" : X ( -- ) 300 dup U8C drop ;"
   s" literal `300` does not fit `u8` (`0`..`255`)" LIT-REJECTED
   s" : X ( -- ) 0 300 swap drop U8C ;"
   s" literal `300` does not fit `u8` (`0`..`255`)" LIT-REJECTED
   s" : X ( -- ) 300 >r r> U8C ;"
   s" literal `300` does not fit `u8` (`0`..`255`)" LIT-REJECTED
   s" : X ( bool -- ) if 300 else 300 then U8C ;"
   s" literal `300` does not fit `u8` (`0`..`255`)" LIT-REJECTED
   s" : X ( ^u16 -- ) 70000 swap ! ;"
   s" literal `70000` does not fit `u16` (`0`..`65535`)" LIT-REJECTED
   \ The callable's variable takes `u16` from the pointee, which is demanded; a
   \ rule that treats every link between variables as provided accepts this.
   s" : X ( ^u16 -- ) 70000 swap [: ! ;] K ;"
   s" literal `70000` does not fit `u16` (`0`..`65535`)" LIT-REJECTED
   s" : X ( -- ) 300 {: c :} c U8C ;"
   s" literal `300` does not fit `u8` (`0`..`255`)" LIT-REJECTED ;


: TEST-LITERAL-ACCEPTED ( -- )
   s" : X ( -- ) 255 U8C ;" LIT-ACCEPTED
   s" : X ( -- ) 0 U8C ;" LIT-ACCEPTED
   s" : X ( -- ) -1 I64C ;" LIT-ACCEPTED
   s" : X ( -- ) -1 NC ;" LIT-ACCEPTED
   s" : X ( -- ) 300 1 - U8C ;" LIT-ACCEPTED
   s" : X ( ^u16 -- ) 65 swap ! ;" LIT-ACCEPTED
   s" : X ( bool -- ) if 5 else 300 then U8C ;" LIT-ACCEPTED
   \ The carried cell is an `n`, which passes into `u8`; a rule that rejects an
   \ `n` there rejects this.
   s" : X ( -- ) 300 begin dup U8C 1+ dup 310 = until drop ;" LIT-ACCEPTED
   s" : X ( -- ) 0 begin drop 5 dup 3 = until drop ;" LIT-ACCEPTED
   s" : X ( -- ) 300 0 swap drop U8C ;" LIT-ACCEPTED
   \ A variable bound from the stack's own cell demands nothing of the literal,
   \ whichever argument binds it first.
   s" : X ( u8 -- ) 300 EQ2 ;" LIT-ACCEPTED
   s" : X ( u8 -- ) 300 swap EQ2 ;" LIT-ACCEPTED
   \ Through a callable's shared variable the `u8` still comes from the stack's
   \ cell: the chain is provided as its last variable is.
   s" : X ( u8 -- ) 300 [: EQ2 ;] K ;" LIT-ACCEPTED
   s" : X ( u8 -- ) 300 swap [: EQ2 ;] K ;" LIT-ACCEPTED
   \ A chain the pointee demands still admits a literal that fits.
   s" : X ( ^u16 -- ) 65 swap [: ! ;] K ;" LIT-ACCEPTED ;


: TEST-LITERAL-FIT ( -- )
   TEST-LITERAL-REJECTED
   TEST-LITERAL-ACCEPTED ;

\ A match whose scrutinee does not fit, and a match or case one of whose
\ branches fails, still has live words that the load compiles.
: TEST-LIVE-BRANCHES ( -- )
   s" an undefined word in a match whose scrutinee does not fit" T-LABEL
   s" SUMTYPE mres VARIANT ok n ;VARIANT ;SUMTYPE : X ( n -- n ) MATCH mres ok OF FIRST ENDOF ;MATCH SECOND ;"
      s" scrutinee-first.f" LINE-FIXTURE
   s" E-UNDEFINED" s" FIRST" 1 77 76 81 REJECTED-AT
   s" an undefined word after that match" T-LABEL
   s" SUMTYPE mres VARIANT ok n ;VARIANT ;SUMTYPE : X ( n -- n ) MATCH mres ok OF 1 ENDOF ;MATCH SECOND ;"
      s" scrutinee-after.f" LINE-FIXTURE
   s" E-UNDEFINED" s" SECOND" 1 92 91 97 REJECTED-AT
   s" an undefined word after a match branch that fails" T-LABEL
   s" SUMTYPE mres VARIANT ok n ;VARIANT ;SUMTYPE : X ( mres -- n ) MATCH mres ok OF 1 c@ ENDOF ;MATCH NOPE ;"
      s" match-branch-after.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 98 97 101 REJECTED-AT
   s" an undefined word after a case branch that fails" T-LABEL
   s" : X ( n -- n ) CASE 1 OF 1 c@ ENDOF ENDCASE NOPE ;" s" case-branch-after.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 45 44 48 REJECTED-AT ;

: CERTIFIED ( -- )
   s" " 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" --all-errors" 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 0 T= ;

\ A word `[']` ticks that does not exist rejects the definition at that word
\ before a signature that does not parse and before an earlier failing check.
: TEST-TICK-TARGET ( -- )
   s" a signature that does not parse and an undefined ticked word" T-LABEL
   s" : X ( n -- zz ) ['] NOPE ;" s" badsig-tick.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 21 20 24 REJECTED-AT
   s" an undefined ticked word after a trust-boundary word" T-LABEL
   s" : X ( n -- n ) evaluate ['] NOPE ;" s" unsafe-tick.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 29 28 32 REJECTED-AT
   s" an undefined ticked word alone" T-LABEL
   s" : X ( -- n ) ['] NOPE ;" s" tick-undefined.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 18 17 21 REJECTED-AT
   s" a defined ticked word" T-LABEL
   s" : X ( -- ) ['] dup drop ;" s" tick-defined.f" LINE-FIXTURE
   CERTIFIED ;

: MK-FIXTURE ( str str str -- )
   {: def:str text:str name:str :}
   SB-RESET
   def SB-APPEND LF+
   s" MK MADE" SB-APPEND LF+
   text SB-APPEND LF+
   name FIXTURE! ;

\ MK-FIXTURE whose row bounds MK's call to the name it reads, so the check goes
\ on after it.
: MADE-FIXTURE ( str str -- )
   {: text:str name:str :}
   s" : MK ( -- ) create ; parses: MK 1" text name MK-FIXTURE ;

: MK-WARNED ( n n str -- )
   {: k:n bs:n code:str :}
   k s" code" code FIELD=
   k s" severity" s" warning" FIELD=
   k s" MK" 2 1 bs bs 2 + FX-PATH$ CANON$ FX$ AT-IN ;

: MK-DEFERRED ( n n -- )
   s" W-CHECK-DEFERRED" MK-WARNED ;

: RUN-REJECTED-AT ( str str n n n n -- )
   {: c:str tok:str line:n col:n bs:n be:n :}
   s" " CHECK
   c REJECTED-AS
   0 tok line col bs be AT
   s" --all-errors" CHECK
   c REJECTED-AS
   0 tok line col bs be AT ;

\ --verify-only defers both MK's call and X's body, whose use of MADE needs its
\ own warning where the check stopped.
: VERIFY-DEFERS ( str -- )
   {: tok:str :}
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 2 T=
   0 34 MK-DEFERRED
   1 s" code" s" W-CHECK-DEFERRED" FIELD=
   1 s" severity" s" warning" FIELD=
   1 tok 3 12 53 53 tok STR:LEN + FX-PATH$ CANON$ FX$ AT-IN ;

: VERIFY-REJECT-AT ( str str n n n n -- )
   {: c:str tok:str line:n col:n bs:n be:n :}
   c tok line col bs be RUN-REJECTED-AT
   s" --verify-only" CHECK
   GJA-LINE# @ 2 T=
   0 34 MK-DEFERRED
   1 s" code" c FIELD=
   1 tok line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

\ hb, run on the arguments added and IN as its stdin, exits WANT with TEXT in
\ its stderr, or with an empty stderr when TEXT is empty.
: PIPED ( str str n -- )
   {: in:str text:str want:n :}
   FX-PATH$
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$  in  [: GT-OUT-ROOM ;] [: GT-ERR-ROOM ;]
   PROCESS:WAIT-FOREVER RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME
   STORE!
   RC @ want T=
   text STR:LEN 0= IF GT-ERR-U @ 0 T= EXIT THEN
   GT-ERR$ text STR:CONTAINS? TTRUE ;

: LAUNCHED ( str n -- )
   {: text:str want:n :}
   EMPTY 0 >str text want PIPED ;

: LOADED ( str n -- )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   FX-PATH$ PROC-ARGV+
   LAUNCHED ;

\ The load exits RC after its record for CODE, placed at AT after the fixture's
\ path (`:line:column`, or nothing for a record with no place).
: LOADED-AT ( str str n -- )
   {: c:str at:str rc:n :}
   SB-RESET s" error[" SB-APPEND c SB-APPEND s" ]" SB-APPEND
   SB$ rc LOADED
   SB-RESET s" --> " SB-APPEND FX-PATH$ SB-APPEND at SB-APPEND
   GT-ERR$ SB$ STR:CONTAINS? TTRUE ;

: MADE-LINES ( -- )
   SB-RESET
   s" : MK ( -- ) create ; parses: MK 1" SB-APPEND LF+
   s" MK MADE" SB-APPEND LF+ ;

: LEFT-AT ( n str n n n n -- )
   {: k:n tok:str line:n col:n bs:n be:n :}
   k s" code" s" W-CHECK-DEFERRED" FIELD=
   k s" message" s" only the run can tell whether a quotation is kept or what it does" FIELD-HAS
   k tok line col bs be AT ;

\ The check cannot follow MADE, so it does not know what RUN and PRUN do with a
\ quotation: a caller's, or its caller's, that reads a local is left to the run.
: TEST-QUOT-LEFT ( -- )
   s" a quotation that reads a local, handed to a body left to the run" T-LABEL
   MADE-LINES
   s" : RUN ( [ -- n ] -- n ) MADE drop execute ;" SB-APPEND LF+
   s" : USE ( n -- n ) {: a:n :} [: a ;] RUN ;" SB-APPEND LF+
   s" : VIA ( [ -- n ] -- n ) RUN ;" SB-APPEND LF+
   s" : USE-VIA ( n -- n ) {: a:n :} [: a ;] VIA ;" SB-APPEND LF+
   s" : MKC ( -- ) create does> ( n -- n ) drop {: a:n :} [: a ;] RUN ;" SB-APPEND LF+
   s" package P public : PRUN ( [ -- n ] -- n ) MADE drop execute ; ;package" SB-APPEND LF+
   s" : USE-PKG ( n -- n ) {: a:n :} [: a ;] P:PRUN ;" SB-APPEND LF+
   s" : USE-PLAIN ( -- n ) [: 5 ;] RUN ;" SB-APPEND LF+
   s" : ONLY ( [ -- n ] -- n ) execute ;" SB-APPEND LF+
   s" : USE-ONLY ( n -- n ) {: a:n :} [: a ;] ONLY ;" SB-APPEND LF+
   s" quot-left.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 7 T=
   2 s" RUN" 4 36 121 124 LEFT-AT
   2 s" help" s" use a quotation that reads no local, calls only words the check judges and does only what the type that takes it states, or state the rest in that type, and the check judges `RUN`" FIELD=
   3 s" VIA" 6 40 196 199 LEFT-AT
   4 s" RUN" 7 61 262 265 LEFT-AT
   6 s" P:PRUN" 9 40 378 384 LEFT-AT
   s" " 0 CHECK-EXIT
   s" a quotation that reads a local, kept by a body left to the run" T-LABEL
   MADE-LINES
   s" TYPED-VARIABLE KEPT [ -- n ]" SB-APPEND LF+
   s" : RUN ( [ -- n ] -- n ) MADE drop KEPT ! 0 ;" SB-APPEND LF+
   s" : USE ( n -- n ) {: a:n :} [: a ;] RUN ;" SB-APPEND LF+
   s" : GETQ ( -- [ -- n ] ) MADE drop KEPT @ ;" SB-APPEND LF+
   s" : JOIN2 ( n bool -- n ) {: a:n f:bool :} f if GETQ else [: a ;] then execute ;" SB-APPEND LF+
   s" quot-left-kept.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 5 T=
   2 s" RUN" 5 36 151 154 LEFT-AT
   4 s" then" 7 65 263 267 LEFT-AT
   s" error[E-LOCAL-ESCAPE]" 70 LOADED
   s" a quotation that reads a local and leaves the wrong row" T-LABEL
   MADE-LINES
   s" : RUN ( [ -- n ] -- n ) MADE drop execute ;" SB-APPEND LF+
   s" : U7 ( n -- n ) {: a:n :} [: a a ;] RUN ;" SB-APPEND LF+
   s" quot-left-row.f" FIXTURE!
   s" --verify-only" CHECK
   GJA-LINE# @ 3 T=
   2 s" code" s" E-MISMATCH" FIELD=
   2 s" RUN" 4 37 122 125 AT
   s" error[E-MISMATCH]" 70 LOADED
   s" a quotation that reads a local and binds the row it is handed" T-LABEL
   MADE-LINES
   s" : ONCE ( R [ -- S ] -- S ) MADE drop execute ;" SB-APPEND LF+
   s" : U ( n -- n ) {: a:n :} [: a ;] ONCE ;" SB-APPEND LF+
   s" quot-left-tail.f" FIXTURE!
   s" --verify-only" CHECK
   GJA-LINE# @ 3 T=
   2 s" code" s" E-MISMATCH" FIELD=
   2 s" ONCE" 4 34 122 126 AT
   s" error[E-MISMATCH]" 70 LOADED
   s" a quotation that reads a local, handed to a body left to the run over a record" T-LABEL
   MADE-LINES
   s" STRUCTURE pt FIELD x n FIELD y n ;STRUCTURE" SB-APPEND LF+
   s" : EACH ( ^t [ ^t -- ] -- ) MADE drop execute ;" SB-APPEND LF+
   s" TYPED-VARIABLE P pt" SB-APPEND LF+
   s" : SHOW ( n ^pt -- ) drop . ;" SB-APPEND LF+
   s" : USE ( n -- ) {: a:n :} P [: a swap SHOW ;] EACH ;" SB-APPEND LF+
   s" 7 USE" SB-APPEND LF+
   s" quot-left-stored.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" EACH" 7 46 227 231 LEFT-AT
   s" " 0 LOADED
   s" a quotation that reads a local, handed to a declared signature" T-LABEL
   SB-RESET
   s" defer RUN ( [ -- n ] -- n )" SB-APPEND LF+
   s" : USE ( n -- n ) {: a:n :} [: a ;] RUN ;" SB-APPEND LF+
   s" quot-declared.f" FIXTURE!
   s" --verify-only" CHECK
   GJA-LINE# @ 1 T=
   0 s" code" s" E-LOCAL-ESCAPE" FIELD=
   0 s" RUN" 2 36 63 66 AT ;

\ Token TOK, record K, is left to the run because of a type it is handed.
: TAKES-AT ( n str n n n n -- )
   {: k:n tok:str line:n col:n bs:n be:n :}
   k s" code" s" W-CHECK-DEFERRED" FIELD=
   k s" message" s" only the run can tell which types" FIELD-HAS
   k tok line col bs be AT ;

\ The check leaves the call of TOK, past its first two records, where the load
\ rejects it with code C.
: TAKES-BOTH ( str str str n n n n -- )
   {: c:str tok:str at:str line:n col:n bs:n be:n :}
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 tok line col bs be TAKES-AT
   c at 70 LOADED-AT ;

: RECORD-LINES ( -- )
   MADE-LINES
   s" STRUCTURE pt FIELD x n FIELD y n ;STRUCTURE" SB-APPEND LF+
   s" STRUCTURE one FIELD x n ;STRUCTURE" SB-APPEND LF+
   s" TYPED-VARIABLE P pt" SB-APPEND LF+
   s" TYPED-VARIABLE N n" SB-APPEND LF+ ;

: EACH-LINES ( -- )
   RECORD-LINES
   s" : EACH ( ^t [ ^t -- ] -- ) MADE drop swap @ drop drop ;" SB-APPEND LF+ ;

\ What a body left to the run does with its type variables only the run can
\ tell, so only a plain scalar's caller is judged.
: TEST-TYPE-LEFT ( -- )
   s" a record behind the pointer of a body left to the run" T-LABEL
   EACH-LINES
   s" : SHOW ( ^pt -- ) drop ;" SB-APPEND LF+
   s" : U ( -- ) P [: SHOW ;] EACH ;" SB-APPEND LF+
   s" type-left.f" FIXTURE!
   s" E-MISMATCH" s" EACH" s" :9:25" 9 25 265 269 TAKES-BOTH
   s" a record of one cell, behind that pointer" T-LABEL
   EACH-LINES
   s" TYPED-VARIABLE O one" SB-APPEND LF+
   s" : SHOW ( ^one -- ) drop ;" SB-APPEND LF+
   s" : U ( -- ) O [: SHOW ;] EACH ;" SB-APPEND LF+
   s" type-left-one.f" FIXTURE!
   s" E-MISMATCH" s" EACH" s" :10:25" 10 25 287 291 TAKES-BOTH
   s" a quotation behind the pointer of a body left to the run" T-LABEL
   MADE-LINES
   s" TYPED-VARIABLE KV [ -- ]" SB-APPEND LF+
   s" : W ( ^t -- ) MADE drop NULL-PTR = drop ;" SB-APPEND LF+
   s" : U ( -- ) KV W ;" SB-APPEND LF+
   s" type-left-quot.f" FIXTURE!
   s" E-RAW-CELL-PTR" s" W" s" :5:15" 5 15 123 124 TAKES-BOTH
   s" an atom behind the pointer of a body left to the run" T-LABEL
   MADE-LINES
   s" package Q" SB-APPEND LF+
   s" private" SB-APPEND LF+
   s" cast: PN ( ^t -- ^n )" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : PNN ( ^t -- ) PN drop ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" : W ( ^t -- ) MADE drop Q:PNN ;" SB-APPEND LF+
   s" : U ( ^xt -- ) W ;" SB-APPEND LF+
   s" type-left-atom.f" FIXTURE!
   s" E-CAST-CLASS" s" W" s" :10:16" 10 16 171 172 TAKES-BOTH
   s" a number behind that pointer" T-LABEL
   EACH-LINES
   s" : SHOW ( ^n -- ) drop ;" SB-APPEND LF+
   s" : U ( -- ) N [: SHOW ;] EACH ;" SB-APPEND LF+
   s" type-left-scalar.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 2 T=
   s" " 0 LOADED
   s" a generic caller of a body left to the run, and its callers" T-LABEL
   RECORD-LINES
   s" : W ( ^t -- ) MADE drop @ drop ;" SB-APPEND LF+
   s" : VIA ( ^u -- ) W ;" SB-APPEND LF+
   s" : UN ( -- ) N VIA ;" SB-APPEND LF+
   s" type-left-scalar-via.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" W" 8 17 209 210 TAKES-AT
   s" " 0 LOADED
   RECORD-LINES
   s" : W ( ^t -- ) MADE drop @ drop ;" SB-APPEND LF+
   s" : VIA ( ^u -- ) W ;" SB-APPEND LF+
   s" : UN ( -- ) N VIA ;" SB-APPEND LF+
   s" : U ( -- ) P VIA ;" SB-APPEND LF+
   s" type-left-via.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 4 T=
   2 s" W" 8 17 209 210 TAKES-AT
   3 s" VIA" 10 14 246 249 TAKES-AT
   s" E-MISMATCH" s" :10:14" 70 LOADED-AT
   s" a tick of a body left to the run" T-LABEL
   RECORD-LINES
   s" : W ( ^t -- ) MADE drop @ drop ;" SB-APPEND LF+
   s" : UN ( -- ) N ['] W execute ;" SB-APPEND LF+
   s" type-left-tick-scalar.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" W" 8 19 211 212 TAKES-AT
   s" " 0 LOADED
   RECORD-LINES
   s" : W ( ^t -- ) MADE drop @ drop ;" SB-APPEND LF+
   s" : U ( -- ) P ['] W execute ;" SB-APPEND LF+
   s" type-left-tick.f" FIXTURE!
   s" E-MISMATCH" s" W" s" :8:20" 8 18 210 211 TAKES-BOTH
   s" a caller that hands its parameter over in a quotation it never runs" T-LABEL
   RECORD-LINES
   s" : W ( ^t -- ) MADE drop @ drop ;" SB-APPEND LF+
   s" : VIA ( ^u -- ) {: p:^ :} [: p W ;] drop ;" SB-APPEND LF+
   s" : U ( -- ) P VIA ;" SB-APPEND LF+
   s" type-left-literal.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 4 T=
   2 s" W" 8 32 224 225 TAKES-AT
   3 s" VIA" 9 14 249 252 TAKES-AT
   s" E-MISMATCH" s" :9:14" 70 LOADED-AT ;

\ The check, past K records, rejects with code C where the load does.
: REJECTED-BOTH ( n str str str n n n n -- )
   {: k:n c:str tok:str at:str line:n col:n bs:n be:n :}
   s" --verify-only" CHECK
   GJA-LINE# @ k 1 + T=
   k s" code" c FIELD=
   k tok line col bs be AT
   c at 70 LOADED-AT ;

: RUN-LINES ( -- )
   MADE-LINES
   s" : RUN ( [ -- n ] -- n ) MADE drop execute ;" SB-APPEND LF+ ;

\ A call whose quotation only the run can judge hides no other judgement of its body.
: TEST-QUOT-LEFT-JUDGED ( -- )
   s" a token left to the run after a left call" T-LABEL
   MADE-LINES
   s" TYPED-VARIABLE KEPT [ -- n ]" SB-APPEND LF+
   s" : RUN ( [ -- n ] -- n ) MADE drop KEPT ! 5 ;" SB-APPEND LF+
   s" : U ( n -- n ) {: a:n :} [: a ;] RUN MADE drop ;" SB-APPEND LF+
   s" left-twice.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" RUN" 5 34 149 152 LEFT-AT
   s" E-LOCAL-ESCAPE" s" :5:34" 70 LOADED-AT
   s" a rejection after a left call" T-LABEL
   RUN-LINES
   s" : PUT ( n -- n ) {: a:n :} [: a ;] RUN true + ;" SB-APPEND LF+
   s" left-later.f" FIXTURE!
   2 s" E-MISMATCH" s" +" s" :4:45" 4 45 130 131 REJECTED-BOTH
   s" another input of a left call" T-LABEL
   MADE-LINES
   s" : RUN2 ( n [ -- n ] -- n ) MADE drop execute + ;" SB-APPEND LF+
   s" : U8 ( n -- n ) {: a:n :} true [: a ;] RUN2 ;" SB-APPEND LF+
   s" left-input.f" FIXTURE!
   2 s" E-MISMATCH" s" RUN2" s" :4:40" 4 40 130 134 REJECTED-BOTH
   MADE-LINES
   s" : RUN2 ( n [ -- n ] -- n ) MADE drop execute + ;" SB-APPEND LF+
   s" : U8 ( n -- n ) {: a:n :} 1 [: a ;] RUN2 ;" SB-APPEND LF+
   s" left-input-right.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" RUN2" 4 37 127 131 LEFT-AT
   s" " 0 LOADED
   s" a quotation that reads a local, handed to RECURSE" T-LABEL
   RUN-LINES
   s" : REC ( [ -- n ] n -- n ) {: q n :} n 0 > if q RUN drop [: n ;] n 1 - RECURSE else n then ;" SB-APPEND LF+
   s" left-recurse.f" FIXTURE!
   2 s" E-LOCAL-ESCAPE" s" RECURSE" s" :4:71" 4 71 156 163 REJECTED-BOTH
   s" a stored quotation after a left call" T-LABEL
   RUN-LINES
   s" TYPED-VARIABLE KEPT [ -- n ]" SB-APPEND LF+
   s" : U ( n -- n ) {: a:n :} [: a ;] dup RUN swap KEPT ! ;" SB-APPEND LF+
   s" left-stored.f" FIXTURE!
   2 s" E-LOCAL-ESCAPE" s" !" s" :5:52" 5 52 166 167 REJECTED-BOTH
   s" a stop after a left call" T-LABEL
   RUN-LINES
   s" : IM ( -- ) ; immediate" SB-APPEND LF+
   s" : PUT ( n -- ) {: a:n :} [: a ;] RUN drop IM ['] ffi-call drop ;" SB-APPEND LF+
   s" left-stopped.f" FIXTURE!
   s" --verify-only" 1 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" code" s" W-CHECK-STOPPED" FIELD=
   2 s" message" s" the check cannot follow the source from `ffi-call` on" FIELD-HAS
   2 s" ffi-call" 5 50 159 167 FX-PATH$ CANON$ FX$ AT-IN
   s" a name no word can have, after a left call" T-LABEL
   RUN-LINES
   s" : U ( n -- n ) {: a:n :} [: a ;] RUN P:X:Y ;" SB-APPEND LF+
   s" left-name.f" FIXTURE!
   2 s" E-BAD-QUALIFIED" s" P:X:Y" s" :4:38" 4 38 123 128 REJECTED-BOTH
   s" a name no word can have, after one the run defines" T-LABEL
   MADE-LINES
   s" : U ( -- n ) MADE P:X:Y ;" SB-APPEND LF+
   s" made-name.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 2 T=
   1 s" code" s" W-CHECK-DEFERRED" FIELD=
   1 s" message" s" the check cannot follow the source from `MADE` on" FIELD-HAS
   1 s" MADE" 3 14 55 59 AT
   s" E-BAD-QUALIFIED" s" :3:19" 70 LOADED-AT
   s" left calls in an arm, in a quotation, twice in one body and in a trial taken back" T-LABEL
   RUN-LINES
   s" : ONCE ( [ -- n ] -- n ) MADE drop execute ;" SB-APPEND LF+
   s" : ARM ( n -- n ) {: a:n :} a 0 > if [: a ;] RUN exit then a ;" SB-APPEND LF+
   s" : TWO ( n -- n ) {: a:n :} [: a ;] RUN [: a ;] ONCE + ;" SB-APPEND LF+
   s" : LIT ( n -- n ) {: a:n :} [: a [: a ;] RUN + ;] execute ;" SB-APPEND LF+
   s" TYPED-VARIABLE KEPT [ -- n ]" SB-APPEND LF+
   s" : GETQ ( -- [ -- n ] ) MADE drop KEPT @ ;" SB-APPEND LF+
   s" : TRY ( n -- ) {: a:n :} [: a ;] [: drop 1 GETQ ;] catch drop drop drop ;" SB-APPEND LF+
   s" left-calls.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 8 T=
   3 s" RUN" 5 45 175 178 LEFT-AT
   4 s" RUN" 6 36 228 231 LEFT-AT
   5 s" RUN" 7 41 289 292 LEFT-AT
   s" " 0 LOADED ;

\ A body left to the run only by a call, REG, keeps what its walk found, so its
\ callers are judged by it.
: LATE-LINES ( -- )
   MADE-LINES
   s" : LATE ( -- ) MADE drop ;" SB-APPEND LF+
   s" TYPED-VARIABLE KV [ -- ]" SB-APPEND LF+ ;

: REG-LINES ( -- )
   LATE-LINES
   s" : REG ( -- ) [: LATE ;] KV ! ;" SB-APPEND LF+ ;

: TEST-QUOT-LEFT-RECORD ( -- )
   s" a parameter kept by a body left at another call" T-LABEL
   LATE-LINES
   s" TYPED-VARIABLE KN [ -- n ]" SB-APPEND LF+
   s" : REG ( [ -- n ] -- ) KN !  [: LATE ;] KV ! ;" SB-APPEND LF+
   s" : U ( n -- ) {: a:n :} [: a ;] REG ;" SB-APPEND LF+
   s" record-local.f" FIXTURE!
   3 s" E-LOCAL-ESCAPE" s" REG" s" :7:32" 7 32 197 200 REJECTED-BOTH
   s" a parameter's facts, kept by a body left at another call" T-LABEL
   LATE-LINES
   s" : REG ( [ -- ] -- ) KV !  [: LATE ;] KV ! ;" SB-APPEND LF+
   s" : U ( -- ) [: parse-name drop ;] REG ;" SB-APPEND LF+
   s" record-fact.f" FIXTURE!
   3 s" E-MISMATCH" s" REG" s" :6:34" 6 34 170 173 REJECTED-BOTH
   s" an inferred effect of a body left at a call" T-LABEL
   LATE-LINES
   s" : REG [: LATE ;] KV ! 5 ;" SB-APPEND LF+
   s" : U ( -- ) REG true + drop ;" SB-APPEND LF+
   s" record-inferred.f" FIXTURE!
   3 s" E-MISMATCH" s" +" s" :6:21" 6 21 139 140 REJECTED-BOTH
   s" a parameter only run by a body left at another call" T-LABEL
   LATE-LINES
   s" : REG ( [ -- n ] -- n ) execute  [: LATE ;] KV ! ;" SB-APPEND LF+
   s" : U ( n -- n ) {: a:n :} [: a ;] REG ;" SB-APPEND LF+
   s" record-run.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" !" 5 48 140 141 LEFT-AT
   s" " 0 LOADED
   s" a row for a body left at a call" T-LABEL
   REG-LINES
   s" parses: REG 1" SB-APPEND LF+
   s" record-parses-row.f" FIXTURE!
   3 s" E-PARSES-ROW" s" REG" s" :6:9" 6 9 132 135 REJECTED-BOTH
   REG-LINES
   s" generates: REG ( -- )" SB-APPEND LF+
   s" record-generates-row.f" FIXTURE!
   3 s" E-GENERATES-ROW" s" REG" s" :6:12" 6 12 135 138 REJECTED-BOTH
   s" quotations that call a body left at a call" T-LABEL
   REG-LINES
   s" TYPED-VARIABLE K2 [ -- ]" SB-APPEND LF+
   s" : INST ( -- ) [: REG ;] K2 ! ;" SB-APPEND LF+
   s" : INST2 ( -- ) [: INST ;] K2 ! ;" SB-APPEND LF+
   s" record-called.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" !" 5 28 120 121 LEFT-AT
   s" " 0 LOADED
   s" a wrapper of a definer, left at a call" T-LABEL
   LATE-LINES
   s" TYPED-VARIABLE KP [ -- parses ]" SB-APPEND LF+
   s" : DF ( n -- ) create , does> ( -- n ) @ ;" SB-APPEND LF+
   s" : DF2 ( -- ) 5 DF ['] LATE KP ! ;" SB-APPEND LF+
   s" DF2 W1" SB-APPEND LF+
   s" : USE ( -- n ) W1 1 + ;" SB-APPEND LF+
   s" record-wrapper.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" !" 7 31 197 198 LEFT-AT
   s" " 0 LOADED ;

\ LATE calls a word only rendered text defines, which parses.
: RENDERED-LATE-LINES ( -- )
   SB-RESET
   s\" : GEN ( -- ) s\" : MADE ( -- ) parse-name drop ;\" evaluate-closed ;" SB-APPEND LF+
   s" GEN" SB-APPEND LF+
   s" : LATE ( -- ) MADE ;" SB-APPEND LF+ ;

: STORE-LINE ( -- )
   s" : W2 ( ^a a -- ) {: p:^ v :} v p ! ;" SB-APPEND LF+ ;

: STORE-USE ( -- )
   STORE-LINE
   s" : USE ( -- ) KV [: LATE ;] W2 ;" SB-APPEND LF+ ;

\ A quotation with a fact its parameter's type lacks is left to the run where the
\ callee's body was, and rejected where a checked body keeps it.
: PARSE-USE ( -- )
   s" : USE ( -- ) [: parse-name drop ;] RUN ;" SB-APPEND LF+ ;

\ USE keeps what GETQ's line returns in a cell that states no fact; its `!` is
\ at byte BS.
: GETQ-KEPT ( str n -- ) {: getq:str bs:n :}
   RENDERED-LATE-LINES
   s" TYPED-VARIABLE KV [ -- ]" SB-APPEND LF+
   getq SB-APPEND LF+
   s" : USE ( -- ) GETQ KV ! ;" SB-APPEND LF+
   s" fact-returned-lacks.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" !" 6 22 bs bs 1 + LEFT-AT
   s" E-MISMATCH" s" :6:22" 70 LOADED-AT ;

\ STOQ's line keeps a quotation in a cell that states `parses`; USE's call of it
\ is at byte BS.
: STOQ-KEPT ( str n -- ) {: stoq:str bs:n :}
   RENDERED-LATE-LINES
   s" TYPED-VARIABLE KP [ -- parses ]" SB-APPEND LF+
   stoq SB-APPEND LF+
   s" : USE ( -- ) [: parse-name drop ;] 0 STOQ ;" SB-APPEND LF+
   s" fact-joined.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 4 T=
   3 s" STOQ" 6 38 bs bs 4 + LEFT-AT
   s" " 0 LOADED ;

: TEST-QUOT-LEFT-FACT ( -- )
   s" a quotation that parses, handed to a body left to the run" T-LABEL
   MADE-LINES
   s" : RUN ( [ -- ] -- ) MADE drop execute ;" SB-APPEND LF+
   PARSE-USE
   s" fact-left.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" RUN" 4 36 117 120 LEFT-AT
   s" " 0 LOADED
   s" a quotation that parses, kept by a body left to the run" T-LABEL
   MADE-LINES
   s" TYPED-VARIABLE KEPT [ -- ]" SB-APPEND LF+
   s" : RUN ( [ -- ] -- ) MADE drop KEPT ! ;" SB-APPEND LF+
   PARSE-USE
   s" fact-left-kept.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" RUN" 5 36 143 146 LEFT-AT
   s" error[E-MISMATCH]" 70 LOADED
   \ MADE takes `parse-name` as its operand, so P does not parse: the check, which
   \ cannot follow MADE, judges no fact it read past it.
   s" a kept quotation whose body parses only as the check misreads it" T-LABEL
   SB-RESET
   s\" : GEN ( -- ) s\" : MADE ( -- ) parse-name drop ; immediate parses: MADE 1\" evaluate-closed ;" SB-APPEND LF+
   s" GEN" SB-APPEND LF+
   s" : P ( -- ) MADE parse-name ;" SB-APPEND LF+
   s" TYPED-VARIABLE KV [ -- ]" SB-APPEND LF+
   s" : USE ( -- ) [: P ;] KV ! ;" SB-APPEND LF+
   s" fact-left-misread.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" !" 5 25 174 175 LEFT-AT
   s" " 0 LOADED
   s" a quotation of a parsing word left to the run, handed to a left word" T-LABEL
   SB-RESET
   s\" : GEN ( -- ) s\" : MADE ( -- ) ;\" evaluate-closed ;" SB-APPEND LF+
   s" GEN" SB-APPEND LF+
   s" : P ( -- ) MADE parse-name drop ;" SB-APPEND LF+
   s" : RUN ( [ -- ] -- ) MADE execute ;" SB-APPEND LF+
   s" : C ( -- ) [: P ;] RUN ;" SB-APPEND LF+
   s\" C \\ : BAD ( -- n ) s\" x\" ;" SB-APPEND LF+
   s" fact-left-handed.f" FIXTURE!
   s" --verify-only" 1 CHECK-EXIT
   GJA-LINE# @ 5 T=
   4 s" code" s" W-CHECK-STOPPED" FIELD=
   4 s" C" 6 1 149 150 AT
   s" a quotation that calls a body left to the run, kept" T-LABEL
   MADE-LINES
   s" TYPED-VARIABLE KEPT [ -- ]" SB-APPEND LF+
   s" : LATE ( -- ) MADE drop ;" SB-APPEND LF+
   s" : USE ( -- ) [: LATE ;] KEPT ! ;" SB-APPEND LF+
   s" fact-calls-left.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" !" 5 30 124 125 LEFT-AT
   s" " 0 LOADED
   s" a quotation that parses through a body left to the run, kept" T-LABEL
   RENDERED-LATE-LINES
   s" TYPED-VARIABLE KV [ -- ]" SB-APPEND LF+
   s" : USE ( -- ) [: LATE ;] KV ! ;" SB-APPEND LF+
   s" fact-parses-left.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" !" 5 28 144 145 LEFT-AT
   s" E-MISMATCH" s" :5:28" 70 LOADED-AT
   s" a quotation that calls a body left to the run, and the type of a cell" T-LABEL
   RENDERED-LATE-LINES
   s" TYPED-VARIABLE KV [ -- ]" SB-APPEND LF+
   STORE-USE
   s" fact-cell-lacks.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" W2" 6 28 181 183 LEFT-AT
   s" E-MISMATCH" s" :6:28" 70 LOADED-AT
   RENDERED-LATE-LINES
   s" TYPED-VARIABLE KV [ -- parses ]" SB-APPEND LF+
   STORE-USE
   s" fact-cell-states.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" W2" 6 28 188 190 LEFT-AT
   s" " 0 LOADED
   s" a quotation that calls a body left to the run, returned by an unsigned body" T-LABEL
   s" : GETQ [: LATE ;] ;" 158 GETQ-KEPT
   s" : GETQ ['] LATE ;" 156 GETQ-KEPT
   RENDERED-LATE-LINES
   s" TYPED-VARIABLE KP [ -- parses ]" SB-APPEND LF+
   STORE-LINE
   s" : GETQ [: LATE ;] ;" SB-APPEND LF+
   s" : USE ( -- ) KP GETQ W2 ;" SB-APPEND LF+
   s" fact-returned-states.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" W2" 7 22 202 204 LEFT-AT
   s" " 0 LOADED
   s" a quotation that parses, kept with one that calls a body left to the run" T-LABEL
   s" : STOQ {: q n :} n 0= if [: LATE ;] else q then KP ! ;" 216 STOQ-KEPT
   s" : STOQ 0= if drop [: LATE ;] then KP ! ;" 202 STOQ-KEPT
   s" a quotation that calls a body left to the run before a token that parses, kept" T-LABEL
   SB-RESET
   s\" : GEN ( -- ) s\" : MADE ( -- ) parse-name drop ; immediate parses: MADE 1\" evaluate-closed ;" SB-APPEND LF+
   s" GEN" SB-APPEND LF+
   s" : LATE ( -- ) MADE parse-name ;" SB-APPEND LF+
   s" TYPED-VARIABLE KV [ -- ]" SB-APPEND LF+
   s" : USE ( -- ) [: LATE ;] KV ! ;" SB-APPEND LF+
   s" fact-after-left.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" !" 5 28 180 181 LEFT-AT
   s" " 0 LOADED
   s" a quotation that parses, kept by a checked body" T-LABEL
   SB-RESET
   s" TYPED-VARIABLE KEPT [ -- ]" SB-APPEND LF+
   s" : RUN ( [ -- ] -- ) KEPT ! ;" SB-APPEND LF+
   PARSE-USE
   s" fact-judged.f" FIXTURE!
   s" --verify-only" CHECK
   GJA-LINE# @ 1 T=
   0 s" code" s" E-MISMATCH" FIELD=
   0 s" message" s" this quotation does more than the type that keeps it states" FIELD-HAS
   0 s" RUN" 3 36 91 94 AT ;

: PAYLOAD-LINES ( -- )
   SB-RESET
   s" TYPED-VARIABLE KV [ -- ]" SB-APPEND LF+
   s" ENUM act VARIANT idle ;VARIANT VARIANT fire FIELD q [ [ -- ] -- [ -- ] ] ;VARIANT ;ENUM" SB-APPEND LF+ ;

\ A quotation type inside a matched payload's quotation type counts as kept.
: TEST-PAYLOAD-QUOT ( -- )
   s" a quotation that parses, taken by a matched payload's quotation" T-LABEL
   PAYLOAD-LINES
   s" : RUN ( act -- ) MATCH act idle OF ENDOF fire OF [: parse-name type ;] swap execute KV ! ENDOF ;MATCH ;" SB-APPEND LF+
   s" payload-parses.f" FIXTURE!
   0 s" E-MISMATCH" s" execute" s" :3:77" 3 77 189 196 REJECTED-BOTH
   s" a quotation that reads a local, taken by a matched payload's quotation" T-LABEL
   PAYLOAD-LINES
   s" : RUN ( act n -- ) {: x:n :} MATCH act idle OF ENDOF fire OF [: x . ;] swap execute KV ! ENDOF ;MATCH ;" SB-APPEND LF+
   s" payload-local.f" FIXTURE!
   0 s" E-LOCAL-ESCAPE" s" execute" s" :3:77" 3 77 189 196 REJECTED-BOTH ;

\ The check cannot follow MADE, so it does not know whether LATE throws or
\ returns: the load decides a body that catches a quotation calling it, and DD
\ from the token after LATE on.
: CATCH-AT ( n n n n n -- )
   {: k:n line:n col:n bs:n be:n :}
   k s" code" s" W-CHECK-DEFERRED" FIELD=
   k s" message" s" only the run can tell whether what `catch` runs throws" FIELD-HAS
   k s" catch" line col bs be AT ;

: MAYDIE-AT ( n str n n n n -- )
   {: k:n tok:str line:n col:n bs:n be:n :}
   k s" code" s" W-CHECK-DEFERRED" FIELD=
   k s" message" s" only the run can tell whether the call before it returns" FIELD-HAS
   k tok line col bs be AT ;

: JOIN-AT ( n str n n n n -- )
   {: k:n tok:str line:n col:n bs:n be:n :}
   k s" code" s" W-CHECK-DEFERRED" FIELD=
   k s" message" s" only the run can tell whether `LATE` returns" FIELD-HAS
   k tok line col bs be AT ;

: CATCH-LINES ( str -- ) {: body:str :}
   SB-RESET
   s\" : GEN ( -- ) s\" : MADE ( n -- n ) dup 0 > if " SB-APPEND  body SB-APPEND
   s\"  then ;\" evaluate-closed ;" SB-APPEND LF+
   s" GEN" SB-APPEND LF+
   s" : LATE ( n -- n ) MADE ;" SB-APPEND LF+
   s\" : DD ( n -- ) LATE drop s\" x\" 1 die ;" SB-APPEND LF+
   s" : U ( n -- n ) [: LATE ;] catch drop ;" SB-APPEND LF+ ;

: TEST-CATCH-LEFT ( -- )
   s" a catch of a body left to the run, which throws" T-LABEL
   s" drop 7 throw" CATCH-LINES
   s" : D ( n -- ) [: DD ;] catch 2drop ;" SB-APPEND LF+
   s" catch-left-throws.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 5 T=
   2 s" drop" 4 20 132 136 MAYDIE-AT
   3 5 27 177 182 CATCH-AT
   4 6 23 212 217 CATCH-AT
   s" E-STALE-READ" s" :5:33" 70 LOADED-AT
   s" and which throws nothing" T-LABEL
   s" drop 7" CATCH-LINES
   s" : D ( n -- ) [: DD ;] catch ;" SB-APPEND LF+
   s" catch-left-returns.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 5 T=
   3 5 27 171 176 CATCH-AT
   4 6 23 206 211 CATCH-AT
   s" " 0 LOADED
   s" a catch after a token the check cannot follow keeps that token's cause" T-LABEL
   s" drop 7 throw" CATCH-LINES
   s" : R ( n -- n ) MADE [: LATE ;] catch drop ;" SB-APPEND LF+
   s" catch-after-unseen.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 5 T=
   4 s" message" s" cannot follow the source from `MADE` on" FIELD-HAS
   4 s" MADE" 6 16 205 209 AT
   s" a caught quotation's facts stay in the record of a body left at the catch" T-LABEL
   s" drop 7 throw" CATCH-LINES
   s" : P ( -- n ) [: parse-name drop 5 DD ;] catch ;" SB-APPEND LF+
   s\" P \\ drop  : BAD ( -- n ) s\" x\" ;" SB-APPEND LF+
   s" catch-left-parses.f" FIXTURE!
   s" --verify-only" 1 CHECK-EXIT
   GJA-LINE# @ 6 T=
   5 s" code" s" W-CHECK-STOPPED" FIELD=
   5 s" P" 7 1 238 239 AT
   s" a catch the check rejects is rejected though its quotation is left" T-LABEL
   s" drop 7" CATCH-LINES
   s" : S ( str -- n ) [: DD ;] catch ;" SB-APPEND LF+
   s" catch-left-mismatch.f" FIXTURE!
   4 s" E-MISMATCH" s" catch" s" :6:27" 6 27 210 215 REJECTED-BOTH ;

\ MADE is made while the file loads, so the check cannot tell whether LATE
\ returns: the load judges the token after a call of LATE, or of a word that
\ only calls it, a join where an arm ending in that call leaves other rows, an
\ exit's included, and the rest of a body whose every joined arm ends in it; the
\ load alone rejects what follows on a dead path.
: MADE-THEN-DIES ( -- )
   SB-RESET
   s\" : GEN ( -- ) s\" : MADE ( n -- n ) dup drop ;\" evaluate-closed ;" SB-APPEND LF+
   s" GEN" SB-APPEND LF+ ;

: TEST-MAYDIE-LEFT ( -- )
   s" a body that ends after a call that may never return" T-LABEL
   SB-RESET
   s\" : GEN ( -- ) s\" : MADE ( n -- n ) ;\" evaluate-closed ;" SB-APPEND LF+
   s" GEN" SB-APPEND LF+
   s\" : LATE ( -- ) 0 MADE drop s\" x\" 1 die ;" SB-APPEND LF+
   s" : V ( -- n ) LATE ;" SB-APPEND LF+
   s" : JN ( bool -- n ) if LATE else 5 then ;" SB-APPEND LF+
   s" : JT ( n bool -- n ) if drop LATE then ;" SB-APPEND LF+
   s" : A ( -- n ) 0 MADE ;" SB-APPEND LF+
   s" : F ( bool -- n ) if A else LATE then ;" SB-APPEND LF+
   s" : G ( bool -- n ) F 1 + ;" SB-APPEND LF+
   s" : FX ( bool -- n ) if 5 exit then LATE ;" SB-APPEND LF+
   s" : VP ( ^a -- ^n ) LATE ;" SB-APPEND LF+
   s" : K1 ( n -- n ) case 1 of LATE endof 2 of 5 endof 7 swap endcase ;" SB-APPEND LF+
   s" require src/lib/adt/nullable.f" SB-APPEND LF+
   s" : M1 ( nullable<u8> -- n ) MATCH nullable none OF LATE ENDOF some OF c@ ENDOF ;MATCH ;"
   SB-APPEND LF+
   s" : L1 ( n -- n ) begin drop LATE again ;" SB-APPEND LF+
   s" : L2 ( n -- ) 3 0 do drop LATE loop ;" SB-APPEND LF+
   s" : L4 ( n -- n ) begin dup 0 > while drop LATE repeat ;" SB-APPEND LF+
   s" maydie-end.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 15 T=
   2 s" ;" 4 19 117 118 JOIN-AT
   s" a join whose arm may never return and leaves other rows" T-LABEL
   3 s" then" 5 35 153 157 JOIN-AT
   4 s" then" 6 35 194 198 JOIN-AT
   s" a join whose every arm may never return, and a call of it" T-LABEL
   6 s" then" 8 34 256 260 JOIN-AT
   7 s" 1" 9 21 283 284 MAYDIE-AT
   s" an exit's rows joined with an end that may never return" T-LABEL
   8 s" ;" 10 40 328 329 JOIN-AT
   s" a body that may never return, which would make its type variable concrete" T-LABEL
   9 s" ;" 11 24 353 354 JOIN-AT
   s" the joins of a case, a match and each loop" T-LABEL
   10 s" endof" 12 45 399 404 JOIN-AT
   11 s" ENDOF" 14 73 525 530 JOIN-AT
   12 s" again" 15 33 572 577 JOIN-AT
   13 s" loop" 16 32 611 615 JOIN-AT
   14 s" repeat" 17 47 664 670 JOIN-AT
   s" " 0 LOADED
   s" a token after a call that may never return" T-LABEL
   MADE-THEN-DIES
   s\" : LATE ( n -- n ) MADE drop s\" x\" 1 die ;" SB-APPEND LF+
   s" : E ( n -- n ) LATE 1 + ;" SB-APPEND LF+
   s" maydie-token.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 3 T=
   2 s" 1" 4 21 130 131 MAYDIE-AT
   s" E-DEAD-CODE" s" :4:21" 70 LOADED-AT
   s" a token after a call of a catch that may never return" T-LABEL
   MADE-THEN-DIES
   s" : LATE ( n -- n ) MADE ;" SB-APPEND LF+
   s\" : DD ( n -- ) LATE drop s\" x\" 1 die ;" SB-APPEND LF+
   s" : C9 ( n -- n n ) [: DD ;] catch ;" SB-APPEND LF+
   s" : E ( n -- n ) C9 drop ;" SB-APPEND LF+
   s" maydie-catch.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 5 T=
   2 s" drop" 4 20 112 116 MAYDIE-AT
   3 5 28 158 163 CATCH-AT
   4 s" drop" 6 19 184 188 MAYDIE-AT
   s" E-DEAD-CODE" s" :6:19" 70 LOADED-AT ;

\ The check cannot follow MADE, so it does not know whether DV divides by what it
\ takes: the load decides a literal 0 handed to DV or to a word that hands it on.
\ No load marks ID's type variable, so the check judges W.
: TEST-ZERO-LEFT ( -- )
   s" a literal 0 handed to a body left to the run" T-LABEL
   SB-RESET
   s\" : GEN ( -- ) s\" : MADE ( -- ) ;\" evaluate-closed ;" SB-APPEND LF+
   s" GEN" SB-APPEND LF+
   s" : DV ( n n -- n ) MADE / ;" SB-APPEND LF+
   s" : V ( n n -- n ) DV ;" SB-APPEND LF+
   s" : U ( -- n ) 1 0 V ;" SB-APPEND LF+
   s" : ID ( t -- t ) MADE ;" SB-APPEND LF+
   s" : W ( -- n ) 0 ID ;" SB-APPEND LF+
   s" zero-left.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 4 T=
   2 s" code" s" W-CHECK-DEFERRED" FIELD=
   2 s" message" s" only the run can tell whether `V` divides by or throws its 0" FIELD-HAS
   2 s" V" 5 18 121 122 AT
   s" E-DIV-ZERO" s" :5:18" 70 LOADED-AT ;

\ The check, which compiles nothing, names a mixed-case private twin as the
\ load does: as its declaration spells it, never folded.
: CASE-MSG$ ( -- str )
   s" public `Pk:EE` has a different stack effect from its package's private `Ee`" ;

: TEST-SHADOWED-CASE ( -- )
   s" a public whose mixed-case private twin moves other cells" T-LABEL
   SB-RESET
   s" package Pk" SB-APPEND LF+
   s" : Ee ( -- ) ;" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : EE ( -- n ) 1 ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" shadowed-case.f" FIXTURE!
   SB-RESET  s" error[E-SHADOWED-ARITY]: " SB-APPEND  CASE-MSG$ SB-APPEND
   SB$ 70 LOADED
   s" --verify-only" CHECK
   GJA-LINE# @ 1 T=
   0 s" message" CASE-MSG$ FIELD=
   0 s" private" s" Ee (0 -- 0)" NOTE= ;

: GEN-CASE-MSG ( str -- ) {: w:str :}
   s" public `Q:" SB-APPEND  w SB-APPEND
   s" ` has a different stack effect from its package's private `" SB-APPEND
   w SB-APPEND  s" `" SB-APPEND ;

\ The check names a generated private twin W as the load does: in the bytes
\ the load builds for it from declaration DECL, never folded.
: GEN-CASE ( str str str str -- )
   {: decl:str name:str w:str note:str :}
   SB-RESET
   s" package Q" SB-APPEND LF+
   decl SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : " SB-APPEND  w SB-APPEND  s"  ( -- ) ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   name FIXTURE!
   SB-RESET  s" error[E-SHADOWED-ARITY]: " SB-APPEND  w GEN-CASE-MSG
   SB$ 70 LOADED
   s" --verify-only" CHECK
   GJA-LINE# @ 1 T=
   SB-RESET  w GEN-CASE-MSG
   0 s" message" SB$ FIELD=
   0 s" private" note NOTE= ;

: TEST-GENERATED-CASE ( -- )
   s" a public whose generated private twin moves other cells" T-LABEL
   s" STRUCTURE sq FIELD x n ;STRUCTURE" s" generated-case-sq.f"
   s" SQ-MAKE" s" SQ-MAKE (1 -- 1)" GEN-CASE
   s" SUMTYPE shp VARIANT circ n ;VARIANT ;SUMTYPE" s" generated-case-shp.f"
   s" SHP-CIRC" s" SHP-CIRC (1 -- 2)" GEN-CASE ;

\ A definition named across a package boundary is spelled by the check, as by
\ the load, without the qualifier its declaration wrote.
: QUAL-MSG$ ( -- str )
   s" `t` in `Yy`'s effect is one cell; `thing2` is 2 cells" ;

: TEST-QUALIFIED-CASE ( -- )
   s" a call of a mixed-case word defined under a qualifier" T-LABEL
   SB-RESET
   s" STRUCTURE thing2 FIELD x n FIELD y n ;STRUCTURE" SB-APPEND LF+
   s" package P" SB-APPEND LF+
   s" : Q:Yy ( ^t -- n ) @ drop 0 ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" : USE ( ^thing2 -- n ) Q:Yy ;" SB-APPEND LF+
   s" qualified-case.f" FIXTURE!
   SB-RESET  s" error[E-MISMATCH]: " SB-APPEND  QUAL-MSG$ SB-APPEND
   SB$ 70 LOADED
   s" --verify-only" CHECK
   GJA-LINE# @ 1 T=
   0 s" message" QUAL-MSG$ FIELD= ;

\ The fixture path names NAME under the test's root, without making it.
: NAMED! ( str -- ) {: name:str :}
   BASE$ name FX-P CAP SPAN:MAKE FS:JOIN-PATH-INTO FX-PU ! ;

\ The check's only record stands in no source: it names no file and no word.
: UNPLACED ( -- )
   GJA-LINE# @ 1 T=
   GJA-LINE# @ 0 > 0= IF EXIT THEN
   0 GJA-LINE$ GJA-PARSE {: root:SPAN:span<u8> :}
   root s" file" FIELD-ABSENT
   root s" notes" GJA-FIELD? IF s" word" FIELD-ABSENT ELSE drop THEN ;

\ NAME under the test's root, the root spelled with every link resolved.
: CANON-NAMED ( str -- str ) {: name:str :}
   BASE$ CANON$ name DIR-P FS:JOIN-PATH {: u:n :}
   DIR-P u >str ;

\ The fixture path, a named input that does not exist, gets the load's record
\ and status, searched at WANT, from the load and the check alike.
: NAMED-MISSING ( str -- ) {: want:str :}
   SB-RESET  s" error[E-MISSING-SOURCE]: cannot open `" SB-APPEND  FX-PATH$ SB-APPEND
   S\" `\n  = searched: " SB-APPEND  want SB-APPEND
   S\" \n  = help: a path given on the command line is looked up from the" SB-APPEND
   S\"  directory hb runs in\n\n" SB-APPEND
   SB$ 70 LOADED  GT-ERR$ SB$ T$=
   s" " CHECK
   0 s" code" s" E-MISSING-SOURCE" FIELD=
   SB-RESET  s" cannot open `" SB-APPEND  FX-PATH$ SB-APPEND  s" `" SB-APPEND
   0 s" message" SB$ FIELD=
   0 s" searched" want NOTE=
   0 s" help" s" a path given on the command line is looked up from the directory hb runs in" FIELD=
   UNPLACED ;

: TEST-NAMED-MISSING ( -- )
   s" a named input that does not exist" T-LABEL
   s" named-missing.f" NAMED!
   s" named-missing.f" CANON-NAMED NAMED-MISSING ;

\ The search resolves `..` after a link from the link's target, and the record
\ names the path it looked up.
: TEST-NAMED-LINK-UP ( -- )
   s" a missing named input whose `..` follows a link" T-LABEL
   s" named-up" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" named-up/other" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" named-up/other/sub" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" named-up/lnk" NAMED!  s" other/sub" FX-PATH$ FS:MAKE-SYMLINK
   s" named-up/lnk/../gone.f" NAMED!
   s" named-up/other/gone.f" CANON-NAMED NAMED-MISSING ;

\ `..` after a dangling link cancels nothing, though `named-dangle/x.f` exists.
: TEST-NAMED-DANGLING-UP ( -- )
   s" a missing named input whose `..` follows a dangling link" T-LABEL
   s" named-dangle" NAMED!  FX-PATH$ FS:MAKE-DIR
   SB-RESET  s" 1 drop" SB-APPEND LF+
   s" named-dangle/x.f" FIXTURE!
   s" named-dangle/dg" NAMED!  s" nowhere/sub" FX-PATH$ FS:MAKE-SYMLINK
   s" named-dangle/dg/../x.f" NAMED!
   s" named-dangle/dg/../x.f" CANON-NAMED NAMED-MISSING ;

\ A trailing `/` after a file searches the file's `.`, never the file itself.
: TEST-NAMED-FILE-DOT ( -- )
   s" a missing named input whose trailing `/` follows a file" T-LABEL
   s" named-file" NAMED!  FX-PATH$ FS:MAKE-DIR
   SB-RESET  s" 1 drop" SB-APPEND LF+
   s" named-file/x.f" FIXTURE!
   s" named-file/x.f/" NAMED!
   s" named-file/x.f/." CANON-NAMED NAMED-MISSING ;

\ hb runs the fixture path as a program, exiting WANT with TEXT in its stderr.
: RAN ( str n -- )
   PROC-ARGV-RESET  FX-PATH$ PROC-ARGV+  LAUNCHED ;

\ The arguments start tools/check.f with --json-errors.
: CHECK-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+  s" tools/check.f" PROC-ARGV+
   s" --" PROC-ARGV+  s" --json-errors" PROC-ARGV+ ;

\ tools/check.f checks the fixture path with --json-errors, exiting WANT with
\ TEXT in its stderr, whose records are split for FIELD=.
: CHECKED ( str n -- )
   CHECK-ARGV  FX-PATH$ PROC-ARGV+
   LAUNCHED  GT-ERR$ GJA-SPLIT-LINES ;

\ The string builder holds the resolver's unplaced record of a command-line
\ path, its message appended by MSG.
: RANGE-RECORD ( [ -- ] -- ) {: msg :}
   SB-RESET  s" error[E-PATH-RANGE]: " SB-APPEND  msg execute
   S\" \n  = help: pass a path of 1 to " SB-APPEND
   PATH-CAP FMT:SB-U  S\"  bytes\n\n" SB-APPEND ;

\ The check's one record is that record, unplaced.
: RANGE-FIELDS ( [ -- ] -- ) {: msg :}
   0 s" code" s" E-PATH-RANGE" FIELD=
   SB-RESET  msg execute  0 s" message" SB$ FIELD=
   SB-RESET  s" pass a path of 1 to " SB-APPEND  PATH-CAP FMT:SB-U
   s"  bytes" SB-APPEND
   0 s" help" SB$ FIELD=
   UNPLACED ;

\ The fixture path gets that record and status 70 from the program run, the
\ load and the check alike.
: RANGE-REJECTED ( [ -- ] -- ) {: msg :}
   msg RANGE-RECORD
   SB$ 70 RAN  GT-ERR$ SB$ T$=
   SB$ 70 LOADED  GT-ERR$ SB$ T$=
   s" E-PATH-RANGE" 70 CHECKED
   msg RANGE-FIELDS ;

: TOO-LONG-MSG ( -- )
   s" a path resolves to more than the " SB-APPEND  PATH-CAP FMT:SB-U
   s"  bytes a path may have" SB-APPEND ;

: EMPTY-MSG ( -- )
   s" a path is empty" SB-APPEND ;

\ The fixture path names a file under the test's root, its name past the cap.
: LONG-NAMED! ( -- )
   LONG-NAME-BYTES 0 ?do 97 LONG-NAME i + c! loop
   LONG-NAME LONG-NAME-BYTES >str NAMED! ;

\ A named input whose path resolves past the cap, or is empty, is judged by the
\ resolver alone.
: TEST-NAMED-TOO-LONG ( -- )
   s" a named input whose path is over the cap" T-LABEL
   LONG-NAMED!
   [: TOO-LONG-MSG ;] RANGE-REJECTED
   s" a named input whose path is empty" T-LABEL
   0 FX-PU !
   [: EMPTY-MSG ;] RANGE-REJECTED ;

\ stdin's path is judged by the resolver as a named input's is.
: TEST-STDIN-PATH-TOO-LONG ( -- )
   s" a --stdin-path over the cap" T-LABEL
   LONG-NAMED!
   CHECK-ARGV
   s" --verify-only" PROC-ARGV+  s" --stdin-path" PROC-ARGV+
   FX-PATH$ PROC-ARGV+
   s" E-PATH-RANGE" 70 LAUNCHED  GT-ERR$ GJA-SPLIT-LINES
   [: TOO-LONG-MSG ;] RANGE-FIELDS ;

\ A source read from stdin is placed in its own bytes, as the file holding them
\ is, where an `evaluate` of them counts its places too.
: TEST-STDIN-CAST ( -- )
   s" a cast evaluated from stdin's bytes, at its place in <stdin>" T-LABEL
   s" parse-name x str> drop 31 + 32 evaluate \ cast: LRD-WIDE ( n -- ptr u8 n )"
   {: src:str :}
   CHECK-ARGV
   src s" E-CAST-FAM" 70 PIPED  GT-ERR$ GJA-SPLIT-LINES
   GJA-LINE# @ 1 T=
   0 s" ptr" 1 65 64 67 s" <stdin>" src PLACED-IN ;

\ CALLBACK builds the duplicate's name in its own buffer, so the check places
\ the record at the `;CALLBACK` that ran it, as the load does.
: TEST-CALLBACK-DUPLICATE ( -- )
   s" a duplicate a definer builds is placed at the token that ran it" T-LABEL
   SB-RESET
   s" require src/lib/ffi-callback.f" SB-APPEND LF+
   s" CALLBACK: CB ( -- )" SB-APPEND LF+
   s" : CB-BODY ( -- n ) 1 ;" SB-APPEND LF+
   s" ;CALLBACK" SB-APPEND LF+
   s" callback-duplicate.f" FIXTURE!
   s" E-DUPLICATE-DEFINITION" 70 CHECKED
   0 s" ;CALLBACK" 4 1 74 83 FX-PATH$ CANON$ FX$ AT-IN ;

\ The fixture's path spelled BASE/./.../NAME, past the cap.
: DOTTED! ( str -- ) {: name:str :}
   BASE$ {: b:str :}
   b STR:DATA FX-P b STR:LEN BYTE-COPY
   47 FX-P b STR:LEN + c!  b STR:LEN 1+ FX-PU !
   begin FX-PU @ name STR:LEN + PATH-CAP <= while
      46 FX-P FX-PU @ + c!  47 FX-P FX-PU @ 1+ + c!
      FX-PU @ 2 + FX-PU !
   repeat
   name STR:DATA FX-P FX-PU @ + name STR:LEN BYTE-COPY
   FX-PU @ name STR:LEN + FX-PU ! ;

\ Fixture NAME's one line includes the fixture path, which may be past the
\ string builder's capacity.
: INCLUDER! ( str -- ) {: name:str :}
   s" include " {: k:str :}
   k STR:DATA FX k STR:LEN BYTE-COPY
   FX-P FX k STR:LEN + FX-PU @ BYTE-COPY
   k STR:LEN FX-PU @ + {: u:n :}
   10 FX u + c!
   u 1+ FX-U !
   BASE$ name FX-P FS:JOIN-PATH FX-PU !
   FX-PATH$ FX$ FS:WRITE-ALL ;

\ A named input written past the cap that resolves within it runs, is loaded
\ and is checked, and so is a file that includes it so written.
: TEST-NAMED-DOTTED ( -- )
   s" a named input written past the cap that resolves within it" T-LABEL
   s\" s\" ran\" type cr" s" dotted.f" LINE-FIXTURE
   s" dotted.f" DOTTED!
   FX-PU @ PATH-CAP > TTRUE
   s" " 0 RAN  GT-OUT$ S\" ran\n" T$=
   s" " 0 LOADED  GT-OUT$ S\" ran\n" T$=
   s" " 0 CHECKED
   s" dotted-includer.f" INCLUDER!
   s" " 0 LOADED  GT-OUT$ S\" ran\n" T$=
   s" " 0 CHECKED  GT-OUT$ S\" ran\n" T$= ;

\ A program's script-required of an empty path is the entry's record: unplaced,
\ from the load and the check alike.
: TEST-SCRIPT-REQUIRED-EMPTY ( -- )
   s" script-required of an empty path" T-LABEL
   s\" s\" \" script-required" s" script-empty.f" LINE-FIXTURE
   [: EMPTY-MSG ;] RANGE-RECORD
   SB$ 70 LOADED  GT-ERR$ SB$ T$=
   s" E-PATH-RANGE" 70 CHECKED
   [: EMPTY-MSG ;] RANGE-FIELDS ;

\ The run places the record for CODE at line L, column C of the fixture.
: RAN-AT ( str n n -- ) {: c:str l:n col:n :}
   SB-RESET  s" error[" SB-APPEND  c SB-APPEND
   SB$ 70 RAN
   SB-RESET  s" --> " SB-APPEND  FX-PATH$ SB-APPEND  s" :" SB-APPEND
   l FMT:SB-U  s" :" SB-APPEND  col FMT:SB-U  LF+
   GT-ERR$ SB$ STR:CONTAINS? TTRUE ;

\ The run, the check and --verify-only place the record for CODE at the
\ fixture's bytes BS to BE on line 1.
: LITERAL-PLACED ( str n n -- ) {: c:str bs:n be:n :}
   c 1 bs 1+ RAN-AT
   c 70 CHECKED
   c REJECTED-AS
   0 FX bs + be bs - >str  1 bs 1+ bs be FX-PATH$ FX$ PLACED-IN
   s" --verify-only" 70 CHECK-EXIT
   c REJECTED-AS
   0 FX bs + be bs - >str  1 bs 1+ bs be FX-PATH$ CANON$ FX$ PLACED-IN ;

\ Fixture NAME's one line passes `included` an escaped literal, LONG-NAME-BYTES
\ `x` then `\x41.f`, past the cap; written in FX, as the string builder holds
\ less.
: LONG-ESCAPED! ( str -- ) {: name:str :}
   BASE$ name FX-P FS:JOIN-PATH FX-PU !
   s\" s\\\" " {: k:str :}
   k STR:DATA FX k STR:LEN BYTE-COPY
   LONG-NAME-BYTES 0 ?do 120 FX k STR:LEN + i + c! loop
   s\" \\x41.f\" included\n" {: t:str :}
   t STR:DATA FX k STR:LEN + LONG-NAME-BYTES + t STR:LEN BYTE-COPY
   k STR:LEN LONG-NAME-BYTES + t STR:LEN + FX-U !
   FX-PATH$ FX$ FS:WRITE-ALL ;

\ A top-level literal's path the loader rejects is placed at the literal, an
\ escaped one's copy included; an empty one, with no bytes, at the loader word.
: TEST-LITERAL-PATH-PLACE ( -- )
   s" an escaped top-level literal past the cap" T-LABEL
   s" literal-long.f" LONG-ESCAPED!
   s" E-PATH-RANGE" 4 LONG-NAME-BYTES 10 + LITERAL-PLACED
   s" a top-level literal naming a missing file" T-LABEL
   s\" s\" literal-absent.f\" INCLUDED" s" literal-missing.f" LINE-FIXTURE
   s" E-MISSING-SOURCE" 3 19 LITERAL-PLACED
   s" an empty top-level literal" T-LABEL
   s\" s\" \" included" s" literal-empty.f" LINE-FIXTURE
   s" E-PATH-RANGE" 5 13 LITERAL-PLACED ;

\ str-dep.f defines STR-DEP, for a load the check follows.
: STR-DEP! ( -- )
   SB-RESET  s" : STR-DEP ( -- ) ;" SB-APPEND LF+
   s" str-dep.f" FIXTURE! ;

\ The string builder holds a skipped arm of the line given, then a load of
\ str-dep.f.
: STR-ARM! ( str -- ) {: s:str :}
   STR-DEP!
   SB-RESET  s" HB-TARGET-WASM? [if]" SB-APPEND LF+
   s SB-APPEND LF+  s" [then]" SB-APPEND LF+
   s\" s\" str-dep.f\" required" SB-APPEND LF+
   s" STR-DEP" SB-APPEND LF+ ;

\ The walk reads a skipped arm: a name its `:` gives is declared.
: TEST-LOADER-ARM ( -- )
   s" a loader after a skipped arm's `required`" T-LABEL
   s" : required ( str -- ) drop ;" STR-ARM!
   s" required-arm.f" FIXTURE!
   s" --verify-only" CHECK
   0 s" code" s" E-LOADER-FORM" FIELD=
   0 s" required" 4 15 71 79 FX-PATH$ CANON$ FX$ AT-IN ;

\ A literal's record lasts only the next token of its own text: a path a
\ loader is given later, in bytes its copy matches, is not placed at the
\ literal but at the loader where source read it last, else nowhere, and names
\ no word.
: TEST-LITERAL-SPENT ( -- )
   s" a literal's copy rewritten and passed later" T-LABEL
   SB-RESET  s" : Z ( ^u8 -- ) 8 0 ?do 122 over i + c! loop drop ;" SB-APPEND LF+
   s\" s\" nofile.f\" drop -8 allot" SB-APPEND LF+
   s" here 8 allot dup Z 8 >str included" SB-APPEND LF+
   s" spent-rewritten.f" FIXTURE!
   s" E-MISSING-SOURCE" 3 27 RAN-AT
   s" a literal in an evaluated buffer rewritten before the load" T-LABEL
   SB-RESET  s" create B 64 allot" SB-APPEND LF+
   s" : Q ( -- ) 64 0 ?do 81 B i + c! loop ;" SB-APPEND LF+
   s" : PUT ( str -- ^u8 n ) str> dup 0 ?do over i + c@ B i + c! loop nip B swap ;" SB-APPEND LF+
   s\" s\\\" s\\\" nofile.f\\\"\" PUT evaluate" SB-APPEND LF+
   s" Q" SB-APPEND LF+  s" included" SB-APPEND LF+
   s" spent-evaluated.f" FIXTURE!
   s" E-MISSING-SOURCE" 6 1 RAN-AT
   GT-ERR$ s" = word:" STR:CONTAINS? TFALSE
   s" a buffer evaluated again at its address, its literal's copy held" T-LABEL
   SB-RESET  s" create B 64 allot" SB-APPEND LF+
   s" : PUT ( str -- ^u8 n ) str> dup 0 ?do over i + c@ B i + c! loop nip B swap ;" SB-APPEND LF+
   s\" : S2 ( -- str ) s\" 1 2 2drop included\" ;" SB-APPEND LF+
   s\" s\\\" s\\\" nofile.f\\\"\" PUT evaluate" SB-APPEND LF+
   s" S2 PUT evaluate" SB-APPEND LF+
   s" spent-reevaluated.f" FIXTURE!
   s" E-MISSING-SOURCE" 5 8 RAN-AT
   GT-ERR$ s" = word:" STR:CONTAINS? TFALSE
   s" a buffer evaluated closed twice in one word, its literal's copy passed later" T-LABEL
   SB-RESET  s" create B 64 allot" SB-APPEND LF+
   s" PTR-VARIABLE H0" SB-APPEND LF+
   s" : PUT ( str -- str ) str> dup 0 ?do over i + c@ B i + c! loop nip B swap >str ;" SB-APPEND LF+
   s" : LOADX ( -- ) H0 @ 8 >str included ;" SB-APPEND LF+
   s" : TWICE ( -- )" SB-APPEND LF+
   s"    here H0 !" SB-APPEND LF+
   s\"    [: s\\\" s\\\" nofile.f\\\"\" PUT evaluate-closed ;] catch drop" SB-APPEND LF+
   s\"    s\" LOADX       \" PUT evaluate-closed ;" SB-APPEND LF+
   s" TWICE" SB-APPEND LF+
   s" spent-closed-twice.f" FIXTURE!
   s" error[E-MISSING-SOURCE]" 70 RAN
   GT-ERR$ s"  --> " STR:CONTAINS? TFALSE
   GT-ERR$ s" = word:" STR:CONTAINS? TFALSE
   s" a literal a definition passes to `included` at run time" T-LABEL
   SB-RESET  s\" : GO ( -- ) s\" nofile.f\" included ;" SB-APPEND LF+
   s" GO" SB-APPEND LF+
   s" spent-run-time.f" FIXTURE!
   s" error[E-MISSING-SOURCE]" 70 RAN
   GT-ERR$ s"  --> " STR:CONTAINS? TFALSE
   GT-ERR$ s" = word:" STR:CONTAINS? TFALSE ;

\ The named directory gets the load's record of a read that fails with EISDIR,
\ and its status, from the load and the check alike.
: DIR-REJECTED ( -- )
   SB-RESET  s" error[E-UNREADABLE-SOURCE]: cannot read `" SB-APPEND  FX-PATH$ SB-APPEND
   S\" `: is a directory\n  = help: name a source file, not a directory\n\n" SB-APPEND
   SB$ 74 LOADED  GT-ERR$ SB$ T$=
   s" " 74 CHECK-EXIT
   0 s" code" s" E-UNREADABLE-SOURCE" FIELD=
   SB-RESET  s" cannot read `" SB-APPEND  FX-PATH$ SB-APPEND  s" `: is a directory" SB-APPEND
   0 s" message" SB$ FIELD=
   0 s" help" s" name a source file, not a directory" FIELD=
   UNPLACED ;

\ A named directory, readable or not, gets that record.
: TEST-NAMED-DIR ( -- )
   s" a named input that is a directory" T-LABEL
   s" named-dir" NAMED!  FX-PATH$ FS:MAKE-DIR
   DIR-REJECTED
   s" a named input that is a directory it cannot read" T-LABEL
   s" named-locked-dir" NAMED!  FX-PATH$ FS:MAKE-DIR
   FX-PATH$ 0 FS:CHMOD-MODE
   [: DIR-REJECTED ;] [: FX-PATH$ $1ED FS:CHMOD-MODE ;] finally ;

\ A required directory gets the load's record of a read that fails with EISDIR,
\ placed at the request, from the load and the check alike.
: TEST-REQUIRED-DIR ( -- )
   s" a required path that is a directory" T-LABEL
   s" required-dir.f" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" require required-dir.f" s" require-dir.f" LINE-FIXTURE
   s" " 74 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" code" s" E-UNREADABLE-SOURCE" FIELD=
   0 s" message" s" cannot read `required-dir.f`" FIELD=
   0 s" required-dir.f" 1 9 8 22 AT
   0 s" source_line" s" require required-dir.f" FIELD=
   0 s" label" s" is a directory" FIELD=
   SB-RESET  BASE$ SB-APPEND  s" /required-dir.f" SB-APPEND
   0 s" resolved" SB$ NOTE=
   0 s" help" s" name a source file, not a directory" FIELD=
   SB-RESET  S\" error[E-UNREADABLE-SOURCE]: cannot read `required-dir.f`\n --> " SB-APPEND
   FX-PATH$ SB-APPEND  s" :1:9" SB-APPEND
   SB$ 74 LOADED ;

\ The fixture path names a FIFO, which a load's open would wait on.
: FIFO! ( -- )
   FX-PATH$ FS:>PATHZ $180 MKFIFO-CALL 0 T= ;

\ A named FIFO gets the load's record of a source no stamp names, and its
\ status, from hb, the load and the check alike, before any open waits on it.
: TEST-NAMED-FIFO ( -- )
   s" a named input that is a FIFO" T-LABEL
   s" named-fifo" NAMED!  FIFO!
   SB-RESET  s" error[E-UNREADABLE-SOURCE]: cannot read `" SB-APPEND  FX-PATH$ SB-APPEND
   S\" `: is not a regular file\n  = help: load a regular file, whose bytes a stamp" SB-APPEND
   S\"  can name\n\n" SB-APPEND
   SB$ 74 RAN  GT-ERR$ SB$ T$=
   SB$ 74 LOADED  GT-ERR$ SB$ T$=
   s" " 74 CHECK-EXIT
   0 s" code" s" E-UNREADABLE-SOURCE" FIELD=
   SB-RESET  s" cannot read `" SB-APPEND  FX-PATH$ SB-APPEND  s" `: is not a regular file" SB-APPEND
   0 s" message" SB$ FIELD=
   0 s" help" s" load a regular file, whose bytes a stamp can name" FIELD=
   UNPLACED ;

\ procfs states /proc/version as a regular file of size 0 that yields bytes: hb,
\ the load and the check alike reject it once it passes its size.
HB-TARGET-LINUX? [if]
: PROCFS-RECORD ( -- )
   0 s" code" s" E-UNREADABLE-SOURCE" FIELD=
   SB-RESET  s" cannot read `/proc/version`: changed while it was read" SB-APPEND
   0 s" message" SB$ FIELD=
   0 s" help" s" load it again once nothing is writing it" FIELD=
   UNPLACED ;

: TEST-NAMED-PROCFS ( -- )
   s" a named input that outruns its size" T-LABEL
   s" named-procfs" NAMED!  s" /proc/version" FX-PATH$ FS:MAKE-SYMLINK
   SB-RESET  s" error[E-UNREADABLE-SOURCE]: cannot read `/proc/version`" SB-APPEND
   S\" : changed while it was read\n  = help: load it again once nothing" SB-APPEND
   S\"  is writing it\n\n" SB-APPEND
   SB$ 74 RAN  GT-ERR$ SB$ T$=
   SB$ 74 LOADED  GT-ERR$ SB$ T$=
   s" " 74 CHECK-EXIT  PROCFS-RECORD
   s" --all-errors" 74 CHECK-EXIT  PROCFS-RECORD ;

\ A stop ends the check, --all-errors included, before a later listed file is
\ read, as it ends the load.
: TEST-LISTED-PROCFS ( -- )
   s" a stop before a listed input that outruns its size, under --all-errors" T-LABEL
   BASE$ s" listed-procfs" DEP-P PATH-CAP SPAN:MAKE FS:JOIN-PATH-INTO DEP-PU !
   s" /proc/version" DEP-PATH$ FS:MAKE-SYMLINK
   SB-RESET
   s" require listed-absent.f" SB-APPEND LF+
   s" : LP1 ( -- ) ;" SB-APPEND LF+
   s" listed-first.f" FIXTURE!
   LSP-CHECK:RESET
   s" all-errors" LSP-CHECK:OPT  s" json-errors" LSP-CHECK:OPT  s" source-list" LSP-CHECK:OPT
   FX-PATH$ LSP-CHECK:FILE  DEP-PATH$ LSP-CHECK:FILE
   70 CHECK-CAPTURE
   s" E-MISSING-SOURCE" REJECTED-AS
   0 s" listed-absent.f" 1 9 8 23 AT ;
[then]
HB-TARGET-LINUX? 0= [if]
: TEST-NAMED-PROCFS ( -- ) ;
: TEST-LISTED-PROCFS ( -- ) ;
[then]

\ The check's one record for the required FIFO, at the request.
: REQUIRED-FIFO-AT ( -- )
   GJA-LINE# @ 1 T=
   0 s" code" s" E-UNREADABLE-SOURCE" FIELD=
   0 s" message" s" cannot read `required-fifo.f`" FIELD=
   0 s" required-fifo.f" 1 9 8 23 AT
   0 s" source_line" s" require required-fifo.f" FIELD=
   0 s" label" s" is not a regular file" FIELD=
   SB-RESET  BASE$ SB-APPEND  s" /required-fifo.f" SB-APPEND
   0 s" resolved" SB$ NOTE=
   0 s" help" s" load a regular file, whose bytes a stamp can name" FIELD= ;

\ A required FIFO gets that record placed at the request, from the load and the
\ check alike; under --all-errors the check returns instead of waiting on it.
: TEST-REQUIRED-FIFO ( -- )
   s" a required path that is a FIFO" T-LABEL
   s" required-fifo.f" NAMED!  FIFO!
   s" require required-fifo.f" s" require-fifo.f" LINE-FIXTURE
   s" " 74 CHECK-EXIT  REQUIRED-FIFO-AT
   s" --all-errors" 74 CHECK-EXIT  REQUIRED-FIFO-AT
   SB-RESET  S\" error[E-UNREADABLE-SOURCE]: cannot read `required-fifo.f`\n --> " SB-APPEND
   FX-PATH$ SB-APPEND  s" :1:9" SB-APPEND
   SB$ 74 LOADED ;

\ The fixture directory TEST-REQUIRED-UNSEARCHABLE makes unsearchable.
: UNSEARCHABLE$ ( -- str )
   BASE$ s" unsearchable" DIR-P FS:JOIN-PATH {: u:n :}
   DIR-P u >str ;

\ The help of a record whose lookup UNSEARCHABLE$ denied.
: UNSEARCHABLE-HELP ( -- )
   SB-RESET  s" if the directory `" SB-APPEND  UNSEARCHABLE$ SB-APPEND
   s" ` is yours, make it searchable with `chmod u+x '" SB-APPEND
   UNSEARCHABLE$ SB-APPEND  s" '`; else ask its owner to let you search it" SB-APPEND ;

\ The load's help names UNSEARCHABLE$, not a directory below it.
: UNSEARCHABLE-LOADED ( -- )
   SB-RESET  s" = help: if the directory `" SB-APPEND  UNSEARCHABLE$ SB-APPEND
   s" ` is" SB-APPEND
   GT-ERR$ SB$ STR:CONTAINS? TTRUE ;

\ Appends the searched note of a request for x.f under UNSEARCHABLE$.
: UNSEARCHABLE-NOTE ( -- )
   BASE$ SB-APPEND  s" /unsearchable/x.f" SB-APPEND ;

\ The check's and the load's record of the request under UNSEARCHABLE$: an
\ unreadable file's, placed at the request, its help naming the directory.
: UNSEARCHABLE-REJECTED ( -- )
   UNSEARCHABLE-HELP
   s" " 74 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" code" s" E-UNREADABLE-SOURCE" FIELD=
   0 s" help" SB$ FIELD=
   0 s" message" s" cannot read `unsearchable/x.f`" FIELD=
   0 s" unsearchable/x.f" 1 9 8 24 AT
   0 s" label" s" permission denied" FIELD=
   SB-RESET  UNSEARCHABLE-NOTE  0 s" searched" SB$ NOTE=
   SB-RESET  S\" error[E-UNREADABLE-SOURCE]: cannot read `unsearchable/x.f`\n --> " SB-APPEND
   FX-PATH$ SB-APPEND  s" :1:9" SB-APPEND
   SB$ 74 LOADED
   UNSEARCHABLE-LOADED ;

\ A request of 4096 bytes or more, `./` repeated before `lnk/x.f`, gets the help
\ and searched note of the request `lnk/x.f`, from the check and the load.
: LONG-REJECTED ( -- )
   s" require-long.f" NAMED!
   0 FX-U !  s" require " FX+
   4096 2 / 0 ?do s" ./" FX+ loop
   S\" lnk/x.f\n" FX+  FX-PATH$ FX$ FS:WRITE-ALL
   UNSEARCHABLE-HELP
   s" " 74 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" help" SB$ FIELD=
   SB-RESET  BASE$ SB-APPEND  s" /lnk/x.f" SB-APPEND
   0 s" searched" SB$ NOTE=
   s" error[E-UNREADABLE-SOURCE]" 74 LOADED
   UNSEARCHABLE-LOADED
   SB-RESET  s" = searched: " SB-APPEND  BASE$ SB-APPEND  s" /lnk/x.f" SB-APPEND  LF+
   GT-ERR$ SB$ STR:CONTAINS? TTRUE ;

\ A request through `lnk`, a link into UNSEARCHABLE$, names UNSEARCHABLE$,
\ not the searchable directory holding the link.
: LINKED-REJECTED ( -- )
   s" require lnk/x.f" s" require-lnk.f" LINE-FIXTURE
   UNSEARCHABLE-HELP
   s" " 74 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" help" SB$ FIELD=
   SB-RESET  BASE$ SB-APPEND  s" /lnk/x.f" SB-APPEND
   0 s" searched" SB$ NOTE=
   s" error[E-UNREADABLE-SOURCE]" 74 LOADED
   UNSEARCHABLE-LOADED ;

\ `..` after `alnk`, a link with an absolute target, leaves through it into
\ UNSEARCHABLE$, which the load names; the searched note keeps that `..`.
: PARENT-LINK-REJECTED ( -- )
   s" require alnk/../x.f" s" require-lnk-up.f" LINE-FIXTURE
   s" " 74 CHECK-EXIT
   SB-RESET  BASE$ SB-APPEND  s" /alnk/../x.f" SB-APPEND
   0 s" searched" SB$ NOTE=
   s" error[E-UNREADABLE-SOURCE]" 74 LOADED
   UNSEARCHABLE-LOADED
   SB-RESET  s" = searched: " SB-APPEND  BASE$ SB-APPEND  s" /alnk/../x.f" SB-APPEND  LF+
   GT-ERR$ SB$ STR:CONTAINS? TTRUE ;

\ A required file a directory without search permission hides is unreadable, not
\ missing; its mode goes back to $1ED so the tree can be deleted.
: TEST-REQUIRED-UNSEARCHABLE ( -- )
   s" a required file under a directory that cannot be searched" T-LABEL
   UNSEARCHABLE$ FS:MAKE-DIR
   s" unsearchable/sub" NAMED!  FX-PATH$ FS:MAKE-DIR
   SB-RESET  s" 1 drop" SB-APPEND LF+
   s" unsearchable/x.f" FIXTURE!
   s" unsearchable/sub/x.f" FIXTURE!
   s" lnk" NAMED!  s" unsearchable/sub" FX-PATH$ FS:MAKE-SYMLINK
   s" alnk" NAMED!
   BASE$ s" unsearchable/sub" DIR-P FS:JOIN-PATH {: au:n :}
   DIR-P au >str FX-PATH$ FS:MAKE-SYMLINK
   s" require unsearchable/x.f" s" require-unsearchable.f" LINE-FIXTURE
   UNSEARCHABLE$ 0 FS:CHMOD-MODE
   [: UNSEARCHABLE-REJECTED LONG-REJECTED LINKED-REJECTED
      PARENT-LINK-REJECTED ;]
   [: UNSEARCHABLE$ $1ED FS:CHMOD-MODE ;] finally ;

\ The load meets the undefined word before the require, and so does the check.
: TEST-UNDEFINED-MISSING ( -- )
   s" an undefined word before a require of a missing file" T-LABEL
   SB-RESET
   s" nosuchword require undefined-missing-absent.f" SB-APPEND LF+
   s" undefined-missing.f" FIXTURE!
   s" " CHECK
   s" E-UNDEFINED" REJECTED-AS
   0 s" nosuchword" 1 1 0 10 AT
   s" E-UNDEFINED" s" :1:1" 70 LOADED-AT ;

\ A listed file that does not exist is met in its place, so the undefined word
\ in the file listed before it comes first, in the check as in the load.
: TEST-UNDEFINED-LISTED-MISSING ( -- )
   s" an undefined word in a file listed before a missing one" T-LABEL
   s" nosuchword" s" undefined-listed.f" LINE-FIXTURE
   BASE$ s" undefined-listed-absent.f" DEP-P PATH-CAP SPAN:MAKE FS:JOIN-PATH-INTO DEP-PU !
   LSP-CHECK:RESET
   s" json-errors" LSP-CHECK:OPT  s" source-list" LSP-CHECK:OPT
   FX-PATH$ LSP-CHECK:FILE  DEP-PATH$ LSP-CHECK:FILE
   70 CHECK-CAPTURE
   s" E-UNDEFINED" REJECTED-AS
   0 s" nosuchword" 1 1 0 10 AT
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+  FX-PATH$ PROC-ARGV+  DEP-PATH$ PROC-ARGV+
   s" error[E-UNDEFINED]" 70 LAUNCHED
   SB-RESET s" --> " SB-APPEND FX-PATH$ SB-APPEND s" :1:1" SB-APPEND
   GT-ERR$ SB$ STR:CONTAINS? TTRUE ;

\ The load rejects a malformed qualified name at its `:`, as the check does.
: TEST-MALFORMED-LOAD ( -- )
   s" a malformed colon definition the load rejects at its `:`" T-LABEL
   s" : P:Q:R ( -- ) ;" s" malformed-colon.f" LINE-FIXTURE
   s" error[E-BAD-QUALIFIED-RECORD]" 70 LOADED
   SB-RESET
   S\" error[E-BAD-QUALIFIED-RECORD]: the record name `P:Q:R` has more than one `:`\n --> "
   SB-APPEND  FX-PATH$ SB-APPEND
   S\" :1:3\n  |\n1 | : P:Q:R ( -- ) ;\n  |   ^^^^^ malformed qualified name\n" SB-APPEND
   s"   = rule: one `:` not at an edge selects a package," SB-APPEND
   S\"  and a second `:` names no word\n" SB-APPEND
   s"   = help: write `P:Q:R` with one `:` between the package and the word," SB-APPEND
   S\"  as `PKG:WORD`\n\n" SB-APPEND
   GT-ERR$ SB$ T$= ;

: VERIFY-REJECTED-AT ( n str str n n n n -- )
   {: k:n c:str tok:str line:n col:n bs:n be:n :}
   k s" code" c FIELD=
   k tok line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

\ A bad escape is rejected at the literal's opener, by each check as by the
\ load, before an unclosed string after it. An earlier definition's E-UNDEFINED
\ comes first, as the load gives it; --verify-only composes the file as it
\ reads it and keeps that definition's rejection before the string's.
: TEST-BAD-ESCAPE ( -- )
   s" a bad escape" T-LABEL
   s\" s\\\" A\\yB\" 2drop" s" escaped-bad.f" LINE-FIXTURE
   s" E-UNTERMINATED-STRING" s\" s\\\"" 1 1 0 3 REJECTED-MODES
   s" E-UNTERMINATED-STRING" s" :1:1" 70 LOADED-AT
   s" a bad escape before a string that never closes" T-LABEL
   SB-RESET
   s\" s\\\" A\\yB\" 2drop" SB-APPEND LF+
   s\" s\" abc" SB-APPEND LF+
   s" escaped-bad-unterminated.f" FIXTURE!
   s" E-UNTERMINATED-STRING" s\" s\\\"" 1 1 0 3 REJECTED-MODES
   s" E-UNTERMINATED-STRING" s" :1:1" 70 LOADED-AT
   s" a bad escape in a body before a string that never closes" T-LABEL
   SB-RESET
   s\" : L ( -- ) s\\\" A\\yB\" 2drop ;" SB-APPEND LF+
   s\" s\" abc" SB-APPEND LF+
   s" escaped-bad-body-unterminated.f" FIXTURE!
   s" E-UNTERMINATED-STRING" s\" s\\\"" 1 12 11 14 REJECTED-MODES
   s" E-UNTERMINATED-STRING" s" :1:12" 70 LOADED-AT
   s" a definition that does not check before a string that never closes" T-LABEL
   SB-RESET
   s" : X ( -- ) NO-SUCH ;" SB-APPEND LF+
   s\" s\" abc" SB-APPEND LF+
   s" undefined-unterminated.f" FIXTURE!
   s" " CHECK
   s" E-UNDEFINED" REJECTED-AS
   0 s" NO-SUCH" 1 12 11 18 AT
   s" --all-errors" CHECK
   GJA-LINE# @ 2 T=
   0 s" code" s" E-UNDEFINED" FIELD=
   0 s" NO-SUCH" 1 12 11 18 AT
   1 s" code" s" E-UNTERMINATED-STRING" FIELD=
   1 S\" s\"" 2 1 21 23 AT
   s" --verify-only" CHECK
   GJA-LINE# @ 2 T=
   0 s" E-UNDEFINED" s" NO-SUCH" 1 12 11 18 VERIFY-REJECTED-AT
   1 s" E-UNTERMINATED-STRING" s\" s\"" 2 1 21 23 VERIFY-REJECTED-AT
   s" E-UNDEFINED" s" :1:12" 70 LOADED-AT ;

\ The load knows no `EPRIM:`, so it rejects the fixture at it: p is the
\ record's place after the path, through its marked source line.
: ROW-LOADED ( str -- ) {: p:str :}
   s" error[E-UNDEFINED]" 70 LOADED
   SB-RESET
   S\" error[E-UNDEFINED]: cannot find word `EPRIM:`\n --> " SB-APPEND
   FX-PATH$ SB-APPEND  p SB-APPEND
   S\"   = help: define `EPRIM:` before this point, or check its spelling\n\n"
   SB-APPEND
   GT-ERR$ SB$ T$= ;

\ The check rejects a program's well-formed row at its opener, as the load does.
: TEST-PRIM-ROW ( -- )
   s" a primitive row in a program file, at its opener" T-LABEL
   SB-RESET
   s" \ a row" SB-APPEND LF+
   s"   EPRIM: FOO PE-N EPRIM;" SB-APPEND LF+
   s" program-row.f" FIXTURE!
   s" --verify-only" CHECK
   0 s" code" s" E-UNDEFINED" FIELD=
   0 s" EPRIM:" 2 3 10 16 FX-PATH$ CANON$ FX$ AT-IN
   S\" :2:3\n  |\n2 |   EPRIM: FOO PE-N EPRIM;\n  |   ^^^^^^ not found\n" ROW-LOADED ;

\ A declaration piped to hb with no file argument is rejected at its token in
\ `<input>`, as the file's bytes place it, naming no word.
: TEST-STDIN-DECLARATION ( -- )
   s" a declaration read from stdin, at its token in <input>" T-LABEL
   PROC-ARGV-RESET
   s" STRUCTURE a11x<q> FIELD next ^a11x<n> FIELD w read-view<q,q,n> ;STRUCTURE"
   s" --> <input>:1:31" 70 PIPED
   GT-ERR$ s" = word:" STR:CONTAINS? TFALSE ;

\ The loader rejects a VALUE-RECORD field where the source wrote it.
: TEST-VALUE-RECORD-LOAD ( -- )
   s" a duplicate VALUE-RECORD field under the loader, at the second field" T-LABEL
   s" VALUE-RECORD vr x n x n END-VALUE-RECORD" s" value-record-dup.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" :1:21" 70 LOADED-AT ;

: TEST-VALUE-RECORD-NO-TYPE ( -- )
   s" a VALUE-RECORD field with no type under the loader, at the field" T-LABEL
   s" VALUE-RECORD vr x n y END-VALUE-RECORD" s" value-record-no-type.f" LINE-FIXTURE
   s" error[E-BAD-DECLARATION]" 70 LOADED
   SB-RESET
   s" error[E-BAD-DECLARATION]: field `y` of value record `vr` has no type" SB-APPEND
   S\" \n --> " SB-APPEND  FX-PATH$ SB-APPEND
   S\" :1:21\n  |\n1 | VALUE-RECORD vr x n y END-VALUE-RECORD\n" SB-APPEND
   S\"   |                     ^ no type follows\n" SB-APPEND
   S\"   = help: write a declared type after the field's name, as `y n`\n\n" SB-APPEND
   GT-ERR$ SB$ T$= ;

: TEST-VALUE-RECORD-BAD-TYPE ( -- )
   s" a VALUE-RECORD field with an unknown type under the loader, at the type" T-LABEL
   s" VALUE-RECORD vr x n y nope END-VALUE-RECORD" s" value-record-bad-type.f" LINE-FIXTURE
   s" error[E-BAD-DECLARATION]" 70 LOADED
   SB-RESET
   s" error[E-BAD-DECLARATION]: field `y` of value record `vr`" SB-APPEND
   S\"  has an unknown or malformed type\n --> " SB-APPEND  FX-PATH$ SB-APPEND
   S\" :1:23\n  |\n1 | VALUE-RECORD vr x n y nope END-VALUE-RECORD\n" SB-APPEND
   S\"   |                       ^^^^ not a valid type\n" SB-APPEND
   S\"   = help: write a declared type after the field's name, as `y n`\n\n" SB-APPEND
   GT-ERR$ SB$ T$= ;

\ A step is 5 bytes written and 2 decoded: PATH-CAP 4 / steps pass PATH-CAP
\ written only, PATH-CAP 3 * 5 / decoded too, and the resolver takes both.
: STEPS-FIXTURE ( n str -- ) {: steps:n name:str :}
   BASE$ name FX-P FS:JOIN-PATH FX-PU !
   0 FX-U !
   s\" s\\\" " FX+
   steps 0 ?do s\" .\\x2F" FX+ loop
   s\" inc-target.f\" included\nINCLUDED-WORD drop\n" FX+
   FX-PATH$ FX$ FS:WRITE-ALL ;

: VERIFY-ONLY-AT ( str n n str n n n n -- )
   {: call:str cline:n cbs:n tok:str line:n col:n bs:n be:n :}
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 2 T=
   0 s" code" s" W-CHECK-DEFERRED" FIELD=
   0 s" severity" s" warning" FIELD=
   0 call cline 1 cbs cbs call STR:LEN + FX-PATH$ CANON$ FX$ AT-IN
   1 s" code" s" W-CHECK-DEFERRED" FIELD=
   1 s" severity" s" warning" FIELD=
   1 tok line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

: VERIFY-STOPPED-AT ( str n n -- )
   {: call:str cline:n cbs:n :}
   s" " 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" --all-errors" 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" --verify-only" 1 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" code" s" W-CHECK-STOPPED" FIELD=
   0 s" severity" s" warning" FIELD=
   0 s" label" s" may move the source root" FIELD=
   0 call cline 1 cbs cbs call STR:LEN + FX-PATH$ CANON$ FX$ AT-IN ;

\ A string loader's path is the bytes the load's literal makes. A loader in a
\ body loads when the word runs: the plain check and --all-errors follow its
\ path after the definition, kept for that load whatever the check decodes
\ first. --verify-only runs no body: a call of the body's word may move the
\ source root, so it leaves the source after the call to the run. A bad escape,
\ at top level or in a body, stops the load: each check rejects that literal
\ once, at its opener, and follows nothing after it.
: TEST-ESCAPED-PATH ( -- )
   SB-RESET
   s" : INCLUDED-WORD ( -- n ) 7 ;" SB-APPEND LF+
   s" inc-target.f" FIXTURE!
   s" an escaped included path" T-LABEL
   SB-RESET
   s\" s\\\" inc-t\\x61rget.f\" included" SB-APPEND LF+
   s" INCLUDED-WORD drop" SB-APPEND LF+
   s" escaped-included.f" FIXTURE!
   CERTIFIED
   s" an escaped required path" T-LABEL
   SB-RESET
   s\" s\\\" inc-t\\x61rget.f\" required" SB-APPEND LF+
   s" INCLUDED-WORD drop" SB-APPEND LF+
   s" escaped-required.f" FIXTURE!
   CERTIFIED
   s" a plain path holding a backslash" T-LABEL
   SB-RESET
   s" : BS-WORD ( -- n ) 9 ;" SB-APPEND LF+
   s" bs\x41.f" FIXTURE!
   SB-RESET
   s\" s\" bs\\x41.f\" included" SB-APPEND LF+
   s" BS-WORD drop" SB-APPEND LF+
   s" plain-backslash-path.f" FIXTURE!
   CERTIFIED
   s" an escaped path in a body" T-LABEL
   SB-RESET
   s\" : L ( -- ) s\\\" inc-t\\x61rget.f\" included ;" SB-APPEND LF+
   s" L" SB-APPEND LF+
   s" INCLUDED-WORD drop" SB-APPEND LF+
   s" escaped-body-path.f" FIXTURE!
   s" L" 2 43 VERIFY-STOPPED-AT
   s" an escaped path with no escape in a body" T-LABEL
   SB-RESET
   s\" : L ( -- ) s\\\" inc-target.f\" included ;" SB-APPEND LF+
   s" L" SB-APPEND LF+
   s" INCLUDED-WORD drop" SB-APPEND LF+
   s" escape-free-body-path.f" FIXTURE!
   s" L" 2 40 VERIFY-STOPPED-AT
   SB-RESET
   s" : BODY-A-WORD ( -- n ) 1 ;" SB-APPEND LF+
   s" body-a.f" FIXTURE!
   SB-RESET
   s" : BODY-B-WORD ( -- n ) 2 ;" SB-APPEND LF+
   s" body-b.f" FIXTURE!
   SB-RESET
   s" : BODY-C-WORD ( -- n ) 3 ;" SB-APPEND LF+
   s" body-c.f" FIXTURE!
   s" three escaped paths in a body" T-LABEL
   SB-RESET
   s\" : L ( -- ) s\\\" b\\x6Fdy-a.f\" included" SB-APPEND
   s\"  s\\\" b\\x6Fdy-b.f\" included" SB-APPEND
   s\"  s\\\" b\\x6Fdy-c.f\" included ;" SB-APPEND LF+
   s" L" SB-APPEND LF+
   s" BODY-A-WORD drop BODY-B-WORD drop BODY-C-WORD drop" SB-APPEND LF+
   s" escaped-body-paths.f" FIXTURE!
   s" L" 2 91 VERIFY-STOPPED-AT
   s" an escaped path in a body, escaped literals before its package closes" T-LABEL
   SB-RESET
   s" package DPT-ESC" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s\" : L ( -- ) s\\\" b\\x6Fdy-a.f\" included ;" SB-APPEND LF+
   s\" s\\\" X\\x41\" drop" SB-APPEND LF+
   s\" s\\\" Y\\x42YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY\" drop" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" DPT-ESC:L" SB-APPEND LF+
   s" BODY-A-WORD drop" SB-APPEND LF+
   s" escaped-body-path-package.f" FIXTURE!
   s" DPT-ESC:L" 7 135 VERIFY-STOPPED-AT
   s" an escaped path over the capacity as written, within it decoded" T-LABEL
   PATH-CAP 4 / s" escaped-steps-fit.f" STEPS-FIXTURE
   CERTIFIED
   s" an escaped path over the capacity decoded, within it normalized" T-LABEL
   PATH-CAP 3 * 5 / s" escaped-steps-over.f" STEPS-FIXTURE
   CERTIFIED
   s" a bad escape in a loader's path" T-LABEL
   SB-RESET
   s\" s\\\" inc-t\\yarget.f\" included" SB-APPEND LF+
   s" require missing-dep.f" SB-APPEND LF+
   s" escaped-bad-path.f" FIXTURE!
   s" E-UNTERMINATED-STRING" s\" s\\\"" 1 1 0 3 REJECTED-MODES
   s" E-UNTERMINATED-STRING" s" :1:1" 70 LOADED-AT
   s" a bad escape in a loader's path in a body" T-LABEL
   SB-RESET
   s\" : L ( -- ) s\\\" inc-t\\yarget.f\" included ;" SB-APPEND LF+
   s" require missing-dep.f" SB-APPEND LF+
   s" escaped-bad-body-path.f" FIXTURE!
   s" E-UNTERMINATED-STRING" s\" s\\\"" 1 12 11 14 REJECTED-MODES
   s" E-UNTERMINATED-STRING" s" :1:12" 70 LOADED-AT
   s" a bad escape in a loader's path in a kernel: body" T-LABEL
   SB-RESET
   s\" kernel: L ( -- ) s\\\" inc-t\\yarget.f\" included ;" SB-APPEND LF+
   s" require missing-dep.f" SB-APPEND LF+
   s" escaped-bad-kernel-path.f" FIXTURE!
   s" E-UNTERMINATED-STRING" s\" s\\\"" 1 18 17 20 REJECTED-MODES
   s" E-UNTERMINATED-STRING" s" :1:18" 70 LOADED-AT ;

\ A word a create caller made is bound, and typed `-- ^a`, by the run of that
\ caller: its tick is admitted, and a call to it fails its check in source
\ order, after a signature that does not parse and an earlier failing check. A
\ name nothing defines is still the load's compiler's to reject, before any
\ check. --verify-only does not run MK: with no row the call may read any of
\ the rest, and --verify-only discovers nothing after it.
: TEST-MADE ( -- )
   s" a ticked word a create caller made" T-LABEL
   s" : X ( -- ) ['] MADE drop ; X" s" made-tick.f" MADE-FIXTURE
   s" " 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" --all-errors" 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" [']" VERIFY-DEFERS
   s" a signature that does not parse and a call of that word" T-LABEL
   s" : X ( -- zz ) MADE ;" s" made-badsig.f" MADE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 3 10 51 53 VERIFY-REJECT-AT
   s" E-UNKNOWN-SIGNATURE-TYPE" s" :3:10" 70 LOADED-AT
   s" a signature that does not parse and a tick of that word" T-LABEL
   s" : X ( -- zz ) ['] MADE drop ;" s" made-badsig-tick.f" MADE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 3 10 51 53 VERIFY-REJECT-AT
   s" E-UNKNOWN-SIGNATURE-TYPE" s" :3:10" 70 LOADED-AT
   s" that signature, a call of that word and a trust-boundary word" T-LABEL
   s" : X ( -- zz ) MADE evaluate ;" s" made-badsig-unsafe.f" MADE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 3 10 51 53 VERIFY-REJECT-AT
   s" E-UNKNOWN-SIGNATURE-TYPE" s" :3:10" 70 LOADED-AT
   s" a trust-boundary word before a call of that word" T-LABEL
   s" : X ( -- ) evaluate MADE ;" s" made-unsafe.f" MADE-FIXTURE
   s" E-UNSAFE" s" evaluate" 3 12 53 61 VERIFY-REJECT-AT
   s" E-UNSAFE" s" :3:12" 70 LOADED-AT
   s" a call of that word, whose address X does not declare" T-LABEL
   s" : X ( -- ) MADE ;" s" made-call.f" MADE-FIXTURE
   s" E-MISMATCH" s" MADE" 3 12 53 57 RUN-REJECTED-AT
   s" MADE" VERIFY-DEFERS
   s" E-MISMATCH" s" :3:12" 70 LOADED-AT
   s" a signature that does not parse and a word nothing defines" T-LABEL
   s" : X ( -- zz ) NOPE ;" s" made-nope.f" MADE-FIXTURE
   s" E-UNDEFINED" s" :3:15" 70 LOADED-AT
   s" that signature, a call of MADE and a word nothing defines" T-LABEL
   s" : X ( -- zz ) MADE NOPE ;" s" made-call-nope.f" MADE-FIXTURE
   s" E-UNDEFINED" s" :3:20" 70 LOADED-AT
   s" that signature and a call of MADE, with no row for MK" T-LABEL
   s" : MK ( -- ) create ;" s" : X ( -- zz ) MADE ;" s" made-stop.f" MK-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 3 10 38 40 RUN-REJECTED-AT
   s" --verify-only" 1 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 21 s" W-CHECK-STOPPED" MK-WARNED
   s" E-UNKNOWN-SIGNATURE-TYPE" s" :3:10" 70 LOADED-AT ;

\ The fixture's first line: RAW-IMM, an immediate whose effect is not ( -- ),
\ so the load rejects each body that compiles it before it runs.
: RAW-IMM-LINE ( -- )
   SB-RESET
   s" : RAW-IMM ( n -- ) drop ; immediate" SB-APPEND LF+ ;

\ A rejection in the run, before an immediate runs or at `;`, names its token's
\ place in the file though comments, blank lines, tabs and runs of spaces
\ before it are not in the text the load checks.
: TEST-RUN-CAPTURED ( -- )
   s" an unmodeled immediate after comments of both kinds" T-LABEL
   RAW-IMM-LINE
   s" : P ( -- ) s" SB-APPEND 34 SB-APPEND-C s"  a does> b" SB-APPEND 34 SB-APPEND-C
   s"  2drop ( does> here ) " SB-APPEND 92 SB-APPEND-C s"  x does>" SB-APPEND LF+
   s"    RAW-IMM ;" SB-APPEND LF+
   s" imm-comments.f" FIXTURE!
   s" E-UNMODELED-IMMEDIATE" s" RAW-IMM" 3 4 95 102 RUN-REJECTED-AT
   IMM-EFFECT-NOTES
   s" an unmodeled immediate after a paren comment on its line" T-LABEL
   RAW-IMM-LINE
   s" : P ( -- ) 1 drop ( a note ) RAW-IMM ;" SB-APPEND LF+
   s" imm-paren.f" FIXTURE!
   s" E-UNMODELED-IMMEDIATE" s" RAW-IMM" 2 30 65 72 RUN-REJECTED-AT
   IMM-EFFECT-NOTES
   s" an unmodeled immediate after blank lines and tabs" T-LABEL
   RAW-IMM-LINE
   s" : P ( -- )" SB-APPEND TAB+ s" 1" SB-APPEND TAB+ TAB+ s" drop" SB-APPEND LF+
   LF+ LF+
   TAB+ s"   RAW-IMM ;" SB-APPEND LF+
   s" imm-blanks.f" FIXTURE!
   s" E-UNMODELED-IMMEDIATE" s" RAW-IMM" 5 4 60 67 RUN-REJECTED-AT
   IMM-EFFECT-NOTES
   s" a call of MADE, whose address X does not declare, after comments" T-LABEL
   SB-RESET
   s" : MK ( -- ) create ; parses: MK 1" SB-APPEND LF+
   s" MK MADE" SB-APPEND LF+
   s" : X ( -- ) ( c ) " SB-APPEND 92 SB-APPEND-C s"  c" SB-APPEND LF+
   s"    MADE ;" SB-APPEND LF+
   s" made-call-comments.f" FIXTURE!
   s" E-MISMATCH" s" MADE" 4 4 66 70 RUN-REJECTED-AT ;

\ The check with FLAG gives one record, the immediate `EV` at 2:38, which may
\ write into the body it runs in.
: EV-WRITES ( str -- )
   70 CHECK-EXIT
   s" E-UNMODELED-IMMEDIATE" REJECTED-AS
   0 s" EV" 2 38 86 88 AT
   0 s" message"
   s" `EV` runs while the definition compiles and may define words or add to a definition"
   FIELD= ;

\ An immediate that evaluates text into the body it runs in is rejected at the
\ immediate.
: TEST-IMM-COMPILES ( -- )
   s" an immediate that evaluates into its body" T-LABEL
   SB-RESET
   s\" : EV ( -- ) s\" drop\" evaluate-closed ; immediate" SB-APPEND LF+
   s" : P ( -- ) ( a comment here ) 1 drop EV ;" SB-APPEND LF+
   s" ev-none.f" FIXTURE!
   s" " EV-WRITES
   s" --verify-only" EV-WRITES
   s" E-UNMODELED-IMMEDIATE" s" :2:38" 70 LOADED-AT
   GT-ERR$ s" may define words or add to a definition" STR:CONTAINS? TTRUE ;

\ The fixture's first two lines: OPEN2 and OPEN3, definers that evaluate `drop`
\ and `1 1` into the definition they open.
: OPEN-LINES ( -- )
   SB-RESET
   s\" : OPEN2 ( str -- ) EFFECT:EMPTY define-word s\" drop\" evaluate-closed ;" SB-APPEND LF+
   s\" : OPEN3 ( str -- ) EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word s\" 1 1\" evaluate-closed ;" SB-APPEND LF+ ;

\ A rejection at a token a definer evaluates, which no source holds, is placed at the
\ `;` the reader is on; a file token after it keeps its own place.
: TEST-RUN-UNSOURCED ( -- )
   s" an underflow at a drop a definer evaluates, after a comment" T-LABEL
   OPEN-LINES
   s\" s\" Q\" OPEN2 ( a comment ) ;" SB-APPEND LF+
   s" open-none.f" FIXTURE!
   s" E-INPUT-UNDERFLOW" s" ;" 3 27 187 188 RUN-REJECTED-AT
   s" a mismatch at a drop in the file after an evaluated one" T-LABEL
   OPEN-LINES
   s\" s\" Q\" OPEN3 ( a comment here )" SB-APPEND LF+
   s"   drop drop ;" SB-APPEND LF+
   s" open-after.f" FIXTURE!
   s" E-MISMATCH" s" drop" 4 8 199 203 RUN-REJECTED-AT ;

\ The fixture's first three lines: V reads one token, and M postpones it.
: PP-LINES ( -- )
   SB-RESET
   s" : V ( -- ) parse-name drop ; immediate" SB-APPEND LF+
   s" parses: V 1" SB-APPEND LF+
   s" : M ( -- ) postpone V ; immediate" SB-APPEND LF+ ;

\ The check with FLAG gives one record, the unread immediate `M` at 4:12.
: PP-UNREAD ( str -- )
   70 CHECK-EXIT
   s" E-UNMODELED-IMMEDIATE" REJECTED-AS
   0 s" M" 4 12 96 97 AT
   0 s" message"
   s" `M` reads the source after it and no `parses:` row says how many tokens" FIELD= ;

\ The check, --verify-only and the load agree: rc 0, and the load's stderr
\ is empty.
: THREE-ADMIT ( -- )
   s" " 0 CHECK-EXIT  GJA-LINE# @ 0 T=
   s" --verify-only" 0 CHECK-EXIT  GJA-LINE# @ 0 T=
   s" " 0 LOADED ;

\ A postponed immediate runs when the word runs, so the word that postpones
\ it reads what it reads.
: TEST-IMM-POSTPONED ( -- )
   s" a postponed reading immediate with no parses: row" T-LABEL
   PP-LINES
   s" : P ( -- ) M x ;" SB-APPEND LF+
   s" imm-pp-unread.f" FIXTURE!
   s" " PP-UNREAD
   s" --verify-only" PP-UNREAD
   s" E-UNMODELED-IMMEDIATE" s" :4:12" 70 LOADED-AT
   GT-ERR$ s" reads the source after it" STR:CONTAINS? TTRUE
   s" a postponed reading immediate with its parses: row" T-LABEL
   PP-LINES
   s" parses: M 1" SB-APPEND LF+
   s" : P ( -- ) M x ;" SB-APPEND LF+
   s" imm-pp-read.f" FIXTURE!
   THREE-ADMIT ;

\ The fixture's first two lines: V executes the xt of W, which reads one token.
: TK-LINES ( -- )
   SB-RESET
   s" : W ( -- ) parse-name drop ;" SB-APPEND LF+
   s" : V ( -- ) ['] W execute ; immediate" SB-APPEND LF+ ;

\ The check with FLAG gives one record, the unread immediate `V` at 3:14.
: TK-UNREAD ( str -- )
   70 CHECK-EXIT
   s" E-UNMODELED-IMMEDIATE" REJECTED-AS
   0 s" V" 3 14 79 80 AT
   0 s" message"
   s" `V` reads the source after it and no `parses:` row says how many tokens" FIELD= ;

\ The check with FLAG gives one record, the unread immediate `V` at 5:16.
: TJ-UNREAD ( str -- )
   70 CHECK-EXIT
   s" E-UNMODELED-IMMEDIATE" REJECTED-AS
   0 s" V" 5 16 152 153 AT ;

\ An xt chosen at a branch join keeps the facts of both branches' ticks.
: TK-JOINED ( -- )
   s" an immediate executing an xt one branch ticks from a reading word" T-LABEL
   SB-RESET
   s" : W ( -- ) parse-name drop ;" SB-APPEND LF+
   s" : NOP ( -- ) ;" SB-APPEND LF+
   s" variable FLAG  -1 FLAG !" SB-APPEND LF+
   s" : V ( -- ) FLAG @ 0= IF ['] NOP ELSE ['] W THEN execute ; immediate"
   SB-APPEND LF+
   s" : P ( -- n n ) V 7 5 ;" SB-APPEND LF+
   s" P . ." SB-APPEND LF+
   s" imm-tk-join.f" FIXTURE!
   s" " TJ-UNREAD
   s" --verify-only" TJ-UNREAD
   s" E-UNMODELED-IMMEDIATE" s" :5:16" 70 LOADED-AT ;

\ An immediate reads what the words it ticks and executes read.
: TEST-IMM-TICKED ( -- )
   s" an immediate executing a ticked reading word, with no parses: row" T-LABEL
   TK-LINES
   s" : P ( -- n ) V a 5 ;" SB-APPEND LF+
   s" P ." SB-APPEND LF+
   s" imm-tk-unread.f" FIXTURE!
   s" " TK-UNREAD
   s" --verify-only" TK-UNREAD
   s" E-UNMODELED-IMMEDIATE" s" :3:14" 70 LOADED-AT
   GT-ERR$ s" reads the source after it" STR:CONTAINS? TTRUE
   s" an immediate executing a ticked reading word, with its parses: row" T-LABEL
   TK-LINES
   s" parses: V 1" SB-APPEND LF+
   s" : P ( -- n ) V a 5 ;" SB-APPEND LF+
   s" P ." SB-APPEND LF+
   s" imm-tk-read.f" FIXTURE!
   THREE-ADMIT
   GT-OUT$ s" 5" STR:CONTAINS? TTRUE
   TK-JOINED ;

\ The check with FLAG gives one record, the immediate `V` at 3:16, which may
\ write into the body it runs in.
: ITEM-WRITES ( str -- )
   70 CHECK-EXIT
   s" E-UNMODELED-IMMEDIATE" REJECTED-AS
   0 s" V" 3 16 84 85 AT
   0 s" message"
   s" `V` runs while the definition compiles and may define words or add to a definition"
   FIELD= ;

\ An immediate that appends a definition's items through EFFECT writes into
\ the body it runs in.
: TEST-IMM-ITEM ( -- )
   s" an immediate appending a call through EFFECT:CALL" T-LABEL
   SB-RESET
   s" : NEG ( n -- n ) negate ;" SB-APPEND LF+
   s\" : V ( -- ) s\" NEG\" EFFECT:CALL ; immediate" SB-APPEND LF+
   s" : P ( n -- n ) V ;" SB-APPEND LF+
   s" 5 P ." SB-APPEND LF+
   s" imm-item.f" FIXTURE!
   s" " ITEM-WRITES
   s" --verify-only" ITEM-WRITES
   s" E-UNMODELED-IMMEDIATE" s" :3:16" 70 LOADED-AT
   GT-ERR$ s" may define words or add to a definition" STR:CONTAINS? TTRUE ;

\ Record 0 of the check: `compile,` at 3:48 keeps V, which reads the source,
\ in the word MK makes, and its type states no `parses`.
: COMPILE-KEPT ( -- )
   0 s" code" s" E-MISMATCH" FIELD=
   0 s" compile," 3 48 98 106 AT
   0 s" message" s" this quotation does more than the type that keeps it states" FIELD=
   0 s" found" s" [ -- parses ]" NOTE= ;

\ `compile,` keeps the word it compiles, so its type states what that word may
\ do: compiling a reader is rejected there, and --verify-only goes on to find no Q.
: TEST-COMPILE-KEPT ( -- )
   s" compile, keeping a reader its type does not state" T-LABEL
   SB-RESET
   s" : V ( -- ) parse-name drop ; immediate" SB-APPEND LF+
   s" parses: V 1" SB-APPEND LF+
   s" : MK ( str -- ) EFFECT:EMPTY define-word ['] V compile, postpone ; ;" SB-APPEND LF+
   s\" s\" Q\" MK" SB-APPEND LF+
   s" : R ( -- ) Q ; immediate" SB-APPEND LF+
   s" : P ( -- ) R x ;" SB-APPEND LF+
   s" imm-compile-kept.f" FIXTURE!
   s" " CHECK  GJA-LINE# @ 1 T=  COMPILE-KEPT
   s" --verify-only" CHECK  GJA-LINE# @ 3 T=  COMPILE-KEPT
   1 s" code" s" E-UNDEFINED" FIELD=  1 s" Q" 5 12 140 141 AT
   s" E-MISMATCH" s" :3:48" 70 LOADED-AT ;

\ An immediate calling a word whose body the check deferred: the walk saw no fact of it, so every
\ reader admits it.
: TEST-IMM-CALLS-DEFERRED ( -- )
   s" an immediate calling a word whose body the check deferred" T-LABEL
   SB-RESET
   s" : MK ( str -- ) EFFECT:EMPTY define-word postpone ; ;" SB-APPEND LF+
   s\" s\" Q\" MK" SB-APPEND LF+
   s" : R ( -- ) Q ;" SB-APPEND LF+
   s" : S ( -- ) R ; immediate" SB-APPEND LF+
   s" : P ( -- ) S ;" SB-APPEND LF+
   s" P" SB-APPEND LF+
   s" imm-dq-call.f" FIXTURE!
   s" " 0 CHECK-EXIT  GJA-LINE# @ 0 T=
   s" --verify-only" 0 CHECK-EXIT  GJA-LINE# @ 2 T=
   s" " 0 LOADED ;

\ The check with FLAG gives one record, the immediate `V` at 2:14, which may
\ move the source root.
: ROOT-MOVES ( str -- )
   70 CHECK-EXIT
   s" E-UNMODELED-IMMEDIATE" REJECTED-AS
   0 s" V" 2 14 63 64 AT
   0 s" message"
   s" `V` runs while the definition compiles and may move the source root" FIELD=
   0 s" help" s" move the root at top level, before the definition" FIELD= ;

\ An immediate that may move the source root is rejected at the immediate.
: TEST-IMM-ROOTS ( -- )
   s" an immediate moving the source root" T-LABEL
   SB-RESET
   s\" : V ( -- ) s\" /tmp\" SOURCE-ROOT:ROOT! ; immediate" SB-APPEND LF+
   s" : P ( -- n ) V 5 ;" SB-APPEND LF+
   s" P ." SB-APPEND LF+
   s" imm-roots.f" FIXTURE!
   s" " ROOT-MOVES
   s" --verify-only" ROOT-MOVES
   s" E-UNMODELED-IMMEDIATE" s" :2:14" 70 LOADED-AT
   GT-ERR$ s" may move the source root" STR:CONTAINS? TTRUE ;

\ The fixture's first five lines: V calls the deferred D, set to W, which reads
\ one token.
: DC-LINES ( -- )
   SB-RESET
   s" : W ( -- ) parse-name drop ;" SB-APPEND LF+
   s" defer D ( -- )" SB-APPEND LF+
   s" : SET ( -- ) ['] W is D ;" SB-APPEND LF+
   s" SET" SB-APPEND LF+
   s" : V ( -- ) D ; immediate" SB-APPEND LF+ ;

\ Record K of the check: `is` at 3:20 keeps W, which reads the source, in D,
\ whose type states no `parses`.
: DC-KEPT ( n -- ) {: k:n :}
   k s" code" s" E-MISMATCH" FIELD=
   k s" is" 3 20 63 65 AT
   k s" message" s" this quotation does more than the type that keeps it states" FIELD= ;

\ A deferred word's type states what the words set in it may do: setting it to
\ a reader its type does not state is rejected where it is kept, and a
\ `parses:` row on the immediate that calls it does not admit it.
: TEST-IMM-DEFERRED-CALL ( -- )
   s" an immediate calling a deferred word set to a reader its type does not state" T-LABEL
   DC-LINES
   s" : P ( -- n ) V a 5 ;" SB-APPEND LF+
   s" P ." SB-APPEND LF+
   s" imm-dc-kept.f" FIXTURE!
   s" " CHECK  GJA-LINE# @ 1 T=  0 DC-KEPT
   s" --verify-only" CHECK  GJA-LINE# @ 2 T=  0 DC-KEPT
   1 s" code" s" E-UNDEFINED" FIELD=  1 s" a" 6 16 114 115 AT
   s" E-MISMATCH" s" :3:20" 70 LOADED-AT
   s" the same, with a parses: row on the immediate" T-LABEL
   DC-LINES
   s" parses: V 1" SB-APPEND LF+
   s" : P ( -- n ) V a 5 ;" SB-APPEND LF+
   s" P ." SB-APPEND LF+
   s" imm-dc-read.f" FIXTURE!
   s" " CHECK  GJA-LINE# @ 1 T=  0 DC-KEPT
   s" --verify-only" CHECK  GJA-LINE# @ 3 T=  0 DC-KEPT
   1 s" code" s" E-PARSES-ROW" FIELD=  1 s" V" 6 9 107 108 AT
   2 s" code" s" E-UNDEFINED" FIELD=  2 s" a" 7 16 126 127 AT
   s" E-MISMATCH" s" :3:20" 70 LOADED-AT ;

\ A parses: count reads the operand on the next line.
: TEST-IMM-LINE-END ( -- )
   s" a parses: count read across a line end" T-LABEL
   SB-RESET
   s" : V ( -- ) parse-name drop ; immediate" SB-APPEND LF+
   s" parses: V 1" SB-APPEND LF+
   s" : P ( -- n ) V" SB-APPEND LF+
   s"   a 5 ;" SB-APPEND LF+
   s" P ." SB-APPEND LF+
   s" imm-line-end.f" FIXTURE!
   THREE-ADMIT
   GT-OUT$ s" 5" STR:CONTAINS? TTRUE ;

: TEST-HEAD-CLOSER ( -- )
   s" an if open at the does> ending a definer's head, after a comment" T-LABEL
   SB-RESET
   s" true REJECTION:JSON !" SB-APPEND LF+
   s" : CONST ( n -- ) ( a note ) create dup ," SB-APPEND LF+
   s"    0= if does> ( -- n ) @ ;" SB-APPEND LF+
   s" head-open-cmt.f" FIXTURE!
   s\" \"line\":3,\"column\":10,\"byte_start\":72,\"byte_end\":77" 70 LOADED
   GT-ERR$ s\" \"token\":\"does>\"" STR:CONTAINS? TTRUE ;

: TEST-VARIANT-COLLIDE ( -- )
   s" a variant spelled like a derived word" T-LABEL
   SB-RESET
   s" SUMTYPE tdpos DERIVE eq VARIANT tag ;VARIANT ;SUMTYPE" SB-APPEND LF+
   s" variant-collide.f" FIXTURE!
   s" E-BAD-DECLARATION" s" tag" 1 33 32 35 RUN-REJECTED-AT ;

: TEST-DERIVE-PAYLOAD ( -- )
   s" a sum payload DERIVE eq cannot compare" T-LABEL
   SB-RESET
   s" SUMTYPE dpsum DERIVE eq VARIANT hold ^n ;VARIANT ;SUMTYPE"
   SB-APPEND LF+
   s" derive-payload.f" FIXTURE!
   s" E-BAD-DECLARATION" s" ^n" 1 38 37 39 RUN-REJECTED-AT ;

: TEST-DERIVE-FIELD ( -- )
   s" a structure field DERIVE eq cannot compare" T-LABEL
   SB-RESET
   s" STRUCTURE dpst DERIVE eq FIELD f ^n ;STRUCTURE" SB-APPEND LF+
   s" derive-field.f" FIXTURE!
   s" E-BAD-DECLARATION" s" ^n" 1 34 33 35 RUN-REJECTED-AT ;

: TEST-DERIVE-ENUM-FIELD ( -- )
   s" an enum field DERIVE eq cannot compare" T-LABEL
   SB-RESET
   s" ENUM dpen DERIVE eq VARIANT hold FIELD raw ^u8 ;VARIANT ;ENUM"
   SB-APPEND LF+
   s" derive-enum-field.f" FIXTURE!
   s" E-BAD-DECLARATION" s" ^u8" 1 44 43 46 RUN-REJECTED-AT ;

: TEST-DERIVE-BYTES-FIELD ( -- )
   s" a structure byte-pointer field DERIVE eq cannot compare" T-LABEL
   SB-RESET
   s" STRUCTURE dppr DERIVE eq FIELD b ^u8 ;STRUCTURE" SB-APPEND LF+
   s" derive-product-field.f" FIXTURE!
   s" E-BAD-DECLARATION" s" ^u8" 1 34 33 36 RUN-REJECTED-AT ;

: GEN-LINES ( -- )
   SB-RESET
   s\" : GEN ( -- ) s\" : MADE ( -- ) ;\" evaluate-closed ;" SB-APPEND LF+
   s" GEN" SB-APPEND LF+ ;

: SUGGESTS ( str n n str str -- )
   {: code:str want:n k:n tok:str sug:str :}
   s" --verify-only" want CHECK-EXIT
   k s" code" code FIELD=
   k s" token" tok FIELD=
   sug STR:LEN 0 > IF k s" help" sug FIELD= EXIT THEN
   GJA-LINE# @ k > dup TTRUE 0= IF EXIT THEN
   k GJA-LINE$ GJA-PARSE s" help" FIELD-ABSENT ;

: DEFER-SUGGESTS ( n n str str -- )
   {: want:n k:n tok:str sug:str :}
   s" W-CHECK-DEFERRED" want k tok sug SUGGESTS ;

\ A stop fails --verify-only when nothing is rejected.
: STOP-SUGGESTS ( n str str -- )
   {: k:n tok:str sug:str :}
   s" W-CHECK-STOPPED" 1 k tok sug SUGGESTS
   k s" message" s" checks nothing after it" FIELD-HAS ;

\ A deferral's help names what lifts its cause; one with no known cause keeps the
\ general text, and a parsing word its row bounds has no help.
: TEST-DEFER-SUGGEST ( -- )
   s" a top-level parsing word with no parses: row" T-LABEL
   SB-RESET
   s" : PN ( -- ) parse-name drop ;" SB-APPEND LF+
   s" PN foo" SB-APPEND LF+
   s" : Y ( -- ) NOPE ;" SB-APPEND LF+
   s" defer-parser.f" FIXTURE!
   0 s" PN" s" write `parses: PN n` or `parses-through: PN n ( END )` after the definition of `PN`, or call it through a word that has such a row" STOP-SUGGESTS
   s" a top-level call of a deferred word" T-LABEL
   SB-RESET
   s" defer D ( -- )" SB-APPEND LF+
   s" D" SB-APPEND LF+
   s" : Y ( -- n ) ;" SB-APPEND LF+
   s" defer-defer.f" FIXTURE!
   0 s" D" s" call `D` from a definition" STOP-SUGGESTS
   s" a top-level parsing word its parses: row bounds" T-LABEL
   SB-RESET
   s" : PN ( -- ) parse-name drop ;" SB-APPEND LF+
   s" parses: PN 1" SB-APPEND LF+
   s" PN foo" SB-APPEND LF+
   s" : Y ( -- ) NOPE ;" SB-APPEND LF+
   s" defer-parses.f" FIXTURE!
   70 0 s" PN" s" " DEFER-SUGGESTS
   s" a renderer no generates: row declares" T-LABEL
   GEN-LINES
   s" : Y ( -- ) NOPE ;" SB-APPEND LF+
   s" defer-renderer.f" FIXTURE!
   0 0 s" GEN" s" if `GEN` makes a word from the name after it, write `generates: GEN ( effect )` after its definition, or declare a foreign function with a `FUNCTION:` group" DEFER-SUGGESTS
   s" a deferred word called inside an open stretch" T-LABEL
   SB-RESET
   s\" : GEN ( -- ) s\" : MADE ( -- ) ;\" evaluate-closed ;" SB-APPEND LF+
   s" defer D ( -- )" SB-APPEND LF+
   s" GEN" SB-APPEND LF+
   s" MADE" SB-APPEND LF+
   s" D" SB-APPEND LF+
   s" : Y ( -- n ) ;" SB-APPEND LF+
   s" defer-defer-stretch.f" FIXTURE!
   70 2 s" D" s" call `D` from a definition" DEFER-SUGGESTS
   s" a deferred word whose signature names a type rendered text may make" T-LABEL
   GEN-LINES
   s" defer D ( widget -- )" SB-APPEND LF+
   s" : Y ( -- n ) ;" SB-APPEND LF+
   s" defer-type.f" FIXTURE!
   70 1 s" widget"
   s" declare `widget` before its use; a declaration left to the run is reported at its own warning"
   DEFER-SUGGESTS
   s" a top-level name only rendered text may define" T-LABEL
   GEN-LINES
   s" MADE" SB-APPEND LF+
   s" : Y ( -- ) NOPE ;" SB-APPEND LF+
   s" defer-top-rendered.f" FIXTURE!
   0 1 s" MADE" s" rewrite the code from `MADE` on with words whose effects the check knows" DEFER-SUGGESTS
   s" a body deferred to the run" T-LABEL
   GEN-LINES
   s" : X ( -- ) MADE ;" SB-APPEND LF+
   s" : Y ( -- ) NOPE ;" SB-APPEND LF+
   s" defer-body.f" FIXTURE!
   0 1 s" MADE" s" rewrite the code from `MADE` on with words whose effects the check knows" DEFER-SUGGESTS
   s" a top-level call that moves the source root before a require under it" T-LABEL
   s" defer-roots" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" defer-roots/sub" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" : LIB-ONE ( -- n ) 1 ;" s" defer-roots/sub/lib.f" LINE-FIXTURE
   SB-RESET
   s\" s\" sub\" SOURCE-ROOT:ROOT!" SB-APPEND LF+
   s" require lib.f" SB-APPEND LF+
   s" LIB-ONE drop" SB-APPEND LF+
   s" defer-roots/top.f" FIXTURE!
   0 s" SOURCE-ROOT:ROOT!" s" the run loads the files later requires name from the root it sets" STOP-SUGGESTS
   s" " 0 CHECK-EXIT
   s" a top-level parsing word that moves the source root before a require under it" T-LABEL
   s" defer-roots-parser" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" defer-roots-parser/sub" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" : LIB-ONE ( -- n ) 1 ;" s" defer-roots-parser/sub/lib.f" LINE-FIXTURE
   SB-RESET
   s" : GO ( -- ) parse-name SOURCE-ROOT:ROOT! ;" SB-APPEND LF+
   s" parses: GO 1" SB-APPEND LF+
   s" GO sub" SB-APPEND LF+
   s" require lib.f" SB-APPEND LF+
   s" LIB-ONE drop" SB-APPEND LF+
   s" defer-roots-parser/top.f" FIXTURE!
   0 s" GO" s" the run loads the files later requires name from the root it sets" STOP-SUGGESTS
   s" " 0 CHECK-EXIT
   s" a root move inside an open stretch before a require under it" T-LABEL
   s" defer-roots-stretch" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" defer-roots-stretch/sub" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" : LIB-ONE ( -- n ) 1 ;" s" defer-roots-stretch/sub/lib.f" LINE-FIXTURE
   SB-RESET
   s\" s\" : RX ( -- ) ;\" evaluate" SB-APPEND LF+
   s" RX" SB-APPEND LF+
   s\" s\" sub\" SOURCE-ROOT:ROOT!" SB-APPEND LF+
   s" require lib.f" SB-APPEND LF+
   s" LIB-ONE drop" SB-APPEND LF+
   s" defer-roots-stretch/top.f" FIXTURE!
   2 s" SOURCE-ROOT:ROOT!" s" the run loads the files later requires name from the root it sets" STOP-SUGGESTS
   s" " 0 CHECK-EXIT
   s" a call of a word that moves the source root before a require under it" T-LABEL
   s" defer-roots-caller" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" defer-roots-caller/sub" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" : LIB-ONE ( -- n ) 1 ;" s" defer-roots-caller/sub/lib.f" LINE-FIXTURE
   SB-RESET
   s\" : GO ( -- ) s\" sub\" SOURCE-ROOT:ROOT! ;" SB-APPEND LF+
   s" GO" SB-APPEND LF+
   s" require lib.f" SB-APPEND LF+
   s" LIB-ONE drop" SB-APPEND LF+
   s" defer-roots-caller/top.f" FIXTURE!
   0 s" GO" s" the run loads the files later requires name from the root it sets" STOP-SUGGESTS
   s" a top-level tick of a word that moves the source root before a require under it" T-LABEL
   s" defer-roots-tick" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" defer-roots-tick/sub" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" : LIB-ONE ( -- n ) 1 ;" s" defer-roots-tick/sub/lib.f" LINE-FIXTURE
   SB-RESET
   s\" s\" sub\" ' SOURCE-ROOT:ROOT! execute" SB-APPEND LF+
   s" require lib.f" SB-APPEND LF+
   s" LIB-ONE drop" SB-APPEND LF+
   s" defer-roots-tick/top.f" FIXTURE!
   0 s" SOURCE-ROOT:ROOT!" s" the run loads the files later requires name from the root it sets" STOP-SUGGESTS
   s" a call of a word that ticks a word that moves the source root before a require under it" T-LABEL
   s" defer-roots-btick" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" defer-roots-btick/sub" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" : LIB-ONE ( -- n ) 1 ;" s" defer-roots-btick/sub/lib.f" LINE-FIXTURE
   SB-RESET
   s\" : GO ( -- ) s\" sub\" ['] SOURCE-ROOT:ROOT! execute ;" SB-APPEND LF+
   s" GO" SB-APPEND LF+
   s" require lib.f" SB-APPEND LF+
   s" LIB-ONE drop" SB-APPEND LF+
   s" defer-roots-btick/top.f" FIXTURE!
   0 s" GO" s" the run loads the files later requires name from the root it sets" STOP-SUGGESTS
   s" a top-level move of the engine root before a require under it" T-LABEL
   s" defer-engine" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" defer-engine/src" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" defer-engine/src/lib" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" : ZZ-ONE ( -- n ) 1 ;" s" defer-engine/src/lib/zz-only.f" LINE-FIXTURE
   s" defer-engine" NAMED!
   SB-RESET
   s\" s\" " SB-APPEND  FX-PATH$ SB-APPEND  s\" \" SOURCE-ROOT:ENGINE!" SB-APPEND LF+
   s" require src/lib/zz-only.f" SB-APPEND LF+
   s" ZZ-ONE drop" SB-APPEND LF+
   s" defer-engine.f" FIXTURE!
   0 s" SOURCE-ROOT:ENGINE!" s" the run loads the files later requires name from the root it sets" STOP-SUGGESTS
   s" " 0 CHECK-EXIT ;

\ A control-flow word with no structure open, or another structure's, is one
\ the load cannot compile: it rejects the definition at that word.
: TEST-CONTROL-ORPHAN ( -- )
   s" a signature that does not parse and a then with no if" T-LABEL
   s" : X ( n -- zz ) then ;" s" badsig-then.f" LINE-FIXTURE
   s" E-CONTROL-ORPHAN" s" then" 1 17 16 20 REJECTED-MODES
   s" a then with no if before an undefined word" T-LABEL
   s" : X ( n -- n ) then NOPE ;" s" then-undefined.f" LINE-FIXTURE
   s" E-CONTROL-ORPHAN" s" then" 1 16 15 19 REJECTED-AT
   0 s" message" s" this word closes nothing open here" FIELD=
   s" a trust-boundary word before a then with no if" T-LABEL
   s" : X ( n -- n ) evaluate then ;" s" unsafe-then.f" LINE-FIXTURE
   s" E-CONTROL-ORPHAN" s" then" 1 25 24 28 REJECTED-AT
   s" an endcase with no case before an undefined word" T-LABEL
   s" : X ( n -- n ) endcase NOPE ;" s" endcase-undefined.f" LINE-FIXTURE
   s" E-CONTROL-ORPHAN" s" endcase" 1 16 15 22 REJECTED-AT
   s" a signature that does not parse and a then closing a begin" T-LABEL
   s" : X ( n -- zz ) begin then ;" s" badsig-begin-then.f" LINE-FIXTURE
   s" E-CONTROL-ORPHAN" s" then" 1 23 22 26 REJECTED-AT
   s" a leave outside every do before an undefined word" T-LABEL
   s" : X ( n -- n ) leave NOPE ;" s" leave-undefined.f" LINE-FIXTURE
   s" E-NO-LOOP" s" leave" 1 16 15 20 REJECTED-AT ;


: TEST-CONTROL-REASON ( -- )
   s" a loop body that changes the stack's depth" T-LABEL
   s" : X ( n -- n ) 3 0 do dup loop ;" s" loop-depth.f" LINE-FIXTURE
   s" E-MISMATCH" s" loop" 1 27 26 30 REJECTED-AT
   0 s" message" s" the loop body changes the stack's depth" FIELD=
   0 s" expected" s" n" NOTE=
   0 s" found" s" n n" NOTE=
   s" an if arm that changes the stack's depth" T-LABEL
   s" : X ( n bool -- n ) if 1 then ;" s" if-depth.f" LINE-FIXTURE
   s" E-MISMATCH" s" then" 1 26 25 29 REJECTED-AT
   0 s" message" s" the if arm changes the stack's depth" FIELD=
   0 s" expected" s" n" NOTE=
   0 s" found" s" n n" NOTE=
   s" if and else arms that leave different depths" T-LABEL
   s" : X ( n bool -- n ) if 1 else then ;" s" arms-depth.f" LINE-FIXTURE
   s" E-MISMATCH" s" then" 1 31 30 34 REJECTED-AT
   0 s" message" s" the if and else arms leave different stack depths" FIELD=
   0 s" expected" s" n" NOTE=
   0 s" found" s" n n" NOTE=
   s" an i outside every loop" T-LABEL
   s" : X ( -- ) i drop ;" s" i-outside.f" LINE-FIXTURE
   s" E-NO-LOOP" s" i" 1 12 11 12 REJECTED-AT
   0 s" message" s" no loop is open here" FIELD=
   s" a j in a single loop" T-LABEL
   s" : X ( -- ) 3 0 do j drop loop ;" s" j-single.f" LINE-FIXTURE
   s" E-NO-OUTER-LOOP" s" j" 1 19 18 19 REJECTED-AT
   0 s" message" s" no outer loop is open here" FIELD=
   s" an exit inside a loop with no unloop" T-LABEL
   s" : X ( -- ) 3 0 do exit loop ;" s" exit-loop.f" LINE-FIXTURE
   s" E-EXIT-IN-LOOP" s" exit" 1 19 18 22 REJECTED-AT
   0 s" message" s" exit inside a loop needs unloop first" FIELD=
   s" a loop body that changes the return stack's depth" T-LABEL
   s" : X ( -- ) 3 0 do 1 >r loop ;" s" loop-rdepth.f" LINE-FIXTURE
   s" E-LOOP-RETURN-DEPTH" s" loop" 1 24 23 27 REJECTED-AT
   0 s" message" s" the loop body changes the return stack's depth" FIELD=
   s" an if still open at ;" T-LABEL
   s" : X ( bool -- ) if ;" s" if-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 20 19 20 REJECTED-AT
   0 s" message" s" a control structure is still open at the end of the definition" FIELD=
   s" E-CONTROL-OPEN" s" :1:20" 70 LOADED-AT
   s" an if still open at ; after a rejected drop" T-LABEL
   s" : X ( -- ) drop 0 0= if ;" s" if-open-drop.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 25 24 25 REJECTED-AT
   0 s" message" s" a control structure is still open at the end of the definition" FIELD=
   s" E-CONTROL-OPEN" s" :1:25" 70 LOADED-AT
   s" a case still open at ;" T-LABEL
   s" : X ( n -- ) case ;" s" case-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 19 18 19 REJECTED-AT
   0 s" message" s" a control structure is still open at the end of the definition" FIELD=
   s" E-CONTROL-OPEN" s" :1:19" 70 LOADED-AT
   s" a loop that meets an if inside its do" T-LABEL
   s" : X ( -- ) 3 0 do 0 0= if loop then ;" s" loop-in-if.f" LINE-FIXTURE
   s" E-CONTROL-ORPHAN" s" loop" 1 27 26 30 REJECTED-AT
   0 s" message" s" this word closes nothing open here" FIELD=
   s" a loop that meets an if with no do open" T-LABEL
   s" : X ( -- ) 0 0= if loop then ;" s" loop-no-do.f" LINE-FIXTURE
   s" E-NO-LOOP" s" loop" 1 20 19 23 REJECTED-AT
   0 s" message" s" no loop is open here" FIELD=
   s" match arms that unloop different loops" T-LABEL
   SB-RESET
   s" ENUM choice first second ;ENUM" SB-APPEND LF+
   s" : LOB-MATCH ( choice -- ) 3 0 do dup MATCH choice first OF unloop ENDOF second OF ENDOF ;MATCH loop drop ;" SB-APPEND LF+
   s" match-unloop.f" FIXTURE!
   s" E-UNLOOP-JOIN" s" ENDOF" 2 83 113 118 REJECTED-AT
   0 s" message" s" the paths that meet here have unlooped different loops" FIELD= ;

\ The load's compiler takes a second else, as it takes an else over an if's
\ frame: the check fails there, which leaves the rejection to a later undefined
\ word, a signature that does not parse and an earlier trust-boundary word.
: TEST-SECOND-ELSE ( -- )
   s" a second else before an undefined word" T-LABEL
   s" : X ( bool -- ) if else else then NOPE ;" s" else2-undefined.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 35 34 38 REJECTED-AT
   s" a signature that does not parse and a second else" T-LABEL
   s" : X ( bool -- zz ) if else else then ;" s" badsig-else2.f" LINE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 1 15 14 16 REJECTED-AT
   s" a trust-boundary word before a second else" T-LABEL
   s" : X ( bool -- ) evaluate if else else then ;" s" unsafe-else2.f" LINE-FIXTURE
   s" E-UNSAFE" s" evaluate" 1 17 16 24 REJECTED-AT
   s" a second else alone" T-LABEL
   s" : X ( bool -- ) if else else then ;" s" else2.f" LINE-FIXTURE
   s" E-ELSE-TWICE" s" else" 1 25 24 28 REJECTED-AT
   0 s" message" s" this if already has an else" FIELD= ;

\ A structure still open where its definition ends is one the load cannot
\ compile: it rejects the definition at the `;`, or at the `does>` ending a
\ definer's body. A definer whose head calls a word the run decides is rejected
\ as the load rejects it, never as a duplicate of itself, and --verify-only
\ defers that head.
: TEST-CONTROL-OPEN ( -- )
   s" a signature that does not parse and an if open at ;" T-LABEL
   s" : X ( n -- zz ) dup if ;" s" badsig-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 24 23 24 REJECTED-AT
   s" a trust-boundary word and an if open at ;" T-LABEL
   s" : X ( n -- n ) evaluate if ;" s" unsafe-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 28 27 28 REJECTED-AT
   s" dead code and an if open at ;" T-LABEL
   s" : X ( n -- n ) dup if exit 1 ;" s" dead-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 30 29 30 REJECTED-AT
   s" an if open at the does> ending a definer's body" T-LABEL
   s" : CONST ( n -- ) create dup , 0= if does> ( -- n ) @ ;" s" does-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" does>" 1 37 36 41 REJECTED-AT
   s" an if open at does> after a head call the run decides" T-LABEL
   s" : MYK ( n -- ) MADE drop create dup , 0= if does> ( -- n ) @ ;"
   s" made-does-open.f" MADE-FIXTURE
   s" E-CONTROL-OPEN" s" does>" 3 45 86 91 RUN-REJECTED-AT
   s" MK" 2 34 s" MADE" 3 16 57 61 VERIFY-ONLY-AT
   s" an if open at does> on the line after a head call the run decides" T-LABEL
   SB-RESET
   s" : MK ( -- ) create ; parses: MK 1" SB-APPEND LF+
   s" MK MADE" SB-APPEND LF+
   s" : MYK ( n -- ) ( c ) MADE drop create dup ," SB-APPEND LF+
   s"    0= if does> ( -- n ) @ ;" SB-APPEND LF+
   s" made-does-open2.f" FIXTURE!
   s" E-CONTROL-OPEN" s" does>" 4 10 95 100 RUN-REJECTED-AT
   s" an orphan then before does> after a head call the run decides" T-LABEL
   s" : MYK ( n -- ) MADE drop create dup , then does> ( -- n ) @ ;"
   s" made-does-then.f" MADE-FIXTURE
   s" E-CONTROL-ORPHAN" s" then" 3 39 80 84 RUN-REJECTED-AT
   s" an undefined head call and an if open at does>" T-LABEL
   s" : MYK ( n -- ) NOSUCH drop create dup , 0= if does> ( -- n ) @ ;"
   s" made-does-nosuch.f" MADE-FIXTURE
   s" E-UNDEFINED" s" NOSUCH" 3 16 57 63 RUN-REJECTED-AT
   s" an if open at the ; ending a does> clause" T-LABEL
   s" : CONST ( n -- ) create , does> ( -- n ) @ dup 0= if ;" s" clause-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 54 53 54 REJECTED-AT
   s" a match open at ;" T-LABEL
   SB-RESET
   s" SUMTYPE mres VARIANT ok n ;VARIANT ;SUMTYPE" SB-APPEND LF+
   s" : X ( mres -- n ) MATCH mres ok OF 1 ;" SB-APPEND LF+
   s" match-open.f" FIXTURE!
   s" E-MATCH-UNTERMINATED" s" ;" 2 38 81 82 REJECTED-AT ;

\ A does> clause that walks no token and fails its end-of-body check is
\ rejected at its closing `;`, on that token's own line and bytes. The parses:
\ row then names a rejected word, a second packet only --all-errors gives.
: TEST-EMPTY-CLAUSE ( -- )
   s" an empty does> clause rejected at its ;" T-LABEL
   s" : MYK ( n -- ) create drop does> ( -- ) ;" s" empty-clause.f" LINE-FIXTURE
   s" E-MISMATCH" s" ;" 1 41 40 41 REJECTED-AT
   s" an empty does> clause rejected at its ; on a later line" T-LABEL
   SB-RESET
   s" : MYK ( n -- ) create drop" SB-APPEND LF+
   s"   does> ( -- )" SB-APPEND LF+
   s"   ;" SB-APPEND LF+
   s" empty-clause-lines.f" FIXTURE!
   s" E-MISMATCH" s" ;" 3 3 44 45 REJECTED-AT
   s" E-MISMATCH" s" :3:3" 70 LOADED-AT
   s" an empty does> clause of a definer with a parses: row" T-LABEL
   s" : MK ( n -- ) create , does> ( -- ) ; parses: MK 1" s" empty-clause-parses.f" LINE-FIXTURE
   s" " CHECK
   s" E-MISMATCH" REJECTED-AS
   0 s" ;" 1 37 36 37 AT ;

\ A TRUSTED: definer's clause is judged, in its parent's regime, before the
\ definer's row.
: TEST-TRUSTED-CLAUSE ( -- )
   s" a trusted definer's clause that leaves too much" T-LABEL
   s" TRUSTED: D ( n -- ) create , does> ( ^n -- n ) dup ;"
   s" trusted-clause.f" LINE-FIXTURE
   0 s" E-MISMATCH" s" dup" s" :1:48" 1 48 47 50 REJECTED-BOTH
   s" a trusted definer's clause that leaves a cell it promises none of" T-LABEL
   s" TRUSTED: DF ( -- ) create does> ( -- ) drop 1 2 + ;"
   s" trusted-clause-none.f" LINE-FIXTURE
   0 s" E-MISMATCH" s" +" s" :1:49" 1 49 48 49 REJECTED-BOTH
   s" a trusted definer's empty clause, at its ;" T-LABEL
   SB-RESET
   s" TRUSTED: D ( n -- ) create drop" SB-APPEND LF+
   s"   does> ( -- )" SB-APPEND LF+
   s"   ;" SB-APPEND LF+
   s" trusted-clause-empty.f" FIXTURE!
   0 s" E-MISMATCH" s" ;" s" :3:3" 3 3 49 50 REJECTED-BOTH
   s" an if open at the ; ending a trusted definer's clause" T-LABEL
   s" TRUSTED: D ( n -- ) create , does> ( -- n ) @ dup if ;"
   s" trusted-clause-open.f" LINE-FIXTURE
   0 s" E-CONTROL-OPEN" s" ;" s" :1:54" 1 54 53 54 REJECTED-BOTH
   s" a quotation in a trusted definer's clause reads a local" T-LABEL
   s" TRUSTED: D ( n -- ) create , does> ( -- n ) @ {: a:n :} [: a ;] execute ;"
   s" trusted-clause-local.f" LINE-FIXTURE
   0 s" E-BAD-LOCAL-SHAPE" s" a" s" :1:60" 1 60 59 60 REJECTED-BOTH
   s" a trusted definer's clause and its own row" T-LABEL
   s" TRUSTED: D ( -- nosuch:fam ) create does> ( ^n -- n ) dup ;"
   s" trusted-clause-row.f" LINE-FIXTURE
   s" " CHECK
   s" E-MISMATCH" REJECTED-AS
   0 s" dup" 1 55 54 57 AT
   s" E-MISMATCH" s" :1:55" 70 LOADED-AT
   s" a word the run makes in a trusted definer's clause" T-LABEL
   MADE-LINES
   s" TRUSTED: D ( n -- ) create , does> ( -- n ) @ MADE drop ;" SB-APPEND LF+
   s" 5 D W1" SB-APPEND LF+
   s" trusted-clause-made.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 2 T=
   1 s" code" s" W-CHECK-DEFERRED" FIELD=
   1 s" MADE" 3 47 88 92 AT
   s" " 0 LOADED
   s" what follows a trusted definer whose clause is rejected, which is no word" T-LABEL
   SB-RESET
   s" TRUSTED: D ( n -- ) create , does> ( ^n -- n ) dup ;" SB-APPEND LF+
   s" : Z ( -- n ) 1 ;" SB-APPEND LF+
   s" 5 D W1" SB-APPEND LF+
   s" : BAD ( -- n ) 1 2 ;" SB-APPEND LF+
   s" trusted-clause-used.f" FIXTURE!
   s" --all-errors" CHECK
   GJA-LINE# @ 4 T=
   0 s" code" s" E-MISMATCH" FIELD=
   1 s" code" s" E-UNDEFINED" FIELD=
   1 s" D" 3 3 72 73 AT
   2 s" code" s" E-UNDEFINED" FIELD=
   2 s" W1" 3 5 74 76 AT
   3 s" code" s" E-MISMATCH" FIELD=
   3 s" 2" 4 18 94 95 AT
   s" a second does> in a trusted definer, before its row" T-LABEL
   s" TRUSTED: D ( -- nosuch:fam ) create does> ( -- n ) @ does> ( -- n ) @ ;"
   s" trusted-clause-twice.f" LINE-FIXTURE
   0 s" E-DOES-TWICE" s" does>" s" :1:54" 1 54 53 58 REJECTED-BOTH ;

\ A structure or FUNCTION: group still open when its file ends, or when another
\ declaration opens, is rejected at the token that opened it.
: TEST-UNCLOSED ( -- )
   s" a structure still open at the file's end" T-LABEL
   s" BEGIN-STRUCTURE P" s" unclosed.f" LINE-FIXTURE
   s" E-UNCLOSED" s" :1:1" 70 LOADED-AT
   s" E-UNCLOSED" s" BEGIN-STRUCTURE" 1 1 0 15 REJECTED-MODES
   s" a structure opened inside an open one" T-LABEL
   SB-RESET
   s" BEGIN-STRUCTURE P" SB-APPEND LF+
   s" BEGIN-STRUCTURE Q" SB-APPEND LF+
   s" unclosed-nested.f" FIXTURE!
   s" E-UNCLOSED" s" :1:1" 70 LOADED-AT
   s" E-UNCLOSED" s" BEGIN-STRUCTURE" 1 1 0 15 REJECTED-MODES
   s" a FUNCTION: group still open at the file's end" T-LABEL
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND LF+
   s" PROCESS-SYMBOLS" SB-APPEND LF+
   s" FUNCTION: F getpid ( -- i32 )" SB-APPEND LF+
   s" unclosed-ffi.f" FIXTURE!
   s" E-UNCLOSED" s" :3:1" 70 LOADED-AT
   s" E-UNCLOSED" s" FUNCTION:" 3 1 42 51 REJECTED-MODES
   s" a structure opened inside an open FUNCTION: group" T-LABEL
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND LF+
   s" PROCESS-SYMBOLS" SB-APPEND LF+
   s" FUNCTION: F getpid ( -- i32 )" SB-APPEND LF+
   s" BEGIN-STRUCTURE P" SB-APPEND LF+
   s" END-STRUCTURE" SB-APPEND LF+
   s" ;FUNCTION" SB-APPEND LF+
   s" unclosed-ffi-nested.f" FIXTURE!
   s" E-UNCLOSED" s" :3:1" 70 LOADED-AT
   s" E-UNCLOSED" s" FUNCTION:" 3 1 42 51 REJECTED-MODES ;

: TAKEN$ ( -- str )
   s" `GP` is already defined in this wordlist" ;

\ The load's only record, and the check's first, is GP's duplicate at its name.
: TAKEN-AT ( -- )
   s" E-DUPLICATE-DEFINITION" s" :4:11" 70 LOADED-AT
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T=
   GT-ERR$ TAKEN$ STR:CONTAINS? TTRUE
   s" " CHECK
   0 s" code" s" E-DUPLICATE-DEFINITION" FIELD=
   0 s" message" TAKEN$ FIELD=
   0 s" GP" 4 11 70 72 AT ;

\ A FUNCTION: whose name is taken is rejected at the name before the group
\ opens, so an unclosed one is not rejected for being open.
: TEST-TAKEN-FUNCTION ( -- )
   s" a FUNCTION: name already taken" T-LABEL
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND LF+
   s" PROCESS-SYMBOLS" SB-APPEND LF+
   s" : GP ( -- n ) 1 ;" SB-APPEND LF+
   s" FUNCTION: GP getpid ( -- n )" SB-APPEND LF+
   s" ;FUNCTION" SB-APPEND LF+
   s" taken-ffi.f" FIXTURE!
   TAKEN-AT
   GJA-LINE# @ 1 T=
   s" a FUNCTION: name already taken, left open" T-LABEL
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND LF+
   s" PROCESS-SYMBOLS" SB-APPEND LF+
   s" : GP ( -- n ) 1 ;" SB-APPEND LF+
   s" FUNCTION: GP getpid ( -- n )" SB-APPEND LF+
   s" taken-ffi-open.f" FIXTURE!
   TAKEN-AT ;

\ The load's only record, and the check's first, is c at the token tok.
: FIRST-AT ( str str str n n n n -- )
   {: c:str at:str tok:str line:n col:n bs:n be:n :}
   c at 70 LOADED-AT
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T=
   s" " CHECK
   0 s" code" c FIELD=
   0 tok line col bs be AT ;

\ A FUNCTION: inside an open declaration rejects the open one at its opener
\ before its own name is tested, though that name is taken.
: TEST-TAKEN-FUNCTION-NESTED ( -- )
   s" a taken FUNCTION: name inside an open structure" T-LABEL
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND LF+
   s" PROCESS-SYMBOLS" SB-APPEND LF+
   s" : GP ( -- n ) 1 ;" SB-APPEND LF+
   s" BEGIN-STRUCTURE P" SB-APPEND LF+
   s" FUNCTION: GP getpid ( -- n )" SB-APPEND LF+
   s" ;FUNCTION" SB-APPEND LF+
   s" END-STRUCTURE" SB-APPEND LF+
   s" taken-ffi-nested.f" FIXTURE!
   s" E-UNCLOSED" s" :4:1" s" BEGIN-STRUCTURE" 4 1 60 75 FIRST-AT ;

\ A CALLBACK: inside an open declaration rejects the open one at its opener
\ before its own name is claimed, though that name is taken.
: TEST-TAKEN-CALLBACK-NESTED ( -- )
   s" a taken CALLBACK: name inside an open structure" T-LABEL
   SB-RESET
   s" require src/lib/ffi-callback.f" SB-APPEND LF+
   s" : GP ( -- n ) 1 ;" SB-APPEND LF+
   s" BEGIN-STRUCTURE P" SB-APPEND LF+
   s" CALLBACK: GP ( -- )" SB-APPEND LF+
   s" ;CALLBACK" SB-APPEND LF+
   s" END-STRUCTURE" SB-APPEND LF+
   s" taken-cb-nested.f" FIXTURE!
   s" E-UNCLOSED" s" :3:1" s" BEGIN-STRUCTURE" 3 1 49 64 FIRST-AT ;

\ A FUNCTION: name no definer takes is rejected at the name, as `:` would
\ reject it, before the group opens.
: TEST-WALLED-FUNCTION ( -- )
   s" a FUNCTION: name that is a keyword" T-LABEL
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND LF+
   s" PROCESS-SYMBOLS" SB-APPEND LF+
   s" FUNCTION: create getpid ( -- n )" SB-APPEND LF+
   s" ;FUNCTION" SB-APPEND LF+
   s" taken-ffi-keyword.f" FIXTURE!
   s" E-RESERVED-DEFINITION" s" :3:11" s" create" 3 11 52 58 FIRST-AT
   s" a FUNCTION: name that is a number, left open" T-LABEL
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND LF+
   s" PROCESS-SYMBOLS" SB-APPEND LF+
   s" FUNCTION: 42 getpid ( -- n )" SB-APPEND LF+
   s" taken-ffi-number.f" FIXTURE!
   s" E-NUMERIC-DEFINITION" s" :3:11" s" 42" 3 11 52 54 FIRST-AT ;

\ A FUNCTION: name the engine holds is rejected at the name as ;FUNCTION would
\ reject it: a primitive's as taken, a sealed package's as sealed.
: TEST-TAKEN-ENGINE-FUNCTION ( -- )
   s" a FUNCTION: name a primitive holds, left open" T-LABEL
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND LF+
   s" PROCESS-SYMBOLS" SB-APPEND LF+
   s" FUNCTION: VARIADIC getpid ( -- n )" SB-APPEND LF+
   s" taken-ffi-prim.f" FIXTURE!
   s" E-DUPLICATE-DEFINITION" s" :3:11" s" VARIADIC" 3 11 52 60 FIRST-AT
   s" a FUNCTION: name in a sealed package" T-LABEL
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND LF+
   s" s" SB-APPEND 34 SB-APPEND-C s"  a" SB-APPEND 34 SB-APPEND-C
   s"  s" SB-APPEND 34 SB-APPEND-C s"  b" SB-APPEND 34 SB-APPEND-C
   s"  STR:= drop" SB-APPEND LF+
   s" PROCESS-SYMBOLS" SB-APPEND LF+
   s" FUNCTION: STR:= getpid ( -- n )" SB-APPEND LF+
   s" ;FUNCTION" SB-APPEND LF+
   s" taken-ffi-sealed.f" FIXTURE!
   s" E-SEALED-PACKAGE" s" :4:11" 70 LOADED-AT
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T=
   s" " CHECK
   s" E-SEALED-PACKAGE" REJECTED-AS
   0 s" STR:=" 4 11 75 80 AT ;

\ A FUNCTION: name with a second colon is rejected at the name, though the
\ effect after it is read first.
: TEST-QUALIFIED-FUNCTION ( -- )
   s" a FUNCTION: name with two colons" T-LABEL
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND LF+
   s" PROCESS-SYMBOLS" SB-APPEND LF+
   s" FUNCTION: A:B:C getpid ( -- n )" SB-APPEND LF+
   s" ;FUNCTION" SB-APPEND LF+
   s" qualified-ffi.f" FIXTURE!
   s" E-BAD-QUALIFIED-RECORD" s" :3:11" s" A:B:C" 3 11 52 57 FIRST-AT ;

\ A throw nothing catches, of a code no table names and outside the exit
\ statuses 1..255, ends the load with one E-THROW record at the word that threw
\ it, in the file whose text ran that word. -100000 lies in error.f's range for
\ programs' own codes, which Habu never names.
\ A throw from an evaluated text is placed at the word that ran the text.
: TEST-UNCAUGHT ( -- )
   s" a throw nothing catches" T-LABEL
   SB-RESET
   s" : fail ( -- ) -100000 throw ;" SB-APPEND LF+
   s" fail" SB-APPEND LF+
   s" uncaught.f" FIXTURE!
   s" E-THROW" s" :2:1" EXIT-STATUS:UNCAUGHT LOADED-AT
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T=
   FX-P DEP-P FX-PU @ BYTE-COPY  FX-PU @ DEP-PU !
   s" a throw nothing catches in a required file" T-LABEL
   s" require uncaught.f" s" uncaught-outer.f" LINE-FIXTURE
   s" error[E-THROW]" EXIT-STATUS:UNCAUGHT LOADED
   SB-RESET  s" --> " SB-APPEND  DEP-PATH$ SB-APPEND  s" :2:1" SB-APPEND
   GT-ERR$ SB$ STR:CONTAINS? TTRUE
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T=
   s" a throw nothing catches in an evaluated text" T-LABEL
   SB-RESET
   s" : EV ( -- ) s" SB-APPEND  34 SB-APPEND-C
   s"  1 2 -77 throw" SB-APPEND  34 SB-APPEND-C
   s"  evaluate-closed ;" SB-APPEND LF+
   s" EV" SB-APPEND LF+
   s" uncaught-eval.f" FIXTURE!
   s" error[" EXIT-STATUS:UNCAUGHT LOADED
   SB-RESET  s" --> " SB-APPEND  FX-PATH$ SB-APPEND  s" :2:1" SB-APPEND
   GT-ERR$ SB$ STR:CONTAINS? TTRUE
   GT-ERR$ s" = evaluated:" STR:CONTAINS? TTRUE
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T= ;

\ A record its throw carried survives a finally whose cleanup throws and
\ catches: the load prints that record, not E-THROW.
: TEST-FINALLY-CARRIED ( -- )
   s" a carried record through finally's cleanup" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN" SB-APPEND
   S\"  s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : INNER ( -- ) 5000 throw ;" SB-APPEND LF+
   s" : CLEAN ( -- ) ['] INNER catch drop ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY ['] CLEAN finally ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" finally-carried.f" FIXTURE!
   s" error[E-PROBE]: probe text" EXIT-STATUS:UNCAUGHT LOADED
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T= ;

\ A caught code thrown on in the body that caught it keeps its record, after a
\ cleanup that throws and catches, whether the stack or a local holds it.
: TEST-RETHROW-STACK ( -- )
   s" a caught code thrown on from the stack keeps its record" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN" SB-APPEND
   S\"  s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : INNER ( -- ) 5000 throw ;" SB-APPEND LF+
   s" : CLEAN ( -- ) ['] INNER catch drop ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch dup 0<> if CLEAN throw then drop ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-stack.f" FIXTURE!
   s" error[E-PROBE]: probe text" EXIT-STATUS:UNCAUGHT LOADED
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T= ;

: TEST-RETHROW-LOCAL ( -- )
   s" a caught code thrown on from a local keeps its record" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN" SB-APPEND
   S\"  s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : INNER ( -- ) 5000 throw ;" SB-APPEND LF+
   s" : CLEAN ( -- ) ['] INNER catch drop ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch {: rc:n :} CLEAN rc 0<> if rc throw then ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-local.f" FIXTURE!
   s" error[E-PROBE]: probe text" EXIT-STATUS:UNCAUGHT LOADED
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T= ;

\ A caught code joined with another value at an arm is a new throw.
: TEST-RETHROW-JOINED ( -- )
   s" a caught code joined with another value is a new throw" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN" SB-APPEND
   S\"  s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch dup 9999 = if drop 9998 then dup 0<> if throw then drop ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-joined.f" FIXTURE!
   s" error[E-THROW]: nothing caught a throw of code 9998" EXIT-STATUS:UNCAUGHT LOADED
   GT-ERR$ s" E-PROBE" STR:CONTAINS? TFALSE
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T= ;

\ A record kept after the catch answered its code is not the caught throw's: the
\ code thrown on is a new throw.
: TEST-RETHROW-KEPT-SINCE ( -- )
   s" a caught code thrown on after a later kept record is a new throw" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN" SB-APPEND
   S\"  s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   S\" : BODY2 ( -- ) s\" E-PROBE2\" REJECTION:OPEN" SB-APPEND
   S\"  s\" probe two\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch {: rc:n :} ['] BODY2 catch drop rc 0<> if rc throw then ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-kept-since.f" FIXTURE!
   s" error[E-THROW]: nothing caught a throw of code 9999" EXIT-STATUS:UNCAUGHT LOADED
   GT-ERR$ s" E-PROBE" STR:CONTAINS? TFALSE
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T= ;

: TEST-RETHROW-RSTACK ( -- )
   s" a caught code thrown on through the return stack keeps its record" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN" SB-APPEND
   S\"  s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : INNER ( -- ) 5000 throw ;" SB-APPEND LF+
   s" : CLEAN ( -- ) ['] INNER catch drop ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch >r CLEAN r> dup 0<> if throw then drop ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-rstack.f" FIXTURE!
   s" error[E-PROBE]: probe text" EXIT-STATUS:UNCAUGHT LOADED
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T= ;

\ `ri throw` never runs, but it counts INNER's catch, so each catch keeps a slot
\ of its own: a slot shared with BODY's would lose the record.
: TEST-RETHROW-TWO ( -- )
   s" a caught code thrown on past a second counted catch keeps its record" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN" SB-APPEND
   S\"  s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : INNER ( -- ) 5000 throw ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch {: rc:n :} ['] INNER catch {: ri:n :}" SB-APPEND
   s"  rc 0<> if rc throw then ri throw ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-two.f" FIXTURE!
   s" error[E-PROBE]: probe text" EXIT-STATUS:UNCAUGHT LOADED
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T= ;

: TEST-RETHROW-QUOT ( -- )
   s" a caught local thrown from a quotation is a new throw" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN" SB-APPEND
   S\"  s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch {: rc:n :} rc 0<> if [: rc throw ;] execute then ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-quot.f" FIXTURE!
   s" error[E-THROW]: nothing caught a throw of code 9999" EXIT-STATUS:UNCAUGHT LOADED
   GT-ERR$ s" E-PROBE" STR:CONTAINS? TFALSE
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T= ;

\ A nested require's throw the program caught prints nothing, even when the
\ same code later goes uncaught.
: TEST-CAUGHT-STAYS ( -- )
   s" a caught nested throw stays caught" T-LABEL
   s" -1234 throw" s" caught-dep.f" LINE-FIXTURE
   DEP!
   SB-RESET
   S\" : R ( -- ) [: s\" " SB-APPEND  DEP-PATH$ SB-APPEND
   S\" \" required ;] catch drop ; R" SB-APPEND LF+
   s" : fail ( -- ) -1234 throw ;" SB-APPEND LF+
   s" fail" SB-APPEND LF+
   s" caught-outer.f" FIXTURE!
   s" E-THROW" s" :3:1" EXIT-STATUS:UNCAUGHT LOADED-AT
   GT-ERR$ s" error[" STR:COUNT-SUB 1 T=
   GT-ERR$ s" caught-dep.f" STR:CONTAINS? TFALSE ;

\ Package SP, holding A and sealed, in 53 bytes over five lines.
: SEALED-SP+ ( -- )
   s" package SP" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : A ( -- ) ;" SB-APPEND LF+
   s" seal-package" SB-APPEND LF+
   s" ;package" SB-APPEND LF+ ;

: TEST-SEALED-DEFINE ( -- )
   s" a definition into an engine-sealed package names its whole name" T-LABEL
   s" : ENGINE-INTERNAL:FOO ( -- ) ;" s" sealed-engine.f" LINE-FIXTURE
   s" " 70 CHECK-EXIT
   s" E-SEALED-PACKAGE" REJECTED-AS
   0 s" ENGINE-INTERNAL:FOO" 1 3 2 21 AT
   s" a definition into a package the source sealed names its whole name" T-LABEL
   SB-RESET
   SEALED-SP+
   s" : SP:B ( -- ) ;" SB-APPEND LF+
   s" sealed-source.f" FIXTURE!
   s" " 70 CHECK-EXIT
   s" E-SEALED-PACKAGE" REJECTED-AS
   0 s" SP:B" 6 3 55 59 AT ;

\ A name a sealed package already holds is rejected as sealed, as the load
\ rejects it: the seal is asked before the duplicate.
: TEST-SEALED-DUPLICATE ( -- )
   s" a duplicate definition into a sealed package names the seal" T-LABEL
   SB-RESET
   SEALED-SP+
   s" : SP:A ( -- ) ;" SB-APPEND LF+
   s" sealed-dup-define.f" FIXTURE!
   s" " 70 CHECK-EXIT
   s" E-SEALED-PACKAGE" REJECTED-AS
   0 s" SP:A" 6 3 55 59 AT
   s" a duplicate variable into a sealed package names the seal" T-LABEL
   SB-RESET
   s" package SP" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" variable X" SB-APPEND LF+
   s" seal-package" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" variable SP:X" SB-APPEND LF+
   s" sealed-dup-variable.f" FIXTURE!
   s" E-SEALED-PACKAGE" s" SP:X" 6 10 60 64 70 REJECTED-MODES-RC
   s" a duplicate export into a sealed package names the seal" T-LABEL
   SB-RESET
   s" package SP2" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : A ( -- ) ;" SB-APPEND LF+
   s" : MK ( n -- ) create , does> ( -- n ) @ ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" package SP" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : A ( -- ) ;" SB-APPEND LF+
   s" seal-package" SB-APPEND LF+
   s" EXPORT SP2:A" SB-APPEND LF+
   S\" s\" ACCEPTED\" type cr" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" sealed-dup-export.f" FIXTURE!
   s" " 70 CHECK-EXIT
   s" E-SEALED-PACKAGE" REJECTED-AS
   0 s" SP2:A" 10 8 134 139 AT ;

\ The source scan records Q-ONE's parses: row inside a checker scope that
\ rolls back; Q-TWO takes the symbol id it retired, and the restore dropped the
\ row, so Q-TWO is rejected as never declared.
: TEST-STALE-PARSES ( -- )
   s" a parses: row made in a scope that rolls back" T-LABEL
   SB-RESET
   s" require src/habu/verify-source.f" SB-APPEND LF+
   s" : Q-SCAN ( -- )" SB-APPEND LF+
   s\"    s\\\" : Q-ONE ( -- ) parse-name drop ; immediate parses: Q-ONE 1\""
   SB-APPEND LF+
   s" [: ;] VERIFY:SOURCE-BUF-THEN-IN-SCOPE ;" SB-APPEND LF+
   s" ' Q-SCAN CHECK:SCOPED" SB-APPEND LF+
   s" : Q-TWO ( -- ) parse-name drop ; immediate" SB-APPEND LF+
   s" : USE-TWO ( -- n ) Q-TWO 73 ;" SB-APPEND LF+
   s" stale-parses.f" FIXTURE!
   s" E-UNMODELED-IMMEDIATE" s" :7:20" 70 LOADED-AT ;

\ The parses: table grows: a check and a load accept 40 declared parsing
\ immediates, and the load runs each one at compile time.
: PI-NAME+ ( n -- ) {: k:n :}
   s" PI" SB-APPEND  k 10 / 48 + SB-APPEND-C  k 10 mod 48 + SB-APPEND-C ;

\ Ends the line in the string builder and moves it onto the fixture text.
: PI-LINE+ ( -- )
   LF+ SB$ FX+ SB-RESET ;

: TEST-GROWN-PARSES ( -- )
   s" forty parsing immediates" T-LABEL
   BASE$ s" grown-parses.f" FX-P FS:JOIN-PATH FX-PU !
   0 FX-U !
   SB-RESET
   s" variable HITS" SB-APPEND PI-LINE+
   s" : HIT ( -- ) parse-name drop 1 HITS +! ;" SB-APPEND PI-LINE+
   40 0 ?do
      s" : " SB-APPEND  i PI-NAME+  s"  ( -- ) HIT ; immediate parses: " SB-APPEND
      i PI-NAME+  s"  1" SB-APPEND PI-LINE+
   loop
   s" : USE ( -- n )" SB-APPEND
   40 0 ?do s"  " SB-APPEND  i PI-NAME+  s"  x" SB-APPEND loop
   s"  73 ;" SB-APPEND PI-LINE+
   s" HITS @ . USE . cr" SB-APPEND PI-LINE+
   FX-PATH$ FX$ FS:WRITE-ALL
   CERTIFIED
   s" " 0 LOADED
   GT-OUT$ s\" 40\n73\n" STR:CONTAINS? TTRUE ;

\ The load runs a declared parsing immediate on its payload, so the check leaves the payload out of
\ the body, whatever it spells, and the body takes none of its facts: a top-level call is certified.
: TEST-PARSES-PAYLOAD ( -- )
   s" parses: payloads that spell a word, a number, ; and (" T-LABEL
   SB-RESET
   s" : HIT ( -- ) parse-name drop ;" SB-APPEND LF+
   s" : PI00 ( -- ) HIT ; immediate parses: PI00 1" SB-APPEND LF+
   s" : USE1 ( -- n ) PI00 zz 73 ;" SB-APPEND LF+
   s" : USE2 ( -- ) PI00 73 ;" SB-APPEND LF+
   s" : USE3 ( -- n ) PI00 ; 73 ;" SB-APPEND LF+
   s" : USE4 ( -- n ) PI00 ( 73 ;" SB-APPEND LF+
   s" USE1 . cr" SB-APPEND LF+
   s" parses-payload.f" FIXTURE!
   CERTIFIED
   s" " 0 LOADED ;

\ The load prints each `?` local's type on its output: a layout local's is the
\ layout type, not the tag term its group's top cell carries.
: TEST-SHOW-LAYOUT-LOCAL ( -- )
   s" a ? local over an option<n> and over an n" T-LABEL
   SB-RESET
   s" require src/lib/adt/option.f" SB-APPEND LF+
   s" : Q ( option<n> -- ) {: c:? :} ;" SB-APPEND LF+
   s" : Q1 ( n -- ) {: c:? :} ;" SB-APPEND LF+
   s" show-layout-local.f" FIXTURE!
   s" " 0 LOADED
   GT-OUT$ s\" inferred c: option<n>\n" STR:CONTAINS? TTRUE
   GT-OUT$ s\" inferred c: n\n" STR:CONTAINS? TTRUE ;

\ The top-level dp-held, after DP-HOLD's public dp-held, is rejected in
\ each check mode, and the last run names DP-HOLD in its help.
: TEST-UNDEF-HOLDER ( -- )
   s" a top-level word a package holds is told its qualified form" T-LABEL
   SB-RESET
   s" package DP-HOLD" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : dp-held ( -- ) ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" dp-held" SB-APPEND LF+
   s" undef-holder.f" FIXTURE!
   s" E-UNDEFINED" s" dp-held" 5 1 51 58 REJECTED-MODES
   SB-RESET
   s" `dp-held` is public in `DP-HOLD`: " SB-APPEND
   s" write `DP-HOLD:dp-held`" SB-APPEND
   0 s" help" SB$ FIELD= ;

\ A file's `;package` of the package its includer opened is rejected in that
\ file, at its own line, by the check, --verify-only and the load alike.
: TEST-CLOSE-OUTER ( -- )
   s" an included file's ;package of its includer's package" T-LABEL
   s" ;package" s" close-outer-inc.f" LINE-FIXTURE
   DEP!
   SB-RESET
   s" package DP-OUTER" SB-APPEND LF+
   s" include close-outer-inc.f" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" close-outer.f" FIXTURE!
   s" " 70 CHECK-EXIT
   s" E-PKG-CONTEXT" REJECTED-AS
   0 s" ;package" 1 1 0 8 DEP-AT
   s" --verify-only" 70 CHECK-EXIT
   s" E-PKG-CONTEXT" REJECTED-AS
   0 s" ;package" 1 1 0 8 DEP-AT
   s" error[E-PKG-CONTEXT]" 70 LOADED
   SB-RESET  s" --> " SB-APPEND  DEP-PATH$ SB-APPEND  s" :1:1" SB-APPEND
   GT-ERR$ SB$ STR:CONTAINS? TTRUE ;

: MAIN ( -- )
   T-RESET
   ROOT!
   TEST-LINES
   TEST-TAB
   TEST-CRLF
   TEST-SPACES
   TEST-COMMENTS
   TEST-STRING
   TEST-EMOJI-STRING
   TEST-UTF8
   TEST-PAREN-EMOJI
   TEST-SIGNATURE-LINE
   TEST-LATER
   TEST-TOP-LINE
   TEST-TOP-DEP
   TEST-TOP-PRIVATE
   TEST-TOP-PRIVATE-TICK
   TEST-UNDEF-HOLDER
   TEST-CLOSE-OUTER
   TEST-SHADOWED-ARITY
   TEST-SHADOWED-FUNCTION
   TEST-SHADOWED-CASE
   TEST-GENERATED-CASE
   TEST-QUALIFIED-CASE
   TEST-ALL-RESERVED
   TEST-ALL-ORDER
   TEST-DYN-PLACE
   TEST-ALL-UNREADABLE
   TEST-REQUIRED-UNSEARCHABLE
   TEST-UNDEFINED-MISSING
   TEST-UNDEFINED-LISTED-MISSING
   TEST-NAMED-MISSING
   TEST-NAMED-LINK-UP
   TEST-NAMED-DANGLING-UP
   TEST-NAMED-FILE-DOT
   TEST-NAMED-TOO-LONG
   TEST-STDIN-PATH-TOO-LONG
   TEST-STDIN-CAST
   TEST-CALLBACK-DUPLICATE
   TEST-NAMED-DOTTED
   TEST-SCRIPT-REQUIRED-EMPTY
   TEST-LITERAL-PATH-PLACE
   TEST-LOADER-ARM
   TEST-LITERAL-SPENT
   TEST-NAMED-DIR
   TEST-REQUIRED-DIR
   TEST-NAMED-FIFO
   TEST-NAMED-PROCFS
   TEST-LISTED-PROCFS
   TEST-REQUIRED-FIFO
   TEST-ALL-DEAD-ARM
   TEST-NOT-RECORDED
   TEST-STORED-SPAN
   TEST-SOURCE-IMMEDIATE
   TEST-UNDEFINE-ABSENT
   TEST-UNDEFINE-RENDERED
   TEST-LOCAL-ESCAPE-ROWS
   TEST-TICK-STOPPED
   TEST-PROGRAM-TARGET-WORD
   TEST-PARSES-REJECT
   TEST-GENERATES
   TEST-GENERATES-READS-NONE
   TEST-COMPOSE-DEP
   TEST-COMPOSE-ALL
   TEST-COMPOSE-VERIFY
   TEST-DECLARATION
   TEST-GENERATED-NAME
   TEST-NEWTYPE
   TEST-ENUM
   TEST-ENUM-CLOSE
   TEST-STRUCTURE
   TEST-PREPASS-NOMINAL
   TEST-STDIN-DECLARATION
   TEST-VALUE-RECORD-LOAD
   TEST-VALUE-RECORD-NO-TYPE
   TEST-VALUE-RECORD-BAD-TYPE
   TEST-PREPASS-OPEN
   TEST-SUM-PAYLOAD-DEFER
   TEST-PREPASS-COMMENT
   TEST-PREPASS-NAME-COMMENT
   TEST-ARENA
   TEST-STORAGE-MALFORMED
   TEST-BAD-SIGNATURE
   TEST-LOCAL-GROUP
   TEST-STORED-SIGNATURE
   TEST-REPLACED-SIGNATURE
   TEST-DEFINITION-BINDER
   TEST-MALFORMED-RECORD
   TEST-MALFORMED-LOAD
   TEST-BAD-ESCAPE
   TEST-PRIM-ROW
   TEST-ESCAPED-PATH
   TEST-REJECTION-ORDER
   TEST-REPAIR-FOLLOWS
   TEST-RETURN-MEET
   TEST-OPEN-LAYOUT-XPORT
   TEST-LIVE-BRANCHES
   TEST-TICK-TARGET
   TEST-MADE
   TEST-RUN-CAPTURED
   TEST-IMM-COMPILES
   TEST-RUN-UNSOURCED
   TEST-IMM-POSTPONED
   TEST-IMM-TICKED
   TEST-IMM-ITEM
   TEST-COMPILE-KEPT
   TEST-IMM-CALLS-DEFERRED
   TEST-IMM-ROOTS
   TEST-IMM-DEFERRED-CALL
   TEST-IMM-LINE-END
   TEST-HEAD-CLOSER
   TEST-VARIANT-COLLIDE
   TEST-DERIVE-PAYLOAD
   TEST-DERIVE-FIELD
   TEST-DERIVE-ENUM-FIELD
   TEST-DERIVE-BYTES-FIELD
   TEST-DEFER-SUGGEST
   TEST-QUOT-LEFT
   TEST-TYPE-LEFT
   TEST-QUOT-LEFT-JUDGED
   TEST-QUOT-LEFT-RECORD
   TEST-QUOT-LEFT-FACT
   TEST-PAYLOAD-QUOT
   TEST-CATCH-LEFT
   TEST-MAYDIE-LEFT
   TEST-ZERO-LEFT
   TEST-CONTROL-ORPHAN
   TEST-CONTROL-REASON
   TEST-SECOND-ELSE
   TEST-CONTROL-OPEN
   TEST-EMPTY-CLAUSE
   TEST-TRUSTED-CLAUSE
   TEST-UNCLOSED
   TEST-TAKEN-FUNCTION
   TEST-TAKEN-FUNCTION-NESTED
   TEST-TAKEN-CALLBACK-NESTED
   TEST-WALLED-FUNCTION
   TEST-TAKEN-ENGINE-FUNCTION
   TEST-QUALIFIED-FUNCTION
   TEST-UNCAUGHT
   TEST-FINALLY-CARRIED
   TEST-RETHROW-STACK
   TEST-RETHROW-LOCAL
   TEST-RETHROW-JOINED
   TEST-RETHROW-KEPT-SINCE
   TEST-RETHROW-RSTACK
   TEST-RETHROW-TWO
   TEST-RETHROW-QUOT
   TEST-CAUGHT-STAYS
   TEST-TOP-MISMATCH
   TEST-SEALED-DEFINE
   TEST-SEALED-DUPLICATE
   TEST-STALE-PARSES
   TEST-GROWN-PARSES
   TEST-PARSES-PAYLOAD
   TEST-SHOW-LAYOUT-LOCAL
   TEST-LITERAL-FIT
   FS:CLEANUP-RUN
   T-REPORT
   s" diag-position-test: ok" type cr ;

MAIN

;package
