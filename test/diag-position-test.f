split-str
\ diag-position-test.f - a checker packet locates its token in the checked file,
\ with LF ending a line; a packet in a required file names and locates there.
\ Semantic checks call LSP-CHECK's public words in process; the verifier and run
\ stages use HABU_UNDER_TEST when set, else this engine.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test.f
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
require tools/gate-json-assert-core.f
require tools/check-core.f

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
   BASE BASE-U @ ;

: FX$ ( -- str )
   FX FX-U @ ;

: FX-PATH$ ( -- str )
   FX-P FX-PU @ ;

: DEP$ ( -- str )
   DEP DEP-U @ ;

: DEP-PATH$ ( -- str )
   DEP-P DEP-PU @ ;

: ROOT! ( -- )
   FS:CLEANUP-RESET
   s" habu-diag-position" FS:MKTEMP-DIR SOURCE-ROOT:CANONICAL drop
   {: a:^ u:n :}
   a BASE u BYTE-COPY
   u BASE-U !
   BASE$ FS:CLEANUP-TREE+ ;

: LF+ ( -- ) 10 SB-APPEND-C ;
: CR+ ( -- ) 13 SB-APPEND-C ;
: TAB+ ( -- ) 9 SB-APPEND-C ;

\ Writes the string builder's text as fixture NAME and keeps a copy for the
\ packets' byte spans.
: FIXTURE! ( str -- )
   {: name:^ nameu:n :}
   BASE$ name nameu FX-P FS:JOIN-PATH FX-PU !
   SB$
   {: a:^ u:n :}
   a FX u BYTE-COPY
   u FX-U !
   FX-PATH$ FX$ FS:WRITE-ALL ;

\ The u bytes at a end the fixture text, built in place for a fixture longer
\ than the string builder holds.
: FX+ ( str -- ) {: a:^ u:n :}
   FX-U @ u + CAP > IF E-STR-CAPACITY throw THEN
   a FX FX-U @ + u BYTE-COPY
   FX-U @ u + FX-U ! ;

\ PATH with every link resolved: the checker names a required file so, and
\ --verify-only its subject too.
: CANON$ ( str -- str )
   FS:>PATHZ CANON FS:PATHZ-CAP realpath {: n:n :}
   n 0 <= IF E-FS-PATH throw THEN
   CANON n ;

\ The fixture becomes the required file later packets name.
: DEP! ( -- )
   FX DEP FX-U @ BYTE-COPY
   FX-U @ DEP-U !
   FX-PATH$ CANON$ {: a:^ u:n :}
   a DEP-P u BYTE-COPY
   u DEP-PU ! ;

: STORE! ( str len len PROCESS:ending -- )
   {: src:^ srcu:n outu:len erru:len oc :}
   outu LEN>N GT-OUT-U !
   erru LEN>N GT-ERR-U !
   oc MATCH PROCESS:ending
     exited OF RC ! ENDOF
     signaled OF drop -1 RC ! ENDOF
     timeout OF src srcu GT-OUT$ GT-ERR$ T-TIMED-OUT ENDOF
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
   {: flag:^ flagu:n want:n :}
   LSP-CHECK:RESET
   flagu 0 > IF flag 2 + flagu 2 - LSP-CHECK:OPT THEN
   s" json-errors" LSP-CHECK:OPT
   FX-PATH$ LSP-CHECK:FILE
   want CHECK-CAPTURE ;

: CHECK ( ^u8 n -- )
   70 CHECK-EXIT ;

: FIELD-MISSING ( str -- )
   {: key:^ keyu:n :}
   T-NEXT
   T-FAIL
   s" assert: missing JSON field " type key keyu type cr ;

: FIELD-NOT ( str str -- )
   {: key:^ keyu:n kind:^ kindu:n :}
   T-NEXT
   T-FAIL
   s" assert: JSON field " type key keyu type s"  is not " type kind kindu type cr ;


: STR-FIELD= ( SPAN:span<u8> str str -- )
   {: root:SPAN:span<u8> key:^ keyu:n want:^ wantu:n :}
   root key keyu GJA-FIELD? 0= IF drop key keyu FIELD-MISSING EXIT THEN
   {: v:SPAN:span<u8> :}
   v GJA-KIND JSON:T-STR <> IF key keyu s" a string" FIELD-NOT EXIT THEN
   v GJA-STRING$ want wantu T$= ;


: INT-FIELD= ( SPAN:span<u8> str n -- )
   {: root:SPAN:span<u8> key:^ keyu:n want:n :}
   root key keyu GJA-FIELD? 0= IF drop key keyu FIELD-MISSING EXIT THEN
   {: v:SPAN:span<u8> :}
   v GJA-KIND JSON:T-INT <> IF key keyu s" an integer" FIELD-NOT EXIT THEN
   v GJA-OPEN JSON:READ-INT {: got:n :} JSON:READ-CLOSE
   got want T= ;


: FIELD-ABSENT ( SPAN:span<u8> str -- )
   {: root:SPAN:span<u8> key:^ keyu:n :}
   root key keyu GJA-FIELD? 0= IF drop T-NEXT EXIT THEN
   drop
   T-NEXT
   T-FAIL
   s" assert: unexpected JSON field " type key keyu type cr ;


: FIELD= ( n str str -- )
   {: k:n key:^ keyu:n want:^ wantu:n :}
   GJA-LINE# @ k > dup TTRUE 0= IF EXIT THEN
   k GJA-LINE$ GJA-PARSE key keyu want wantu STR-FIELD= ;

: FIELD-HAS ( n str str -- )
   {: k:n key:^ keyu:n want:^ wantu:n :}
   GJA-LINE# @ k > dup TTRUE 0= IF EXIT THEN
   k GJA-LINE$ GJA-PARSE key keyu GJA-FIELD? 0= IF drop key keyu FIELD-MISSING EXIT THEN
   {: v:SPAN:span<u8> :}
   v GJA-KIND JSON:T-STR <> IF key keyu s" a string" FIELD-NOT EXIT THEN
   v GJA-STRING$ want wantu CONTAINS? TTRUE ;

: NOTE= ( n str str -- )
   {: k:n key:^ keyu:n want:^ wantu:n :}
   GJA-LINE# @ k > dup TTRUE 0= IF EXIT THEN
   k GJA-LINE$ GJA-PARSE s" notes" GJA-FIELD? 0= IF drop s" notes" FIELD-MISSING EXIT THEN
   key keyu want wantu STR-FIELD= ;

: PLACED-IN ( n str n n n n str str -- )
   {: k:n sp:^ spu:n line:n col:n bs:n be:n path:^ pathu:n text:^ textu:n :}
   GJA-LINE# @ k > dup TTRUE 0= IF EXIT THEN
   k GJA-LINE$ GJA-PARSE
   {: root:SPAN:span<u8> :}
   root s" file" path pathu STR-FIELD=
   root s" line" line INT-FIELD=
   root s" column" col INT-FIELD=
   root s" byte_start" bs INT-FIELD=
   root s" byte_end" be INT-FIELD=
   bs 0 >=  be bs >= and  be textu <= and IF
      text bs + be bs - sp spu T$=
   ELSE
      T-NEXT s" expected span lies outside the fixture" T-ASSERT-DETAIL
   THEN ;

: AT-IN ( n str n n n n str str -- )
   {: k:n tok:^ toku:n line:n col:n bs:n be:n path:^ pathu:n text:^ textu:n :}
   k tok toku line col bs be path pathu text textu PLACED-IN
   k s" token" tok toku FIELD= ;

: AT ( n ^u8 n n n n n -- )
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
\ two-byte character in a string and in the refused token itself.
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
   {: c:^ cu:n tok:^ toku:n line:n col:n bs:n be:n path:^ pathu:n text:^ textu:n :}
   0 s" code" c cu FIELD=
   0 tok toku line col bs be path pathu text textu AT-IN ;

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
   s" a top-level refusal in a required file names that file" T-LABEL
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

: SHADOW-FIXTURE ( str str -- )
   {: decl:^ declu:n body:^ bodyu:n :}
   SB-RESET
   s" package P" SB-APPEND LF+
   s" public" SB-APPEND LF+
   decl declu SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   decl declu SB-APPEND LF+
   s" using P" SB-APPEND LF+
   s" : USE-IT ( -- )" SB-APPEND LF+
   body bodyu SB-APPEND LF+
   s" ;using" SB-APPEND LF+ ;

\ The checker resolves the token's lowercase fold; the packet names the file's
\ spelling where it lies.
: TEST-SHADOW ( -- )
   s" a bare name a global and a used public share" T-LABEL
   s" : SHADE ( -- ) ;" s"    SHADE ;" SHADOW-FIXTURE
   s" shadow.f" FIXTURE!
   s" " 70 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" SHADE" 8 4 87 92 AT
   s" --all-errors" 70 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" SHADE" 8 4 87 92 AT ;

\ `[']` and `is` read their target themselves, past the body walk's own token.
: TEST-TICK-SHADOW ( -- )
   s" that name as a ['] target" T-LABEL
   s" : SHADE ( -- ) ;" s"    ['] SHADE drop ;" SHADOW-FIXTURE
   s" tick-shadow.f" FIXTURE!
   s" " 70 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" SHADE" 8 8 91 96 AT ;

: TEST-IS-SHADOW ( -- )
   s" that name as an is target" T-LABEL
   s" defer SHADE ( -- )" s"    [: ;] is SHADE ;" SHADOW-FIXTURE
   s" is-shadow.f" FIXTURE!
   s" " 70 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" SHADE" 8 13 100 105 AT ;

\ The public F moves other cells than the private F, whose tail it shares: the
\ rejection names the folded tail and is placed at the public definition's name.
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
   {: path:^ pathu:n :}
   0 s" code" s" E-SHADOWED-ARITY" FIELD=
   0 s" message" s" private `f`" FIELD-HAS
   0 s" F" 5 5 47 48 path pathu FX$ AT-IN ;

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
   S\" : RT-P ( -- ^u8 n ) s\" absent.f\" ;" SB-APPEND LF+
   s" RT-P required" SB-APPEND LF+
   s" dyn-place.f" FIXTURE!
   s" " CHECK
   GJA-LINE# @ 1 T=
   0 s" code" s" E-LOADER-FORM" FIELD=
   0 s" required" 2 6 40 48 AT ;

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

: NOT-RECORDED ( ^u8 n -- )
   {: path:^ pathu:n :}
   0 s" code" s" W-EFFECT-NOT-RECORDED" FIELD=
   0 s" message" s" `WIDE`" FIELD-HAS
   0 s" WIDE" 3 5 71 75 path pathu FX$ AT-IN
   1 begin dup GJA-LINE# @ < while
      dup GJA-LINE$ s" W-EFFECT-NOT-RECORDED" CONTAINS? TFALSE
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
   s" " 67 CHECK-EXIT
   FX-PATH$ NOT-RECORDED
   s" --all-errors" 67 CHECK-EXIT
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
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" code" s" W-CHECK-DEFERRED" FIELD=
   0 s" [if]" 3 18 66 70 FX-PATH$ CANON$ FX$ AT-IN
   s" a target predicate undefined and redefined" T-LABEL
   SB-RESET
   s" undefine HB-TARGET-LINUX?" SB-APPEND LF+
   s" : HB-TARGET-LINUX? ( -- bool ) false ;" SB-APPEND LF+
   s" HB-TARGET-LINUX? 0= [if] require target-absent.f [then]" SB-APPEND LF+
   s" redefined-target.f" FIXTURE!
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" code" s" W-CHECK-DEFERRED" FIELD=
   0 s" [if]" 3 21 85 89 FX-PATH$ CANON$ FX$ AT-IN ;

\ The fixture's one rejection, E-PARSES-ROW at TOK, under the check and under
\ --verify-only.
: ROW-REJECTED ( str n n n n -- )
   {: tok:^ toku:n line:n col:n bs:n be:n :}
   s" " CHECK
   GJA-LINE# @ 1 T=
   0 s" code" s" E-PARSES-ROW" FIELD=
   0 tok toku line col bs be AT
   s" --verify-only" CHECK
   GJA-LINE# @ 1 T=
   0 s" code" s" E-PARSES-ROW" FIELD=
   0 tok toku line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

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

\ A refused `generates:` row is the verifier's own packet.
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

\ A row on a word that reads no name after it is refused: kept, it makes the
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

\ The row's registrar resolves its name as a use does.
: TEST-GENERATES-SHADOW ( -- )
   s" a generates: row naming that shared name" T-LABEL
   SB-RESET
   s" : SHADE ( -- ) ;" SB-APPEND LF+
   s" package P" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : SHADE ( -- ) ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" using P" SB-APPEND LF+
   s" generates: SHADE ( -- n )" SB-APPEND LF+
   s" ;using" SB-APPEND LF+
   s" generates-shadow.f" FIXTURE!
   s" " CHECK
   GJA-LINE# @ 1 T=
   0 s" SHADE" 7 12 79 84 AT ;

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
   s" --all-errors past a required file's refusal to the subject's" T-LABEL
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
   {: tail:^ tailu:n :}
   SB-RESET
   s" package HUES" SB-APPEND LF+
   s" : HUE-RED ( -- n ) 1 ;" SB-APPEND LF+
   s" SUMTYPE hue VARIANT red ;VARIANT VARIANT blue ;VARIANT ;SUMTYPE"
   SB-APPEND LF+
   tailu 0 > IF tail tailu SB-APPEND LF+ THEN
   s" ;package" SB-APPEND LF+
   s" sum-generated.f" FIXTURE! ;

\ A STRUCTURE in a private section defines CKDPROD-MAKE.
: CKDPROD-FIXTURE ( str -- )
   {: tail:^ tailu:n :}
   SB-RESET
   s" package CKDPRODS" SB-APPEND LF+
   s" : CKDPROD-MAKE ( n -- n ) 1 + ;" SB-APPEND LF+
   s" STRUCTURE ckdprod FIELD x n ;STRUCTURE" SB-APPEND LF+
   tailu 0 > IF tail tailu SB-APPEND LF+ THEN
   s" ;package" SB-APPEND LF+
   s" prod-generated.f" FIXTURE! ;

\ The generated name lies in no source text: the record is placed at the
\ family's name and names the word in its `word` note.
: GENERATED-AT ( str -- )
   {: wa:^ wu:n :}
   0 s" code" s" E-BAD-DECLARATION" FIELD=
   0 s" label" s" generated declaration already defined" FIELD=
   0 s" word" wa wu NOTE= ;

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
   {: text:^ textu:n name:^ nameu:n :}
   SB-RESET
   text textu SB-APPEND LF+
   name nameu FIXTURE! ;

: REJECTED-AS ( str -- )
   {: c:^ cu:n :}
   GJA-LINE# @ 1 T=
   0 s" code" c cu FIELD= ;

\ The composition check gives one packet, a rejection. Plain preverification
\ uses this same composition verifier; REFUSED-MODES keeps representative
\ checks across the mode boundary.
: REFUSED-AT ( str str n n n n -- )
   {: c:^ cu:n tok:^ toku:n line:n col:n bs:n be:n :}
   s" --all-errors" CHECK
   c cu REJECTED-AS
   0 tok toku line col bs be AT ;

: REFUSED-MODES-RC ( str str n n n n n -- )
   {: c:^ cu:n tok:^ toku:n line:n col:n bs:n be:n rc:n :}
   s" " rc CHECK-EXIT
   c cu REJECTED-AS
   0 tok toku line col bs be AT
   s" --all-errors" rc CHECK-EXIT
   c cu REJECTED-AS
   0 tok toku line col bs be AT
   s" --verify-only" rc CHECK-EXIT
   c cu REJECTED-AS
   0 tok toku line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

: REFUSED-MODES ( str str n n n n -- )
   70 REFUSED-MODES-RC ;

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
\ refused at the declared name.
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
   s" E-BAD-STORAGE" s" [ lin -- ]" 3 9 32 42 REFUSED-MODES
   s" an ARENA whose element type hands out an atom" T-LABEL
   SB-RESET
   s" package P" SB-APPEND LF+
   s" ARENA X [ -- xt ]" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" arena-atom.f" FIXTURE!
   s" E-BAD-STORAGE" s" [ -- xt ]" 2 9 18 27 REFUSED-MODES ;

\ A storage type that does not parse is rejected at the token the parse stopped
\ on, or on the type where the parse ran off its end.
: TEST-STORAGE-MALFORMED ( -- )
   s" a storage type that does not parse is rejected at the token the parse stopped on" T-LABEL
   SB-RESET
   s" package P" SB-APPEND LF+
   s" TYPED-VARIABLE XB [ n n ]" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" typed-var-malformed.f" FIXTURE!
   s" E-BAD-STORAGE" s" ]" 2 25 34 35 REFUSED-MODES
   0 s" label" s" the type does not parse here" FIELD=
   s" a bare `^` storage type is rejected at the `^`" T-LABEL
   SB-RESET
   s" package P" SB-APPEND LF+
   s" TYPED-VARIABLE XB ^" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" typed-var-bare-ptr.f" FIXTURE!
   s" E-BAD-STORAGE" s" ^" 2 19 28 29 REFUSED-MODES
   0 s" label" s" `^` with no element type" FIELD= ;

\ Under --verify-only the declaration pre-pass alone rejects the fixture; the
\ record's label says why.
: PREPASS-REJECTED ( str str str n n n n -- )
   {: c:^ cu:n why:^ whyu:n tok:^ toku:n line:n col:n bs:n be:n :}
   s" --verify-only" CHECK
   c cu REJECTED-AS
   0 s" label" why whyu FIELD=
   0 tok toku line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

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
   s" E-BAD-DECLARATION" s" Foo" 1 9 8 11 REFUSED-MODES
   s" a NEWTYPE header with parameters and no name, at the header" T-LABEL
   s" NEWTYPE <a>" s" newtype-params-no-name.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" <a>" 1 9 8 11 REFUSED-MODES
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
   {: one:^ oneu:n two:^ twou:n name:^ nameu:n :}
   SB-RESET
   one oneu SB-APPEND LF+
   two twou SB-APPEND LF+
   name nameu FIXTURE! ;

\ A definer reads its body with `parse-name`, so a `\` or `(` there, even one
\ hiding the terminator, is the token it refuses.
: TEST-PREPASS-COMMENT ( -- )
   s" a comment in a closed SUMTYPE" T-LABEL
   s" SUMTYPE shape VARIANT dot ;VARIANT \ note" s" ;SUMTYPE" s" sum-comment.f"
      TWO-LINE-FIXTURE
   s" E-BAD-DECLARATION" s" \" 1 36 35 36 REFUSED-MODES
   0 s" label" s" unexpected token in sum declaration" FIELD=
   s" a comment holding a SUMTYPE's terminator" T-LABEL
   s" SUMTYPE shape VARIANT dot ;VARIANT \ ;SUMTYPE" s" sum-comment-end.f" LINE-FIXTURE
   s" E-BAD-DECLARATION" s" \" 1 36 35 36 REFUSED-MODES
   0 s" label" s" unexpected token in sum declaration" FIELD=
   s" a comment in a closed STRUCTURE" T-LABEL
   s" STRUCTURE pt FIELD x n \ note" s" ;STRUCTURE" s" prod-comment.f" TWO-LINE-FIXTURE
   s" E-BAD-DECLARATION" s" \" 1 24 23 24 REFUSED-MODES
   0 s" label" s" unexpected token in structure declaration" FIELD=
   s" a comment in a closed VALUE-RECORD" T-LABEL
   s" VALUE-RECORD vr x n \ note" s" END-VALUE-RECORD" s" vrec-comment.f"
      TWO-LINE-FIXTURE
   s" E-BAD-DECLARATION" s" \" 1 21 20 21 REFUSED-MODES
   0 s" label" s" a comment, not a name" FIELD=
   s" a closed paren comment in a VALUE-RECORD" T-LABEL
   s" VALUE-RECORD vr x n ( note )" s" END-VALUE-RECORD" s" vrec-paren.f"
      TWO-LINE-FIXTURE
   s" E-BAD-DECLARATION" s" (" 1 21 20 21 REFUSED-MODES
   0 s" label" s" a signature, not a name" FIELD= ;

: TEST-PREPASS-NAME-COMMENT ( -- )
   s" a comment after a SUMTYPE's name" T-LABEL
   s" SUMTYPE shape \ note" s\" VARIANT dot ;VARIANT\n;SUMTYPE" s" sum-name-comment.f"
      TWO-LINE-FIXTURE
   s" E-BAD-DECLARATION" s" \" 1 15 14 15 REFUSED-MODES
   s" a comment after an ENUM's name" T-LABEL
   s" ENUM colr \ note" s" red green ;ENUM" s" enum-name-comment.f" TWO-LINE-FIXTURE
   s" E-BAD-DECLARATION" s" \" 1 11 10 11 REFUSED-MODES
   0 s" label" s" name must be a lowercase tail" FIELD=
   s" a comment after a STRUCTURE's name" T-LABEL
   s" STRUCTURE spt \ note" s" FIELD x n ;STRUCTURE" s" struct-name-comment.f"
      TWO-LINE-FIXTURE
   s" E-BAD-DECLARATION" s" \" 1 15 14 15 REFUSED-MODES
   s" a comment as a typed variable's name" T-LABEL
   s" TYPED-VARIABLE \ n" s" tvar-name-comment.f" LINE-FIXTURE
   s" E-MISSING-NAME" s" \" 1 16 15 16 REFUSED-MODES
   0 s" label" s" a comment, not a name" FIELD= ;

\ The load compiles a body before it checks the definition at `;`, so a body
\ word it cannot compile, undefined or a local it cannot bind, refuses a
\ definition whose signature does not parse, at that word; any other error in
\ the body leaves the refusal to the signature, at its type.
: TEST-BAD-SIGNATURE ( -- )
   s" a signature that does not parse and an undefined word" T-LABEL
   s" : X ( n -- zz ) NOPE ;" s" badsig-undefined.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 17 16 20 REFUSED-MODES
   s" that signature alone" T-LABEL
   s" : X ( n -- zz ) 1 ;" s" badsig.f" LINE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 1 12 11 13 REFUSED-MODES
   s" that undefined word alone" T-LABEL
   s" : X ( n -- n ) NOPE ;" s" undefined.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 16 15 19 REFUSED-AT
   s" that signature and an unsafe word" T-LABEL
   s" : X ( n -- zz ) evaluate ;" s" badsig-unsafe.f" LINE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 1 12 11 13 REFUSED-AT
   s" that signature and a quotation local" T-LABEL
   s" : X ( n -- zz ) [: {: y:n :} ;] drop 1 ;" s" badsig-local.f" LINE-FIXTURE
   s" E-BAD-LOCAL-SHAPE" s" {:" 1 20 19 21 REFUSED-MODES ;

\ The load binds `--` and `|` in a locals group as locals; the checker refuses
\ the definition at that token with the reason, and a body reference to it
\ keeps that refusal rather than naming an undefined word.
: TEST-LOCAL-GROUP ( -- )
   s" -- in a locals group" T-LABEL
   s" : X ( n -- n ) {: a -- b :} a ;" s" local-dash.f" LINE-FIXTURE
   s" E-BAD-LOCAL-SHAPE" s" --" 1 21 20 22 REFUSED-MODES
   0 s" message" s" a locals group names only locals; the signature declares the outputs" FIELD=
   s" | in a locals group" T-LABEL
   s" : X ( n n -- n ) {: a | :} a ;" s" local-bar.f" LINE-FIXTURE
   s" E-BAD-LOCAL-SHAPE" s" |" 1 23 22 23 REFUSED-MODES
   0 s" message" s" a locals group names only locals; Habu has no uninitialised locals" FIELD=
   s" a body reference to -- from a locals group" T-LABEL
   s" : X ( n -- n ) {: a -- b :} -- ;" s" local-dash-ref.f" LINE-FIXTURE
   s" E-BAD-LOCAL-SHAPE" s" --" 1 21 20 22 REFUSED-MODES
   s" a body reference to | from a locals group" T-LABEL
   s" : X ( n n -- n ) {: a | :} | ;" s" local-bar-ref.f" LINE-FIXTURE
   s" E-BAD-LOCAL-SHAPE" s" |" 1 23 22 23 REFUSED-MODES ;

: RECORD-PLACED ( n str str str n n n n str -- )
   {: k:n code:^ codeu:n tok:^ toku:n sp:^ spu:n line:n col:n bs:n be:n path:^ pathu:n :}
   k s" code" code codeu FIELD=
   k s" token" tok toku FIELD=
   k sp spu line col bs be path pathu FX$ PLACED-IN ;

: STORED-PLACED ( str str n n n n str -- )
   {: word:^ wordu:n tok:^ toku:n line:n col:n bs:n be:n path:^ pathu:n :}
   0 s" E-BAD-STORED-SIGNATURE" tok toku tok toku line col bs be path pathu RECORD-PLACED
   0 s" message" word wordu FIELD-HAS ;

\ STORED-PLACED for each check of the fixture, plain, --all-errors and
\ --verify-only: each gives the refusal alone, never followed by its statement's
\ throw.
: STORED-AT ( str str n n n n -- )
   {: word:^ wordu:n tok:^ toku:n line:n col:n bs:n be:n :}
   s" " CHECK
   GJA-LINE# @ 1 T=
   word wordu tok toku line col bs be FX-PATH$ STORED-PLACED
   s" --all-errors" CHECK
   GJA-LINE# @ 1 T=
   word wordu tok toku line col bs be FX-PATH$ STORED-PLACED
   s" --verify-only" CHECK
   GJA-LINE# @ 1 T=
   word wordu tok toku line col bs be FX-PATH$ CANON$ STORED-PLACED ;

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

: RECORD-AT ( ^u8 n ^u8 n ^u8 n n n n n n n -- )
   {: code:^ codeu:n tok:^ toku:n sp:^ spu:n line:n col:n bs:n be:n count:n lead:n :}
   s" " CHECK
   GJA-LINE# @ count T=
   0 code codeu tok toku sp spu line col bs be FX-PATH$ RECORD-PLACED
   s" --all-errors" CHECK
   GJA-LINE# @ count T=
   0 code codeu tok toku sp spu line col bs be FX-PATH$ RECORD-PLACED
   s" --verify-only" CHECK
   GJA-LINE# @ count lead + T=
   lead code codeu tok toku sp spu line col bs be FX-PATH$ CANON$ RECORD-PLACED ;

\ RECORD-AT for E-BAD-QUALIFIED-RECORD: each check gives the record alone, at
\ the name as written.
: MALFORMED-AT ( str n n n n -- )
   {: tok:^ toku:n line:n col:n bs:n be:n :}
   s" E-BAD-QUALIFIED-RECORD" tok toku tok toku line col bs be 1 0 RECORD-AT ;

\ A colon definition CHECK rejects: the plain check stops at that refusal, and
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

\ A record asked for a malformed qualified name is refused at the name its
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
\ compile refuses the definition at that word before any check made at `;`, and
\ otherwise the first token whose check fails does, its verdict included.
: TEST-REFUSAL-ORDER ( -- )
   s" an undefined word before a trust-boundary word" T-LABEL
   s" : X ( n -- n ) NOPE evaluate ;" s" undef-unsafe.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 16 15 19 REFUSED-AT
   s" an undefined word after a trust-boundary word" T-LABEL
   s" : X ( n -- n ) evaluate NOPE ;" s" unsafe-undef.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 25 24 28 REFUSED-AT
   s" a malformed qualified call after a trust-boundary word" T-LABEL
   s" : X ( n -- n ) evaluate A:B:C ;" s" unsafe-qual.f" LINE-FIXTURE
   s" E-BAD-QUALIFIED" s" A:B:C" 1 25 24 29 REFUSED-AT
   s" a trust-boundary word before another package's private primitive" T-LABEL
   s" : X ( n -- n ) evaluate 1 c2-invoke ;" s" unsafe-cap.f" LINE-FIXTURE
   s" E-UNSAFE" s" evaluate" 1 16 15 23 REFUSED-AT
   s" an unknown construct variant after a trust-boundary word" T-LABEL
   s" SUMTYPE rf VARIANT ok n ;VARIANT ;SUMTYPE : X ( n -- rf ) evaluate construct rf nope ;"
      s" unsafe-variant.f" LINE-FIXTURE
   s" E-CONSTRUCT-UNKNOWN-VARIANT" s" nope" 1 81 80 84 REFUSED-AT
   s" a mismatch before a refused locals group token" T-LABEL
   s" : X ( n -- n ) dup c@ {: a -- b :} a ;" s" mismatch-local.f" LINE-FIXTURE
   s" E-MISMATCH" s" c@" 1 20 19 21 REFUSED-AT
   s" a mismatch before a quotation local" T-LABEL
   s" : X ( n -- n ) dup c@ [: {: y:n :} ;] drop 1 ;" s" mismatch-qlocal.f" LINE-FIXTURE
   s" E-BAD-LOCAL-SHAPE" s" {:" 1 26 25 27 REFUSED-AT
   s" a signature that does not parse and another package's private primitive" T-LABEL
   s" : X ( n -- zz ) 1 c2-invoke ;" s" badsig-cap.f" LINE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 1 12 11 13 REFUSED-AT ;

\ A read below the return-stack cells pushed here is the return row's to repair,
\ and its record says so.
: READS-BELOW ( bool -- ) {: quoted:bool :}
   quoted IF
      0 s" message" s" return stack: the quotation reads below the return-stack cells it pushed" FIELD=
   ELSE
      0 s" message" s" return stack: this reads below the return-stack cells pushed here" FIELD=
   THEN ;

\ A read below the return-stack cells pushed here is refused at the read, not
\ at the last token walked: the measure is taken before the read's unify binds
\ the open return base.
: TEST-REPAIR-FOLLOWS ( -- )
   s" a mismatch before an unbalanced return row" T-LABEL
   s" : X ( n -- ) >r 1 c@ ;" s" mismatch-rstack.f" LINE-FIXTURE
   s" E-MISMATCH" s" c@" 1 19 18 20 REFUSED-AT
   s" a >r whose cell does not fit, as dup over that cell" T-LABEL
   SB-RESET
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE" SB-APPEND LF+
   s" : X ( rvbox<a> -- ) >r r> drop ;" SB-APPEND LF+
   s" rvbox-tor.f" FIXTURE!
   s" E-MISMATCH" s" >r" 2 21 60 62 REFUSED-AT
   0 s" expected" s" a" NOTE=
   0 s" found" s" rvbox<b>" NOTE=
   0 s" family" s" rvbox" NOTE=
   s" a 2>r whose pair does not fit, as 2dup over that pair" T-LABEL
   SB-RESET
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE" SB-APPEND LF+
   s" : X ( n rvbox<a> -- ) 2>r 2r> 2drop ;" SB-APPEND LF+
   s" rvbox-2tor.f" FIXTURE!
   s" E-MISMATCH" s" 2>r" 2 23 62 65 REFUSED-AT
   0 s" expected" s" c a" NOTE=
   0 s" found" s" n rvbox<b>" NOTE=
   0 s" family" s" rvbox" NOTE=
   s" that unbalanced return row alone" T-LABEL
   s" : X ( n -- ) >r ;" s" rstack.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" >r" 1 14 13 15 REFUSED-AT
   0 s" message" s" return stack: the return row does not balance at exit" FIELD=
   s" an r> below the return cells pushed here" T-LABEL
   s" : X ( -- ) r> drop ;" s" rread.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r>" 1 12 11 13 REFUSED-AT  false READS-BELOW
   s" an r@ below them" T-LABEL
   s" : X ( -- ) r@ drop ;" s" rfetch.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r@" 1 12 11 13 REFUSED-AT  false READS-BELOW
   s" a 2r> below them" T-LABEL
   s" : X ( -- ) 2r> 2drop ;" s" r2read.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" 2r>" 1 12 11 14 REFUSED-AT  false READS-BELOW
   s" a 2r@ below them" T-LABEL
   s" : X ( -- ) 2r@ 2drop ;" s" r2fetch.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" 2r@" 1 12 11 14 REFUSED-AT  false READS-BELOW
   s" an r> whose cell goes back with >r" T-LABEL
   s" : X ( -- ) r> >r ;" s" rround.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r>" 1 12 11 13 REFUSED-AT  false READS-BELOW
   s" an r> in the else arm, whose if arm pushed its own" T-LABEL
   s" : X ( bool -- ) if 1 >r r> drop else r> drop then ;" s" relse.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r>" 1 38 37 39 REFUSED-AT  false READS-BELOW
   s" an r> in a quotation literal" T-LABEL
   s" : X ( -- ) [: r> drop ;] drop ;" s" rquot.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r>" 1 15 14 16 REFUSED-AT  true READS-BELOW
   s" an r@ in a do loop, whose cells are not on the return row" T-LABEL
   s" : X ( n n -- ) do r@ drop loop ;" s" rloopf.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r@" 1 19 18 20 REFUSED-AT  false READS-BELOW
   s" an r> in a do loop" T-LABEL
   s" : X ( n n -- ) do r> drop loop ;" s" rloopr.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r>" 1 19 18 20 REFUSED-AT  false READS-BELOW
   s" an r> below a return row the signature names" T-LABEL
   s" : X ( | U -- | U ) r> drop ;" s" rnamed.f" LINE-FIXTURE
   s" E-RETURN-STACK" s" r>" 1 20 19 21 REFUSED-AT  false READS-BELOW
   s" a 2r> that moves a record whole, one cell short" T-LABEL
   SB-RESET
   s" STRUCTURE pr2 FIELD x n FIELD y n ;STRUCTURE" SB-APPEND LF+
   s" : X ( pr2 -- ) >r 2r> drop drop ;" SB-APPEND LF+
   s" rwide.f" FIXTURE!
   s" E-RETURN-STACK" s" 2r>" 2 19 63 66 REFUSED-AT  false READS-BELOW ;

\ A stack word over an open layout is refused by the step path's mismatch,
\ whatever else is among its operands: a proof or an expanded layout beside it
\ does not send it to the transport path, which has no reason to give.
: TEST-OPEN-LAYOUT-XPORT ( -- )
   s" an open layout under swap" T-LABEL
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE : X ( rvbox<a> n -- n rvbox<a> ) swap ;"
      s" open-layout.f" LINE-FIXTURE
   s" E-MISMATCH" s" swap" 1 74 73 77 REFUSED-AT
   s" an open layout beside a proof under swap" T-LABEL
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE : X ( rvbox<a> -- n rvbox<a> ) [: ;] catch swap ;"
      s" open-layout-proof.f" LINE-FIXTURE
   s" E-MISMATCH" s" swap" 1 84 83 87 REFUSED-AT
   s" an open layout beside an expanded layout under swap" T-LABEL
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE STRUCTURE pr2 FIELD x n FIELD y n ;STRUCTURE : X ( rvbox<a> pr2 -- pr2 rvbox<a> ) swap ;"
      s" open-layout-pr2.f" LINE-FIXTURE
   s" E-MISMATCH" s" swap" 1 123 122 126 REFUSED-AT
   s" a stale open layout under drop, a non-linear family first" T-LABEL
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE STRUCTURE pr2 FIELD x n FIELD y n ;STRUCTURE : ID ( rvbox<a> -- rvbox<a> ) ; : X ( pr2 rvbox<a> bool -- pr2 ) [: ( rvbox<a> bool -- rvbox<a> bool ) >r ID r> dup if 1 throw then ;] catch drop drop drop ;"
      s" open-layout-stale-pr2.f" LINE-FIXTURE
   s" E-STALE-READ" s" drop" 1 237 236 240 REFUSED-AT
   0 s" family" s" rvbox" NOTE=
   s" a stale open layout in a locals capture" T-LABEL
   s" STRUCTURE rvbox<a> FIELD v a ;STRUCTURE STRUCTURE pr2 FIELD x n FIELD y n ;STRUCTURE : ID ( rvbox<a> -- rvbox<a> ) ; : X ( pr2 rvbox<a> bool -- pr2 ) [: ( rvbox<a> bool -- rvbox<a> bool ) >r ID r> dup if 1 throw then ;] catch drop drop {: x :} ;"
      s" open-layout-stale-local.f" LINE-FIXTURE
   s" E-STALE-READ" s" :}" 1 242 241 243 REFUSED-AT ;


: LIT-FIXTURE ( str -- )
   {: text:^ textu:n :}
   SB-RESET
   s" : U8C ( u8 -- ) drop ;" SB-APPEND LF+
   s" : U16C ( u16 -- ) drop ;" SB-APPEND LF+
   s" : U32C ( u32 -- ) drop ;" SB-APPEND LF+
   s" : I64C ( i64 -- ) drop ;" SB-APPEND LF+
   s" : NC ( n -- ) drop ;" SB-APPEND LF+
   s" : EQ2 ( a a -- ) 2drop ;" SB-APPEND LF+
   s" : K ( a b [ a b -- ] -- ) execute ;" SB-APPEND LF+
   text textu SB-APPEND LF+
   s" literal.f" FIXTURE! ;


: LIT-REFUSED ( str str -- )
   {: text:^ textu:n why:^ whyu:n :}
   text textu T-LABEL
   text textu LIT-FIXTURE
   s" " CHECK
   s" E-MISMATCH" REJECTED-AS
   0 s" message" why whyu FIELD= ;


: LIT-ACCEPTED ( str -- )
   {: text:^ textu:n :}
   text textu T-LABEL
   text textu LIT-FIXTURE
   s" " 0 CHECK-EXIT ;


\ A number literal in a body passes into a fixed-width input or output only when
\ it fits; its fact rides stack words, the return stack, a local and equal joins.
: TEST-LITERAL-REFUSED ( -- )
   s" : X ( -- ) -1 U8C ;" s" literal `-1` does not fit `u8` (`0`..`255`)" LIT-REFUSED
   s" : X ( -- ) 256 U8C ;" s" literal `256` does not fit `u8` (`0`..`255`)" LIT-REFUSED
   s" : X ( -- ) 65536 U16C ;"
   s" literal `65536` does not fit `u16` (`0`..`65535`)" LIT-REFUSED
   s" : X ( -- ) 4294967296 U32C ;"
   s" literal `4294967296` does not fit `u32` (`0`..`4294967295`)" LIT-REFUSED
   s" : X ( -- ) 300 dup U8C drop ;"
   s" literal `300` does not fit `u8` (`0`..`255`)" LIT-REFUSED
   s" : X ( -- ) 0 300 swap drop U8C ;"
   s" literal `300` does not fit `u8` (`0`..`255`)" LIT-REFUSED
   s" : X ( -- ) 300 >r r> U8C ;"
   s" literal `300` does not fit `u8` (`0`..`255`)" LIT-REFUSED
   s" : X ( bool -- ) if 300 else 300 then U8C ;"
   s" literal `300` does not fit `u8` (`0`..`255`)" LIT-REFUSED
   s" : X ( ^u16 -- ) 70000 swap ! ;"
   s" literal `70000` does not fit `u16` (`0`..`65535`)" LIT-REFUSED
   \ The callable's variable takes `u16` from the pointee, which is demanded; a
   \ rule that treats every link between variables as provided accepts this.
   s" : X ( ^u16 -- ) 70000 swap [: ! ;] K ;"
   s" literal `70000` does not fit `u16` (`0`..`65535`)" LIT-REFUSED
   s" : X ( -- ) 300 {: c :} c U8C ;"
   s" literal `300` does not fit `u8` (`0`..`255`)" LIT-REFUSED ;


: TEST-LITERAL-ACCEPTED ( -- )
   s" : X ( -- ) 255 U8C ;" LIT-ACCEPTED
   s" : X ( -- ) 0 U8C ;" LIT-ACCEPTED
   s" : X ( -- ) -1 I64C ;" LIT-ACCEPTED
   s" : X ( -- ) -1 NC ;" LIT-ACCEPTED
   s" : X ( -- ) 300 1 - U8C ;" LIT-ACCEPTED
   s" : X ( ^u16 -- ) 65 swap ! ;" LIT-ACCEPTED
   s" : X ( bool -- ) if 5 else 300 then U8C ;" LIT-ACCEPTED
   \ The carried cell is an `n`, which passes into `u8`; a rule that refuses an
   \ `n` there refuses this.
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
   TEST-LITERAL-REFUSED
   TEST-LITERAL-ACCEPTED ;

\ A match whose scrutinee does not fit, and a match or case one of whose
\ branches fails, still has live words that the load compiles.
: TEST-LIVE-BRANCHES ( -- )
   s" an undefined word in a match whose scrutinee does not fit" T-LABEL
   s" SUMTYPE mres VARIANT ok n ;VARIANT ;SUMTYPE : X ( n -- n ) MATCH mres ok OF FIRST ENDOF ;MATCH SECOND ;"
      s" scrutinee-first.f" LINE-FIXTURE
   s" E-UNDEFINED" s" FIRST" 1 77 76 81 REFUSED-AT
   s" an undefined word after that match" T-LABEL
   s" SUMTYPE mres VARIANT ok n ;VARIANT ;SUMTYPE : X ( n -- n ) MATCH mres ok OF 1 ENDOF ;MATCH SECOND ;"
      s" scrutinee-after.f" LINE-FIXTURE
   s" E-UNDEFINED" s" SECOND" 1 92 91 97 REFUSED-AT
   s" an undefined word after a match branch that fails" T-LABEL
   s" SUMTYPE mres VARIANT ok n ;VARIANT ;SUMTYPE : X ( mres -- n ) MATCH mres ok OF 1 c@ ENDOF ;MATCH NOPE ;"
      s" match-branch-after.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 98 97 101 REFUSED-AT
   s" an undefined word after a case branch that fails" T-LABEL
   s" : X ( n -- n ) CASE 1 OF 1 c@ ENDOF ENDCASE NOPE ;" s" case-branch-after.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 45 44 48 REFUSED-AT ;

: CERTIFIED ( -- )
   s" " 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" --all-errors" 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 0 T= ;

\ A word `[']` ticks that does not exist refuses the definition at that word
\ before a signature that does not parse and before an earlier failing check.
: TEST-TICK-TARGET ( -- )
   s" a signature that does not parse and an undefined ticked word" T-LABEL
   s" : X ( n -- zz ) ['] NOPE ;" s" badsig-tick.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 21 20 24 REFUSED-AT
   s" an undefined ticked word after a trust-boundary word" T-LABEL
   s" : X ( n -- n ) evaluate ['] NOPE ;" s" unsafe-tick.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 29 28 32 REFUSED-AT
   s" an undefined ticked word alone" T-LABEL
   s" : X ( -- n ) ['] NOPE ;" s" tick-undefined.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 18 17 21 REFUSED-AT
   s" a defined ticked word" T-LABEL
   s" : X ( -- ) ['] dup drop ;" s" tick-defined.f" LINE-FIXTURE
   CERTIFIED ;

: MK-FIXTURE ( str str str -- )
   {: def:^ defu:n text:^ textu:n name:^ nameu:n :}
   SB-RESET
   def defu SB-APPEND LF+
   s" MK MADE" SB-APPEND LF+
   text textu SB-APPEND LF+
   name nameu FIXTURE! ;

\ MK-FIXTURE whose row bounds MK's call to the name it reads, so the check goes
\ on after it.
: MADE-FIXTURE ( str str -- )
   {: text:^ textu:n name:^ nameu:n :}
   s" : MK ( -- ) create ; parses: MK 1" text textu name nameu MK-FIXTURE ;

: MK-DEFERRED ( n n -- )
   {: k:n bs:n :}
   k s" code" s" W-CHECK-DEFERRED" FIELD=
   k s" severity" s" warning" FIELD=
   k s" MK" 2 1 bs bs 2 + FX-PATH$ CANON$ FX$ AT-IN ;

: RUN-REFUSED-AT ( str str n n n n -- )
   {: c:^ cu:n tok:^ toku:n line:n col:n bs:n be:n :}
   s" " CHECK
   c cu REJECTED-AS
   0 tok toku line col bs be AT
   s" --all-errors" CHECK
   c cu REJECTED-AS
   0 tok toku line col bs be AT ;

\ --verify-only defers both MK's call and X's body, whose use of MADE needs its
\ own warning where the check stopped.
: VERIFY-DEFERS ( str -- )
   {: tok:^ toku:n :}
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 2 T=
   0 34 MK-DEFERRED
   1 s" code" s" W-CHECK-DEFERRED" FIELD=
   1 s" severity" s" warning" FIELD=
   1 tok toku 3 12 53 53 toku + FX-PATH$ CANON$ FX$ AT-IN ;

: VERIFY-REFUSED-AT ( str str n n n n -- )
   {: c:^ cu:n tok:^ toku:n line:n col:n bs:n be:n :}
   c cu tok toku line col bs be RUN-REFUSED-AT
   s" --verify-only" CHECK
   GJA-LINE# @ 2 T=
   0 34 MK-DEFERRED
   1 s" code" c cu FIELD=
   1 tok toku line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

\ hb, run on the arguments added and IN as its stdin, exits WANT with TEXT in
\ its stderr, or with an empty stderr when TEXT is empty.
: PIPED ( ^u8 n ^u8 n n -- )
   {: in:^ inu:n text:^ textu:n want:n :}
   FX-PATH$
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN  in inu >LEN  [: GT-OUT-ROOM ;] [: GT-ERR-ROOM ;]
   PROCESS:WAIT-FOREVER RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME
   STORE!
   RC @ want T=
   textu 0= IF GT-ERR-U @ 0 T= EXIT THEN
   GT-ERR$ text textu CONTAINS? TTRUE ;

: LAUNCHED ( str n -- )
   {: text:^ textu:n want:n :}
   EMPTY 0 text textu want PIPED ;

: LOADED ( ^u8 n n -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   FX-PATH$ >LEN PROC-ARGV+
   LAUNCHED ;

\ The load exits RC after its record for CODE, placed at AT after the fixture's
\ path (`:line:column`, or nothing for a record with no place).
: LOADED-AT ( str str n -- )
   {: c:^ cu:n at:^ atu:n rc:n :}
   SB-RESET s" error[" SB-APPEND c cu SB-APPEND s" ]" SB-APPEND
   SB$ rc LOADED
   SB-RESET s" --> " SB-APPEND FX-PATH$ SB-APPEND at atu SB-APPEND
   GT-ERR$ SB$ CONTAINS? TTRUE ;

\ The fixture path names NAME under the test's root, without making it.
: NAMED! ( str -- ) {: name:^ nameu:n :}
   BASE$ name nameu FX-P CAP SPAN:MAKE FS:JOIN-PATH-INTO FX-PU ! ;

\ The check's only record stands in no source: it names no file and no word.
: UNPLACED ( -- )
   GJA-LINE# @ 1 T=
   GJA-LINE# @ 0 > 0= IF EXIT THEN
   0 GJA-LINE$ GJA-PARSE {: root:SPAN:span<u8> :}
   root s" file" FIELD-ABSENT
   root s" notes" GJA-FIELD? IF s" word" FIELD-ABSENT ELSE drop THEN ;

\ NAME under the test's root, the root spelled with every link resolved.
: CANON-NAMED ( str -- str ) {: name:^ nameu:n :}
   BASE$ CANON$ name nameu DIR-P FS:JOIN-PATH {: u:n :}
   DIR-P u ;

\ The fixture path, a named input that does not exist, gets the load's record
\ and status, searched at WANT, from the load and the check alike.
: NAMED-MISSING ( str -- ) {: want:^ wantu:n :}
   SB-RESET  s" error[E-MISSING-SOURCE]: cannot open `" SB-APPEND  FX-PATH$ SB-APPEND
   S\" `\n  = searched: " SB-APPEND  want wantu SB-APPEND
   S\" \n  = help: a path given on the command line is looked up from the" SB-APPEND
   S\"  directory hb runs in\n\n" SB-APPEND
   SB$ 70 LOADED  GT-ERR$ SB$ T$=
   s" " CHECK
   0 s" code" s" E-MISSING-SOURCE" FIELD=
   SB-RESET  s" cannot open `" SB-APPEND  FX-PATH$ SB-APPEND  s" `" SB-APPEND
   0 s" message" SB$ FIELD=
   0 s" searched" want wantu NOTE=
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
: RAN ( ^u8 n n -- )
   PROC-ARGV-RESET  FX-PATH$ >LEN PROC-ARGV+  LAUNCHED ;

\ The arguments start tools/check.f with --json-errors.
: CHECK-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+  s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+  s" --json-errors" >LEN PROC-ARGV+ ;

\ tools/check.f checks the fixture path with --json-errors, exiting WANT with
\ TEXT in its stderr, whose records are split for FIELD=.
: CHECKED ( ^u8 n n -- )
   CHECK-ARGV  FX-PATH$ >LEN PROC-ARGV+
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
   LONG-NAME LONG-NAME-BYTES NAMED! ;

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
   s" --verify-only" >LEN PROC-ARGV+  s" --stdin-path" >LEN PROC-ARGV+
   FX-PATH$ >LEN PROC-ARGV+
   s" E-PATH-RANGE" 70 LAUNCHED  GT-ERR$ GJA-SPLIT-LINES
   [: TOO-LONG-MSG ;] RANGE-FIELDS ;

\ A source read from stdin is placed in its own bytes, as the file holding them
\ is, where an `evaluate` of them counts its places too.
: TEST-STDIN-CAST ( -- )
   s" a cast evaluated from stdin's bytes, at its place in <stdin>" T-LABEL
   s" parse-name x drop 26 + 32 evaluate \ cast: LRD-WIDE ( n -- ptr u8 n )"
   {: src:^ srcu:n :}
   CHECK-ARGV
   src srcu s" E-CAST-FAM" 70 PIPED  GT-ERR$ GJA-SPLIT-LINES
   GJA-LINE# @ 1 T=
   0 s" ptr" 1 60 59 62 s" <stdin>" src srcu PLACED-IN ;

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
: DOTTED! ( str -- ) {: name:^ nameu:n :}
   BASE$ {: b:^ bu:n :}
   b FX-P bu BYTE-COPY  47 FX-P bu + c!  bu 1+ FX-PU !
   begin FX-PU @ nameu + PATH-CAP <= while
      46 FX-P FX-PU @ + c!  47 FX-P FX-PU @ 1+ + c!
      FX-PU @ 2 + FX-PU !
   repeat
   name FX-P FX-PU @ + nameu BYTE-COPY
   FX-PU @ nameu + FX-PU ! ;

\ Fixture NAME's one line includes the fixture path, which may be past the
\ string builder's capacity.
: INCLUDER! ( str -- ) {: name:^ nameu:n :}
   s" include " {: k:^ ku:n :}
   k FX ku BYTE-COPY
   FX-P FX ku + FX-PU @ BYTE-COPY
   ku FX-PU @ + {: u:n :}
   10 FX u + c!
   u 1+ FX-U !
   BASE$ name nameu FX-P FS:JOIN-PATH FX-PU !
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
: RAN-AT ( str n n -- ) {: c:^ cu:n l:n col:n :}
   SB-RESET  s" error[" SB-APPEND  c cu SB-APPEND
   SB$ 70 RAN
   SB-RESET  s" --> " SB-APPEND  FX-PATH$ SB-APPEND  s" :" SB-APPEND
   l FMT:SB-U  s" :" SB-APPEND  col FMT:SB-U  LF+
   GT-ERR$ SB$ CONTAINS? TTRUE ;

\ The run, the check and --verify-only place the record for CODE at the
\ fixture's bytes BS to BE on line 1.
: LITERAL-PLACED ( str n n -- ) {: c:^ cu:n bs:n be:n :}
   c cu 1 bs 1+ RAN-AT
   c cu 70 CHECKED
   c cu REJECTED-AS
   0 FX bs + be bs -  1 bs 1+ bs be FX-PATH$ FX$ PLACED-IN
   s" --verify-only" 70 CHECK-EXIT
   c cu REJECTED-AS
   0 FX bs + be bs -  1 bs 1+ bs be FX-PATH$ CANON$ FX$ PLACED-IN ;

\ Fixture NAME's one line passes `included` an escaped literal, LONG-NAME-BYTES
\ `x` then `\x41.f`, past the cap; written in FX, as the string builder holds
\ less.
: LONG-ESCAPED! ( str -- ) {: name:^ nameu:n :}
   BASE$ name nameu FX-P FS:JOIN-PATH FX-PU !
   s\" s\\\" " {: k:^ ku:n :}
   k FX ku BYTE-COPY
   LONG-NAME-BYTES 0 ?do 120 FX ku + i + c! loop
   s\" \\x41.f\" included\n" {: t:^ tu:n :}
   t FX ku + LONG-NAME-BYTES + tu BYTE-COPY
   ku LONG-NAME-BYTES + tu + FX-U !
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

\ A literal's record lasts only the next token of its own text: a path a loader
\ is given later, in bytes its copy matches, is not placed at the literal but
\ at the loader where source read it last, else nowhere, and names no word.
: TEST-LITERAL-SPENT ( -- )
   s" a literal's copy rewritten and passed later" T-LABEL
   SB-RESET  s" : Z ( ^u8 -- ) 8 0 ?do 122 over i + c! loop drop ;" SB-APPEND LF+
   s\" s\" nofile.f\" 2drop -8 allot" SB-APPEND LF+
   s" here 8 allot dup Z 8 included" SB-APPEND LF+
   s" spent-rewritten.f" FIXTURE!
   s" E-MISSING-SOURCE" 3 22 RAN-AT
   s" a literal in an evaluated buffer rewritten before the load" T-LABEL
   SB-RESET  s" create B 64 allot" SB-APPEND LF+
   s" : Q ( -- ) 64 0 ?do 81 B i + c! loop ;" SB-APPEND LF+
   s" : PUT ( ^u8 n -- ^u8 n ) dup 0 ?do over i + c@ B i + c! loop nip B swap ;" SB-APPEND LF+
   s\" s\\\" s\\\" nofile.f\\\"\" PUT evaluate" SB-APPEND LF+
   s" Q" SB-APPEND LF+  s" included" SB-APPEND LF+
   s" spent-evaluated.f" FIXTURE!
   s" E-MISSING-SOURCE" 6 1 RAN-AT
   GT-ERR$ s" = word:" CONTAINS? TFALSE
   s" a buffer evaluated again at its address, its literal's copy held" T-LABEL
   SB-RESET  s" create B 64 allot" SB-APPEND LF+
   s" : PUT ( ^u8 n -- ^u8 n ) dup 0 ?do over i + c@ B i + c! loop nip B swap ;" SB-APPEND LF+
   s\" : S2 ( -- ^u8 n ) s\" 1 2 2drop    included\" ;" SB-APPEND LF+
   s\" s\\\" s\\\" nofile.f\\\"\" PUT evaluate" SB-APPEND LF+
   s" S2 PUT evaluate" SB-APPEND LF+
   s" spent-reevaluated.f" FIXTURE!
   s" E-MISSING-SOURCE" 5 8 RAN-AT
   GT-ERR$ s" = word:" CONTAINS? TFALSE
   s" a buffer evaluated closed twice in one word, its literal's copy passed later" T-LABEL
   SB-RESET  s" create B 64 allot" SB-APPEND LF+
   s" PTR-VARIABLE H0" SB-APPEND LF+
   s" : PUT ( ^u8 n -- ^u8 n ) dup 0 ?do over i + c@ B i + c! loop nip B swap ;" SB-APPEND LF+
   s" : LOADX ( -- ) H0 @ 8 included ;" SB-APPEND LF+
   s" : TWICE ( -- )" SB-APPEND LF+
   s"    here H0 !" SB-APPEND LF+
   s\"    [: s\\\" s\\\" nofile.f\\\"\" PUT evaluate-closed ;] catch drop" SB-APPEND LF+
   s\"    s\" LOADX       \" PUT evaluate-closed ;" SB-APPEND LF+
   s" TWICE" SB-APPEND LF+
   s" spent-closed-twice.f" FIXTURE!
   s" error[E-MISSING-SOURCE]" 70 RAN
   GT-ERR$ s"  --> " CONTAINS? TFALSE
   GT-ERR$ s" = word:" CONTAINS? TFALSE
   s" a literal a definition passes to `included` at run time" T-LABEL
   SB-RESET  s\" : GO ( -- ) s\" nofile.f\" included ;" SB-APPEND LF+
   s" GO" SB-APPEND LF+
   s" spent-run-time.f" FIXTURE!
   s" error[E-MISSING-SOURCE]" 70 RAN
   GT-ERR$ s"  --> " CONTAINS? TFALSE
   GT-ERR$ s" = word:" CONTAINS? TFALSE ;

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
   DIR-P u ;

\ The help of a record whose lookup UNSEARCHABLE$ denied.
: UNSEARCHABLE-HELP ( -- )
   SB-RESET  s" if the directory `" SB-APPEND  UNSEARCHABLE$ SB-APPEND
   s" ` is yours, make it searchable with `chmod u+x '" SB-APPEND
   UNSEARCHABLE$ SB-APPEND  s" '`; else ask its owner to let you search it" SB-APPEND ;

\ The load's help names UNSEARCHABLE$, not a directory below it.
: UNSEARCHABLE-LOADED ( -- )
   SB-RESET  s" = help: if the directory `" SB-APPEND  UNSEARCHABLE$ SB-APPEND
   s" ` is" SB-APPEND
   GT-ERR$ SB$ CONTAINS? TTRUE ;

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
   GT-ERR$ SB$ CONTAINS? TTRUE ;

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
   GT-ERR$ SB$ CONTAINS? TTRUE ;

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
   DIR-P au FX-PATH$ FS:MAKE-SYMLINK
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
   s" --load" >LEN PROC-ARGV+  FX-PATH$ >LEN PROC-ARGV+  DEP-PATH$ >LEN PROC-ARGV+
   s" error[E-UNDEFINED]" 70 LAUNCHED
   SB-RESET s" --> " SB-APPEND FX-PATH$ SB-APPEND s" :1:1" SB-APPEND
   GT-ERR$ SB$ CONTAINS? TTRUE ;

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
   {: k:n c:^ cu:n tok:^ toku:n line:n col:n bs:n be:n :}
   k s" code" c cu FIELD=
   k tok toku line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

\ A bad escape is rejected at the literal's opener, by each check as by the
\ load, before an unclosed string after it. An earlier definition's E-UNDEFINED
\ comes first, as the load gives it; --verify-only composes the file as it
\ reads it and keeps that definition's rejection before the string's.
: TEST-BAD-ESCAPE ( -- )
   s" a bad escape" T-LABEL
   s\" s\\\" A\\yB\" 2drop" s" escaped-bad.f" LINE-FIXTURE
   s" E-UNTERMINATED-STRING" s\" s\\\"" 1 1 0 3 REFUSED-MODES
   s" E-UNTERMINATED-STRING" s" :1:1" 70 LOADED-AT
   s" a bad escape before a string that never closes" T-LABEL
   SB-RESET
   s\" s\\\" A\\yB\" 2drop" SB-APPEND LF+
   s\" s\" abc" SB-APPEND LF+
   s" escaped-bad-unterminated.f" FIXTURE!
   s" E-UNTERMINATED-STRING" s\" s\\\"" 1 1 0 3 REFUSED-MODES
   s" E-UNTERMINATED-STRING" s" :1:1" 70 LOADED-AT
   s" a bad escape in a body before a string that never closes" T-LABEL
   SB-RESET
   s\" : L ( -- ) s\\\" A\\yB\" 2drop ;" SB-APPEND LF+
   s\" s\" abc" SB-APPEND LF+
   s" escaped-bad-body-unterminated.f" FIXTURE!
   s" E-UNTERMINATED-STRING" s\" s\\\"" 1 12 11 14 REFUSED-MODES
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

\ A primitive row's record in check mode flag: E-MALFORMED-REGISTRY-ROW with
\ message m mu, label l lu and the row rule's help ending in h hu.
: ROW-RECORD ( str str str str -- )
   {: flag:^ flagu:n m:^ mu:n l:^ lu:n h:^ hu:n :}
   flag flagu CHECK
   s" E-MALFORMED-REGISTRY-ROW" REJECTED-AS
   0 s" message" m mu FIELD=
   0 s" label" l lu FIELD=
   SB-RESET
   s" a row's name and package are neither an opener nor a closer, " SB-APPEND
   s" its body holds only `PE-` or `;PE-` atoms and `REF` clauses, " SB-APPEND
   s" and it ends at `EPRIM;` or `ECLOSE-PRIVATE`: " SB-APPEND  h hu SB-APPEND
   0 s" help" SB$ FIELD= ;

\ The row's record in each check mode, at the token tok toku on line, column
\ and bytes bs..be of the fixture.
: ROW-MODES ( str n n n n str str str -- )
   {: tok:^ toku:n line:n col:n bs:n be:n m:^ mu:n l:^ lu:n h:^ hu:n :}
   s" " m mu l lu h hu ROW-RECORD
   0 tok toku line col bs be AT
   s" --all-errors" m mu l lu h hu ROW-RECORD
   0 tok toku line col bs be AT
   s" --verify-only" m mu l lu h hu ROW-RECORD
   0 tok toku line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

\ The load knows no `EPRIM:`, so it rejects the fixture at it: p pu is the
\ record's place after the path, through its marked source line.
: ROW-LOADED ( str -- ) {: p:^ pu:n :}
   s" error[E-UNDEFINED]" 70 LOADED
   SB-RESET
   S\" error[E-UNDEFINED]: cannot find word `EPRIM:`\n --> " SB-APPEND
   FX-PATH$ SB-APPEND  p pu SB-APPEND
   S\"   = help: define `EPRIM:` before this point, or check its spelling\n\n"
   SB-APPEND
   GT-ERR$ SB$ T$= ;

\ Each check rejects a primitive-axiom row at the token that breaks it, or at
\ the source's end inside it, and names the row's opener.
: TEST-PRIM-ROW ( -- )
   s" a token that breaks a primitive row" T-LABEL
   SB-RESET
   s" \ a row" SB-APPEND LF+
   s"   EPRIM: FOO PE-N" SB-APPEND LF+
   s"    postpone EPRIM;" SB-APPEND LF+
   s" bad-row.f" FIXTURE!
   s" postpone" 3 4 29 37
   s" found `postpone` in the primitive-axiom row opened at 2:3"
   s" breaks the row"  s" close the row before this token or replace it"
   ROW-MODES
   S\" :2:3\n  |\n2 |   EPRIM: FOO PE-N\n  |   ^^^^^^ not found\n" ROW-LOADED
   s" a primitive row the source ends inside" T-LABEL
   SB-RESET
   s" EPRIM: FOO PE-N" SB-APPEND LF+
   s" open-row.f" FIXTURE!
   s" " 2 1 16 16
   s" found the end of the source in the primitive-axiom row opened at 1:1"
   s" the source ends before the row closes"  s" close the row"
   ROW-MODES
   S\" :1:1\n  |\n1 | EPRIM: FOO PE-N\n  | ^^^^^^ not found\n" ROW-LOADED ;

\ A declaration piped to hb with no file argument is rejected at its token in
\ `<input>`, as the file's bytes place it, naming no word.
: TEST-STDIN-DECLARATION ( -- )
   s" a declaration read from stdin, at its token in <input>" T-LABEL
   PROC-ARGV-RESET
   s" STRUCTURE a11x<q> FIELD next ^a11x<n> FIELD w read-view<q,q,n> ;STRUCTURE"
   s" --> <input>:1:31" 70 PIPED
   GT-ERR$ s" = word:" CONTAINS? TFALSE ;

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
: STEPS-FIXTURE ( n str -- ) {: steps:n name:^ nameu:n :}
   BASE$ name nameu FX-P FS:JOIN-PATH FX-PU !
   0 FX-U !
   s\" s\\\" " FX+
   steps 0 ?do s\" .\\x2F" FX+ loop
   s\" inc-target.f\" included\nINCLUDED-WORD drop\n" FX+
   FX-PATH$ FX$ FS:WRITE-ALL ;

: VERIFY-ONLY-AT ( str n n str n n n n -- )
   {: call:^ callu:n cline:n cbs:n tok:^ toku:n line:n col:n bs:n be:n :}
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 2 T=
   0 s" code" s" W-CHECK-DEFERRED" FIELD=
   0 s" severity" s" warning" FIELD=
   0 call callu cline 1 cbs cbs callu + FX-PATH$ CANON$ FX$ AT-IN
   1 s" code" s" W-CHECK-DEFERRED" FIELD=
   1 s" severity" s" warning" FIELD=
   1 tok toku line col bs be FX-PATH$ CANON$ FX$ AT-IN ;

: VERIFY-DEFERRED-AT ( str n n -- )
   {: call:^ callu:n cline:n cbs:n :}
   s" " 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" --all-errors" 0 CHECK-EXIT
   GJA-LINE# @ 0 T=
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 s" code" s" W-CHECK-DEFERRED" FIELD=
   0 s" severity" s" warning" FIELD=
   0 s" label" s" may move the source root" FIELD=
   0 call callu cline 1 cbs cbs callu + FX-PATH$ CANON$ FX$ AT-IN ;

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
   s" L" 2 43 VERIFY-DEFERRED-AT
   s" an escaped path with no escape in a body" T-LABEL
   SB-RESET
   s\" : L ( -- ) s\\\" inc-target.f\" included ;" SB-APPEND LF+
   s" L" SB-APPEND LF+
   s" INCLUDED-WORD drop" SB-APPEND LF+
   s" escape-free-body-path.f" FIXTURE!
   s" L" 2 40 VERIFY-DEFERRED-AT
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
   s\" : L ( -- ) s\\\" b\\x6Fdy-a.f\" included s\\\" b\\x6Fdy-b.f\" included" SB-APPEND
   s\"  s\\\" b\\x6Fdy-c.f\" included ;" SB-APPEND LF+
   s" L" SB-APPEND LF+
   s" BODY-A-WORD drop BODY-B-WORD drop BODY-C-WORD drop" SB-APPEND LF+
   s" escaped-body-paths.f" FIXTURE!
   s" L" 2 91 VERIFY-DEFERRED-AT
   s" an escaped path in a body, escaped literals before its package closes" T-LABEL
   SB-RESET
   s" package DPT-ESC" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s\" : L ( -- ) s\\\" b\\x6Fdy-a.f\" included ;" SB-APPEND LF+
   s\" s\\\" X\\x41\" 2drop" SB-APPEND LF+
   s\" s\\\" Y\\x42YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY\" 2drop" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" DPT-ESC:L" SB-APPEND LF+
   s" BODY-A-WORD drop" SB-APPEND LF+
   s" escaped-body-path-package.f" FIXTURE!
   s" DPT-ESC:L" 7 137 VERIFY-DEFERRED-AT
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
   s" E-UNTERMINATED-STRING" s\" s\\\"" 1 1 0 3 REFUSED-MODES
   s" E-UNTERMINATED-STRING" s" :1:1" 70 LOADED-AT
   s" a bad escape in a loader's path in a body" T-LABEL
   SB-RESET
   s\" : L ( -- ) s\\\" inc-t\\yarget.f\" included ;" SB-APPEND LF+
   s" require missing-dep.f" SB-APPEND LF+
   s" escaped-bad-body-path.f" FIXTURE!
   s" E-UNTERMINATED-STRING" s\" s\\\"" 1 12 11 14 REFUSED-MODES
   s" E-UNTERMINATED-STRING" s" :1:12" 70 LOADED-AT
   s" a bad escape in a loader's path in a kernel: body" T-LABEL
   SB-RESET
   s\" kernel: L ( -- ) s\\\" inc-t\\yarget.f\" included ;" SB-APPEND LF+
   s" require missing-dep.f" SB-APPEND LF+
   s" escaped-bad-kernel-path.f" FIXTURE!
   s" E-UNTERMINATED-STRING" s\" s\\\"" 1 18 17 20 REFUSED-MODES
   s" E-UNTERMINATED-STRING" s" :1:18" 70 LOADED-AT ;

\ A word a create caller made is bound, and typed `-- ^a`, by the run of that
\ caller: its tick is admitted, and a call to it fails its check in source
\ order, after a signature that does not parse and an earlier failing check. A
\ name nothing defines is still the load's compiler's to refuse, before any
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
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 3 10 51 53 VERIFY-REFUSED-AT
   s" E-UNKNOWN-SIGNATURE-TYPE" s" :3:10" 70 LOADED-AT
   s" a signature that does not parse and a tick of that word" T-LABEL
   s" : X ( -- zz ) ['] MADE drop ;" s" made-badsig-tick.f" MADE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 3 10 51 53 VERIFY-REFUSED-AT
   s" E-UNKNOWN-SIGNATURE-TYPE" s" :3:10" 70 LOADED-AT
   s" that signature, a call of that word and a trust-boundary word" T-LABEL
   s" : X ( -- zz ) MADE evaluate ;" s" made-badsig-unsafe.f" MADE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 3 10 51 53 VERIFY-REFUSED-AT
   s" E-UNKNOWN-SIGNATURE-TYPE" s" :3:10" 70 LOADED-AT
   s" a trust-boundary word before a call of that word" T-LABEL
   s" : X ( -- ) evaluate MADE ;" s" made-unsafe.f" MADE-FIXTURE
   s" E-UNSAFE" s" evaluate" 3 12 53 61 VERIFY-REFUSED-AT
   s" E-UNSAFE" s" :3:12" 70 LOADED-AT
   s" a call of that word, whose address X does not declare" T-LABEL
   s" : X ( -- ) MADE ;" s" made-call.f" MADE-FIXTURE
   s" E-MISMATCH" s" MADE" 3 12 53 57 RUN-REFUSED-AT
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
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 3 10 38 40 RUN-REFUSED-AT
   s" --verify-only" 0 CHECK-EXIT
   GJA-LINE# @ 1 T=
   0 21 MK-DEFERRED
   s" E-UNKNOWN-SIGNATURE-TYPE" s" :3:10" 70 LOADED-AT ;

\ The fixture's first line: RAW-IMM, an immediate whose effect is not ( -- ),
\ so the load refuses each body that compiles it before it runs.
: RAW-IMM-LINE ( -- )
   SB-RESET
   s" : RAW-IMM ( n -- ) drop ; immediate" SB-APPEND LF+ ;

\ A refusal in the run, before an immediate runs or at `;`, names its token's
\ place in the file though comments, blank lines, tabs and runs of spaces
\ before it are not in the text the load checks.
: TEST-RUN-CAPTURED ( -- )
   s" an unmodeled immediate after comments of both kinds" T-LABEL
   RAW-IMM-LINE
   s" : P ( -- ) s" SB-APPEND 34 SB-APPEND-C s"  a does> b" SB-APPEND 34 SB-APPEND-C
   s"  2drop ( does> here ) " SB-APPEND 92 SB-APPEND-C s"  x does>" SB-APPEND LF+
   s"    RAW-IMM ;" SB-APPEND LF+
   s" imm-comments.f" FIXTURE!
   s" E-UNMODELED-IMMEDIATE" s" RAW-IMM" 3 4 95 102 RUN-REFUSED-AT
   IMM-EFFECT-NOTES
   s" an unmodeled immediate after a paren comment on its line" T-LABEL
   RAW-IMM-LINE
   s" : P ( -- ) 1 drop ( a note ) RAW-IMM ;" SB-APPEND LF+
   s" imm-paren.f" FIXTURE!
   s" E-UNMODELED-IMMEDIATE" s" RAW-IMM" 2 30 65 72 RUN-REFUSED-AT
   IMM-EFFECT-NOTES
   s" an unmodeled immediate after blank lines and tabs" T-LABEL
   RAW-IMM-LINE
   s" : P ( -- )" SB-APPEND TAB+ s" 1" SB-APPEND TAB+ TAB+ s" drop" SB-APPEND LF+
   LF+ LF+
   TAB+ s"   RAW-IMM ;" SB-APPEND LF+
   s" imm-blanks.f" FIXTURE!
   s" E-UNMODELED-IMMEDIATE" s" RAW-IMM" 5 4 60 67 RUN-REFUSED-AT
   IMM-EFFECT-NOTES
   s" a call of MADE, whose address X does not declare, after comments" T-LABEL
   SB-RESET
   s" : MK ( -- ) create ; parses: MK 1" SB-APPEND LF+
   s" MK MADE" SB-APPEND LF+
   s" : X ( -- ) ( c ) " SB-APPEND 92 SB-APPEND-C s"  c" SB-APPEND LF+
   s"    MADE ;" SB-APPEND LF+
   s" made-call-comments.f" FIXTURE!
   s" E-MISMATCH" s" MADE" 4 4 66 70 RUN-REFUSED-AT ;

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
   GT-ERR$ s" may define words or add to a definition" CONTAINS? TTRUE ;

\ The fixture's first two lines: OPEN2 and OPEN3, definers that evaluate `drop`
\ and `1 1` into the definition they open.
: OPEN-LINES ( -- )
   SB-RESET
   s\" : OPEN2 ( ^u8 n -- ) EFFECT:EMPTY define-word s\" drop\" evaluate-closed ;" SB-APPEND LF+
   s\" : OPEN3 ( ^u8 n -- ) EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word s\" 1 1\" evaluate-closed ;" SB-APPEND LF+ ;

\ A rejection at a token a definer evaluates, which no source holds, is placed at the
\ `;` the reader is on; a file token after it keeps its own place.
: TEST-RUN-UNSOURCED ( -- )
   s" an underflow at a drop a definer evaluates, after a comment" T-LABEL
   OPEN-LINES
   s\" s\" Q\" OPEN2 ( a comment ) ;" SB-APPEND LF+
   s" open-none.f" FIXTURE!
   s" E-INPUT-UNDERFLOW" s" ;" 3 27 191 192 RUN-REFUSED-AT
   s" a mismatch at a drop in the file after an evaluated one" T-LABEL
   OPEN-LINES
   s\" s\" Q\" OPEN3 ( a comment here )" SB-APPEND LF+
   s"   drop drop ;" SB-APPEND LF+
   s" open-after.f" FIXTURE!
   s" E-MISMATCH" s" drop" 4 8 203 207 RUN-REFUSED-AT ;

\ The fixture's first three lines: V reads one token, and M postpones it.
: PP-LINES ( -- )
   SB-RESET
   s" : V ( -- ) parse-name 2drop ; immediate" SB-APPEND LF+
   s" parses: V 1" SB-APPEND LF+
   s" : M ( -- ) postpone V ; immediate" SB-APPEND LF+ ;

\ The check with FLAG gives one record, the unread immediate `M` at 4:12.
: PP-UNREAD ( str -- )
   70 CHECK-EXIT
   s" E-UNMODELED-IMMEDIATE" REJECTED-AS
   0 s" M" 4 12 97 98 AT
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
   GT-ERR$ s" reads the source after it" CONTAINS? TTRUE
   s" a postponed reading immediate with its parses: row" T-LABEL
   PP-LINES
   s" parses: M 1" SB-APPEND LF+
   s" : P ( -- ) M x ;" SB-APPEND LF+
   s" imm-pp-read.f" FIXTURE!
   THREE-ADMIT ;

\ The fixture's first two lines: V executes the xt of W, which reads one token.
: TK-LINES ( -- )
   SB-RESET
   s" : W ( -- ) parse-name 2drop ;" SB-APPEND LF+
   s" : V ( -- ) ['] W execute ; immediate" SB-APPEND LF+ ;

\ The check with FLAG gives one record, the unread immediate `V` at 3:14.
: TK-UNREAD ( str -- )
   70 CHECK-EXIT
   s" E-UNMODELED-IMMEDIATE" REJECTED-AS
   0 s" V" 3 14 80 81 AT
   0 s" message"
   s" `V` reads the source after it and no `parses:` row says how many tokens" FIELD= ;

\ The check with FLAG gives one record, the unread immediate `V` at 5:16.
: TJ-UNREAD ( str -- )
   70 CHECK-EXIT
   s" E-UNMODELED-IMMEDIATE" REJECTED-AS
   0 s" V" 5 16 153 154 AT ;

\ An xt chosen at a branch join keeps the facts of both branches' ticks.
: TK-JOINED ( -- )
   s" an immediate executing an xt one branch ticks from a reading word" T-LABEL
   SB-RESET
   s" : W ( -- ) parse-name 2drop ;" SB-APPEND LF+
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
   GT-ERR$ s" reads the source after it" CONTAINS? TTRUE
   s" an immediate executing a ticked reading word, with its parses: row" T-LABEL
   TK-LINES
   s" parses: V 1" SB-APPEND LF+
   s" : P ( -- n ) V a 5 ;" SB-APPEND LF+
   s" P ." SB-APPEND LF+
   s" imm-tk-read.f" FIXTURE!
   THREE-ADMIT
   GT-OUT$ s" 5" CONTAINS? TTRUE
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
   GT-ERR$ s" may define words or add to a definition" CONTAINS? TTRUE ;

\ Record 0 of the check: `compile,` at 3:50 keeps V, which reads the source,
\ in the word MK makes, and its type states no `parses`.
: COMPILE-KEPT ( -- )
   0 s" code" s" E-MISMATCH" FIELD=
   0 s" compile," 3 50 101 109 AT
   0 s" message" s" this quotation does more than the type that keeps it states" FIELD=
   0 s" found" s" [ -- parses ]" NOTE= ;

\ `compile,` keeps the word it compiles, so its type states what that word may
\ do: compiling a reader is rejected there, and --verify-only goes on to find no Q.
: TEST-COMPILE-KEPT ( -- )
   s" compile, keeping a reader its type does not state" T-LABEL
   SB-RESET
   s" : V ( -- ) parse-name 2drop ; immediate" SB-APPEND LF+
   s" parses: V 1" SB-APPEND LF+
   s" : MK ( ^u8 n -- ) EFFECT:EMPTY define-word ['] V compile, postpone ; ;" SB-APPEND LF+
   s\" s\" Q\" MK" SB-APPEND LF+
   s" : R ( -- ) Q ; immediate" SB-APPEND LF+
   s" : P ( -- ) R x ;" SB-APPEND LF+
   s" imm-compile-kept.f" FIXTURE!
   s" " CHECK  GJA-LINE# @ 1 T=  COMPILE-KEPT
   s" --verify-only" CHECK  GJA-LINE# @ 3 T=  COMPILE-KEPT
   1 s" code" s" E-UNDEFINED" FIELD=  1 s" Q" 5 12 143 144 AT
   s" E-MISMATCH" s" :3:50" 70 LOADED-AT ;

\ An immediate calling a word whose body the check deferred: the walk saw no fact of it, so every
\ reader admits it.
: TEST-IMM-CALLS-DEFERRED ( -- )
   s" an immediate calling a word whose body the check deferred" T-LABEL
   SB-RESET
   s" : MK ( ^u8 n -- ) EFFECT:EMPTY define-word postpone ; ;" SB-APPEND LF+
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
   GT-ERR$ s" may move the source root" CONTAINS? TTRUE ;

\ The fixture's first five lines: V calls the deferred D, set to W, which reads
\ one token.
: DC-LINES ( -- )
   SB-RESET
   s" : W ( -- ) parse-name 2drop ;" SB-APPEND LF+
   s" defer D ( -- )" SB-APPEND LF+
   s" : SET ( -- ) ['] W is D ;" SB-APPEND LF+
   s" SET" SB-APPEND LF+
   s" : V ( -- ) D ; immediate" SB-APPEND LF+ ;

\ Record K of the check: `is` at 3:20 keeps W, which reads the source, in D,
\ whose type states no `parses`.
: DC-KEPT ( n -- ) {: k:n :}
   k s" code" s" E-MISMATCH" FIELD=
   k s" is" 3 20 64 66 AT
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
   1 s" code" s" E-UNDEFINED" FIELD=  1 s" a" 6 16 115 116 AT
   s" E-MISMATCH" s" :3:20" 70 LOADED-AT
   s" the same, with a parses: row on the immediate" T-LABEL
   DC-LINES
   s" parses: V 1" SB-APPEND LF+
   s" : P ( -- n ) V a 5 ;" SB-APPEND LF+
   s" P ." SB-APPEND LF+
   s" imm-dc-read.f" FIXTURE!
   s" " CHECK  GJA-LINE# @ 1 T=  0 DC-KEPT
   s" --verify-only" CHECK  GJA-LINE# @ 3 T=  0 DC-KEPT
   1 s" code" s" E-PARSES-ROW" FIELD=  1 s" V" 6 9 108 109 AT
   2 s" code" s" E-UNDEFINED" FIELD=  2 s" a" 7 16 127 128 AT
   s" E-MISMATCH" s" :3:20" 70 LOADED-AT ;

\ A parses: count reads the operand on the next line.
: TEST-IMM-LINE-END ( -- )
   s" a parses: count read across a line end" T-LABEL
   SB-RESET
   s" : V ( -- ) parse-name 2drop ; immediate" SB-APPEND LF+
   s" parses: V 1" SB-APPEND LF+
   s" : P ( -- n ) V" SB-APPEND LF+
   s"   a 5 ;" SB-APPEND LF+
   s" P ." SB-APPEND LF+
   s" imm-line-end.f" FIXTURE!
   THREE-ADMIT
   GT-OUT$ s" 5" CONTAINS? TTRUE ;

: TEST-HEAD-CLOSER ( -- )
   s" an if open at the does> ending a definer's head, after a comment" T-LABEL
   SB-RESET
   s" true REJECTION:JSON !" SB-APPEND LF+
   s" : CONST ( n -- ) ( a note ) create dup ," SB-APPEND LF+
   s"    0= if does> ( -- n ) @ ;" SB-APPEND LF+
   s" head-open-cmt.f" FIXTURE!
   s\" \"line\":3,\"column\":10,\"byte_start\":72,\"byte_end\":77" 70 LOADED
   GT-ERR$ s\" \"token\":\"does>\"" CONTAINS? TTRUE ;

: TEST-VARIANT-COLLIDE ( -- )
   s" a variant spelled like a derived word" T-LABEL
   SB-RESET
   s" SUMTYPE tdpos DERIVE eq VARIANT tag ;VARIANT ;SUMTYPE" SB-APPEND LF+
   s" variant-collide.f" FIXTURE!
   s" E-BAD-DECLARATION" s" tag" 1 33 32 35 RUN-REFUSED-AT ;

: TEST-DERIVE-PAYLOAD ( -- )
   s" a sum payload DERIVE eq cannot compare" T-LABEL
   SB-RESET
   s" SUMTYPE dpsum DERIVE eq VARIANT hold ^n ;VARIANT ;SUMTYPE"
   SB-APPEND LF+
   s" derive-payload.f" FIXTURE!
   s" E-BAD-DECLARATION" s" ^n" 1 38 37 39 RUN-REFUSED-AT ;

: TEST-DERIVE-FIELD ( -- )
   s" a structure field DERIVE eq cannot compare" T-LABEL
   SB-RESET
   s" STRUCTURE dpst DERIVE eq FIELD f ^n ;STRUCTURE" SB-APPEND LF+
   s" derive-field.f" FIXTURE!
   s" E-BAD-DECLARATION" s" ^n" 1 34 33 35 RUN-REFUSED-AT ;

: TEST-DERIVE-ENUM-FIELD ( -- )
   s" an enum field DERIVE eq cannot compare" T-LABEL
   SB-RESET
   s" ENUM dpen DERIVE eq VARIANT hold FIELD raw ^u8 ;VARIANT ;ENUM"
   SB-APPEND LF+
   s" derive-enum-field.f" FIXTURE!
   s" E-BAD-DECLARATION" s" ^u8" 1 44 43 46 RUN-REFUSED-AT ;

: TEST-DERIVE-BYTES-FIELD ( -- )
   s" a structure byte-pointer field DERIVE eq cannot compare" T-LABEL
   SB-RESET
   s" STRUCTURE dppr DERIVE eq FIELD b ^u8 ;STRUCTURE" SB-APPEND LF+
   s" derive-product-field.f" FIXTURE!
   s" E-BAD-DECLARATION" s" ^u8" 1 34 33 36 RUN-REFUSED-AT ;

: GEN-LINES ( -- )
   SB-RESET
   s\" : GEN ( -- ) s\" : MADE ( -- ) ;\" evaluate-closed ;" SB-APPEND LF+
   s" GEN" SB-APPEND LF+ ;

: DEFER-SUGGESTS ( n n str str -- )
   {: want:n k:n tok:^ toku:n sug:^ sugu:n :}
   s" --verify-only" want CHECK-EXIT
   k s" code" s" W-CHECK-DEFERRED" FIELD=
   k s" token" tok toku FIELD=
   sugu 0 > IF k s" help" sug sugu FIELD= EXIT THEN
   GJA-LINE# @ k > dup TTRUE 0= IF EXIT THEN
   k GJA-LINE$ GJA-PARSE s" help" FIELD-ABSENT ;

\ A deferral's help names what lifts its cause. A body the check defers, and a
\ stretch opened at a name with no known cause, record none and keep the
\ general text; a parsing word its row bounds leaves nothing to write and no
\ help.
: TEST-DEFER-SUGGEST ( -- )
   s" a top-level parsing word with no parses: row" T-LABEL
   SB-RESET
   s" : PN ( -- ) parse-name 2drop ;" SB-APPEND LF+
   s" PN foo" SB-APPEND LF+
   s" : Y ( -- ) NOPE ;" SB-APPEND LF+
   s" defer-parser.f" FIXTURE!
   0 0 s" PN" s" write `parses: PN n` or `parses-through: PN n ( END )` after the definition of `PN`, or call it through a word that has such a row" DEFER-SUGGESTS
   s" a top-level call of a deferred word" T-LABEL
   SB-RESET
   s" defer D ( -- )" SB-APPEND LF+
   s" D" SB-APPEND LF+
   s" : Y ( -- n ) ;" SB-APPEND LF+
   s" defer-defer.f" FIXTURE!
   0 0 s" D" s" call `D` from a definition" DEFER-SUGGESTS
   s" a top-level parsing word its parses: row bounds" T-LABEL
   SB-RESET
   s" : PN ( -- ) parse-name 2drop ;" SB-APPEND LF+
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
   70 1 s" widget" s" declare `widget` with `NEWTYPE`, `STRUCTURE` or `SUMTYPE` before its use" DEFER-SUGGESTS
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
   0 0 s" SOURCE-ROOT:ROOT!" s" the run loads the files later requires name from the root it sets" DEFER-SUGGESTS
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
   0 0 s" GO" s" the run loads the files later requires name from the root it sets" DEFER-SUGGESTS
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
   0 2 s" SOURCE-ROOT:ROOT!" s" the run loads the files later requires name from the root it sets" DEFER-SUGGESTS
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
   0 0 s" GO" s" the run loads the files later requires name from the root it sets" DEFER-SUGGESTS
   s" a top-level tick of a word that moves the source root before a require under it" T-LABEL
   s" defer-roots-tick" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" defer-roots-tick/sub" NAMED!  FX-PATH$ FS:MAKE-DIR
   s" : LIB-ONE ( -- n ) 1 ;" s" defer-roots-tick/sub/lib.f" LINE-FIXTURE
   SB-RESET
   s\" s\" sub\" ' SOURCE-ROOT:ROOT! execute" SB-APPEND LF+
   s" require lib.f" SB-APPEND LF+
   s" LIB-ONE drop" SB-APPEND LF+
   s" defer-roots-tick/top.f" FIXTURE!
   0 0 s" SOURCE-ROOT:ROOT!" s" the run loads the files later requires name from the root it sets" DEFER-SUGGESTS
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
   0 0 s" GO" s" the run loads the files later requires name from the root it sets" DEFER-SUGGESTS
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
   0 0 s" SOURCE-ROOT:ENGINE!" s" the run loads the files later requires name from the root it sets" DEFER-SUGGESTS
   s" " 0 CHECK-EXIT ;

\ A control-flow word with no structure open, or another structure's, is one
\ the load cannot compile: it refuses the definition at that word.
: TEST-CONTROL-ORPHAN ( -- )
   s" a signature that does not parse and a then with no if" T-LABEL
   s" : X ( n -- zz ) then ;" s" badsig-then.f" LINE-FIXTURE
   s" E-CONTROL-ORPHAN" s" then" 1 17 16 20 REFUSED-MODES
   s" a then with no if before an undefined word" T-LABEL
   s" : X ( n -- n ) then NOPE ;" s" then-undefined.f" LINE-FIXTURE
   s" E-CONTROL-ORPHAN" s" then" 1 16 15 19 REFUSED-AT
   0 s" message" s" this word closes nothing open here" FIELD=
   s" a trust-boundary word before a then with no if" T-LABEL
   s" : X ( n -- n ) evaluate then ;" s" unsafe-then.f" LINE-FIXTURE
   s" E-CONTROL-ORPHAN" s" then" 1 25 24 28 REFUSED-AT
   s" an endcase with no case before an undefined word" T-LABEL
   s" : X ( n -- n ) endcase NOPE ;" s" endcase-undefined.f" LINE-FIXTURE
   s" E-CONTROL-ORPHAN" s" endcase" 1 16 15 22 REFUSED-AT
   s" a signature that does not parse and a then closing a begin" T-LABEL
   s" : X ( n -- zz ) begin then ;" s" badsig-begin-then.f" LINE-FIXTURE
   s" E-CONTROL-ORPHAN" s" then" 1 23 22 26 REFUSED-AT
   s" a leave outside every do before an undefined word" T-LABEL
   s" : X ( n -- n ) leave NOPE ;" s" leave-undefined.f" LINE-FIXTURE
   s" E-NO-LOOP" s" leave" 1 16 15 20 REFUSED-AT ;


: TEST-CONTROL-REASON ( -- )
   s" a loop body that changes the stack's depth" T-LABEL
   s" : X ( n -- n ) 3 0 do dup loop ;" s" loop-depth.f" LINE-FIXTURE
   s" E-MISMATCH" s" loop" 1 27 26 30 REFUSED-AT
   0 s" message" s" the loop body changes the stack's depth" FIELD=
   0 s" expected" s" n" NOTE=
   0 s" found" s" n n" NOTE=
   s" an if arm that changes the stack's depth" T-LABEL
   s" : X ( n bool -- n ) if 1 then ;" s" if-depth.f" LINE-FIXTURE
   s" E-MISMATCH" s" then" 1 26 25 29 REFUSED-AT
   0 s" message" s" the if arm changes the stack's depth" FIELD=
   0 s" expected" s" n" NOTE=
   0 s" found" s" n n" NOTE=
   s" if and else arms that leave different depths" T-LABEL
   s" : X ( n bool -- n ) if 1 else then ;" s" arms-depth.f" LINE-FIXTURE
   s" E-MISMATCH" s" then" 1 31 30 34 REFUSED-AT
   0 s" message" s" the if and else arms leave different stack depths" FIELD=
   0 s" expected" s" n" NOTE=
   0 s" found" s" n n" NOTE=
   s" an i outside every loop" T-LABEL
   s" : X ( -- ) i drop ;" s" i-outside.f" LINE-FIXTURE
   s" E-NO-LOOP" s" i" 1 12 11 12 REFUSED-AT
   0 s" message" s" no loop is open here" FIELD=
   s" a j in a single loop" T-LABEL
   s" : X ( -- ) 3 0 do j drop loop ;" s" j-single.f" LINE-FIXTURE
   s" E-NO-OUTER-LOOP" s" j" 1 19 18 19 REFUSED-AT
   0 s" message" s" no outer loop is open here" FIELD=
   s" an exit inside a loop with no unloop" T-LABEL
   s" : X ( -- ) 3 0 do exit loop ;" s" exit-loop.f" LINE-FIXTURE
   s" E-EXIT-IN-LOOP" s" exit" 1 19 18 22 REFUSED-AT
   0 s" message" s" exit inside a loop needs unloop first" FIELD=
   s" a loop body that changes the return stack's depth" T-LABEL
   s" : X ( -- ) 3 0 do 1 >r loop ;" s" loop-rdepth.f" LINE-FIXTURE
   s" E-LOOP-RETURN-DEPTH" s" loop" 1 24 23 27 REFUSED-AT
   0 s" message" s" the loop body changes the return stack's depth" FIELD=
   s" an if still open at ;" T-LABEL
   s" : X ( bool -- ) if ;" s" if-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 20 19 20 REFUSED-AT
   0 s" message" s" a control structure is still open at the end of the definition" FIELD=
   s" E-CONTROL-OPEN" s" :1:20" 70 LOADED-AT
   s" an if still open at ; after a refused drop" T-LABEL
   s" : X ( -- ) drop 0 0= if ;" s" if-open-drop.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 25 24 25 REFUSED-AT
   0 s" message" s" a control structure is still open at the end of the definition" FIELD=
   s" E-CONTROL-OPEN" s" :1:25" 70 LOADED-AT
   s" a case still open at ;" T-LABEL
   s" : X ( n -- ) case ;" s" case-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 19 18 19 REFUSED-AT
   0 s" message" s" a control structure is still open at the end of the definition" FIELD=
   s" E-CONTROL-OPEN" s" :1:19" 70 LOADED-AT
   s" a loop that meets an if inside its do" T-LABEL
   s" : X ( -- ) 3 0 do 0 0= if loop then ;" s" loop-in-if.f" LINE-FIXTURE
   s" E-CONTROL-ORPHAN" s" loop" 1 27 26 30 REFUSED-AT
   0 s" message" s" this word closes nothing open here" FIELD=
   s" a loop that meets an if with no do open" T-LABEL
   s" : X ( -- ) 0 0= if loop then ;" s" loop-no-do.f" LINE-FIXTURE
   s" E-NO-LOOP" s" loop" 1 20 19 23 REFUSED-AT
   0 s" message" s" no loop is open here" FIELD=
   s" match arms that unloop different loops" T-LABEL
   SB-RESET
   s" ENUM choice first second ;ENUM" SB-APPEND LF+
   s" : LOB-MATCH ( choice -- ) 3 0 do dup MATCH choice first OF unloop ENDOF second OF ENDOF ;MATCH loop drop ;" SB-APPEND LF+
   s" match-unloop.f" FIXTURE!
   s" E-UNLOOP-JOIN" s" ENDOF" 2 83 113 118 REFUSED-AT
   0 s" message" s" the paths that meet here have unlooped different loops" FIELD= ;

\ The load's compiler takes a second else, as it takes an else over an if's
\ frame: the check fails there, which leaves the refusal to a later undefined
\ word, a signature that does not parse and an earlier trust-boundary word.
: TEST-SECOND-ELSE ( -- )
   s" a second else before an undefined word" T-LABEL
   s" : X ( bool -- ) if else else then NOPE ;" s" else2-undefined.f" LINE-FIXTURE
   s" E-UNDEFINED" s" NOPE" 1 35 34 38 REFUSED-AT
   s" a signature that does not parse and a second else" T-LABEL
   s" : X ( bool -- zz ) if else else then ;" s" badsig-else2.f" LINE-FIXTURE
   s" E-UNKNOWN-SIGNATURE-TYPE" s" zz" 1 15 14 16 REFUSED-AT
   s" a trust-boundary word before a second else" T-LABEL
   s" : X ( bool -- ) evaluate if else else then ;" s" unsafe-else2.f" LINE-FIXTURE
   s" E-UNSAFE" s" evaluate" 1 17 16 24 REFUSED-AT
   s" a second else alone" T-LABEL
   s" : X ( bool -- ) if else else then ;" s" else2.f" LINE-FIXTURE
   s" E-ELSE-TWICE" s" else" 1 25 24 28 REFUSED-AT
   0 s" message" s" this if already has an else" FIELD= ;

\ A structure still open where its definition ends is one the load cannot
\ compile: it refuses the definition at the `;`, or at the `does>` ending a
\ definer's body. A definer whose head calls a word the run decides is refused
\ as the load refuses it, never as a duplicate of itself, and --verify-only
\ defers that head.
: TEST-CONTROL-OPEN ( -- )
   s" a signature that does not parse and an if open at ;" T-LABEL
   s" : X ( n -- zz ) dup if ;" s" badsig-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 24 23 24 REFUSED-AT
   s" a called shadowed name and an IF open at ;" T-LABEL
   s" : SHADE ( -- ) ;" s"    SHADE IF ;" SHADOW-FIXTURE
   s" shade-open.f" FIXTURE!
   s" E-CONTROL-OPEN" s" ;" 8 13 96 97 REFUSED-AT
   s" a trust-boundary word and an if open at ;" T-LABEL
   s" : X ( n -- n ) evaluate if ;" s" unsafe-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 28 27 28 REFUSED-AT
   s" dead code and an if open at ;" T-LABEL
   s" : X ( n -- n ) dup if exit 1 ;" s" dead-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 30 29 30 REFUSED-AT
   s" an if open at the does> ending a definer's body" T-LABEL
   s" : CONST ( n -- ) create dup , 0= if does> ( -- n ) @ ;" s" does-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" does>" 1 37 36 41 REFUSED-AT
   s" an if open at does> after a head call the run decides" T-LABEL
   s" : MYK ( n -- ) MADE drop create dup , 0= if does> ( -- n ) @ ;"
   s" made-does-open.f" MADE-FIXTURE
   s" E-CONTROL-OPEN" s" does>" 3 45 86 91 RUN-REFUSED-AT
   s" MK" 2 34 s" does>" 3 45 86 91 VERIFY-ONLY-AT
   s" an if open at does> on the line after a head call the run decides" T-LABEL
   SB-RESET
   s" : MK ( -- ) create ; parses: MK 1" SB-APPEND LF+
   s" MK MADE" SB-APPEND LF+
   s" : MYK ( n -- ) ( c ) MADE drop create dup ," SB-APPEND LF+
   s"    0= if does> ( -- n ) @ ;" SB-APPEND LF+
   s" made-does-open2.f" FIXTURE!
   s" E-CONTROL-OPEN" s" does>" 4 10 95 100 RUN-REFUSED-AT
   s" an orphan then before does> after a head call the run decides" T-LABEL
   s" : MYK ( n -- ) MADE drop create dup , then does> ( -- n ) @ ;"
   s" made-does-then.f" MADE-FIXTURE
   s" E-CONTROL-ORPHAN" s" then" 3 39 80 84 RUN-REFUSED-AT
   s" an undefined head call and an if open at does>" T-LABEL
   s" : MYK ( n -- ) NOSUCH drop create dup , 0= if does> ( -- n ) @ ;"
   s" made-does-nosuch.f" MADE-FIXTURE
   s" E-UNDEFINED" s" NOSUCH" 3 16 57 63 RUN-REFUSED-AT
   s" an if open at the ; ending a does> clause" T-LABEL
   s" : CONST ( n -- ) create , does> ( -- n ) @ dup 0= if ;" s" clause-open.f" LINE-FIXTURE
   s" E-CONTROL-OPEN" s" ;" 1 54 53 54 REFUSED-AT
   s" a match open at ;" T-LABEL
   SB-RESET
   s" SUMTYPE mres VARIANT ok n ;VARIANT ;SUMTYPE" SB-APPEND LF+
   s" : X ( mres -- n ) MATCH mres ok OF 1 ;" SB-APPEND LF+
   s" match-open.f" FIXTURE!
   s" E-MATCH-UNTERMINATED" s" ;" 2 38 81 82 REFUSED-AT ;

\ A does> clause that walks no token and fails its end-of-body check is
\ refused at its closing `;`, on that token's own line and bytes. The parses:
\ row then names a refused word, a second packet only --all-errors gives.
: TEST-EMPTY-CLAUSE ( -- )
   s" an empty does> clause refused at its ;" T-LABEL
   s" : MYK ( n -- ) create drop does> ( -- ) ;" s" empty-clause.f" LINE-FIXTURE
   s" E-MISMATCH" s" ;" 1 41 40 41 REFUSED-AT
   s" an empty does> clause refused at its ; on a later line" T-LABEL
   SB-RESET
   s" : MYK ( n -- ) create drop" SB-APPEND LF+
   s"   does> ( -- )" SB-APPEND LF+
   s"   ;" SB-APPEND LF+
   s" empty-clause-lines.f" FIXTURE!
   s" E-MISMATCH" s" ;" 3 3 44 45 REFUSED-AT
   s" E-MISMATCH" s" :3:3" 70 LOADED-AT
   s" an empty does> clause of a definer with a parses: row" T-LABEL
   s" : MK ( n -- ) create , does> ( -- ) ; parses: MK 1" s" empty-clause-parses.f" LINE-FIXTURE
   s" " CHECK
   s" E-MISMATCH" REJECTED-AS
   0 s" ;" 1 37 36 37 AT ;

\ A structure or FUNCTION: group still open when its file ends, or when another
\ declaration opens, is rejected at the token that opened it.
: TEST-UNCLOSED ( -- )
   s" a structure still open at the file's end" T-LABEL
   s" BEGIN-STRUCTURE P" s" unclosed.f" LINE-FIXTURE
   s" E-UNCLOSED" s" :1:1" 70 LOADED-AT
   s" E-UNCLOSED" s" BEGIN-STRUCTURE" 1 1 0 15 REFUSED-MODES
   s" a structure opened inside an open one" T-LABEL
   SB-RESET
   s" BEGIN-STRUCTURE P" SB-APPEND LF+
   s" BEGIN-STRUCTURE Q" SB-APPEND LF+
   s" unclosed-nested.f" FIXTURE!
   s" E-UNCLOSED" s" :1:1" 70 LOADED-AT
   s" E-UNCLOSED" s" BEGIN-STRUCTURE" 1 1 0 15 REFUSED-MODES
   s" a FUNCTION: group still open at the file's end" T-LABEL
   SB-RESET
   s" require src/lib/ffi-abi.f" SB-APPEND LF+
   s" PROCESS-SYMBOLS" SB-APPEND LF+
   s" FUNCTION: F getpid ( -- i32 )" SB-APPEND LF+
   s" unclosed-ffi.f" FIXTURE!
   s" E-UNCLOSED" s" :3:1" 70 LOADED-AT
   s" E-UNCLOSED" s" FUNCTION:" 3 1 42 51 REFUSED-MODES
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
   s" E-UNCLOSED" s" FUNCTION:" 3 1 42 51 REFUSED-MODES ;

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
   s" E-THROW" s" :2:1" 67 LOADED-AT
   GT-ERR$ s" error[" COUNT-SUB 1 T=
   FX-P DEP-P FX-PU @ BYTE-COPY  FX-PU @ DEP-PU !
   s" a throw nothing catches in a required file" T-LABEL
   s" require uncaught.f" s" uncaught-outer.f" LINE-FIXTURE
   s" error[E-THROW]" 67 LOADED
   SB-RESET  s" --> " SB-APPEND  DEP-PATH$ SB-APPEND  s" :2:1" SB-APPEND
   GT-ERR$ SB$ CONTAINS? TTRUE
   GT-ERR$ s" error[" COUNT-SUB 1 T=
   s" a throw nothing catches in an evaluated text" T-LABEL
   SB-RESET
   s" : EV ( -- ) s" SB-APPEND  34 SB-APPEND-C
   s"  1 2 -77 throw" SB-APPEND  34 SB-APPEND-C
   s"  evaluate-closed ;" SB-APPEND LF+
   s" EV" SB-APPEND LF+
   s" uncaught-eval.f" FIXTURE!
   s" error[" 67 LOADED
   SB-RESET  s" --> " SB-APPEND  FX-PATH$ SB-APPEND  s" :2:1" SB-APPEND
   GT-ERR$ SB$ CONTAINS? TTRUE
   GT-ERR$ s" = evaluated:" CONTAINS? TTRUE
   GT-ERR$ s" error[" COUNT-SUB 1 T= ;

\ A record its throw carried survives a finally whose cleanup throws and
\ catches: the load prints that record, not E-THROW.
: TEST-FINALLY-CARRIED ( -- )
   s" a carried record through finally's cleanup" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : INNER ( -- ) 5000 throw ;" SB-APPEND LF+
   s" : CLEAN ( -- ) ['] INNER catch drop ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY ['] CLEAN finally ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" finally-carried.f" FIXTURE!
   s" error[E-PROBE]: probe text" 67 LOADED
   GT-ERR$ s" error[" COUNT-SUB 1 T= ;

\ A caught code thrown on in the body that caught it keeps its record, after a
\ cleanup that throws and catches, whether the stack or a local holds it.
: TEST-RETHROW-STACK ( -- )
   s" a caught code thrown on from the stack keeps its record" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : INNER ( -- ) 5000 throw ;" SB-APPEND LF+
   s" : CLEAN ( -- ) ['] INNER catch drop ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch dup 0<> if CLEAN throw then drop ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-stack.f" FIXTURE!
   s" error[E-PROBE]: probe text" 67 LOADED
   GT-ERR$ s" error[" COUNT-SUB 1 T= ;

: TEST-RETHROW-LOCAL ( -- )
   s" a caught code thrown on from a local keeps its record" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : INNER ( -- ) 5000 throw ;" SB-APPEND LF+
   s" : CLEAN ( -- ) ['] INNER catch drop ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch {: rc:n :} CLEAN rc 0<> if rc throw then ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-local.f" FIXTURE!
   s" error[E-PROBE]: probe text" 67 LOADED
   GT-ERR$ s" error[" COUNT-SUB 1 T= ;

\ A caught code joined with another value at an arm is a new throw.
: TEST-RETHROW-JOINED ( -- )
   s" a caught code joined with another value is a new throw" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch dup 9999 = if drop 9998 then dup 0<> if throw then drop ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-joined.f" FIXTURE!
   s" error[E-THROW]: nothing caught a throw of code 9998" 67 LOADED
   GT-ERR$ s" E-PROBE" CONTAINS? TFALSE
   GT-ERR$ s" error[" COUNT-SUB 1 T= ;

\ A record kept after the catch answered its code is not the caught throw's: the
\ code thrown on is a new throw.
: TEST-RETHROW-KEPT-SINCE ( -- )
   s" a caught code thrown on after a later kept record is a new throw" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   S\" : BODY2 ( -- ) s\" E-PROBE2\" REJECTION:OPEN s\" probe two\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch {: rc:n :} ['] BODY2 catch drop rc 0<> if rc throw then ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-kept-since.f" FIXTURE!
   s" error[E-THROW]: nothing caught a throw of code 9999" 67 LOADED
   GT-ERR$ s" E-PROBE" CONTAINS? TFALSE
   GT-ERR$ s" error[" COUNT-SUB 1 T= ;

: TEST-RETHROW-RSTACK ( -- )
   s" a caught code thrown on through the return stack keeps its record" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : INNER ( -- ) 5000 throw ;" SB-APPEND LF+
   s" : CLEAN ( -- ) ['] INNER catch drop ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch >r CLEAN r> dup 0<> if throw then drop ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-rstack.f" FIXTURE!
   s" error[E-PROBE]: probe text" 67 LOADED
   GT-ERR$ s" error[" COUNT-SUB 1 T= ;

\ `ri throw` never runs, but it counts INNER's catch, so each catch keeps a slot
\ of its own: a slot shared with BODY's would lose the record.
: TEST-RETHROW-TWO ( -- )
   s" a caught code thrown on past a second counted catch keeps its record" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : INNER ( -- ) 5000 throw ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch {: rc:n :} ['] INNER catch {: ri:n :}" SB-APPEND
   s"  rc 0<> if rc throw then ri throw ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-two.f" FIXTURE!
   s" error[E-PROBE]: probe text" 67 LOADED
   GT-ERR$ s" error[" COUNT-SUB 1 T= ;

: TEST-RETHROW-QUOT ( -- )
   s" a caught local thrown from a quotation is a new throw" T-LABEL
   SB-RESET
   S\" : BODY ( -- ) s\" E-PROBE\" REJECTION:OPEN s\" probe text\" REJECTION:EMIT-STR" SB-APPEND
   s"  REJECTION:SAVE 9999 REJECTION:PASS ;" SB-APPEND LF+
   s" : MAIN ( -- ) ['] BODY catch {: rc:n :} rc 0<> if [: rc throw ;] execute then ;" SB-APPEND LF+
   s" MAIN" SB-APPEND LF+
   s" rethrow-quot.f" FIXTURE!
   s" error[E-THROW]: nothing caught a throw of code 9999" 67 LOADED
   GT-ERR$ s" E-PROBE" CONTAINS? TFALSE
   GT-ERR$ s" error[" COUNT-SUB 1 T= ;

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
   s" E-THROW" s" :3:1" 67 LOADED-AT
   GT-ERR$ s" error[" COUNT-SUB 1 T=
   GT-ERR$ s" caught-dep.f" CONTAINS? TFALSE ;

: AMBIG-FIXTURE ( bool str -- )
   {: global:bool body:^ bodyu:n :}
   SB-RESET
   s" package PA" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : AMB ( -- ) ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   s" package PB" SB-APPEND LF+
   s" public" SB-APPEND LF+
   s" : AMB ( -- ) ;" SB-APPEND LF+
   s" ;package" SB-APPEND LF+
   global IF s" : AMB ( -- ) ;" ELSE s" : AMC ( -- ) ;" THEN SB-APPEND LF+
   s" using PA" SB-APPEND LF+
   s" using PB" SB-APPEND LF+
   s" : USE-IT ( -- )" SB-APPEND LF+
   body bodyu SB-APPEND LF+
   s" ;using" SB-APPEND LF+
   s" ;using" SB-APPEND LF+ ;

\ A called name a used public shadows a global for, or two used publics export
\ beside one, is refused by the load's check at `;`: after every word the load
\ cannot compile, wherever it stands, and before any other check. Ticked, or
\ exported by two used publics and no global, the load's tick or lookup
\ refuses it while compiling, in source order.
: TEST-USING-ORDER ( -- )
   s" a called shadowed name before an undefined word" T-LABEL
   s" : SHADE ( -- ) ;" s"    SHADE NOPE ;" SHADOW-FIXTURE
   s" shade-undefined.f" FIXTURE!
   s" E-UNDEFINED" s" NOPE" 8 10 93 97 REFUSED-AT
   s" an undefined word before a called shadowed name" T-LABEL
   s" : SHADE ( -- ) ;" s"    NOPE SHADE ;" SHADOW-FIXTURE
   s" undefined-shade.f" FIXTURE!
   s" E-UNDEFINED" s" NOPE" 8 4 87 91 REFUSED-AT
   s" an undefined word before a ticked shadowed name" T-LABEL
   s" : SHADE ( -- ) ;" s"    NOPE ['] SHADE drop ;" SHADOW-FIXTURE
   s" undefined-tick-shade.f" FIXTURE!
   s" E-UNDEFINED" s" NOPE" 8 4 87 91 REFUSED-AT
   s" a trust-boundary word before a called shadowed name" T-LABEL
   s" : SHADE ( -- ) ;" s"    evaluate SHADE ;" SHADOW-FIXTURE
   s" unsafe-shade.f" FIXTURE!
   s" E-USING-SHADOW-GLOBAL" s" SHADE" 8 13 96 101 REFUSED-AT
   s" a section word two used publics export" T-LABEL
   s" package DPA1 public : TARGET ( -- ) ; ;package package DPA2 public : TARGET ( -- ) ; ;package using DPA1 using DPA2 target ;using ;using"
   s" section-ambiguous.f" LINE-FIXTURE
   s" E-USING-AMBIGUOUS" s" target" 1 117 116 122 REFUSED-MODES
   s" a section word a global and a used public export" T-LABEL
   s" : TARGET ( -- ) ; package DPG1 public : TARGET ( -- ) ; ;package using DPG1 target ;using"
   s" section-shadow.f" LINE-FIXTURE
   s" E-USING-SHADOW-GLOBAL" s" target" 1 77 76 82 REFUSED-MODES
   s" a called name two used publics and a global export before an undefined word" T-LABEL
   true s"    AMB NOPE ;" AMBIG-FIXTURE
   s" ambig-global-undefined.f" FIXTURE!
   s" E-UNDEFINED" s" NOPE" 13 8 140 144 REFUSED-AT
   s" an undefined word before a name only two used publics export" T-LABEL
   false s"    NOPE AMB ;" AMBIG-FIXTURE
   s" undefined-ambig.f" FIXTURE!
   s" E-UNDEFINED" s" NOPE" 13 4 136 140 REFUSED-AT ;

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

\ A name a sealed package already holds is refused as sealed, as the load
\ refuses it: the seal is asked before the duplicate.
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
   s" E-SEALED-PACKAGE" s" SP:X" 6 10 60 64 70 REFUSED-MODES-RC
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
   s" : Q-SCAN ( -- )" SB-APPEND LF+
   s\"    s\\\" : Q-ONE ( -- ) parse-name 2drop ; immediate parses: Q-ONE 1\""
   SB-APPEND LF+
   s" [: ;] VERIFY:SOURCE-BUF-THEN-IN-SCOPE ;" SB-APPEND LF+
   s" ' Q-SCAN CHECK:SCOPED" SB-APPEND LF+
   s" : Q-TWO ( -- ) parse-name 2drop ; immediate" SB-APPEND LF+
   s" : USE-TWO ( -- n ) Q-TWO 73 ;" SB-APPEND LF+
   s" stale-parses.f" FIXTURE!
   s" E-UNMODELED-IMMEDIATE" s" :6:20" 70 LOADED-AT ;

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
   s" : HIT ( -- ) parse-name 2drop 1 HITS +! ;" SB-APPEND PI-LINE+
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
   GT-OUT$ s\" 40\n73\n" CONTAINS? TTRUE ;

\ The load runs a declared parsing immediate on its payload, so the check leaves the payload out of
\ the body, whatever it spells, and the body takes none of its facts: a top-level call is certified.
: TEST-PARSES-PAYLOAD ( -- )
   s" parses: payloads that spell a word, a number, ; and (" T-LABEL
   SB-RESET
   s" : HIT ( -- ) parse-name drop drop ;" SB-APPEND LF+
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
   GT-OUT$ s\" inferred c: option<n>\n" CONTAINS? TTRUE
   GT-OUT$ s\" inferred c: n\n" CONTAINS? TTRUE ;

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
   s" E-UNDEFINED" s" dp-held" 5 1 51 58 REFUSED-MODES
   SB-RESET
   s" `dp-held` is public in `DP-HOLD`: " SB-APPEND
   s" write `DP-HOLD:dp-held`, or call it inside " SB-APPEND
   s" `using DP-HOLD` … `;using`" SB-APPEND
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
   GT-ERR$ SB$ CONTAINS? TTRUE ;

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
   TEST-SHADOW
   TEST-TICK-SHADOW
   TEST-IS-SHADOW
   TEST-SHADOWED-ARITY
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
   TEST-PROGRAM-TARGET-WORD
   TEST-PARSES-REJECT
   TEST-GENERATES
   TEST-GENERATES-READS-NONE
   TEST-GENERATES-SHADOW
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
   TEST-REFUSAL-ORDER
   TEST-REPAIR-FOLLOWS
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
   TEST-CONTROL-ORPHAN
   TEST-CONTROL-REASON
   TEST-SECOND-ELSE
   TEST-CONTROL-OPEN
   TEST-EMPTY-CLAUSE
   TEST-UNCLOSED
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
   TEST-USING-ORDER
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
