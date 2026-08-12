\ judge/src-test.f - fixtures for the canonical corpus source reader.
\ Run: bin/hb --load tools/judge/src-test.f
\
\ WHAT THESE FIXTURES ARE FOR. tools/judge/src.f decides what a corpus file's
\ definitions ARE - their bodies, their arities and their callees - and both code
\ generators are then handed its answer. A reader that answered by searching text
\ would put a different program in one of the two columns and the comparison
\ would report the difference as a code generator result. So every fixture here
\ is a source built to fool a text matcher:
\
\   the definition that exists only inside a `\` line comment
\   the definition that exists only inside a `( ... )` comment
\   the definition that exists only inside a string literal
\   the `;]` that closes a quotation and must not close the definition
\   the `;package` that closes a package and must not close the definition
\   the `{: a:n :}` locals group, whose tokens carry colons
\   the corpus name written in the SIGNATURE, in the wrong role, which must
\     neither count as a call nor be renamed
\   the same callee named twice, which is one callee
\   the callee defined AFTER its caller, which is still that caller's callee
\   the name the file defines twice, which is a refusal rather than a guess
\
\ THE FIXTURES DRIVE THE PRODUCTION ENTRY POINTS. `SCAN` is what `LOAD` runs
\ once it has the bytes, so a fixture handing bytes to `SCAN` is running the
\ shipped reader and not a copy of it. The last group then runs `LOAD` itself,
\ over a real corpus file in this repository, and pins the one claim the whole
\ transition rests on: the text this reader DERIVES for a corpus word is, byte
\ for byte, the text that used to be retyped by hand in
\ tools/codegen-compare-migrated4.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require tools/judge/src.f

package JUDGE-SRC-TEST

private

: SUFFIX$ ( -- ptr u8 n )
   s" -J" ;

\ ---- one definition, and the shape everything else is measured against -------

: PLAIN$ ( -- ptr u8 n )
   s" : A ( n -- n ) 1 + ;" ;

: PLAIN-CASES ( -- )
   PLAIN$ JUDGE-SRC:SCAN
   JUDGE-SRC:DEFS 1 T=
   0 JUDGE-SRC:NAME$ s" A" T$=
   0 JUDGE-SRC:IN 1 T=
   0 JUDGE-SRC:OUT 1 T=
   0 JUDGE-SRC:CALLS 0 T=
   s" A" JUDGE-SRC:FIND 0 T=
   s" B" JUDGE-SRC:FIND -1 T=
   0 SUFFIX$ JUDGE-SRC:TEXT$ s" : A-J ( n -- n ) 1 + ;" T$= ;

\ ---- definitions that are only text ------------------------------------------
\ Each source below holds ONE real definition and one forgery. A reader that
\ searched for `: ` would find two.

: COMMENT-LINE$ ( -- ptr u8 n )
   S\" \\ : FAKE ( -- ) ;\n: A ( n -- n ) 1 + ;\n" ;

: COMMENT-PAREN$ ( -- ptr u8 n )
   s" : A ( n -- n ) ( : FAKE ; ) 1 + ;" ;

: STRING-LIT$ ( -- ptr u8 n )
   S\" : A ( n -- n ) s\q : FAKE ( -- ) ; \q 2drop 1 + ;" ;

: FORGERY-CASES ( -- )
   COMMENT-LINE$ JUDGE-SRC:SCAN
   JUDGE-SRC:DEFS 1 T=
   0 JUDGE-SRC:NAME$ s" A" T$=
   s" FAKE" JUDGE-SRC:FIND -1 T=

   COMMENT-PAREN$ JUDGE-SRC:SCAN
   JUDGE-SRC:DEFS 1 T=
   s" FAKE" JUDGE-SRC:FIND -1 T=
   0 SUFFIX$ JUDGE-SRC:TEXT$ s" : A-J ( n -- n ) ( : FAKE ; ) 1 + ;" T$=

   STRING-LIT$ JUDGE-SRC:SCAN
   JUDGE-SRC:DEFS 1 T=
   s" FAKE" JUDGE-SRC:FIND -1 T=
   0 JUDGE-SRC:IN 1 T= ;

\ ---- closers that are not the definition's ------------------------------------
\ `;]` ends a quotation and `;package` ends a package block. A reader that
\ closed a definition on either would report a body that stops early, and the
\ chain would be handed a program the engine never compiled.

: QUOTATION$ ( -- ptr u8 n )
   s" : A ( n -- n ) [: 1 + ;] drop 1 + ;" ;

: PACKAGE-CLOSE$ ( -- ptr u8 n )
   S\" : A ( n -- n ) 1 + ;\n;package\n" ;

: LOCALS$ ( -- ptr u8 n )
   s" : A ( n n -- n ) {: a:n b:n :} a b + ;" ;

: CLOSER-CASES ( -- )
   QUOTATION$ JUDGE-SRC:SCAN
   JUDGE-SRC:DEFS 1 T=
   0 SUFFIX$ JUDGE-SRC:TEXT$ s" : A-J ( n -- n ) [: 1 + ;] drop 1 + ;" T$=

   PACKAGE-CLOSE$ JUDGE-SRC:SCAN
   JUDGE-SRC:DEFS 1 T=
   0 SUFFIX$ JUDGE-SRC:TEXT$ s" : A-J ( n -- n ) 1 + ;" T$=

   LOCALS$ JUDGE-SRC:SCAN
   JUDGE-SRC:DEFS 1 T=
   0 JUDGE-SRC:IN 2 T=
   0 JUDGE-SRC:OUT 1 T=
   0 SUFFIX$ JUDGE-SRC:TEXT$ s" : A-J ( n n -- n ) {: a:n b:n :} a b + ;" T$= ;

\ ---- the arity the signature declares -----------------------------------------
\ `ptr` takes the type after it and the two are ONE value, so a string is two
\ values and a pointer to a pointer is one.

: POINTERS$ ( -- ptr u8 n )
   s" : A ( ptr u8 n ptr n -- ptr ptr u8 ) ;" ;

: ARITY-CASES ( -- )
   POINTERS$ JUDGE-SRC:SCAN
   0 JUDGE-SRC:IN 3 T=
   0 JUDGE-SRC:OUT 1 T= ;

\ ---- what the reader refuses to guess -----------------------------------------
\ Every one of these has more than one possible answer, and a reader that picked
\ one would be inventing the fact it exists to read.

: NO-SIG$ ( -- ptr u8 n )
   s" : A 1 + ;" ;

: NO-ARROW$ ( -- ptr u8 n )
   s" : A ( n n ) 1 + ;" ;

: TWO-ARROWS$ ( -- ptr u8 n )
   s" : A ( n -- n -- n ) 1 + ;" ;

: DANGLING-PTR$ ( -- ptr u8 n )
   s" : A ( n -- ptr ) 1 + ;" ;

: QUOT-SIG$ ( -- ptr u8 n )
   s" : A ( [ -- ] -- ) execute ;" ;

: NO-CLOSE$ ( -- ptr u8 n )
   s" : A ( n -- n ) 1 +" ;

: DUP-NAME$ ( -- ptr u8 n )
   S\" : A ( n -- n ) 1 + ;\n: A ( n -- n ) 2 + ;\n" ;

: UNTERMINATED$ ( -- ptr u8 n )
   S\" : A ( n -- n ) s\q open 1 + ;\n" ;

: REFUSAL-CASES ( -- )
   [: NO-SIG$ JUDGE-SRC:SCAN ;] E-JUDGE-SRC-SIG TTHROWSQ
   [: NO-ARROW$ JUDGE-SRC:SCAN ;] E-JUDGE-SRC-SIG TTHROWSQ
   [: TWO-ARROWS$ JUDGE-SRC:SCAN ;] E-JUDGE-SRC-SIG TTHROWSQ
   [: DANGLING-PTR$ JUDGE-SRC:SCAN ;] E-JUDGE-SRC-SIG TTHROWSQ
   [: QUOT-SIG$ JUDGE-SRC:SCAN ;] E-JUDGE-SRC-SIG TTHROWSQ
   [: NO-CLOSE$ JUDGE-SRC:SCAN ;] E-JUDGE-SRC-DEF TTHROWSQ
   [: DUP-NAME$ JUDGE-SRC:SCAN ;] E-JUDGE-SRC-DUP TTHROWSQ
   [: UNTERMINATED$ JUDGE-SRC:SCAN ;] E-JUDGE-SRC-LEX TTHROWSQ ;

: ROW-CASES ( -- )
   PLAIN$ JUDGE-SRC:SCAN
   [: 1 JUDGE-SRC:NAME$ 2drop ;] E-JUDGE-SRC-ROW TTHROWSQ
   [: -1 JUDGE-SRC:IN drop ;] E-JUDGE-SRC-ROW TTHROWSQ
   [: 0 0 JUDGE-SRC:CALL@ drop ;] E-JUDGE-SRC-ROW TTHROWSQ ;

\ ---- the callees a body names -------------------------------------------------
\ Distinct, in first-use order, and read off the BODY. A name in the signature
\ is in the wrong role: it is not a call, and renaming it would rewrite the
\ declared type of the derived word.

: WRONG-ROLE$ ( -- ptr u8 n )
   S\" : N ( n -- n ) 1 + ;\n: B ( N -- N ) 2 * ;\n" ;

: TWICE$ ( -- ptr u8 n )
   S\" : C ( n -- n ) 1 + ;\n: D ( n -- n ) C C ;\n" ;

: ORDER$ ( -- ptr u8 n )
   S\" : E ( n -- n ) F G F ;\n: F ( n -- n ) 1 + ;\n: G ( n -- n ) 2 * ;\n" ;

: FORWARD$ ( -- ptr u8 n )
   S\" : H ( n -- n ) K ;\n: K ( n -- n ) 1 + ;\n" ;

: CALL-CASES ( -- )
   WRONG-ROLE$ JUDGE-SRC:SCAN
   JUDGE-SRC:DEFS 2 T=
   s" B" JUDGE-SRC:FIND JUDGE-SRC:CALLS 0 T=
   s" B" JUDGE-SRC:FIND SUFFIX$ JUDGE-SRC:TEXT$ s" : B-J ( N -- N ) 2 * ;" T$=

   TWICE$ JUDGE-SRC:SCAN
   s" D" JUDGE-SRC:FIND JUDGE-SRC:CALLS 1 T=
   s" D" JUDGE-SRC:FIND 0 JUDGE-SRC:CALL@ s" C" JUDGE-SRC:FIND T=
   s" D" JUDGE-SRC:FIND SUFFIX$ JUDGE-SRC:TEXT$ s" : D-J ( n -- n ) C-J C-J ;" T$=

   ORDER$ JUDGE-SRC:SCAN
   s" E" JUDGE-SRC:FIND JUDGE-SRC:CALLS 2 T=
   s" E" JUDGE-SRC:FIND 0 JUDGE-SRC:CALL@ s" F" JUDGE-SRC:FIND T=
   s" E" JUDGE-SRC:FIND 1 JUDGE-SRC:CALL@ s" G" JUDGE-SRC:FIND T=

   FORWARD$ JUDGE-SRC:SCAN
   s" H" JUDGE-SRC:FIND JUDGE-SRC:CALLS 1 T=
   s" H" JUDGE-SRC:FIND 0 JUDGE-SRC:CALL@ s" K" JUDGE-SRC:FIND T= ;

\ ---- the storage a file declares ---------------------------------------------
\ `create NAME` and `variable NAME` OUTSIDE every definition. Inside one,
\ `create` is a defining word a program RUNS, which is a different thing and no
\ declaration at all - so a body that uses the word cannot register one. A
\ storage word is never renamed either: a derived word names the same cell the
\ word it is compared against names, which is what lets both columns step one
\ cell and is exactly what a rename would break.

: STORAGE$ ( -- ptr u8 n )
   S\" create CELL 1 cells allot\nvariable FLAG\n: A ( n -- n ) CELL ! CELL @ FLAG @ + ;\n" ;

: RUNTIME-CREATE$ ( -- ptr u8 n )
   S\" : MAKER ( -- ) create 8 allot ;\n: A ( n -- n ) 1 + ;\n" ;

: STORAGE-CASES ( -- )
   STORAGE$ JUDGE-SRC:SCAN
   JUDGE-SRC:DATA-DEFS 2 T=
   0 JUDGE-SRC:DATA-NAME$ s" CELL" T$=
   1 JUDGE-SRC:DATA-NAME$ s" FLAG" T$=
   s" CELL" JUDGE-SRC:DATA-FIND 0 T=
   s" NOPE" JUDGE-SRC:DATA-FIND -1 T=
   s" A" JUDGE-SRC:FIND JUDGE-SRC:USES 2 T=
   s" A" JUDGE-SRC:FIND 0 JUDGE-SRC:USE@ 0 T=
   s" A" JUDGE-SRC:FIND 1 JUDGE-SRC:USE@ 1 T=
   s" A" JUDGE-SRC:FIND SUFFIX$ JUDGE-SRC:TEXT$
      s" : A-J ( n -- n ) CELL ! CELL @ FLAG @ + ;" T$=

   RUNTIME-CREATE$ JUDGE-SRC:SCAN
   JUDGE-SRC:DATA-DEFS 0 T=
   s" A" JUDGE-SRC:FIND JUDGE-SRC:USES 0 T= ;

\ The same two questions on the corpora themselves, where the answers decide
\ which migration entry a subject needs.
: FILE-STORAGE-CASES ( -- )
   s" tools/codegen-compare-corpus.f" JUDGE-SRC:LOAD
   JUDGE-SRC:DATA-DEFS 1 T=
   0 JUDGE-SRC:DATA-NAME$ s" BUMP-CELL" T$=
   s" CELL-BUMP" JUDGE-SRC:FIND JUDGE-SRC:USES 1 T=
   s" NOOP" JUDGE-SRC:FIND JUDGE-SRC:USES 0 T=
   s" CELL-BUMP" JUDGE-SRC:FIND s" -N" JUDGE-SRC:TEXT$
      S\" : CELL-BUMP-N ( n -- n )\n   BUMP-CELL !\n   BUMP-CELL @ 1+ dup BUMP-CELL ! ;" T$=

   s" tools/codegen-compare-corpus2.f" JUDGE-SRC:LOAD
   JUDGE-SRC:DATA-DEFS 3 T=
   s" TV-NEXT?" JUDGE-SRC:FIND JUDGE-SRC:USES 1 T=
   s" FILL-COPY" JUDGE-SRC:FIND JUDGE-SRC:USES 2 T=
   s" T-RES-WALK" JUDGE-SRC:FIND JUDGE-SRC:USES 0 T= ;

\ ---- the real corpus file, through the real entry ------------------------------
\ The claim the transition rests on. `-N` is the suffix the hand-retyped column
\ in tools/codegen-compare-migrated4.f used, so the derived texts below are
\ compared against that program word for word: if the reader ever derived
\ something else, the chain would be compiling a different program from the one
\ the engine compiled.
\
\ WHERE THE DERIVED TEXT AND THE RETYPED ONE DIFFER, AND WHY THAT IS THE POINT.
\ The retyped column wrote every body on ONE line because a string literal is
\ one line. The derived text is the corpus's own bytes, so a body the corpus
\ wrote over two lines arrives over two lines. Both spellings are the same
\ program - the assertions below pin one of each - and only one of them is
\ something a person had to keep in step by hand.

: CORPUS4$ ( -- ptr u8 n )
   s" tools/codegen-compare-corpus4.f" ;

: MIGRATED$ ( -- ptr u8 n )
   s" -N" ;

: WORD-TEXT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u JUDGE-SRC:FIND MIGRATED$ JUDGE-SRC:TEXT$ ;

: FILE-CASES ( -- )
   CORPUS4$ JUDGE-SRC:LOAD
   JUDGE-SRC:DEFS 25 T=

   s" C-ADD1" WORD-TEXT$ s" : C-ADD1-N ( n -- n ) 1 + ;" T$=
   s" C-MAD" WORD-TEXT$ s" : C-MAD-N ( n -- n ) 3 * 5 + ;" T$=
   s" C-LONG" WORD-TEXT$
      S\" : C-LONG-N ( n -- n )\n   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;" T$=
   s" CALL-FAN" WORD-TEXT$
      S\" : CALL-FAN-N ( n -- n )\n   C-ADD1-N C-MUL2-N C-AND7-N C-XOR5-N C-ADD1-N ;" T$=
   s" CALL-FAN-BIG" WORD-TEXT$
      S\" : CALL-FAN-BIG-N ( n -- n )\n   C-MAD-N C-MAD-N C-MAD-N C-MAD-N C-MAD-N ;" T$=

   s" CALL-FAN" JUDGE-SRC:FIND JUDGE-SRC:IN 1 T=
   s" CALL-FAN" JUDGE-SRC:FIND JUDGE-SRC:CALLS 4 T=
   s" CALL-FAN-BIG" JUDGE-SRC:FIND JUDGE-SRC:CALLS 1 T=
   s" CALL-LOOP-3" JUDGE-SRC:FIND JUDGE-SRC:IN 5 T=
   s" CALL-LOOP-3" JUDGE-SRC:FIND JUDGE-SRC:CALLS 3 T=
   s" WIDE-ARITY" JUDGE-SRC:FIND JUDGE-SRC:IN 6 T=
   s" MANY-LOCALS" JUDGE-SRC:FIND JUDGE-SRC:IN 9 T=
   s" PRESSURE-LOOP" JUDGE-SRC:FIND JUDGE-SRC:IN 2 T=
   s" PRESSURE-LOOP" JUDGE-SRC:FIND JUDGE-SRC:OUT 1 T=
   s" STORE-LOAD" JUDGE-SRC:FIND JUDGE-SRC:IN 2 T=
   s" SETUP" JUDGE-SRC:FIND JUDGE-SRC:IN 0 T=
   s" SETUP" JUDGE-SRC:FIND JUDGE-SRC:OUT 0 T= ;

public

: RUN ( -- )
   T-RESET
   PLAIN-CASES
   FORGERY-CASES
   CLOSER-CASES
   ARITY-CASES
   REFUSAL-CASES
   ROW-CASES
   CALL-CASES
   STORAGE-CASES
   FILE-STORAGE-CASES
   FILE-CASES
   T-REPORT ;

;package

JUDGE-SRC-TEST:RUN
