\ hb-build-large-source-test.f - checked fixture for tools/hb-build-lib.f: a
\ string literal's body is declared text while the same bytes in a `create`
\ cell are refused, and the CLI builds a source of HBT-LARGE-INPUT-U bytes in
\ --repl and AOT modes. tools/hb-build-test-lib.f lists the other hb-build
\ rows.
\ Run: bin/hb --load tools/hb-build-large-source-test.f

require tools/hb-build-test-lib.f

using BUILD-FIXPOINT                     \ the build tmp root and engine override

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

4096 constant HBT-LARGE-CHUNK-U
$40000 1 - constant HBT-LARGE-INPUT-U

create HBT-LARGE-CHUNK HBT-LARGE-CHUNK-U allot

: HBT-LARGE-AOT-SRC$ ( -- ptr u8 n )
   s" variable SLOT 9 SLOT ! : MAIN ( -- ) SLOT @ . cr ;" ;

: HBT-LITB-SRC ( -- ptr u8 n )
   HBT-LITB-SRC-BUF HBT-LITB-SRC-U @ ;

: HBT-LITB-OUT ( -- ptr u8 n )
   HBT-LITB-OUT-BUF HBT-LITB-OUT-U @ ;

: HBT-LITC-SRC ( -- ptr u8 n )
   HBT-LITC-SRC-BUF HBT-LITC-SRC-U @ ;

: HBT-LITC-OUT ( -- ptr u8 n )
   HBT-LITC-OUT-BUF HBT-LITC-OUT-U @ ;

\ A STRING LITERAL'S BODY IS DECLARED TEXT, and the span scan skips its cells by
\ that declaration the way it skips a declared address cell by the engine's
\ address-cell table: src/compiler/native/string.f writes one row per interned
\ body when it places the bytes, and src/habu/aot-closure.f CELL-LITERAL? reads
\ those rows - never the bytes - for every cell src/habu/aot-lib.f SCAN-DATA-CELL
\ is about to refuse. Tender's stripped server is the case: `s" {pm}"` and the
\ 01 byte after it, read as one aligned cell, spelled an address inside the
\ emitted code span and the build refused the word that owns the body (AUTH
\ ROLE-LITERAL$, dot habu-link-int-cells-45328a4d).
\ The child derives its bytes from a live code address; a fixed Linux address
\ does not establish this property in a Mach-O builder with ASLR.
: HBT-LITB-SRC$ ( -- ptr u8 n )
   S\" require test/stripped-literal-subject.f\n' STRIPPED-LITERAL:ANCHOR STRIPPED-LITERAL:BODY-SOURCE evaluate\n" ;

\ MAIN verifies the length and bytes before printing this witness.
: HBT-LITB-EXPECTED$ ( -- ptr u8 n )
   S\" literal\n" ;

\ ... and THE SAME EIGHT BYTES IN A `create` CELL are refused as before. This is
\ the control that keeps the row above honest: raw storage carries no declaration
\ about its contents, so the value is still read as the code pointer it spells,
\ and a build that stopped refusing it would say the pattern had drifted out of
\ the emitted code span - in which case the row above proves nothing and this one
\ goes red instead of both passing on a value nothing classifies.
: HBT-LITC-SRC$ ( -- ptr u8 n )
   S\" require test/stripped-literal-subject.f\n' STRIPPED-LITERAL:ANCHOR STRIPPED-LITERAL:RAW\n: MAIN ( -- ) STRIPPED-LITERAL:VALUE . cr ;\n" ;

: HBT-LARGE-CHUNK! ( -- )
   HBT-LARGE-CHUNK-U 0 ?do 32 HBT-LARGE-CHUNK i + c! loop ;

: HBT-WRITE-LARGE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n src:ptr srcu:n :}
   path pathu src srcu WRITE-ALL
   HBT-LARGE-INPUT-U srcu - {: spaces:n :}
   spaces HBT-LARGE-CHUNK-U / 0 ?do
      path pathu HBT-LARGE-CHUNK HBT-LARGE-CHUNK-U APPEND-FILE
   loop
   spaces HBT-LARGE-CHUNK-U mod {: rem:n :}
   rem 0 > if path pathu HBT-LARGE-CHUNK rem APPEND-FILE then ;

\ THE CONTROL FIRST, then the body: the refusal is what proves the pattern lands
\ in the emitted code span of a build on this engine, and the link that follows
\ is then a statement about the declaration and not about the value.
: HBT-STRIPPED-LITERAL-BODY ( -- )
   HBT-LITC-SRC HBT-LITC-SRC$ WRITE-ALL
   HBT-LITC-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-LITC-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-LITC-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: cout:n cerr:n crc:n :}
   crc 70 T=
   HBT-ERR cerr s" holds an undeclared code/dict pointer" CONTAINS? TTRUE
   HBT-ERR cerr s" word=LIT-CELL" CONTAINS? TTRUE
   HBT-LITC-OUT FILE? TFALSE

   HBT-LITB-SRC HBT-LITB-SRC$ WRITE-ALL
   HBT-LITB-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-LITB-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-LITB-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: bout:n berr:n brc:n :}
   brc 0 <> if HBT-OUT bout type HBT-ERR berr type then
   brc 0 T=
   HBT-OUT bout s" hb-build OK" CONTAINS? TTRUE
   HBT-LITB-OUT FILE? TTRUE
   HBT-LITB-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 <> if HBT-RUN-OUT outn type HBT-RUN-ERR errn type then
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn HBT-LITB-EXPECTED$ T$=
   HBT-LITB-OUT HBT-REMOVE-FILE? ;

: HBT-CLI-LARGE-SOURCE ( -- )
   HBT-LARGE-CHUNK!
   HBT-REPL-SRC HBT-REPL-SRC$ HBT-WRITE-LARGE
   HBT-REPL-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   s" --repl" >LEN PROC-ARGV+
   HBT-REPL-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-REPL-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: rout:n rerr:n rrc:n :}
   rrc 0 <> if HBT-OUT rout type HBT-ERR rerr type then
   rrc 0 T=
   HBT-OUT rout s" hb-build OK" CONTAINS? TTRUE
   rerr 0 T=
   HBT-TMP BF-TMP!
   s" hb-build-check-src" BF-A$ EXISTS? TFALSE
   s" hb-build-src" BF-A$ EXISTS? TFALSE
   BF-TMP-RESET
   HBT-RUN-REPL

   HBT-AOT-SRC HBT-LARGE-AOT-SRC$ HBT-WRITE-LARGE
   HBT-AOT-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   HBT-AOT-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-AOT-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: aout:n aerr:n arc:n :}
   arc 0 <> if HBT-OUT aout type HBT-ERR aerr type then
   arc 0 T=
   HBT-OUT aout s" hb-build OK" CONTAINS? TTRUE
   aerr 0 T=
   HBT-TMP BF-TMP!
   s" hb-aot-src" BF-A$ EXISTS? TFALSE
   BF-TMP-RESET
   HBT-AOT-OUT FILE? TTRUE
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn s" 9" CONTAINS? TTRUE ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-LARGE-SOURCE-MAIN ( -- )
   T-RESET
   HBT-PREPARE
   HBT-STRIPPED-LITERAL-BODY
   HBT-CLI-LARGE-SOURCE
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-large-source-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-LARGE-SOURCE-MAIN
