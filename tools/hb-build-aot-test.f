\ hb-build-aot-test.f - checked fixture for tools/hb-build-lib.f: the AOT
\ groups that build and run one program each - the object producer, the native
\ call sites, the division refusal, the stripped span definer with its foreign
\ calls, the empty does> clause and a multi-page DATA payload - and the program
\ the keyed linker image refuses.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-aot-test.f

require tools/hb-build-test-lib.f
require tools/image-size-lib.f          \ this fixture measures image bytes directly
require test/linker-builder-engine.f
require src/lib/string.f
require src/lib/codegen.f

using BUILD-FIXPOINT                     \ the build tmp root

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

: HBT-SPAN-SRC ( -- ptr u8 n )
   HBT-SPAN-SRC-BUF HBT-SPAN-SRC-U @ ;

: HBT-SPAN-OUT ( -- ptr u8 n )
   HBT-SPAN-OUT-BUF HBT-SPAN-OUT-U @ ;

\ A zero divisor in a STRIPPED image. The division is compiled by the native
\ compiler (the image is built at tier 1, which `LOADING` holds it to), and its
\ refusal branches to the engine's own `throw` - an address outside this
\ payload's window that the closure walker has to follow and the build has to
\ relocate, exactly as it does for the terminator's `die`. An image whose branch
\ was left pointing at the building engine's text does not come back here with a
\ code at all, and one whose refusal was dropped answers zero and throws -9051.
\ The code is ARITH-ABI:E-DIV-ZERO, spelled out because a built source is a
\ string and cannot see this file's constants.
: HBT-AOT-DIVZ-SRC$ ( -- ptr u8 n )
   S\" package HBT-DIVZ\n: LOADING ( -- ) tier@ 1 <> if -9050 throw then ;\nLOADING\nvariable A\nvariable B\nvariable R\n: DZ ( n n -- n ) / ;\n: TRY ( -- n ) [: A @ B @ DZ R ! ;] catch ;\npublic\n: RUN ( -- ) 7 A ! 0 B ! TRY dup . cr -6400 <> if -9051 throw then 7 A ! 2 B ! TRY 0 <> if -9052 throw then R @ 3 <> if -9053 throw then ;\n;package\n: MAIN ( -- ) HBT-DIVZ:RUN ;\n" ;

\ A nonzero 20 KiB source span has an encoded sparse payload longer than one
\ Mach-O DATA_CONST page. Its last byte must survive the extra mapped page.
: HBT-AOT-MULTI-SRC$ ( -- ptr u8 n )
   S\" package HBT-MULTI\ncreate MULTI-BUF 20000 allot\n: INIT ( -- ) 20000 0 ?do 1 MULTI-BUF i + c! loop ;\nINIT\npublic\n: RUN ( -- ) MULTI-BUF c@ MULTI-BUF 19999 + c@ + . cr ;\n;package\n: MAIN ( -- ) HBT-MULTI:RUN ;\n" ;

4096 CODEGEN:BUFFER HBT-SPAN-PROGRAM

: HBT-SPAN-SRC$ ( -- ptr u8 n )
   HBT-SPAN-PROGRAM CODEGEN:RESET
   S\" require src/lib/span.f\nrequire src/lib/memory.f\nrequire src/lib/ffi-abi.f\npackage HBT-SPAN\n: LOADING ( -- ) tier@ 1 <> if -9060 throw then ;\nLOADING\nPROCESS-SYMBOLS\nFUNCTION: PID getpid ( -- i32 ) ;FUNCTION\n" HBT-SPAN-PROGRAM CODEGEN:APPEND-STRING
   HB-TARGET-MACOS? if
      S\" LIBRARY /usr/lib/libSystem.B.dylib\n" HBT-SPAN-PROGRAM CODEGEN:APPEND-STRING
   else
      S\" LIBRARY libc.so.6\n" HBT-SPAN-PROGRAM CODEGEN:APPEND-STRING
   then
   S\" FUNCTION: HOST gethostname ( ptr u8 n -- i32 )\n   0 1 WRITES-ARG ;FUNCTION\ncreate HOST-BUF 256 allot\n64 SPAN-BUFFER: SB\npublic\n: RUN ( -- )\n   PID 0 > 0= if -9061 throw then\n   HOST-BUF BYTE-VIEW 256 HOST 0 <> if -9062 throw then\n   SB SPAN:LEN . cr\n   128 MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: s :}\n   s\" span\" s SPAN:COPY\n   s 0 4 SPAN:SUB SPAN:$ type cr\n   s 8 SPAN:SKIP 16 SPAN:TAKE SPAN:LEN . cr\n   s MEM:FREE-SPAN ;\n;package\n: MAIN ( -- ) HBT-SPAN:RUN ;\n" HBT-SPAN-PROGRAM CODEGEN:APPEND-STRING
   HBT-SPAN-PROGRAM CODEGEN:CONTENTS ;

: HBT-SPAN-EXPECTED$ ( -- ptr u8 n )
   S\" 64\n\nspan\n16\n\n" ;

\ A definer whose `does>` clause is EMPTY, in a stripped image: the created word
\ keeps the RET `create` emitted (habu2.f DOES-REC:ELIDE-EMPTY), so it reads its
\ cell with no branch out of its own body, and the image prints what was stored.
: HBT-AOT-DOES-SRC$ ( -- ptr u8 n )
   S\" package HBT-DOES\n: LOADING ( -- ) tier@ 1 <> if -9070 throw then ;\nLOADING\n: SLOT: ( -- ) create 0 , does> ( -- ptr n ) ;\nSLOT: S1\npublic\n: RUN ( -- ) 42 S1 ! S1 @ dup . cr 42 <> if -9071 throw then ;\n;package\n: MAIN ( -- ) HBT-DOES:RUN ;\n" ;

\ Run an image at a path of its own and hold it to its whole output, for a case
\ that builds somewhere other than HBT-AOT-OUT or HBT-REPL-OUT.
: HBT-RUN-IMAGE-OUT ( ptr u8 n ptr u8 n -- ) {: img:ptr imgu:n want:ptr wantu:n :}
   img imgu >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn want wantu T$= ;

\ HBT-RUN-AOT's run, for an image that is MEANT to print: the refusal case below
\ prints the code it caught, so a silent image would pass HBT-RUN-AOT for
\ exactly the wrong reason.
: HBT-RUN-AOT-PRINTS ( ptr u8 n -- ) {: want:ptr wantu:n :}
   HBT-AOT-OUT >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   errn 0 T=
   HBT-RUN-OUT outn want wantu CONTAINS? TTRUE ;

: BUILD-AOT-OBJECT-PRODUCER ( -- )
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   -1 HBB-REPORT-JSON !
   HBB-BUILD
   HBB-MAKER-RUN @ 0 <> TTRUE
   HBB-OBJECT-HIT @ 0= TTRUE
   HBB-OBJECT-STORE @ 0 <> TTRUE
   HB-BUILD:REPORT$ JR:T-FALSE JR:T-FALSE JR:T-TRUE JR:T-FALSE JR:T-TRUE CHECK-REPORT
   HBT-OBJ-LOAD? TTRUE
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   -1 HBB-REPORT-JSON !
   HBB-BUILD
   HBB-OBJECT-HIT @ 0 <> TTRUE
   HBB-OBJECT-STORE @ 0= TTRUE
   HBB-MAKER-RUN @ 0= TTRUE
   HBB-MAKER-BUILD @ 0= TTRUE
   HB-BUILD:REPORT$ JR:T-FALSE JR:T-TRUE JR:T-FALSE JR:T-FALSE JR:T-FALSE CHECK-REPORT
   HBT-RUN-AOT
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   BF-TMP-RESET ;

: BUILD-AOT-NATIVE ( -- )
   HBT-AOT-SRC HBT-AOT-SRC2$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   -1 HBB-REPORT-JSON !
   HBB-BUILD
   HBB-ARTIFACT-HIT @ 0= TTRUE
   HBB-OBJECT-HIT @ 0= TTRUE
   HBB-MAKER-HIT @ 0<> TTRUE
   HBB-MAKER-RUN @ 0 <> TTRUE
   HB-BUILD:REPORT$ JR:T-FALSE JR:T-FALSE JR:T-TRUE JR:T-FALSE JR:T-TRUE CHECK-REPORT
   HBT-RUN-AOT
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BF-TMP-RESET ;

\ THE CONTRACT THAT DOES NOT DEPEND ON THE TIER. A program that divides by zero
\ catches ARITH-ABI:E-DIV-ZERO and carries on, and a built executable is where
\ that used to stop being true: the lowering's guard ended in a `brk`, so the
\ same source that refused by name under `bin/hb` died with a register dump once
\ it was an image. The catch is asserted THROUGH A BUILT AND EXECUTED IMAGE
\ because the relocation of the refusal's branch is what makes it true and
\ nothing short of running the image exercises it.
: BUILD-AOT-DIV-REFUSAL ( -- )
   HBT-AOT-SRC HBT-AOT-DIVZ-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   S\" -6400\n" HBT-RUN-AOT-PRINTS
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BF-TMP-RESET ;

\ A `create ... does>` definer whose clause yields src/lib/span.f's two-cell
\ span, built stripped and RUN as an image: the clause is compiled at tier 1
\ (LOADING asserts the tier), and the span set the value belongs to - allocate,
\ copy, skip, take, sub, release - is exercised through it (dot
\ habu-give-a-does-97cd0db2). The program also calls getpid and gethostname
\ through FUNCTION: declarations, resolved at the first call in the image's own
\ process. `SPAN-BUFFER:`'s clause has a body, so the created word's last
\ instruction is the plain B that habu2.f DOESPATCH:EMIT wrote over its RET,
\ aimed at the parent's `;does` companion record. Only src/habu/aot-closure.f
\ SCAN-DIRECT following a direct branch OUT of the member puts that record in
\ the closure; without it the relocation refused this very program with `aot:
\ PC-relative target removed or outside closure`. Running the image is what
\ proves the clause was copied and retargeted rather than merely counted. The
\ engine bakes both modules the program requires, so the linker image links it
\ (KEYED-PRE-WINDOW-REFUSED below says why).
: BUILD-AOT-SPAN ( -- )
   HBT-SPAN-SRC HBT-SPAN-SRC$ WRITE-ALL
   HBT-SPAN-OUT HBT-REMOVE-FILE?
   HBT-SPAN-SRC HBT-SPAN-OUT HBT-HBB-PREPARE-AOT
   HBT-HBB-BUILD-OUT
   HBT-SPAN-OUT FILE? TTRUE
   HBT-SPAN-OUT HBT-SPAN-EXPECTED$ HBT-RUN-IMAGE-OUT
   HB-TARGET-MACOS? if
      HBT-SPAN-OUT IMAGE-SIZE:MEASURE
      IMAGE-SIZE:DATA-BLOB-RANGE drop
      MACHO-READ:DATA-OFF MACHO-READ:GOT-BYTES + T=
   then
   HBT-REMOVE-ARTIFACT
   HBT-SPAN-OUT HBT-REMOVE-FILE? ;

\ A program requiring a module of the linker's lib closure that the engine does
\ not bake, and calling a word of it.
: HBT-REQUIRED-SRC$ ( -- ptr u8 n )
   S\" require src/lib/fmt.f\n: MAIN ( -- ) 42 FMT:.INT cr ;\n" ;

\ THE KEYED LINKER IMAGE REFUSES THAT PROGRAM BY NAME (test/preloaded-engine.f
\ rule 3). The image loaded src/lib/fmt.f with the linker, before its maker
\ latched the band, so the program's require resolves to that copy: its closure
\ reaches a word compiled outside the window, and the maker refuses the first
\ such word rather than carry the image's copy. The module is one the engine
\ does not bake: a baked module's words lie below the engine's seal watermark,
\ where the band starts (src/habu/aot-closure.f PRE-WINDOW?), so every maker
\ carries them and the image links a program that requires one. Without
\ --json-errors the refusal is PRE-WINDOW-DIE's prose arm, naming the word and
\ a suggestion.
: KEYED-PRE-WINDOW-REFUSED ( -- )
   HBT-SPAN-SRC HBT-REQUIRED-SRC$ WRITE-ALL
   HBT-SPAN-SRC HBT-RUN-MAKER {: out:n err:n rc:n :}
   rc 74 T=
   HBB-ERR-BUF err s" aot: closure reaches a word defined before the capture window opened word=" CONTAINS? TTRUE
   HBB-ERR-BUF err s"  suggestion='" CONTAINS? TTRUE ;

\ The same refusal through the CLI under --json-errors, which promises its
\ caller JSON: the maker reads the flag from its argv (tools/aot-build-open.f)
\ and the CLI keeps only the JSON lines of the maker's stderr
\ (HBB-WERR-JSON-ONLY), so a refusal with no JSON arm reaches the caller as
\ text.
: KEYED-PRE-WINDOW-JSON ( -- )
   HBT-SPAN-SRC HBT-REQUIRED-SRC$ WRITE-ALL
   HBT-SPAN-OUT HBT-REMOVE-FILE?
   HBT-ARGV-BASE
   s" --json-errors" >LEN PROC-ARGV+
   HBT-SPAN-SRC >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   HBT-SPAN-OUT >LEN PROC-ARGV+
   HBT-RUN-HB-BUILD {: outu:n erru:n rc:n :}
   rc 74 T=
   outu 0 T=
   HBT-ERR erru S\" \qschema_version\q:1," CONTAINS? TTRUE
   HBT-ERR erru S\" \qcode\q:\qE-AOT-PRE-WINDOW\q" CONTAINS? TTRUE
   HBT-ERR erru S\" \qword\q:\q.INT\q" CONTAINS? TTRUE
   HBT-SPAN-OUT EXISTS? TFALSE ;

\ THE OTHER HALF OF THE DOES> RULE. An EMPTY clause compiles no instruction, so
\ elaborate.f STAGE-DOES-ENTRY publishes the companion record and patches
\ nothing: the created word keeps its RET and emits no branch at all. Such a
\ definer must still build and run stripped - the closure has nothing extra to
\ follow and the unreached companion record is shaken out with every other name.
: BUILD-AOT-DOES-EMPTY ( -- )
   HBT-AOT-SRC HBT-AOT-DOES-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   S\" 42\n" HBT-RUN-AOT-PRINTS
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BF-TMP-RESET ;

: BUILD-AOT-MULTI-PAGE ( -- )
   HB-TARGET-MACOS? 0= if exit then
   HBT-AOT-SRC HBT-AOT-MULTI-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBT-HBB-BUILD-OUT
   HBT-AOT-OUT S\" 2\n\n" HBT-RUN-IMAGE-OUT
   HBT-AOT-OUT IMAGE-SIZE:MEASURE
   MACHO-READ:DATA-BYTES $4000 > TTRUE
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-AOT-MAIN ( -- )
   T-RESET
   LINKER-BUILDER-ENGINE:ENSURE LINKER-BUILDER-ENGINE:HOST$ HBT-KEYED!
   HBT-PREPARE
   BUILD-AOT-OBJECT-PRODUCER
   BUILD-AOT-NATIVE
   BUILD-AOT-DIV-REFUSAL
   BUILD-AOT-SPAN
   KEYED-PRE-WINDOW-REFUSED
   KEYED-PRE-WINDOW-JSON
   BUILD-AOT-DOES-EMPTY
   BUILD-AOT-MULTI-PAGE
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-aot-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-AOT-MAIN
