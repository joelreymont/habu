\ hb-build-aot-test.f - checked fixture for tools/hb-build-lib.f: the AOT
\ groups that build and run one program each - the object producer, the native
\ call sites, the division refusal, the span definer as a snapshot and
\ stripped, the empty does> clause and one foreign call.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-aot-test.f

require tools/hb-build-test-lib.f
require test/preloaded-engine.f

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

: HBT-SPAN-SRC$ ( -- ptr u8 n )
   S\" require lib/span.f\nrequire lib/memory.f\npackage HBT-SPAN\n: LOADING ( -- ) tier@ 1 <> if -9060 throw then ;\nLOADING\n64 SPAN-BUFFER: SB\npublic\n: RUN ( -- )\n   SB SPAN:LEN . cr\n   128 MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: s :}\n   s\" span\" s SPAN:COPY\n   s 0 4 SPAN:SUB SPAN:$ type cr\n   s 8 SPAN:SKIP 16 SPAN:TAKE SPAN:LEN . cr\n   s MEM:FREE-SPAN ;\n;package\n: MAIN ( -- ) HBT-SPAN:RUN ;\n" ;

: HBT-SPAN-EXPECTED$ ( -- ptr u8 n )
   S\" 64\n\nspan\n16\n\n" ;

\ A definer whose `does>` clause is EMPTY, in a stripped image: the created word
\ keeps the RET `create` emitted (habu2.f DOES-REC:ELIDE-EMPTY), so it reads its
\ cell with no branch out of its own body, and the image prints what was stored.
: HBT-AOT-DOES-SRC$ ( -- ptr u8 n )
   S\" package HBT-DOES\n: LOADING ( -- ) tier@ 1 <> if -9070 throw then ;\nLOADING\n: SLOT: ( -- ) create 0 , does> ( -- ptr n ) ;\nSLOT: S1\npublic\n: RUN ( -- ) 42 S1 ! S1 @ dup . cr 42 <> if -9071 throw then ;\n;package\n: MAIN ( -- ) HBT-DOES:RUN ;\n" ;

\ ONE FOREIGN CALL IN A STRIPPED IMAGE. The declarer's generated word reaches
\ package FFI's staged call, which resolves `getpid` through the loader at the
\ FIRST CALL - in the image's own process, never the builder's - so the program
\ prints 1 only if the image carried everything that resolution needs: the FFI
\ table, in the program's own DATA window; the loader's two GOT slots, which
\ src/os/linux/elf.f relocates in every image it writes; and the engine's
\ text-base cell, which src/os/linux/layout.f locates those slots from and
\ src/habu/aot-owned-cells.f claims TEXT-BASE. Left at the fresh mapping's zero
\ that cell made this program build without a word of complaint and take SIGSEGV
\ inside DLSYM-SLOT's `@`. lib/ffi-abi.f is in the linker's lib closure, so
\ BUILD-AOT-FFI compiles the linker above this program.
: HBT-AOT-FFI-SRC$ ( -- ptr u8 n )
   S\" require lib/ffi-abi.f\nPROCESS-SYMBOLS\nFUNCTION: GETPID-CALL getpid ( -- i32 ) ;FUNCTION\n: MAIN ( -- ) GETPID-CALL 0 > if 1 else 0 then . cr ;\n" ;

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
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBB-MAKER-RUN @ 0 <> TTRUE
   HBB-OBJECT-HIT @ 0= TTRUE
   HBB-OBJECT-STORE @ 0 <> TTRUE
   HB-BUILD:REPORT$ JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-TRUE CHECK-REPORT
   HBT-OBJ-LOAD? TTRUE
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
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
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-SRC2$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBB-ARTIFACT-HIT @ 0= TTRUE
   HBB-OBJECT-HIT @ 0= TTRUE
   HBB-MAKER-HIT @ 0= TTRUE
   HBB-MAKER-RUN @ 0 <> TTRUE
   HB-BUILD:REPORT$ JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-FALSE JR:T-TRUE CHECK-REPORT
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
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-DIVZ-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   S\" -6400\n" HBT-RUN-AOT-PRINTS
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BF-TMP-RESET ;

\ A `create ... does>` definer whose clause yields a multi-cell value, built and
\ RUN as an image: the clause is compiled at tier 1 (LOADING asserts the tier),
\ the created word yields lib/span.f's two-cell span, and the span set it belongs
\ to - allocate, copy, skip, take, sub, release - is exercised through that
\ value. Nothing short of running the image shows the created word's row
\ surviving the build (dot habu-give-a-does-97cd0db2).
\ BUILD-AOT-SPAN below is the stripped twin of this case, on the same source.
: BUILD-REPL-SPAN ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-SPAN-SRC HBT-SPAN-SRC$ WRITE-ALL
   HBT-SPAN-OUT HBT-REMOVE-FILE?
   HBT-SPAN-SRC HBT-SPAN-OUT HBT-HBB-PREPARE-REPL
   HBT-HBB-BUILD-OUT
   HBT-SPAN-OUT FILE? TTRUE
   HBT-SPAN-OUT HBT-SPAN-EXPECTED$ HBT-RUN-IMAGE-OUT
   HBT-REMOVE-ARTIFACT
   HBT-SPAN-OUT HBT-REMOVE-FILE? ;

\ THE SAME SOURCE WITH THE NAMES SHAKEN OUT. `SPAN-BUFFER:` is a `create ...
\ does>` definer whose clause has a body, so the created word's last instruction
\ is the plain B that habu2.f DOESPATCH:EMIT wrote over its RET, aimed at the
\ parent's `;does` companion record. Only src/habu/aot-closure.f SCAN-DIRECT
\ following a direct branch OUT of the member puts that record in the closure;
\ without it the relocation refused this very program with `aot: PC-relative
\ target removed or outside closure`. Running the image is what proves the clause
\ was copied and retargeted rather than merely counted. lib/span.f is in the
\ linker's lib closure, so the linker is compiled above this program.
: BUILD-AOT-SPAN ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-SPAN-SRC HBT-SPAN-SRC$ WRITE-ALL
   HBT-SPAN-OUT HBT-REMOVE-FILE?
   HBT-SPAN-SRC HBT-SPAN-OUT HBT-HBB-PREPARE-AOT-SOURCE
   HBT-HBB-BUILD-OUT
   HBT-SPAN-OUT FILE? TTRUE
   HBT-SPAN-OUT HBT-SPAN-EXPECTED$ HBT-RUN-IMAGE-OUT
   HBT-REMOVE-ARTIFACT
   HBT-SPAN-OUT HBT-REMOVE-FILE? ;

\ THE OTHER HALF OF THE DOES> RULE. An EMPTY clause compiles no instruction, so
\ elaborate.f STAGE-DOES-ENTRY publishes the companion record and patches
\ nothing: the created word keeps its RET and emits no branch at all. Such a
\ definer must still build and run stripped - the closure has nothing extra to
\ follow and the unreached companion record is shaken out with every other name.
: BUILD-AOT-DOES-EMPTY ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-DOES-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   S\" 42\n" HBT-RUN-AOT-PRINTS
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BF-TMP-RESET ;

: BUILD-AOT-FFI ( -- )
   HBT-TMP BUILD-CACHE:ROOT!
   HBT-AOT-SRC HBT-AOT-FFI-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT-SOURCE
   HBB-BUILD
   S\" 1\n\n" HBT-RUN-AOT-PRINTS
   HBT-REMOVE-ARTIFACT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BF-TMP-RESET ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-AOT-MAIN ( -- )
   T-RESET
   PRELOADED-ENGINE:LINKER$ APP-IMAGE-ENGINE:PATH$ HBT-KEYED!
   HBT-PREPARE
   BUILD-AOT-OBJECT-PRODUCER
   BUILD-AOT-NATIVE
   BUILD-AOT-DIV-REFUSAL
   BUILD-REPL-SPAN
   BUILD-AOT-SPAN
   BUILD-AOT-DOES-EMPTY
   BUILD-AOT-FFI
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-aot-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-AOT-MAIN
