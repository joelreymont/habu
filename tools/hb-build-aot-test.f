\ hb-build-aot-test.f - checked fixture for tools/hb-build-lib.f: the AOT
\ groups that build and run a program - the object producer, the native call
\ sites, and one program whose packages carry the division refusal, the
\ stripped span definer with its foreign calls, the empty does> clause and a
\ multi-page DATA payload - one program over the forms a product compiles built
\ at each tier, and the program the keyed linker image refuses.
\ tools/hb-build-test-lib.f lists the other hb-build rows.
\ Run: bin/hb --load tools/hb-build-aot-test.f

require tools/hb-build-test-lib.f
require tools/image-size-lib.f          \ this fixture measures image bytes directly
require test/linker-builder-engine.f
require src/lib/string.f
require src/lib/codegen.f
require src/lib/tier.f

using BUILD-FIXPOINT                     \ the build tmp root

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

: HBT-SPAN-SRC ( -- ptr u8 n )
   HBT-SPAN-SRC-BUF HBT-SPAN-SRC-U @ ;

: HBT-SPAN-OUT ( -- ptr u8 n )
   HBT-SPAN-OUT-BUF HBT-SPAN-OUT-U @ ;

\ THE CONTRACT THAT DOES NOT DEPEND ON THE TIER. A program that divides by zero
\ catches ARITH-ABI:E-DIV-ZERO and carries on, and a built executable is where
\ that used to stop being true: the lowering's guard ended in a `brk`, so the
\ same source that refused by name under `bin/hb` died with a register dump once
\ it was an image. The catch is asserted THROUGH A BUILT AND EXECUTED IMAGE
\ because the relocation of the refusal's branch is what makes it true and
\ nothing short of running the image exercises it.
\
\ A zero divisor in a STRIPPED image. The division is compiled by the native
\ compiler (the image is built at tier 1, which `LOADING` holds it to), and its
\ refusal branches to the engine's own `throw` - an address outside this
\ payload's window that the closure walker has to follow and the build has to
\ relocate, exactly as it does for the terminator's `die`. An image whose branch
\ was left pointing at the building engine's text does not come back here with a
\ code at all, and one whose refusal was dropped answers zero and throws -9051.
\ The code is ARITH-ABI:E-DIV-ZERO, spelled out because a built source is a
\ string and cannot see this file's constants.
: HBT-AOT-DIVZ-PKG$ ( -- ptr u8 n )
   S\" package HBT-DIVZ\n: LOADING ( -- ) tier@ 1 <> if -9050 throw then ;\nLOADING\nvariable A\nvariable B\nvariable R\n: DZ ( n n -- n ) / ;\n: TRY ( -- n ) [: A @ B @ DZ R ! ;] catch ;\npublic\n: RUN ( -- ) 7 A ! 0 B ! TRY dup . cr -6400 <> if -9051 throw then 7 A ! 2 B ! TRY 0 <> if -9052 throw then R @ 3 <> if -9053 throw then ;\n;package\n" ;

\ A nonzero 20 KiB source span has an encoded sparse payload longer than one
\ Mach-O DATA_CONST page. Its last byte must survive the extra mapped page.
: HBT-AOT-MULTI-PKG$ ( -- ptr u8 n )
   S\" package HBT-MULTI\ncreate MULTI-BUF 20000 allot\n: INIT ( -- ) 20000 0 ?do 1 MULTI-BUF i + c! loop ;\nINIT\npublic\n: RUN ( -- ) MULTI-BUF c@ MULTI-BUF 19999 + c@ + . cr ;\n;package\n" ;

4096 CODEGEN:BUFFER HBT-POSITIVES

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
: HBT-SPAN-PKG ( -- )
   S\" require src/lib/span.f\nrequire src/lib/memory.f\nrequire src/lib/ffi-abi.f\npackage HBT-SPAN\n: LOADING ( -- ) tier@ 1 <> if -9060 throw then ;\nLOADING\nPROCESS-SYMBOLS\nFUNCTION: PID getpid ( -- i32 ) ;FUNCTION\n" HBT-POSITIVES CODEGEN:APPEND-STRING
   HB-TARGET-MACOS? if
      S\" LIBRARY /usr/lib/libSystem.B.dylib\n" HBT-POSITIVES CODEGEN:APPEND-STRING
   else
      S\" LIBRARY libc.so.6\n" HBT-POSITIVES CODEGEN:APPEND-STRING
   then
   S\" FUNCTION: HOST gethostname ( ptr u8 n -- i32 )\n   0 1 WRITES-ARG ;FUNCTION\ncreate HOST-BUF 256 allot\n64 SPAN-BUFFER: SB\npublic\n: RUN ( -- )\n   PID 0 > 0= if -9061 throw then\n   HOST-BUF BYTE-VIEW 256 HOST 0 <> if -9062 throw then\n   SB SPAN:LEN . cr\n   128 MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: s :}\n   s\" span\" s SPAN:COPY\n   s 0 4 SPAN:SUB SPAN:$ type cr\n   s 8 SPAN:SKIP 16 SPAN:TAKE SPAN:LEN . cr\n   s MEM:FREE-SPAN ;\n;package\n" HBT-POSITIVES CODEGEN:APPEND-STRING ;

\ THE OTHER HALF OF THE DOES> RULE. An EMPTY clause compiles no instruction, so
\ elaborate.f STAGE-DOES-ENTRY publishes the companion record and patches
\ nothing: the created word keeps its RET and emits no branch at all. Such a
\ definer must still build and run stripped - the closure has nothing extra to
\ follow and the unreached companion record is shaken out with every other name.
\
\ A definer whose `does>` clause is EMPTY, in a stripped image: the created word
\ keeps the RET `create` emitted (habu2.f DOES-REC:ELIDE-EMPTY), so it reads its
\ cell with no branch out of its own body, and the image prints what was stored.
: HBT-AOT-DOES-PKG$ ( -- ptr u8 n )
   S\" package HBT-DOES\n: LOADING ( -- ) tier@ 1 <> if -9070 throw then ;\nLOADING\n: SLOT: ( -- ) create 0 , does> ( -- ptr n ) ;\nSLOT: S1\npublic\n: RUN ( -- ) 42 S1 ! S1 @ dup . cr 42 <> if -9071 throw then ;\n;package\n" ;

\ An EXPORT alias in a stripped image (dot habu-compiler-pkg-re-688212c1): MAIN
\ calls one word through its defining package and through a package that
\ re-exports it, and the image prints the body's result once per name.
: HBT-AOT-ALIAS-PKG$ ( -- ptr u8 n )
   S\" package HBT-XA\npublic\n: W ( i64 -- i64 ) dup * ;\n;package\npackage HBT-XB\npublic\nEXPORT HBT-XA:W\n;package\n" ;

\ The programs that build and run cleanly, as one stripped image: each RUN
\ package holds itself to tier 1 and throws its own code on a wrong answer, and
\ MAIN runs them and then the alias calls in turn, so the image's whole output
\ is each one's in that order.
: HBT-POSITIVES-SRC$ ( -- ptr u8 n )
   HBT-POSITIVES CODEGEN:RESET
   HBT-AOT-DIVZ-PKG$ HBT-POSITIVES CODEGEN:APPEND-STRING
   HBT-SPAN-PKG
   HBT-AOT-DOES-PKG$ HBT-POSITIVES CODEGEN:APPEND-STRING
   HBT-AOT-MULTI-PKG$ HBT-POSITIVES CODEGEN:APPEND-STRING
   HBT-AOT-ALIAS-PKG$ HBT-POSITIVES CODEGEN:APPEND-STRING
   S\" : MAIN ( -- ) HBT-DIVZ:RUN HBT-SPAN:RUN HBT-DOES:RUN HBT-MULTI:RUN 5 HBT-XA:W . cr 5 HBT-XB:W . cr ;\n" HBT-POSITIVES CODEGEN:APPEND-STRING
   HBT-POSITIVES CODEGEN:CONTENTS ;

\ Run an image and hold it to its whole output.
: HBT-RUN-IMAGE-OUT ( ptr u8 n ptr u8 n -- ) {: img:ptr imgu:n want:ptr wantu:n :}
   img imgu >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn want wantu T$= ;

: BUILD-AOT-OBJECT-PRODUCER ( -- )
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   -1 HBB-REPORT-JSON !
   HBB-BUILD
   HBB-MAKER-RUN @ 0 <> TTRUE
   HB-BUILD:REPORT$ JR:T-TRUE JR:T-FALSE JR:T-TRUE CHECK-REPORT
   HBT-RUN-AOT
   HBT-REMOVE-AOT-OUT
   BF-TMP-RESET ;

: BUILD-AOT-NATIVE ( -- )
   HBT-AOT-SRC HBT-AOT-SRC2$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   -1 HBB-REPORT-JSON !
   HBB-BUILD
   HBB-MAKER-HIT @ 0<> TTRUE
   HBB-MAKER-RUN @ 0 <> TTRUE
   HB-BUILD:REPORT$ JR:T-TRUE JR:T-FALSE JR:T-TRUE CHECK-REPORT
   HBT-RUN-AOT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL
   BF-TMP-RESET ;

\ The span program's DATA blob starts after its GOT; the multi-page buffer makes
\ DATA longer than one page.
: BUILD-AOT-POSITIVES ( -- )
   HBT-AOT-SRC HBT-POSITIVES-SRC$ WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBT-HBB-BUILD-OUT
   HBT-AOT-OUT S\" -6400\n\n64\n\nspan\n16\n\n42\n\n2\n\n25\n\n25\n\n" HBT-RUN-IMAGE-OUT
   HB-TARGET-MACOS? if
      HBT-AOT-OUT IMAGE-SIZE:MEASURE
      IMAGE-SIZE:DATA-BLOB-RANGE drop
      MACHO-READ:DATA-OFF MACHO-READ:GOT-BYTES + T=
      MACHO-READ:DATA-BYTES $4000 > TTRUE
   then
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL ;

\ THE PRODUCT TIER IS THE CALLER'S. hb-build hands its own `tier@` to the maker,
\ whose application load compiles at it. test/tier0-product-subject.f records the
\ tier it loaded at and prints it last, so each image names the tier that
\ compiled it; the rest of its output - loops, branches, catch, quotations, a
\ does> definer, a deferred word, floats and string literals - is the same at
\ both tiers. Its literals put instruction-shaped bytes at every offset 4k+3,
\ which the stripped link refuses when they lie in the code band; its deferred
\ call is the tier 0 call shape the link must carry. x86-64 has tier 1 only.

\ Build HBT-AOT-SRC from a caller at TIER, then select the tier this row ran at.
: HBT-TIER-BUILD ( n -- )
   tier@ {: tier:n prior:n :}
   tier TIER:SELECT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBT-HBB-BUILD-OUT
   prior TIER:SELECT ;

: BUILD-AOT-TIERS ( -- )
   HB-TARGET-LINUX-X86-64? if exit then
   s" test/tier0-product-subject.f" SOURCE-ROOT:RESOLVE drop
   HBT-AOT-SRC COPY-FILE-STREAM
   HBT-REMOVE-AOT-OUT
   0 HBT-TIER-BUILD
   HBB-MAKER-RUN @ 0<> TTRUE
   0 HBT-SUBJECT-OUT HBT-AOT-OUT SB$ HBT-RUN-IMAGE-OUT
   1 HBT-TIER-BUILD
   HBB-MAKER-RUN @ 0<> TTRUE
   1 HBT-SUBJECT-OUT HBT-AOT-OUT SB$ HBT-RUN-IMAGE-OUT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ WRITE-ALL ;

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

\ FMT:.INT has two valid placements. When the donor engine bakes src/lib/fmt.f
\ below its seal (as the x86-64 engine does), every maker carries it and the
\ keyed image links the program and runs it; otherwise the linker loads it before
\ the window and the program is refused by name. Ask the keyed linker itself:
\ HABU_UNDER_TEST can select a donor other than the engine running this row.
: HBT-FMT-BAKED? ( -- bool )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   HBT-LINKER >LEN
   S\" : FMT-PLACEMENT ( -- ) s\q FMT:.INT\q XREF-FIND-INDEX SEAL-NDICT@ < if s\q baked\q else s\q pre-window\q then type cr ;\nFMT-PLACEMENT\n" >LEN
   HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-ARGV-ENV-STDIN-CAPTURE HBT-CAPTURE>N
   {: out:n err:n rc:n :}
   rc 0 T=
   HBT-RUN-ERR err HBT-EMPTY$ T$=
   HBT-RUN-OUT out S\" baked\n" T-STR= if true exit then
   HBT-RUN-OUT out S\" pre-window\n" T$=
   false ;

: KEYED-BAKED-FMT ( ptr u8 n -- ) {: src:ptr srcu:n :}
   HBT-SPAN-SRC src srcu WRITE-ALL
   HBT-SPAN-OUT HBT-REMOVE-FILE?
   HBT-SPAN-SRC HBT-SPAN-OUT HBT-HBB-PREPARE-AOT
   HBT-HBB-BUILD-OUT
   HBT-SPAN-OUT S\" 42\n" HBT-RUN-IMAGE-OUT
   HBT-SPAN-OUT HBT-REMOVE-FILE? ;

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

: KEYED-PRE-WINDOW ( -- )
   HBT-FMT-BAKED? if HBT-REQUIRED-SRC$ KEYED-BAKED-FMT exit then
   KEYED-PRE-WINDOW-REFUSED
   KEYED-PRE-WINDOW-JSON ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-AOT-MAIN ( -- )
   T-RESET
   1 TIER:SELECT          \ the product tier the LOADING programs hold
   LINKER-BUILDER-ENGINE:ENSURE LINKER-BUILDER-ENGINE:HOST$ HBT-KEYED!
   HBT-PREPARE
   BUILD-AOT-OBJECT-PRODUCER
   BUILD-AOT-NATIVE
   BUILD-AOT-POSITIVES
   BUILD-AOT-TIERS
   KEYED-PRE-WINDOW
   CLEANUP-RUN
   HBT-ROOT EXISTS? TFALSE
   T-REPORT
   s" hb-build-aot-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-AOT-MAIN
