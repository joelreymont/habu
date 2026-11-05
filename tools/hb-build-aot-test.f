\ hb-build-aot-test.f - checked fixture for tools/hb-build-lib.f's AOT groups:
\ native call sites; one stripped program over the division refusal, the span
\ definer with foreign calls, the empty does> clause, a multi-page DATA payload,
\ frame slots, signed remainders and long literals; the product subject built
\ at each tier. Other rows: tools/hb-build-test-lib.f.
\ Run: bin/hb --load tools/hb-build-aot-test.f

require tools/hb-build-test-lib.f
require tools/image-size-lib.f          \ this fixture measures image bytes directly
require test/aot-builder-engine.f
require src/lib/string.f
require src/lib/codegen.f

using BUILD-FIXPOINT                     \ the build tmp root

\ The shared fixture's words are private words of the library's package, so
\ this row reopens it the way tools/hb-build-test-lib.f does.
package HB-BUILD-CLI

\ A zero divisor in a stripped image catches ARITH-ABI:E-DIV-ZERO: the tier-1
\ refusal branches to the engine's `throw`, outside the payload window, which the
\ closure walker must follow and the build relocate, as for the terminator's
\ `die`. Only running the image exercises that: a branch left at the building
\ engine's text returns no code, and a dropped refusal answers zero and throws
\ -9051. The code is spelled out because a built source cannot see this file's
\ constants.
: HBT-AOT-DIVZ-PKG$ ( -- ptr u8 n )
   S\" package HBT-DIVZ\n: LOADING ( -- ) tier@ 1 <> if -9050 throw then ;\nLOADING\nvariable A\nvariable B\nvariable R\n: DZ ( n n -- n ) / ;\n: TRY ( -- n ) [: A @ B @ DZ R ! ;] catch ;\npublic\n: RUN ( -- ) 7 A ! 0 B ! TRY dup . cr -6400 <> if -9051 throw then 7 A ! 2 B ! TRY 0 <> if -9052 throw then R @ 3 <> if -9053 throw then ;\n;package\n" ;

\ A nonzero 20 KiB source span has an encoded sparse payload longer than one
\ Mach-O DATA_CONST page. Its last byte must survive the extra mapped page.
: HBT-AOT-MULTI-PKG$ ( -- ptr u8 n )
   S\" package HBT-MULTI\ncreate MULTI-BUF 20000 allot\n: INIT ( -- ) 20000 0 ?do 1 MULTI-BUF i + c! loop ;\nINIT\npublic\n: RUN ( -- ) MULTI-BUF c@ MULTI-BUF 19999 + c@ + . cr ;\n;package\n" ;

32768 CODEGEN:BUFFER HBT-POSITIVES

\ A `create ... does>` definer whose clause yields src/lib/span.f's two-cell
\ span, built stripped at tier 1 (LOADING asserts it) and run: allocate, copy,
\ skip, take, sub and release go through it, and getpid and gethostname resolve
\ through FUNCTION: at the first call. The clause has a body, so the created word
\ ends in the B that habu2.f DOESPATCH:EMIT wrote over its RET, aimed at the
\ parent's `;does` record; only aot-closure.f SCAN-DIRECT following that branch
\ out of the member puts the record in the closure.
: HBT-SPAN-PKG ( -- )
   S\" require src/lib/span.f\nrequire src/lib/memory.f\nrequire src/lib/ffi-abi.f\npackage HBT-SPAN\n: LOADING ( -- ) tier@ 1 <> if -9060 throw then ;\nLOADING\nPROCESS-SYMBOLS\nFUNCTION: PID getpid ( -- i32 ) ;FUNCTION\n" HBT-POSITIVES CODEGEN:APPEND-STRING
   HB-TARGET-MACOS? if
      S\" LIBRARY /usr/lib/libSystem.B.dylib\n" HBT-POSITIVES CODEGEN:APPEND-STRING
   else
      S\" LIBRARY libc.so.6\n" HBT-POSITIVES CODEGEN:APPEND-STRING
   then
   S\" FUNCTION: HOST gethostname ( ptr u8 n -- i32 )\n   0 1 WRITES-ARG ;FUNCTION\ncreate HOST-BUF 256 allot\n64 SPAN-BUFFER: SB\npublic\n: RUN ( -- )\n   PID 0 > 0= if -9061 throw then\n   HOST-BUF BYTE-VIEW 256 HOST 0 <> if -9062 throw then\n   SB SPAN:LEN . cr\n   128 MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: s :}\n   s\" span\" s SPAN:COPY\n   s 0 4 SPAN:SUB SPAN:$ type cr\n   s 8 SPAN:SKIP 16 SPAN:TAKE SPAN:LEN . cr\n   s MEM:FREE-SPAN ;\n;package\n" HBT-POSITIVES CODEGEN:APPEND-STRING ;

\ An empty `does>` clause compiles nothing: elaborate.f STAGE-DOES-ENTRY
\ publishes the companion record and patches nothing, so the created word keeps
\ the RET `create` emitted (habu2.f DOES-REC:ELIDE-EMPTY), reads its cell with no
\ branch, and still builds and runs stripped; the unreached record is shaken out.
: HBT-AOT-DOES-PKG$ ( -- ptr u8 n )
   S\" package HBT-DOES\n: LOADING ( -- ) tier@ 1 <> if -9070 throw then ;\nLOADING\n: SLOT: ( -- ) create 0 , does> ( -- ptr n ) ;\nSLOT: S1\npublic\n: RUN ( -- ) 42 S1 ! S1 @ dup . cr 42 <> if -9071 throw then ;\n;package\n" ;

\ An EXPORT alias in a stripped image: MAIN calls one word through its defining
\ package and through a re-exporting one, and the image prints the result once
\ per name.
: HBT-AOT-ALIAS-PKG$ ( -- ptr u8 n )
   S\" package HBT-XA\npublic\n: W ( i64 -- i64 ) dup * ;\n;package\npackage HBT-XB\npublic\nEXPORT HBT-XA:W\n;package\n" ;

\ A string literal of SIZE bytes cycling a..z, spelled as source.
: HBT-LONG-LITERAL ( n -- ) {: size:n :}
   S\" s\q " HBT-POSITIVES CODEGEN:APPEND-STRING
   size 0 ?do i 26 mod 97 + HBT-POSITIVES CODEGEN:APPEND-BYTE loop
   34 HBT-POSITIVES CODEGEN:APPEND-BYTE ;

: HBT-LONG-DEF ( ptr u8 n n -- ) {: name:ptr nameu:n size:n :}
   s" : " HBT-POSITIVES CODEGEN:APPEND-STRING
   name nameu HBT-POSITIVES CODEGEN:APPEND-STRING
   s"  ( -- ptr u8 n ) " HBT-POSITIVES CODEGEN:APPEND-STRING
   size HBT-LONG-LITERAL
   S\"  ;\n" HBT-POSITIVES CODEGEN:APPEND-STRING ;

\ String literals of 4096, 4097 and 4135 bytes, beside an empty and a short one;
\ RUN checks each length and byte.
: HBT-LONG-PKG ( -- )
   S\" package HBT-LONG\nprivate\n: CHECK ( ptr u8 n n -- ) {: p:ptr u:n want:n :}\n   u want <> if 71 throw then\n   u 0 ?do p i + c@ i 26 mod 97 + <> if 72 throw then loop ;\n" HBT-POSITIVES CODEGEN:APPEND-STRING
   s" LONG-4096" 4096 HBT-LONG-DEF
   s" LONG-4097" 4097 HBT-LONG-DEF
   s" LONG-4135" 4135 HBT-LONG-DEF
   S\" : EMPTY ( -- ptr u8 n ) s\q \q ;\n" HBT-POSITIVES CODEGEN:APPEND-STRING
   s" SMALL-LITERAL" 3 HBT-LONG-DEF
   S\" public\n: RUN ( -- ) LONG-4096 4096 CHECK LONG-4097 4097 CHECK LONG-4135 4135 CHECK\n   EMPTY nip 0 <> if 73 throw then\n   SMALL-LITERAL 3 CHECK s\q long: ok\q type cr ;\n;package\n" HBT-POSITIVES CODEGEN:APPEND-STRING ;

\ The programs that build and run cleanly, as one stripped image built at tier 1.
\ MAIN runs each in turn and the image's whole output is compared byte for byte.
: HBT-POSITIVES-SRC$ ( -- ptr u8 n )
   HBT-POSITIVES CODEGEN:RESET
   HBT-AOT-DIVZ-PKG$ HBT-POSITIVES CODEGEN:APPEND-STRING
   HBT-SPAN-PKG
   HBT-AOT-DOES-PKG$ HBT-POSITIVES CODEGEN:APPEND-STRING
   HBT-AOT-MULTI-PKG$ HBT-POSITIVES CODEGEN:APPEND-STRING
   HBT-AOT-ALIAS-PKG$ HBT-POSITIVES CODEGEN:APPEND-STRING
   S\" require test/compiler/native-frame-slot-image.f\nrequire test/compiler/native-div-image.f\n" HBT-POSITIVES CODEGEN:APPEND-STRING
   HBT-LONG-PKG
   S\" : MAIN ( -- ) HBT-DIVZ:RUN HBT-SPAN:RUN HBT-DOES:RUN HBT-MULTI:RUN 5 HBT-XA:W . cr 5 HBT-XB:W . cr FRAME-SLOT-IMAGE:CASES NDIV-IMAGE:RUN HBT-LONG:RUN ;\n" HBT-POSITIVES CODEGEN:APPEND-STRING
   HBT-POSITIVES CODEGEN:CONTENTS ;

: HBT-RUN-IMAGE-OUT ( ptr u8 n ptr u8 n -- ) {: img:ptr imgu:n want:ptr wantu:n :}
   img imgu >LEN HBT-RUN-OUT HBT-CAPTURE-CAP >LEN HBT-RUN-ERR HBT-CAPTURE-CAP >LEN
   HBT-TIMEOUT-MS >MS RUN-CAPTURE HBT-CAPTURE>N {: outn:n errn:n rcn:n :}
   rcn 0 T=
   HBT-RUN-ERR errn HBT-EMPTY$ T$=
   HBT-RUN-OUT outn want wantu T$= ;

: BUILD-AOT-NATIVE ( -- )
   HBT-AOT-SRC HBT-AOT-SRC2$ FS:WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBB-BUILD
   HBB-MAKER-RUN @ 0 <> TTRUE
   HBT-RUN-AOT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ FS:WRITE-ALL
   BF-TMP-RESET ;

\ The span program's DATA blob starts after its GOT; the multi-page buffer makes
\ DATA longer than one page.
: BUILD-AOT-POSITIVES ( -- )
   HBT-AOT-SRC HBT-POSITIVES-SRC$ FS:WRITE-ALL
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBT-HBB-BUILD-OUT
   HBT-AOT-OUT S\" -6400\n\n64\n\nspan\n16\n\n42\n\n2\n\n25\n\n25\n\nPAIR\n20\n73\nMERGE\n8\n74\nCT\n12\nCT2\n-10\n10\nALT\n50\n50\n40\nCL\n14\n3\nJN\n27\n18\nNEST\n4\n4\n9\nEXIT-EARLY\n8\n3\n8\n2\nframe-slots: ok\n-1\n0\n0\n1\n0\n0\n1\n-1\n-7\n0\n-2\n-1\nlong: ok\n" HBT-RUN-IMAGE-OUT
   HB-TARGET-MACOS? if
      HBT-AOT-OUT IMAGE-SIZE:MEASURE
      IMAGE-SIZE:DATA-BLOB-RANGE drop
      MACHO-READ:DATA-OFF MACHO-READ:GOT-BYTES + T=
      MACHO-READ:DATA-BYTES $4000 > TTRUE
   then
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ FS:WRITE-ALL ;

\ THE PRODUCT TIER IS THE CALLER'S. hb-build hands its own `tier@` to the maker,
\ whose application load compiles at it. test/tier0-product-subject.f records the
\ tier it loaded at and prints it last, so each image names the tier that
\ compiled it; the rest of its output - loops, branches, catch, quotations, a
\ does> definer, a deferred word, floats and string literals - is the same at
\ both tiers. Its literals put instruction-shaped bytes at every offset 4k+3,
\ which the stripped link refuses when they lie in the code band; its deferred
\ call is the tier 0 call shape the link must carry.

\ Build HBT-AOT-SRC from a caller at TIER, then select the tier this row ran at.
: HBT-TIER-BUILD ( n -- )
   tier@ {: tier:n prior:n :}
   tier set-tier
   HBT-AOT-SRC HBT-AOT-OUT HBT-HBB-PREPARE-AOT
   HBT-HBB-BUILD-OUT
   prior set-tier ;

: BUILD-AOT-TIERS ( -- )
   s" test/tier0-product-subject.f" SOURCE-ROOT:RESOLVE drop
   HBT-AOT-SRC FS:COPY-FILE-STREAM
   HBT-REMOVE-AOT-OUT
   0 HBT-TIER-BUILD
   HBB-MAKER-RUN @ 0<> TTRUE
   0 HBT-SUBJECT-OUT HBT-AOT-OUT SB$ HBT-RUN-IMAGE-OUT
   1 HBT-TIER-BUILD
   HBB-MAKER-RUN @ 0<> TTRUE
   1 HBT-SUBJECT-OUT HBT-AOT-OUT SB$ HBT-RUN-IMAGE-OUT
   HBT-REMOVE-AOT-OUT
   HBT-AOT-SRC HBT-AOT-SRC$ FS:WRITE-ALL ;

\ Public so the driver below runs it with the package CLOSED: the subtests
\ drive real builds, which resolve names in whatever package scope is open.
public
: HBT-AOT-MAIN ( -- )
   T-RESET
   1 set-tier          \ the product tier the LOADING programs hold
   AOT-BUILDER-ENGINE:ENSURE ENGINE-CANDIDATE:PATH$ HBT-KEYED!
   HBT-PREPARE
   BUILD-AOT-NATIVE
   BUILD-AOT-POSITIVES
   BUILD-AOT-TIERS
   FS:CLEANUP-RUN
   HBT-ROOT FS:EXISTS? TFALSE
   T-REPORT
   s" hb-build-aot-test: ok" type cr ;

;package

;using

HB-BUILD-CLI:HBT-AOT-MAIN
