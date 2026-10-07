\ build-fixpoint-source-test.f - checked fixture for tools/build-fixpoint.f:
\ the sources the refresh emits and certifies - the runtime capture kind of
\ the common emitter, the checker and TFAM prefix self-certification, and the
\ boot-prefix and phase-source certify gates. The build and the cases that read
\ its output are tools/build-fixpoint-test.f; this is a gate row of its own,
\ because one row running every build-fixpoint case took 293-338 s in the
\ gate's pool.
\ Run: bin/hb --load tools/build-fixpoint-source-test.f

require lib/string.f
require lib/fs.f
require lib/fs-mutate.f

\ The tool binds this ordinary FILE-SIZE replacement when it loads. For one
\ armed path, the sample precedes a 68 KiB append, so its owning reader must
\ grow beyond the first allocator granule and keep the bytes already read.
package BFT-SIZE
private
$11000 constant PAD-N
create PAD PAD-N allot
create PATH FS-PATH-CAP allot variable PATH-U
create TAIL 128 allot variable TAIL-U
variable ARMED

public
: ARM ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n tail:ptr tailu:n :}
   path PATH pathu BYTE-COPY pathu PATH-U !
   tail TAIL tailu BYTE-COPY tailu TAIL-U !
   PAD-N 0 ?do 10 PAD i + c! loop
   -1 ARMED ! ;

: BREAK-READ ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   path PATH pathu BYTE-COPY pathu PATH-U !
   -2 ARMED ! ;

: DISARM ( -- ) 0 ARMED ! ;

: SIZE ( ptr u8 n -- n )
   {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   ARMED @ path pathu PATH PATH-U @ STR= and if
      ARMED @ -2 = if
         0 ARMED !
         path pathu REMOVE-FILE
         path pathu MAKE-DIRS
         size exit
      then
      0 ARMED !
      path pathu PAD PAD-N APPEND-FILE
      path pathu TAIL TAIL-U @ APPEND-FILE
   then
   size ;
;package

undefine FILE-SIZE
: FILE-SIZE ( ptr u8 n -- n ) BFT-SIZE:SIZE ;

require tools/build-fixpoint-test-lib.f
require lib/fmt.f

\ The shared fixture's words are private words of the tool's package, so this
\ row reopens it the way tools/build-fixpoint-test-lib.f does.
package BUILD-FIXPOINT

: BFT-WHOLE$ ( -- ptr u8 n )
   s" whole-src" BF-A$ ;

: BFT-TEST-WHOLE-READ ( -- )
   BFT-ROOT BF-TMP!
   BFT-WHOLE$ S\" package BFT-WHOLE public\n: HEAD ( -- n ) 1 ;\n;package\n" WRITE-ALL
   BFT-WHOLE$ BF-READ-SOURCE
   BF-SOURCE-LEN @ BFT-WHOLE$ BFT-READ T=
   BF-SOURCE-BUF BF-SOURCE-LEN @ BFT-READ-BUF BF-SOURCE-LEN @ STR= TTRUE
   BFT-WHOLE$ S\" : TAIL ( -- n ) 2 ;\n" BFT-SIZE:ARM
   BFT-WHOLE$ BF-READ-SOURCE
   BF-SOURCE-LEN @ BFT-WHOLE$ BFT-READ T=
   BF-SOURCE-BUF BF-SOURCE-LEN @ BFT-READ-BUF BF-SOURCE-LEN @ STR= TTRUE
   BF-TMP-RESET ;

\ checker.f's string-word rows, then definitions with a real literal after them,
\ and one the certify leaves to the run: it calls what `evaluate` renders.
: BFT-CENSUS-ROWS ( ptr u8 n -- )
   {: p:ptr u:n :}
   p u S\" PRIM: s\\\"    PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;\n" WRITE-ALL
   p u S\" PRIM: c\\\"    PE-PTR-U8 PE-OUT PRIM;\nPRIM: .\\\"    PRIM;\n" APPEND-FILE
   p u S\" : HEAD ( -- n ) 1 ;\n: MIDDLE ( -- n ) 2 ;\n" APPEND-FILE
   p u S\" : TAIL ( -- ptr u8 n ) s\" quoted\" ;\n" APPEND-FILE
   p u S\" s\" : MADE ( -- n ) 3 ;\" evaluate\n: USE ( -- n ) MADE ;\n" APPEND-FILE ;

\ The census is what the certify scan judged: a string-word row's name is read
\ as the row reads it, so it hides none of the definitions after it, and the
\ body the scan leaves to the run is counted as deferred, not as certified.
: BFT-TEST-WHOLE-CENSUS ( -- )
   BFT-ROOT BF-TMP!
   BFT-WHOLE$ BFT-CENSUS-ROWS
   s" census" BFT-WHOLE$ BF-CERTIFY-RC 0 T=
   VERIFY:CENSUS {: certified:n deferred:n :}
   certified 3 T=
   deferred 1 T=
   BF-TMP-RESET ;

: BFT-TEST-WHOLE-ERRORS ( -- )
   BFT-ROOT BF-TMP!
   [: s" absent-src" BF-A$ BF-READ-SOURCE ;] E-FS-STAT TTHROWSQ
   BFT-WHOLE$ s" read must fail" WRITE-ALL
   BFT-WHOLE$ BFT-SIZE:BREAK-READ
   [: BFT-WHOLE$ BF-READ-SOURCE ;] E-FS-IO TTHROWSQ
   BFT-WHOLE$ REMOVE-DIR
   BFT-WHOLE$ s" reusable after read error" WRITE-ALL
   BFT-WHOLE$ BF-READ-SOURCE
   BF-SOURCE-BUF BF-SOURCE-LEN @ s" reusable after read error" STR= TTRUE
   BF-TMP-RESET ;

: BFT-TEST-WHOLE-CERTIFY ( -- )
   BFT-CERT S\" package BFT-GROW public\n: HEAD ( -- n ) 1 ;\n;package\n" WRITE-ALL
   BFT-CERT S\" : TAIL ( -- n ) 2 ;\n" BFT-SIZE:ARM
   s" whole-cert" BFT-CERT BF-CERTIFY-RC 0 T=
   BFT-CERT S\" : BAD ( -- n ) ;\n" BFT-SIZE:ARM
   s" whole-cert" BFT-CERT BF-CERTIFY-RC 70 T=
   BF-CERT-DIAG BF-CERT-DIAG-U @ s" 'BAD'" CONTAINS? TTRUE
   BFT-CERT S\" package BFT-GROW public\n: HEAD ( -- n ) 1 ;\n;package\n" WRITE-ALL
   BFT-CERT S\" : TAIL ( -- n ) 2 ;\n" BFT-SIZE:ARM
   s" whole-core" BFT-CERT [: ;] BF-CERTIFY-GENERATED-CORE
   BFT-CERT S\" : BAD ( -- n ) ;\n" BFT-SIZE:ARM
   [: s" whole-core" BFT-CERT [: ;] BF-CERTIFY-GENERATED-CORE ;]
      E-BUILD-CERTIFY TTHROWSQ
   BFT-SIZE:DISARM ;

\ Self-certification guard: checker.f must certify as the tail of its exact
\ pre-hook prefix. Its layout assertions consume cell.f's CORE-LAYOUT-RC and
\ PTR-VARIABLE has its own pre-checker owner. The generic structure DSL is
\ deliberately post-hook and must not enter this prefix. The source verifier
\ remains independently checked through the same VERIFY:SOURCE-BUF path.
: BFT-CERT-CHECKER$ ( -- ptr u8 n )
   s" cert-checker" BF-A$ ;

: BFT-CERT-CHECKER-BASE ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-RESET-OUT
   out outu s" src/core/util.f" BF-APPEND-SOURCE
   out outu s" src/core/cell.f" BF-APPEND-SOURCE
   out outu s" src/core/pointer-storage.f" BF-APPEND-SOURCE
   out outu s" src/core/engine-error.f" BF-APPEND-SOURCE
   out outu s" src/core/checker-fetch-abi.f" BF-APPEND-SOURCE
   out outu s" src/core/checker-owner-abi.f" BF-APPEND-SOURCE
   out outu s" src/habu/prims.f" BF-APPEND-SOURCE
   out outu s" src/core/does-clause.f" BF-APPEND-SOURCE
   out outu s" src/core/checker.f" BF-APPEND-SOURCE ;

: BFT-TEST-CERTIFY-CHECKER-SELF ( -- )
   BFT-ROOT BF-TMP!
   s" cert-checker" BFT-CERT-CHECKER-BASE
   s" checker-self" BFT-CERT-CHECKER$ BF-CERTIFY-RC 0 T=
   s" verify-source-self" s" src/habu/verify-source.f" BF-CERTIFY-RC 0 T=
   BF-TMP-RESET ;

\ Replaying this package sees its already-published CELLS query. The handoff
\ must use the byte offset's CELL multiplier, not that same-named query.
: BFT-TEST-CERTIFY-CALL-STORE ( -- )
   BFT-ROOT BF-TMP!
   s" cert-checker" BF-RESET-OUT
   s" cert-checker" BF-APPEND-CHECKER-BOOT
   s" call-store-warm" BFT-CERT-CHECKER$ BF-CERTIFY-RC 0 T=
   s" call-store-repeat" BFT-CERT-CHECKER$ BF-CERTIFY-RC 0 T=
   BF-TMP-RESET ;

: BFT-TEST-RUNTIME-KIND ( -- )
   BFT-ROOT BF-TMP!
   8 0 ?do
      s" runtime-kind-src" {: out:ptr outu:n :}
      out outu BF-RESET-OUT
      out outu BF-APPEND-RUN-PRELUDE
      out outu BF-APPEND-COMMON
      out outu COMPILER-BUILD:SEAL
      out outu s" test/aot-runtime-kind-driver.f" BF-APPEND-SOURCE
      SB-RESET i FMT:SB-U s"  AOT-KIND-TEST:RUN" SB-APPEND
      out outu SB$ BF-APPEND-LINE
      s" bin/hb" out outu BF-A$ COMPILER-BUILD:RUN
      i 3 < if 0 else 74 then T=
   loop
   BF-TMP-RESET ;

\ Per-file TFAM-prefix certification: type-schema.f, type-family.f, render.f,
\ and sumtype.f certify clean via the same VERIFY:SOURCE-BUF path, each
\ verified as the tail of its exact BF-APPEND-CHECKER-BOOT prefix context
\ (util, cell, pointer storage, engine error, checker, then the earlier TFAM
\ files), so de-typing any one file fails its own assert. render.f sits between
\ type-family.f and sumtype.f in the real prefix and certifies since its cleanup
\ (habu-make-fixpoint-certify-a11dbad5).
: BFT-CERT-TFAM$ ( -- ptr u8 n )
   s" cert-tfam" BF-A$ ;

: BFT-CERT-TFAM-BASE ( -- )
   s" cert-tfam" {: out:ptr outu:n :}
   out outu BFT-CERT-CHECKER-BASE
   out outu s" src/core/engine-error-effects.f" BF-APPEND-SOURCE
   out outu s" src/core/lower-cert-base.f" BF-APPEND-SOURCE ;

: BFT-TEST-CERTIFY-TFAM-PREFIX ( -- )
   BFT-ROOT BF-TMP!
   BFT-CERT-TFAM-BASE
   s" cert-tfam" s" src/core/type-schema.f" BF-APPEND-SOURCE
   s" tfam-type-schema" BFT-CERT-TFAM$ BF-CERTIFY-RC 0 T=
   s" cert-tfam" s" src/core/type-family.f" BF-APPEND-SOURCE
   s" tfam-type-family" BFT-CERT-TFAM$ BF-CERTIFY-RC 0 T=
   s" cert-tfam" s" src/core/render.f" BF-APPEND-SOURCE
   s" tfam-render" BFT-CERT-TFAM$ BF-CERTIFY-RC 0 T=
   s" cert-tfam" s" src/core/sumtype.f" BF-APPEND-SOURCE
   s" tfam-sumtype" BFT-CERT-TFAM$ BF-CERTIFY-RC 0 T=
   BF-TMP-RESET ;

\ The stage2 and stdin build phases both emit into the one fixed `stage2-src`
\ stage-input path. The first generation uses COMPILER-BUILD's verified
\ `--build` route; later hb-stage generations read that same path. Therefore
\ BF-CERTIFY-STAGE2 and
\ BF-CERTIFY-STDIN read the same path at different times. Prove the certify path
\ exists in each phase and that the stdin phase OVERWRITES it with distinct
\ content — so BF-CERTIFY-STDIN certifies the stdin driver source, not stage2 twice.
\ The boot prefix is a certify phase of its own, and it is BLOCKING. The
\ subject is the real assembly through the real phase word, not a hand-built
\ stand-in: BF-PREFIX-SOURCE writes the bytes the build writes and
\ BF-CERTIFY-PREFIX is the word the build calls. The good case proves those
\ exact bytes certify; the bad case is the SAME bytes with one type-broken
\ definition appended, which must throw rather than warn - the fail-open
\ variant would pass the good case identically.
\ The two membership assertions are structural, not decorative: they name a
\ definition from the first checker-boot file and one from deep inside
\ checker.f, so an assembly that silently emitted nothing, or stopped after its
\ first file, still certifies clean and would pass without them.
: BFT-TEST-CERTIFY-BOOT-PREFIX ( -- )
   BFT-ROOT BF-TMP!
   BF-PREFIX-SOURCE
   BFT-PREFIX FILE? TTRUE
   BF-CERTIFY-PREFIX
   BFT-PREFIX BFT-READ {: u:n :}
   BFT-READ-BUF u s" : CORE-STR=" CONTAINS? TTRUE
   BFT-READ-BUF u s" : SUFFIX$ (" CONTAINS? TTRUE
   BFT-READ-BUF u s" : CHECK! (" CONTAINS? TTRUE
   s" prefix-src" BF-A$ s" : BFT-PFX-BAD ( n -- n ) drop ;" APPEND-FILE
   [: BF-CERTIFY-PREFIX ;] E-BUILD-CERTIFY TTHROWSQ
   BF-TMP-RESET ;

: BFT-TEST-CERTIFY-PHASE-SOURCES ( -- )
   BFT-ROOT BF-TMP!
   BF-STAGE2-SOURCE
   BFT-STAGE2 FILE? TTRUE
   BF-CERTIFY-STAGE2
   BF-RECORD-STAGE
   BF-STDIN-SOURCE
   BFT-STAGE2 FILE? TTRUE
   BF-CERTIFY-STDIN
   BF-RECORD-STDIN
   BF-REC-STAGE-DG BF-STAMP-DG-U BF-REC-STDIN-DG BF-STAMP-DG-U STR= TFALSE
   BF-TMP-RESET ;

\ Public so the driver below can run it with the package CLOSED: the subtests
\ certify generated engine sources in this process (VERIFY:SOURCE-BUF), and the
\ checker resolves the verified source's names in whatever package scope is open
\ when it runs.
public
: BFT-SOURCE-RUN ( -- )
   T-RESET
   BFT-PREPARE
   s" whole source read" [: BFT-TEST-WHOLE-READ ;] BFT-STEP
   s" whole source census" [: BFT-TEST-WHOLE-CENSUS ;] BFT-STEP
   s" whole source errors" [: BFT-TEST-WHOLE-ERRORS ;] BFT-STEP
   s" whole source certify" [: BFT-TEST-WHOLE-CERTIFY ;] BFT-STEP
   HB-TARGET-LINUX-X86-64? 0= if
      s" runtime capture kind" [: BFT-TEST-RUNTIME-KIND ;] BFT-STEP
   then
   s" certify checker self" [: BFT-TEST-CERTIFY-CHECKER-SELF ;] BFT-STEP
   s" certify call store" [: BFT-TEST-CERTIFY-CALL-STORE ;] BFT-STEP
   s" certify tfam prefix" [: BFT-TEST-CERTIFY-TFAM-PREFIX ;] BFT-STEP
   s" certify boot prefix" [: BFT-TEST-CERTIFY-BOOT-PREFIX ;] BFT-STEP
   HB-TARGET-LINUX-X86-64? 0= if
      s" certify phase sources" [: BFT-TEST-CERTIFY-PHASE-SOURCES ;] BFT-STEP
   then
   s" build-fixpoint-source-test: ok" BFT-FINISH ;

;package

BUILD-FIXPOINT:BFT-SOURCE-RUN
