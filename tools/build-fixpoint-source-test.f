\ build-fixpoint-source-test.f - tools/build-fixpoint.f: the sources the refresh
\ certifies (checker and TFAM prefix self-certification, the boot-prefix gate).
\ Run: bin/hb --load tools/build-fixpoint-source-test.f

require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f

\ The tool binds this ordinary FS:FILE-SIZE replacement when it loads. For one
\ armed path, the sample precedes a 68 KiB append, so its owning reader must
\ grow beyond the first allocator granule and keep the bytes already read.
package BFT-SIZE
private
$11000 constant PAD-N
create PAD PAD-N allot
create PATH PATH-CAP allot variable PATH-U
create TAIL 128 allot variable TAIL-U
variable ARMED

public
: ARM ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n tail:ptr tailu:n :}
   path PATH pathu BYTE-COPY pathu PATH-U !
   tail TAIL tailu BYTE-COPY tailu TAIL-U !
   PAD-N 0 ?do 10 PAD i + c! loop
   -1 ARMED ! ;

: SIZE ( ptr u8 n -- n )
   {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   ARMED @ path pathu PATH PATH-U @ STR= and if
      0 ARMED !
      path pathu PAD PAD-N FS:APPEND-FILE
      path pathu TAIL TAIL-U @ FS:APPEND-FILE
   then
   size ;
;package

package FS
public
undefine FILE-SIZE
: FILE-SIZE ( ptr u8 n -- n ) BFT-SIZE:SIZE ;
;package

require tools/build-fixpoint-test-lib.f

\ The shared fixture's words are private words of the tool's package, so this
\ row reopens it the way tools/build-fixpoint-test-lib.f does.
package BUILD-FIXPOINT

: BFT-WHOLE$ ( -- ptr u8 n )
   s" whole-src" BF-A$ ;

\ checker.f's string-word rows, then definitions with a real literal after them,
\ and one the certify leaves to the run: it calls what `evaluate` renders.
: BFT-CENSUS-ROWS ( ptr u8 n -- )
   {: p:ptr u:n :}
   p u S\" PRIM: s\\\"    PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;\n" FS:WRITE-ALL
   p u S\" PRIM: c\\\"    PE-PTR-U8 PE-OUT PRIM;\nPRIM: .\\\"    PRIM;\n" FS:APPEND-FILE
   p u S\" : HEAD ( -- n ) 1 ;\n: MIDDLE ( -- n ) 2 ;\n" FS:APPEND-FILE
   p u S\" : TAIL ( -- ptr u8 n ) s\" quoted\" ;\n" FS:APPEND-FILE
   p u S\" s\" : MADE ( -- n ) 3 ;\" evaluate\n: USE ( -- n ) MADE ;\n" FS:APPEND-FILE ;

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

: BFT-TEST-WHOLE-CERTIFY ( -- )
   BFT-CERT S\" package BFT-GROW public\n: HEAD ( -- n ) 1 ;\n;package\n" FS:WRITE-ALL
   BFT-CERT S\" : TAIL ( -- n ) 2 ;\n" BFT-SIZE:ARM
   s" whole-cert" BFT-CERT BF-CERTIFY-RC 0 T=
   BFT-CERT S\" : BAD ( -- n ) ;\n" BFT-SIZE:ARM
   s" whole-cert" BFT-CERT BF-CERTIFY-RC 70 T=
   BF-CERT-DIAG BF-CERT-DIAG-U @ s" 'BAD'" CONTAINS? TTRUE
   BFT-CERT S\" package BFT-GROW public\n: HEAD ( -- n ) 1 ;\n;package\n" FS:WRITE-ALL
   BFT-CERT S\" : TAIL ( -- n ) 2 ;\n" FS:APPEND-FILE
   s" whole-core" BFT-CERT s" " BF-CERTIFY-GENERATED-CORE
   BFT-CERT S\" : BAD ( -- n ) ;\n" FS:APPEND-FILE
   [: s" whole-core" BFT-CERT s" " BF-CERTIFY-GENERATED-CORE ;]
      E-BUILD-CERTIFY TTHROWSQ ;

\ Self-certification guard: checker.f must certify as the tail of its exact
\ pre-hook prefix. Its target-width check uses cell.f's CORE-LAYOUT-RC, and
\ PTR-VARIABLE has its own pre-checker owner. The generic structure DSL is
\ deliberately post-hook and must not enter this prefix. The source verifier
\ remains independently checked through the same VERIFY:SOURCE-BUF path.
: BFT-CERT-CHECKER$ ( -- ptr u8 n )
   s" cert-checker" BF-A$ ;

: BFT-CERT-CHECKER-BASE ( ptr u8 n -- )
   {: out:ptr outu:n :}
   out outu BF-RESET-OUT
   out outu s" src/core/util.f" BF-APPEND-SOURCE
   out outu s" src/core/cell.f" BF-APPEND-SOURCE
   out outu s" src/core/pointer-storage.f" BF-APPEND-SOURCE
   out outu s" src/core/engine-error.f" BF-APPEND-SOURCE
   out outu s" src/core/checker-fetch-abi.f" BF-APPEND-SOURCE
   out outu s" src/core/checker-owner-extension.f" BF-APPEND-SOURCE
   out outu s" src/core/checker-owner-abi.f" BF-APPEND-SOURCE
   out outu s" src/habu/boot-cells.f" BF-APPEND-SOURCE
   out outu s" src/habu/wl.f" BF-APPEND-SOURCE
   out outu s" src/habu/created-body-abi.f" BF-APPEND-SOURCE
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

\ Per-file TFAM-prefix certification: type-schema.f, type-family.f, render.f and
\ sumtype.f each certify via VERIFY:SOURCE-BUF as the tail of their exact
\ BF-APPEND-CHECKER-BOOT prefix (util, cell, pointer storage, engine error,
\ checker, then the earlier TFAM files), so de-typing one fails its own assert.
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

\ The boot prefix is a blocking certify phase. BF-PREFIX-SOURCE writes the bytes
\ the build writes and BF-CERTIFY-PREFIX is the word the build calls. The bad
\ case is the same bytes plus one type-broken definition, which must throw, not
\ warn: a fail-open variant passes the good case identically. The membership
\ assertions name a definition from the first checker-boot file and one deep in
\ checker.f: an assembly that emitted nothing, or stopped after its first file,
\ also certifies clean.
: BFT-TEST-CERTIFY-BOOT-PREFIX ( -- )
   BFT-ROOT BF-TMP!
   BF-PREFIX-SOURCE
   BFT-PREFIX FS:FILE? TTRUE
   BF-CERTIFY-PREFIX
   BFT-PREFIX BFT-READ {: u:n :}
   BFT-READ-BUF u s" : CORE-STR=" CONTAINS? TTRUE
   BFT-READ-BUF u s" : SUFFIX$ (" CONTAINS? TTRUE
   BFT-READ-BUF u s" : CHECK! (" CONTAINS? TTRUE
   s" prefix-src" BF-A$ s" : BFT-PFX-BAD ( n -- n ) drop ;" FS:APPEND-FILE
   [: BF-CERTIFY-PREFIX ;] E-BUILD-CERTIFY TTHROWSQ
   BF-TMP-RESET ;

\ Public so the driver below can run it with the package CLOSED: the subtests
\ certify generated engine sources in this process (VERIFY:SOURCE-BUF), and the
\ checker resolves the verified source's names in whatever package scope is open
\ when it runs.
public
: BFT-SOURCE-RUN ( -- )
   T-RESET
   BFT-PREPARE
   s" whole source census" [: BFT-TEST-WHOLE-CENSUS ;] BFT-STEP
   s" whole source certify" [: BFT-TEST-WHOLE-CERTIFY ;] BFT-STEP
   s" certify checker self" [: BFT-TEST-CERTIFY-CHECKER-SELF ;] BFT-STEP
   s" certify call store" [: BFT-TEST-CERTIFY-CALL-STORE ;] BFT-STEP
   s" certify tfam prefix" [: BFT-TEST-CERTIFY-TFAM-PREFIX ;] BFT-STEP
   s" certify boot prefix" [: BFT-TEST-CERTIFY-BOOT-PREFIX ;] BFT-STEP
   s" build-fixpoint-source-test: ok" BFT-FINISH ;

;package

BUILD-FIXPOINT:BFT-SOURCE-RUN
