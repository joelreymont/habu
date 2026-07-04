\ public-signatures-test.f - checked fixtures for tools/public-signatures.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f tools/lint/text.f
\ tools/lint/intern.f tools/lint/token.f tools/lint/lib.f
\ tools/public-signatures-core.f tools/public-signatures-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/public-signatures-core.f

8192 constant PST-BUF-CAP

variable PST-ROOT-U
variable PST-FIX-U
variable PST-RUN-A
variable PST-RUN-U

create PST-ROOT-BUF FS-PATH-CAP allot
create PST-FIX-BUF FS-PATH-CAP allot
create PST-DEP-BUF FS-PATH-CAP allot
create PST-ENTRY-BUF FS-PATH-CAP allot
create PST-GDEP-BUF FS-PATH-CAP allot
create PST-GENTRY-BUF FS-PATH-CAP allot
create PST-CONST-BUF FS-PATH-CAP allot
create PST-NOPEN-BUF FS-PATH-CAP allot
create PST-NMID-BUF FS-PATH-CAP allot
create PST-NENTRY-BUF FS-PATH-CAP allot
create PST-SUM-BUF FS-PATH-CAP allot
create PST-OUT PST-BUF-CAP allot
create PST-ERR PST-BUF-CAP allot
variable PST-DEP-U
variable PST-ENTRY-U
variable PST-GDEP-U
variable PST-GENTRY-U
variable PST-CONST-U
variable PST-NOPEN-U
variable PST-NMID-U
variable PST-NENTRY-U
variable PST-SUM-U

: PST-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: PST-ROOT ( -- ptr u8 n )
   PST-ROOT-BUF PST-ROOT-U @ ;

: PST-FIX ( -- ptr u8 n )
   PST-FIX-BUF PST-FIX-U @ ;

: PST-RUN-A-FIELD ( -- ptr ptr u8 )
   PST-RUN-A 0 ptr-field ;

: PST-RUN-A@ ( -- ptr u8 )
   PST-RUN-A-FIELD @ ;

: PST-RUN-A! ( ptr u8 -- )
   PST-RUN-A-FIELD ! ;

: PST-RUN! ( ptr u8 n -- ) {: a:ptr u:n :}
   a PST-RUN-A!
   u PST-RUN-U ! ;

: PST-RUN$ ( -- ptr u8 n )
   PST-RUN-A@ PST-RUN-U @ ;

: PST-LF ( -- )
   10 SB-APPEND-C ;

: PST-DQ ( -- )
   34 SB-APPEND-C ;

: PST-FIXTURE$ ( -- ptr u8 n )
   SB-RESET
   92 SB-APPEND-C s"  public signature fixture" SB-APPEND PST-LF
   s" EXPORT lower" SB-APPEND PST-LF
   s" EXPORT 1+" SB-APPEND PST-LF
   s" : lower (   x -- x   ) dup ;" SB-APPEND PST-LF
   s" : CAPS ( i64 [ i64 -- i64 ] -- i64 ) execute ;" SB-APPEND PST-LF
   s" : Mixed ( i64 -- i64 ) dup ;" SB-APPEND PST-LF
   s" : 1+ ( i64 -- i64 ) 1 + ;" SB-APPEND PST-LF
   s" 99 constant KFIX" SB-APPEND PST-LF
   s" : BAD ( i64 ) dup ;" SB-APPEND PST-LF
   115 SB-APPEND-C PST-DQ s"  : STRINGED ( i64 -- i64 ) dup ;" SB-APPEND PST-DQ PST-LF
   s" ( : COMMENTED ( i64 -- i64 ) dup ; )" SB-APPEND PST-LF
   SB$ ;

: PST-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: PST-JPAIR$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: va:ptr vu ka:ptr ku :}
   SB-RESET
   PST-DQ ka ku SB-APPEND PST-DQ
   58 SB-APPEND-C
   PST-DQ va vu SB-APPEND PST-DQ
   SB$ ;

: PST-WORD$ ( ptr u8 n -- ptr u8 n )
   s" word" PST-JPAIR$ ;

: PST-SIG$ ( ptr u8 n -- ptr u8 n )
   s" signature" PST-JPAIR$ ;

: PST-SCHEMA$ ( -- ptr u8 n )
   SB-RESET
   PST-DQ s" schema_version" SB-APPEND PST-DQ
   s" :1" SB-APPEND
   SB$ ;

: PST-EXPORTED-TRUE$ ( -- ptr u8 n )
   SB-RESET
   PST-DQ s" exported" SB-APPEND PST-DQ
   s" :true" SB-APPEND
   SB$ ;

: PST-EXPORTED-FALSE$ ( -- ptr u8 n )
   SB-RESET
   PST-DQ s" exported" SB-APPEND PST-DQ
   s" :false" SB-APPEND
   SB$ ;

: PST-USAGE$ ( -- ptr u8 n )
   s" usage: tools/public-signatures.f [--trust] file ..." ;

: PST-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-public-signatures" TMPDIR-MKDIR {: a:ptr u :}
   a u PST-ROOT-BUF PST-ROOT-U PST-COPY!
   PST-ROOT CLEANUP-DIR+
   PST-ROOT s" public-signatures-fixture.f" PST-FIX-BUF JOIN-PATH PST-FIX-U !
   PST-FIX CLEANUP+
   PST-FIX PST-FIXTURE$ WRITE-ALL ;

: PST-CORE-SETUP ( n -- ) {: flag:n :}
   PST-OUT PST-BUF-CAP PS-OUT-BUFFER!
   PST-ERR PST-BUF-CAP PS-ERR-BUFFER!
   flag PS-TRUST ! ;

: PST-CORE-FINISH ( n -- n n n n ) {: rc:n :}
   PS-OUT$ nip PS-ERR$ nip PROC-OUTCOME-EXIT rc
   PS-BUFFERS-OFF ;

: PST-RUN-FILE-ACT ( -- )
   PS-JSON-DOC-START
   PST-RUN$ PS-SCAN-FILE
   PS-JSON-DOC-END ;

: PST-RUN-CORE ( ptr u8 n n -- n n n n ) {: flag:n :}
   PST-RUN!
   flag PST-CORE-SETUP
   [: PST-RUN-FILE-ACT ;] catch PST-CORE-FINISH ;

: PST-RUN ( ptr u8 n -- n n n n )
   0 PST-RUN-CORE ;

: PST-RUN-TRUST ( ptr u8 n -- n n n n )
   -1 PST-RUN-CORE ;

: PST-RUN-NOARG-ACT ( -- )
   PS-USAGE ;

: PST-RUN-NOARG ( -- n n n n )
   0 PST-CORE-SETUP
   [: PST-RUN-NOARG-ACT ;] catch PST-CORE-FINISH ;

: PST-EXPECT-EXIT ( n n n n n -- n n ) {: outu erru kind code expect :}
   kind PROC-OUTCOME-EXIT T=
   code expect T=
   outu erru ;

: PST-TEST-GOOD ( -- )
   s" examples/llm/good.f" PST-RUN 0 PST-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   PST-OUT outu PST-SCHEMA$ CONTAINS? TTRUE
   PST-OUT outu s" SQUARE" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu s" (i64 -- i64)" PST-SIG$ CONTAINS? TTRUE
   PST-OUT outu s" APPLY" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu s" (i64 [ i64 -- i64 ] -- i64)" PST-SIG$ CONTAINS? TTRUE ;

: PST-TEST-FIXTURE ( -- )
   PST-FIX PST-RUN 0 PST-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   PST-OUT outu s" LOWER" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu s" (x -- x)" PST-SIG$ CONTAINS? TTRUE
   PST-OUT outu PST-EXPORTED-TRUE$ CONTAINS? TTRUE
   PST-OUT outu s" CAPS" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu PST-EXPORTED-FALSE$ CONTAINS? TTRUE
   PST-OUT outu s" 1+" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu s" MIXED" PST-WORD$ CONTAINS? TFALSE
   PST-OUT outu s" BAD" PST-WORD$ CONTAINS? TFALSE ;

: PST-SQ ( ptr u8 n -- ) {: a:ptr u :}
   115 SB-APPEND-C
   PST-DQ
   STR-SPACE SB-APPEND-C
   a u SB-APPEND
   PST-DQ ;

: PST-TRUST$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: wa:ptr wu sa:ptr su :}
   SB-RESET
   wa wu PST-SQ
   STR-SPACE SB-APPEND-C
   sa su PST-SQ
   s"  TRUST" SB-APPEND
   SB$ ;

: PST-TRUST-LOWER$ ( -- ptr u8 n )
   s" LOWER" s" x -- x" PST-TRUST$ ;

: PST-TRUST-CAPS$ ( -- ptr u8 n )
   s" CAPS" s" i64 [ i64 -- i64 ] -- i64" PST-TRUST$ ;

: PST-TRUST-KFIX$ ( -- ptr u8 n )
   s" KFIX" s" -- a" PST-TRUST$ ;

: PST-TEST-TRUST ( -- )
   PST-FIX PST-RUN-TRUST 0 PST-EXPECT-EXIT {: outu erru :}
   erru 0 T=
   PST-OUT outu PST-TRUST-LOWER$ CONTAINS? TTRUE
   PST-OUT outu PST-TRUST-CAPS$ CONTAINS? TTRUE
   PST-OUT outu PST-TRUST-KFIX$ CONTAINS? TTRUE
   PST-OUT outu s" MIXED" CONTAINS? TFALSE ;

: PST-TEST-NOARG ( -- )
   PST-RUN-NOARG 64 PST-EXPECT-EXIT {: outu erru :}
   PST-OUT outu PST-EMPTY$ T$=
   PST-ERR erru PST-USAGE$ CONTAINS? TTRUE ;

: PST-DEP ( -- ptr u8 n )    PST-DEP-BUF PST-DEP-U @ ;
: PST-ENTRY ( -- ptr u8 n )  PST-ENTRY-BUF PST-ENTRY-U @ ;

\ A dependency opens a package the entry file continues; the closure pre-scan
\ must load that package scope so the entry's public word is rendered qualified.
: PST-ENTRY-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" require " SB-APPEND
   PST-DEP SB-APPEND
   PST-LF
   s" public" SB-APPEND PST-LF
   s" : BAR ( -- ) ;" SB-APPEND PST-LF
   SB$ ;

: PST-PREPARE-CLOSURE ( -- )
   PST-ROOT s" ps-dep-pkg.f" PST-DEP-BUF JOIN-PATH PST-DEP-U !
   PST-ROOT s" ps-entry-pkg.f" PST-ENTRY-BUF JOIN-PATH PST-ENTRY-U !
   PST-DEP CLEANUP+
   PST-ENTRY CLEANUP+
   PST-DEP s\" package FOO\n" WRITE-ALL
   PST-ENTRY PST-ENTRY-SRC$ WRITE-ALL ;

: PST-TEST-CLOSURE-PKG ( -- )
   PST-PREPARE-CLOSURE
   PST-ENTRY PST-RUN 0 PST-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   PST-OUT outu s" FOO:BAR" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu PST-EXPORTED-TRUE$ CONTAINS? TTRUE ;

: PST-GDEP ( -- ptr u8 n )    PST-GDEP-BUF PST-GDEP-U @ ;
: PST-GENTRY ( -- ptr u8 n )  PST-GENTRY-BUF PST-GENTRY-U @ ;

\ The dependency is loaded only through a colon-wrapped `s" ..." required`
\ helper; the whole-file closure producer must still hand its package scope to
\ the pre-scan, so the entry's public word renders qualified.
: PST-GENTRY-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : PST-DEP-LOAD ( -- ) " SB-APPEND
   $73 SB-APPEND-C PST-DQ $20 SB-APPEND-C
   PST-GDEP SB-APPEND
   PST-DQ $20 SB-APPEND-C
   s" required ;" SB-APPEND PST-LF
   s" PST-DEP-LOAD" SB-APPEND PST-LF
   s" public" SB-APPEND PST-LF
   s" : GBAR ( -- ) ;" SB-APPEND PST-LF
   SB$ ;

: PST-PREPARE-GUARDED ( -- )
   PST-ROOT s" ps-dep-guard.f" PST-GDEP-BUF JOIN-PATH PST-GDEP-U !
   PST-ROOT s" ps-entry-guard.f" PST-GENTRY-BUF JOIN-PATH PST-GENTRY-U !
   PST-GDEP CLEANUP+
   PST-GENTRY CLEANUP+
   PST-GDEP s\" package GFOO\n" WRITE-ALL
   PST-GENTRY PST-GENTRY-SRC$ WRITE-ALL ;

: PST-TEST-CLOSURE-GUARDED-PKG ( -- )
   PST-PREPARE-GUARDED
   PST-GENTRY PST-RUN 0 PST-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   PST-OUT outu s" GFOO:GBAR" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu PST-EXPORTED-TRUE$ CONTAINS? TTRUE ;

: PST-NOPEN ( -- ptr u8 n )   PST-NOPEN-BUF PST-NOPEN-U @ ;
: PST-NMID ( -- ptr u8 n )    PST-NMID-BUF PST-NMID-U @ ;
: PST-NENTRY ( -- ptr u8 n )  PST-NENTRY-BUF PST-NENTRY-U @ ;

\ A package opened+public in dep-open, continued unclosed through dep-mid where
\ it switches to `private`, closed in the entry file. Load order replays
\ dep-open then dep-mid, so the residual scope before the entry's own defs is
\ FOO/private: NESTED is a private word (not emitted) and only SHOWN (defined
\ after the entry re-enters `public`) renders as FOO:SHOWN. BFS closure order
\ replays dep-mid before dep-open, so its `private` is a no-op over the not-yet
\ -open package and the residual wrongly stays FOO/public, emitting FOO:NESTED.
: PST-NMID-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" require " SB-APPEND PST-NOPEN SB-APPEND PST-LF
   s" private" SB-APPEND PST-LF
   SB$ ;

: PST-NENTRY-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" require " SB-APPEND PST-NMID SB-APPEND PST-LF
   s" : NESTED ( -- ) ;" SB-APPEND PST-LF
   s" public" SB-APPEND PST-LF
   s" : SHOWN ( -- ) ;" SB-APPEND PST-LF
   s" end-package" SB-APPEND PST-LF
   SB$ ;

: PST-PREPARE-NESTED ( -- )
   PST-ROOT s" ps-nest-open.f" PST-NOPEN-BUF JOIN-PATH PST-NOPEN-U !
   PST-ROOT s" ps-nest-mid.f" PST-NMID-BUF JOIN-PATH PST-NMID-U !
   PST-ROOT s" ps-nest-entry.f" PST-NENTRY-BUF JOIN-PATH PST-NENTRY-U !
   PST-NOPEN CLEANUP+
   PST-NMID CLEANUP+
   PST-NENTRY CLEANUP+
   PST-NOPEN s\" package FOO\npublic\n" WRITE-ALL
   PST-NMID PST-NMID-SRC$ WRITE-ALL
   PST-NENTRY PST-NENTRY-SRC$ WRITE-ALL ;

: PST-TEST-CLOSURE-NESTED ( -- )
   PST-PREPARE-NESTED
   PST-NENTRY PST-RUN 0 PST-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   PST-OUT outu s" FOO:SHOWN" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu PST-EXPORTED-TRUE$ CONTAINS? TTRUE
   PST-OUT outu s" FOO:NESTED" PST-WORD$ CONTAINS? TFALSE ;

: PST-CONST ( -- ptr u8 n )  PST-CONST-BUF PST-CONST-U @ ;

\ TFAM 5 const-b89c90f0: `constant` bakes one physical cell, so a value of the
\ 2-field `cae-cv` layout family is narrowed to the one-cell `-- a` trust here —
\ identical to the verify-source / all-errors / native C-CONSTANT model. This
\ locks that public-signatures does NOT invent a multi-cell shape (nor drop the
\ constant); sound rejection / shape-carrying at the constant is owned by
\ TFAM 12 (habu-tfam-12-layout), which flips this expectation when it lands.
: PST-CONST-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" value-record cae-cv x i64 y i64 END-VALUE-RECORD" SB-APPEND PST-LF
   s" TRUSTED: CAE-CV-MK ( -- cae-cv ) 0 ;" SB-APPEND PST-LF
   s" CAE-CV-MK constant CAE-CV-K" SB-APPEND PST-LF
   s" : CAE-CV-USE ( -- cae-cv ) CAE-CV-K ;" SB-APPEND PST-LF
   SB$ ;

: PST-PREPARE-CONST ( -- )
   PST-ROOT s" ps-const-layout.f" PST-CONST-BUF JOIN-PATH PST-CONST-U !
   PST-CONST CLEANUP+
   PST-CONST PST-CONST-SRC$ WRITE-ALL ;

: PST-CONST-K-TRUST$ ( -- ptr u8 n )
   s" CAE-CV-K" s" -- a" PST-TRUST$ ;

: PST-CONST-USE-TRUST$ ( -- ptr u8 n )
   s" CAE-CV-USE" s" -- cae-cv" PST-TRUST$ ;

: PST-TEST-CONST-LAYOUT ( -- )
   PST-PREPARE-CONST
   PST-CONST PST-RUN-TRUST 0 PST-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   PST-OUT outu PST-CONST-K-TRUST$ CONTAINS? TTRUE
   PST-OUT outu PST-CONST-USE-TRUST$ CONTAINS? TTRUE ;

: PST-SUM ( -- ptr u8 n )    PST-SUM-BUF PST-SUM-U @ ;

\ TFAM 7 (habu-tfam-7-hidden): a sum layout family in a passing (identity)
\ signature renders as the LOGICAL type in public-signatures output — the family
\ tail with its args, never the hidden `@family.slotN`/`@family.tag` physical
\ fields (PLAN item 7 acceptance: public signatures without hidden fields). It is
\ also not narrowed to one bare cell nor to a multi-cell shape: one logical
\ T-PARAM cell in, one out. Item 12 (habu-tfam-12-layout) owns physical layout
\ exposure; this locks that item 7 keeps the surface logical-only.
: PST-SUM-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" SUMTYPE ps-res 2 VARIANT ok a ;VARIANT VARIANT err b ;VARIANT ;SUMTYPE" SB-APPEND PST-LF
   s" : PS-SUM-ID ( ps-res<n,n> -- ps-res<n,n> ) ;" SB-APPEND PST-LF
   SB$ ;

: PST-PREPARE-SUM ( -- )
   PST-ROOT s" ps-sum-layout.f" PST-SUM-BUF JOIN-PATH PST-SUM-U !
   PST-SUM CLEANUP+
   PST-SUM PST-SUM-SRC$ WRITE-ALL ;

: PST-TEST-SUM-LAYOUT ( -- )
   PST-PREPARE-SUM
   PST-SUM PST-RUN 0 PST-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   PST-OUT outu s" PS-SUM-ID" PST-WORD$ CONTAINS? TTRUE
   PST-OUT outu s" (ps-res<n,n> -- ps-res<n,n>)" PST-SIG$ CONTAINS? TTRUE
   PST-OUT outu s" @ps-res" CONTAINS? TFALSE       \ no hidden physical field leaks
   PST-OUT outu s" .slot0" CONTAINS? TFALSE
   PST-OUT outu s" .tag" CONTAINS? TFALSE ;

: PST-MAIN ( -- )
   T-RESET
   PST-PREPARE
   PST-TEST-GOOD
   PST-TEST-FIXTURE
   PST-TEST-TRUST
   PST-TEST-NOARG
   PST-TEST-CLOSURE-PKG
   PST-TEST-CLOSURE-GUARDED-PKG
   PST-TEST-CLOSURE-NESTED
   PST-TEST-CONST-LAYOUT
   PST-TEST-SUM-LAYOUT
   CLEANUP-RUN
   PST-ROOT EXISTS? TFALSE
   T-REPORT
   s" public-signatures-test: ok" type cr ;

PST-MAIN
