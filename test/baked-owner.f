\ baked-owner.f - no source a product engine loads reopens a package it bakes.
\
\ A sealed package refuses `package NAME` (src/habu/habu2.f
\ C-PACKAGE-PROT-GUARD, exit 84 with the package name on stderr), and the
\ native build seals every package a captured engine bakes
\ (src/core/internal-mark.f SEAL-PACKAGES). So every source a product engine
\ loads has to put its words in a package it owns: an error block in the file
\ that owns the package it mints into, MEM:ALLOC-SPAN in src/lib/memory.f, the
\ capture format and the meta-compiler's labels in packages of their own rather
\ than in the DATA-layout packages src/habu/layout.f declares.
\
\ Each case runs a child of the product engine, which loads one source through
\ the real load path, and asserts that the whole source loaded. Two load paths:
\   - `required`, through test/baked-owner-child.f, for the library and tool
\     entries that used to reopen one: src/lib/json-read.f (JR), src/lib/fmath.f
\     (FMATH), src/lib/pg.f (PG) and src/lib/db/rows.f (DB-ROWS) minted into packages
\     src/lib/errors.f created; the native build's driver
\     (tools/native-build-core.f) and the image walk (tools/image-size-lib.f)
\     load src/habu/aot-decl.f and src/habu/address-carrier.f, which reopened
\     AOT-WINDOW, AOT-SIG, AOT-SPAN and SNAP-RELOC; its kernel emitter
\     (tools/native-emit.f) loads the meta-compiler, src/habu/habu1.f, habu2.f
\     and code-origin.f, which reopened PROT, HIDX, SNAP-RELOC, TIER-PROV and
\     the AOT packages;
\   - `--build`, for the stage source the product-hosted refresh compiles
\     (tools/build-fixpoint.f): its run prelude rewinds to the core prefix and
\     its common body recompiles the meta-compiler on top of what is left. The
\     source here is the refresh's own, emitted by its appenders.
\ A baked library is never reloaded on a product engine, so src/lib/span.f has no
\ case. Each path has a control, a source that does reopen a baked package,
\ refused by the package's name, so a green case is not green because nothing
\ was sealed - the rewind in particular must leave the seal standing.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/codesign.f
require src/lib/engine-candidate.f
require tools/build-fixpoint.f

package BAKED-OWNER

private

$4000 constant CAP
120000 constant TIMEOUT-MS
84 constant SEAL-RC                     \ ENGINE-ERROR:SEAL-PACKAGE

create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
variable EXITED
create ROOT-BUF FS-PATH-CAP allot
variable ROOT-U

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;

\ The outcome of a child that loaded src: its exit code or signal, and whether
\ it exited. A deadline is neither: it prints src and the capture and throws
\ E-PROC-TIMEOUT (T-TIMED-OUT).
: STORE! ( ptr u8 n len len outcome -- )
   {: src:ptr srcu:n outu:len erru:len oc :}
   outu LEN>N OUT-U !
   erru LEN>N ERR-U !
   oc MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF src srcu OUT$ ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

\ The child loads src.
: RUN-CHILD ( ptr u8 n -- )
   ENGINE-CANDIDATE:PATH$ >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME STORE! ;

: LOAD ( ptr u8 n -- ) {: src:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" ARG+
   s" test/baked-owner-child.f" ARG+
   s" --" ARG+
   src u ARG+
   src u RUN-CHILD ;

: BUILD ( ptr u8 n -- ) {: src:ptr u:n :}
   PROC-ARGV-RESET
   s" --build" ARG+
   src u ARG+
   src u RUN-CHILD ;

: REPORT ( -- )
   OUT$ type cr ERR$ type cr ;

: REFUSED ( ptr u8 n ptr u8 n -- ) {: lab:ptr labu:n pkg:ptr pkgu:n :}
   EXITED @ RC @ SEAL-RC = and 0= if REPORT then
   lab labu T-LABEL EXITED @ TTRUE
   lab labu T-LABEL RC @ SEAL-RC T=
   lab labu T-LABEL ERR$ pkg pkgu CONTAINS? TTRUE ;

: LOADED ( ptr u8 n -- ) {: lab:ptr labu:n :}
   EXITED @ RC @ 0= and 0= if REPORT then
   lab labu T-LABEL EXITED @ TTRUE
   lab labu T-LABEL RC @ 0 T=
   lab labu T-LABEL OUT$ s" owner-ok" CONTAINS? TTRUE ;

: OWNS ( ptr u8 n -- ) {: src:ptr u:n :}
   src u LOAD
   src u LOADED ;

\ ---- the refresh's stage source ------------------------------------------------
: STAGE-HEAD ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BUILD-FIXPOINT:BF-RESET-OUT
   out outu BUILD-FIXPOINT:BF-APPEND-RUN-PRELUDE ;

\ The control: after the rewind, a reopen of layout.f's PROT is still refused.
: STAGE-CONTROL ( -- )
   s" stage-control.f" STAGE-HEAD
   s" stage-control.f" s" package PROT ;package" BUILD-FIXPOINT:BF-APPEND-LINE
   s" stage-control.f" BUILD-FIXPOINT:BF-A$ BUILD
   s" stage control: a reopen of PROT is refused" s" PROT" REFUSED ;

: STAGE-SOURCE ( -- )
   s" stage-src.f" STAGE-HEAD
   s" stage-src.f" BUILD-FIXPOINT:BF-APPEND-COMMON
   s" stage-src.f" S\" s\" owner-ok\" type cr" BUILD-FIXPOINT:BF-APPEND-LINE
   s" stage-src.f" BUILD-FIXPOINT:BF-A$ BUILD
   s" the refresh's stage source" LOADED ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-baked-owner" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ BUILD-FIXPOINT:BF-TMP! ;

: CASES ( -- )
   s" test/baked-owner-reopen.f" LOAD
   s" control: a reopen of MEM is refused" s" MEM" REFUSED
   s" src/lib/json-read.f" OWNS
   s" src/lib/fmath.f" OWNS
   s" src/lib/pg.f" OWNS
   s" src/lib/db/rows.f" OWNS
   s" tools/native-build-core.f" OWNS
   s" tools/image-size-lib.f" OWNS
   s" tools/native-emit.f" OWNS
   STAGE-CONTROL
   STAGE-SOURCE ;

public

: RUN ( -- )
   T-RESET
   [: PREPARE CASES ;] [: BUILD-FIXPOINT:BF-TMP-RESET CLEANUP-RUN ;] finally
   T-REPORT ;

;package

BAKED-OWNER:RUN
