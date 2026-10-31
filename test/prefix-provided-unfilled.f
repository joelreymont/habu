\ A seeded x86-64 build whose captured runtime leaves a prefix-provided row's
\ dispatch cell empty refuses at the registry gate (76, src/habu/
\ primitive-registry.f COMPLETE-PROVIDED, UNFILLED) and promotes no product:
\ without it the product's evaluate stub jumps through the empty cell. An ARM64
\ record has no cell, so any host builds --target linux-x86-64 with
\ src/arch/x86-64/passes.f loaded before tools/native-build.f.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/lib/tree-copy.f

package PREFIX-PROVIDED-UNFILLED
private

$4000 constant SRC-CAP
600000 constant BUILD-TIMEOUT-MS

create ROOT PATH-CAP allot variable ROOT-U
create PATH PATH-CAP allot
create SRC SRC-CAP allot variable SRC-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: AT-A ( ptr u8 n -- ptr u8 n ) {: rel:ptr relu:n :}
   ROOT$ rel relu PATH FS:JOIN-PATH PATH swap ;

: RUNTIME$ ( -- ptr u8 n ) s" src/habu/runtime-x64.f" ;
: INSTALL$ ( -- ptr u8 n ) S\"    OUTER:INSTALL-EVALUATE\n" ;

\ src/habu/runtime-x64.f without the line that fills the evaluate cell.
: WRITE-RUNTIME ( -- )
   RUNTIME$ AT-A SRC SRC-CAP FS:READ-ALL SRC-U !
   SRC SRC-U @ INSTALL$ FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: at:n :}
   at 0 < if
      s" prefix-provided-unfilled: runtime-x64.f runs no OUTER:INSTALL-EVALUATE" 1 die
   then
   INSTALL$ nip at + {: past:n :}
   RUNTIME$ AT-A SRC at FS:WRITE-ALL
   RUNTIME$ AT-A SRC past + SRC-U @ past - FS:APPEND-FILE ;

\ The private tree holds copies, not links, of the build sources: a source's
\ identity is its physical path, so a linked one lies outside the tree and the
\ build refuses 74 when no compiler.f below it completes (tools/build-producer.f
\ COMPILER-DONE). The printed tree keeps the build's output.
: SETUP ( -- )
   s" prefix-provided-unfilled" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ TREE-COPY:BUILD-SOURCES
   WRITE-RUNTIME ;

\ The production entry, as `bin/hb --load tools/native-build.f -- <out>` runs
\ it, in the private tree, for an x86-64 product on any host. The child inherits
\ HABU_NATIVE_CACHE_DIAG, whose stderr runs to 10 MB per build.
: BUILD ( CMD:command -- CMD:command )
   CMD:RESET
   s" --load" CMD:ARG+
   HB-TARGET-LINUX-X86-64? 0= if s" src/arch/x86-64/passes.f" CMD:ARG+ then
   s" tools/native-build.f" CMD:ARG+
   s" --" CMD:ARG+
   s" hb" AT-A CMD:ARG+
   HB-TARGET-LINUX-X86-64? 0= if
      s" --target" CMD:ARG+  s" linux-x86-64" CMD:ARG+
   then
   ROOT$ CMD:CWD!
   ENGINE-CANDIDATE:PATH$ BUILD-TIMEOUT-MS >MS CMD:RUN-OUTCOME drop
   CMD:OUT$ {: out:ptr outu:n :}
   s" build.out" AT-A out outu FS:WRITE-ALL
   CMD:ERR$ {: err:ptr erru:n :}
   s" build.err" AT-A err erru FS:WRITE-ALL ;

\ The last build refused at the registry gate and left no product.
: REFUSED ( CMD:command -- CMD:command )
   CMD:OUTCOME@ PROC-OUTCOME>RC RC>N {: rc:n :}
   CMD:OUT$ {: out:ptr outu:n :}
   CMD:ERR$ {: err:ptr erru:n :}
   rc 76 <> if out outu type err erru type then
   rc 76 T=
   out outu S\" captured runtime leaves the dispatch cell empty for prefix-provided primitive evaluate-range\n" CONTAINS? TTRUE
   err erru s" prims: prefix-provided row not in the captured runtime" CONTAINS? TTRUE
   s" the refused build promotes no product" T-LABEL
   s" hb" AT-A FS:FILE? 0= TTRUE ;

: CHECK-BUILDS ( CMD:command -- CMD:command )
   BUILD
   s" a build whose runtime leaves the evaluate cell empty refuses 76" T-LABEL
   REFUSED
   \ A second build process against the same out root: the file objects the
   \ first build stored are the boundary under test. A file object the first
   \ build stored and this one reused under the x86-64 shadow would leave its
   \ records without routines, and the link would refuse 74 short of the
   \ registry gate (tools/native-definition-cache.f OBJECT-OPEN, shadow-open).
   BUILD
   s" the same build against the same out root refuses 76" T-LABEL
   REFUSED ;

public

: RUN ( -- )
   T-RESET
   SETUP
   [: CHECK-BUILDS ;] CMD:WITH-COMMAND
   T-REPORT
   s" prefix-provided-unfilled tree: " type ROOT$ type cr ;

;package

PREFIX-PROVIDED-UNFILLED:RUN
