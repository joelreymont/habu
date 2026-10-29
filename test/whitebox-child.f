\ The unsealed engine a spawning suite hands its children. Such a suite's child
\ reopens the native build window or ticks a word the seal closed, which the
\ shipped bin/hb refuses (exit 70, src/core/internal-mark.f). A plain SUITE gets
\ no engine from the gate: it names one private copy of the keyed unsealed
\ engine test/whitebox-engine.f builds, copied from the gate's artifact or
\ built, so a standalone run measures the same bytes the gate row does.

require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/whitebox-engine.f

package WHITEBOX-CHILD

create ROOT-BUF PATH-CAP allot
create ENGINE-BUF PATH-CAP allot
variable ROOT-U
variable ENGINE-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;

public

: ENGINE$ ( -- ptr u8 n ) ENGINE-BUF ENGINE-U @ ;

\ PROVIDE makes a temp root of its own and registers it with FS:CLEANUP-TREE+.
\ PROVIDE-IN takes a root the caller already made and owns. REMOVE is for the
\ caller whose own cases FS:CLEANUP-RESET the shared list, where PROVIDE's
\ registration does not survive to be run.
: PROVIDE-IN ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu s" hb-whitebox" ENGINE-BUF FS:JOIN-PATH ENGINE-U !
   ENGINE$ WHITEBOX-ENGINE:PROVIDE ;

: PROVIDE ( ptr u8 n -- )
   FS:MKTEMP-DIR {: root:ptr rootu:n :}
   root ROOT-BUF rootu BYTE-COPY rootu ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ PROVIDE-IN ;

: REMOVE ( -- )
   ROOT-U @ 0 > if ROOT$ FS:REMOVE-TREE then ;

\ src/lib/engine-candidate.f, the resolver a child's tools ask, reads
\ HABU_UNDER_TEST first; a child that rebuilds an engine reads
\ HABU_FIXPOINT_ENGINE (tools/build-fixpoint.f BF-ENGINE$) and must land on, and
\ promote over, the same private copy. Both are set before
\ PROC-ENV-INHERIT-MISSING, which skips a name already present, so an outer
\ HABU_UNDER_TEST cannot reach past them.
: ENV+ ( -- )
   s" HABU_UNDER_TEST" >LEN ENGINE$ >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE$ >LEN PROC-ENV+ ;

: ENV! ( -- )
   PROC-ENV-RESET ENV+ PROC-ENV-INHERIT-MISSING ;

;package
