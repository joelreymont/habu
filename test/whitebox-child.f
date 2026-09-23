\ whitebox-child.f - the unsealed engine a spawning suite hands its children.
\
\ A suite of this kind names no internal engine token itself; its CHILD does -
\ it reopens the native build window, ticks a word the seal closed - and the
\ shipped bin/hb refuses every one of them, exit 70 with `hb: internal engine
\ word: <TOKEN>` (src/core/internal-mark.f). Such a file is a plain SUITE and
\ green on its own, not a WHITEBOX-SUITE row, so the gate hands it no engine.
\ It names its children's engine itself: one private copy of the keyed unsealed
\ engine test/whitebox-engine.f builds, copied from the gate's artifact when
\ that already exists and built otherwise. A standalone `bin/hb --load <file>`
\ then measures the same bytes the gate row measures - which is what the gate's
\ own SUITE-ENGINE-ENV (test/gate-stdlib-lib.f) did for a WHITEBOX row through
\ the environment alone, and which nothing did for a standalone run.
\
\ BOTH ENVIRONMENT ROWS. lib/engine-candidate.f is the one resolver a child's
\ own tools ask which engine to run, and it reads HABU_UNDER_TEST first;
\ a child that rebuilds an engine reads HABU_FIXPOINT_ENGINE
\ (tools/build-fixpoint.f BF-ENGINE$) and must land on the same private copy,
\ which is also where a build that promotes over it should write. Both are set
\ BEFORE PROC-ENV-INHERIT-MISSING, which skips a name already present, so an
\ outer HABU_UNDER_TEST=<tree>/bin/hb cannot reach past them.
\
\ PROVIDE makes a temp root of its own and registers it with CLEANUP-TREE+.
\ PROVIDE-IN takes a root the caller already made and owns. REMOVE is for the
\ caller whose own cases CLEANUP-RESET the shared list, where PROVIDE's
\ registration does not survive to be run.

require lib/fs.f
require lib/fs-mutate.f
require lib/process-env.f
require test/whitebox-engine.f

package WHITEBOX-CHILD

create ROOT-BUF FS-PATH-CAP allot
create ENGINE-BUF FS-PATH-CAP allot
variable ROOT-U
variable ENGINE-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;

public

: ENGINE$ ( -- ptr u8 n ) ENGINE-BUF ENGINE-U @ ;

: PROVIDE-IN ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu s" hb-whitebox" ENGINE-BUF JOIN-PATH ENGINE-U !
   ENGINE$ WHITEBOX-ENGINE:PROVIDE ;

: PROVIDE ( ptr u8 n -- )
   HB-TMP-MKDIR {: root:ptr rootu:n :}
   root ROOT-BUF rootu BYTE-COPY rootu ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ PROVIDE-IN ;

: REMOVE ( -- )
   ROOT-U @ 0 > if ROOT$ REMOVE-TREE then ;

: ENV+ ( -- )
   s" HABU_UNDER_TEST" >LEN ENGINE$ >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE$ >LEN PROC-ENV+ ;

: ENV! ( -- )
   PROC-ENV-RESET ENV+ PROC-ENV-INHERIT-MISSING ;

;package
