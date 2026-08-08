\ process-fork-test.f - focused proof of the checked fork wrappers.
\
\ A real fork through PROC-FORK:CHECKED returns 0 in the child and the child's
\ positive pid in the parent: the child takes the pid-zero branch and exits
\ cleanly, and the parent sees a positive pid and reaps a clean exit through
\ PROC-WAIT-RC. PROC-FORK:RAW follows the same pid contract without the throw
\ guard. 0 0 PROC-FORK:SET-PGID (run inside a forked child so the test process
\ keeps its own process group) makes that child its own group leader (rc 0),
\ which the parent reads back as a clean child exit. A forked child also starts
\ with an EMPTY fs cleanup table: the entries it would otherwise inherit name
\ paths the live parent still owns, so running them deletes the parent's files.
\ Run: bin/hb --load lib/errors.f lib/prelude.f lib/string.f lib/test.f \
\      lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-fork.f \
\      lib/process-fork-test.f

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-fork.f

package PROC-FORK-TEST

: FORK-EXIT ( n -- )
   s" " rot die ;

\ Reap the forked child by pid: a clean (0) exit lands on the ok arm; anything
\ else is a test failure.
: EXPECT-CLEAN-CHILD ( n -- )
   >PID PROC-WAIT-RC MATCH result
     ok  OF 0 T= ENDOF
     err OF drop -1 0 T= ENDOF
   ;MATCH ;

\ Checked fork: the child sees pid 0 and exits cleanly; the parent sees a
\ positive pid and reaps a clean exit.
: CHECK-CHECKED ( -- )
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if 0 FORK-EXIT then
   pid 0 > TTRUE
   pid EXPECT-CLEAN-CHILD ;

\ Raw fork: same pid contract (0 in child, positive in parent) without the throw.
: CHECK-RAW ( -- )
   PROC-FORK:RAW PID>N {: pid:n :}
   pid 0= if 0 FORK-EXIT then
   pid 0 > TTRUE
   pid EXPECT-CLEAN-CHILD ;

\ 0 0 PROC-FORK:SET-PGID makes the caller its own group leader; done in a child
\ so the test process keeps its group. The child exits 0 iff setpgid returned 0.
: CHECK-SET-PGID ( -- )
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if
      0 >PID 0 >PID PROC-FORK:SET-PGID RC>N 0 = if 0 else 1 then FORK-EXIT
   then
   pid EXPECT-CLEAN-CHILD ;

\ Fork-inherited cleanup registrations, straight through the wrapper instead of
\ through a pool: whoever the caller is, the child arm of PROC-FORK:RAW empties
\ lib/fs-mutate.f's cleanup table before the caller's child code runs.
\
\ The parent registers a tree, forks, and the child exits with the cleanup depth
\ it inherited as its exit code - so the reap alone decides the invariant, since
\ an inherited entry comes back as a nonzero code. The table caps at
\ FS-MUT-CLEANUP-MAX, so the depth always fits in an exit code. The child also
\ registers and runs its OWN cleanup, which lets the parent see afterwards that
\ the child's tree is gone (the machinery really ran) while the parent's tree is
\ untouched. The fixture root itself is never registered, so no cleanup run can
\ erase the evidence; the parent removes it at the end.
create FIX-ROOT FS-PATH-CAP allot        \ fixture root; never registered
create KEEP FS-PATH-CAP allot            \ registered by the parent, before the fork
create KEEP-FILE FS-PATH-CAP allot
create OWN FS-PATH-CAP allot             \ registered by the child, after the fork
variable FIX-ROOT-U
variable KEEP-U
variable KEEP-FILE-U
variable OWN-U

: FIX-ROOT$ ( -- ptr u8 n )
   FIX-ROOT FIX-ROOT-U @ ;

: KEEP$ ( -- ptr u8 n )
   KEEP KEEP-U @ ;

: KEEP-FILE$ ( -- ptr u8 n )
   KEEP-FILE KEEP-FILE-U @ ;

: OWN$ ( -- ptr u8 n )
   OWN OWN-U @ ;

: FIX-ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a FIX-ROOT u BYTE-COPY
   u FIX-ROOT-U ! ;

: SUB! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: base:ptr baseu:n name:ptr nameu:n dst:ptr up:ptr :}
   base baseu name nameu dst JOIN-PATH up ! ;

: PATHS! ( -- )
   s" hb-fork-cleanup" TMPDIR-MKDIR FIX-ROOT!
   FIX-ROOT$ s" keep" KEEP KEEP-U SUB!
   KEEP$ s" file" KEEP-FILE KEEP-FILE-U SUB!
   FIX-ROOT$ s" own" OWN OWN-U SUB! ;

\ Child body: read the inherited depth BEFORE registering anything of its own
\ (its own registration would add to it, and CLEANUP-RUN clears it), then clean
\ up what it registered and carry the depth out as the exit code.
: CHILD-CLEANUP ( -- )
   FS-MUT-CLEANUP-N @ {: depth:n :}
   OWN$ MAKE-DIRS
   OWN$ CLEANUP-TREE+
   CLEANUP-RUN
   depth FORK-EXIT ;

: CHECK-CHILD-CLEANUP ( -- )
   PATHS!
   KEEP$ MAKE-DIRS
   KEEP-FILE$ s" keep" WRITE-ALL
   KEEP$ CLEANUP-TREE+
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if CHILD-CLEANUP then
   s" fork child inherits an empty cleanup table" T-LABEL
   pid EXPECT-CLEAN-CHILD
   s" fork child ran its own cleanup" T-LABEL
   OWN$ EXISTS? TFALSE
   s" the parent's registration survives the child" T-LABEL
   KEEP-FILE$ FILE? TTRUE
   CLEANUP-RUN
   s" the parent's own run removes what it registered" T-LABEL
   KEEP$ EXISTS? TFALSE
   FIX-ROOT$ REMOVE-TREE ;

: RUN ( -- )
   T-RESET
   CHECK-CHECKED
   CHECK-RAW
   CHECK-SET-PGID
   CHECK-CHILD-CLEANUP
   T-REPORT
   s" process-fork-test: ok" type cr ;

RUN

;package
