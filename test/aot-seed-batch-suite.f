\ aot-seed-batch-suite.f - the AOT seed serves BATCH boots (dot
\ habu-decide-arm-the-5234727b, USER RULING 2026-08-11: one dictionary surface for
\ every boot mode).
\
\ WHAT IT LOCKS. The engine bakes its REPL, token stepper and breakpoint debugger
\ as captured code (src/habu/stdin.f) and seeds them into the dictionary at boot.
\ That seed used to be armed at the interactive REPL entry and nowhere else, so a
\ piped program and a `--load` tool run could not see a captured word: 115 names
\ existed on a tty and did not exist in batch. The seed now runs at the end of the
\ ENGINE PREFIX stream on every boot (src/habu/habu2.f EM-COMPILE-EXIT, and
\ BOOT-SRC:USER-END in src/habu/layout.f for how the prefix and the user program
\ became two streams), so the batch dictionary IS the interactive dictionary.
\
\ WHY THE CASES ARE SHAPED THIS WAY.
\
\   THE THREE BATCH MODES ARE THREE DIFFERENT CODE PATHS, not three spellings of
\   one. `--load` goes through C-SOURCE-FILE-PREFIX's load leg, a piped program
\   through C-SOURCE-PIPE, and `hb prog.f` through the plain leg, and each one
\   publishes the boundary between the two streams at its own store site. A case
\   that only piped would leave two of the three sites untested, which is exactly
\   the kind of hole the old shape hid.
\
\   THE SURFACE IS COMPARED AS A WHOLE. Resolving one baked word says the seed
\   ran; it does not say the two dictionaries MATCH. test/aot-seed-surface.f folds
\   every record name into one number, so a mode that seeded a different set, in a
\   different order, or at a different point answers differently. The interactive
\   half of that comparison is test/proc-pty.f's `PTY-SEED-SURFACE`, which prints
\   the same fold at a real terminal and compares it against a batch run of the
\   same engine; it lives there because that file is the tree's one cross-platform
\   PTY driver and a third copy of the PTY plumbing would be worse than the split.
\
\   THE FIVE NAMES COME FROM FIVE CAPTURED FILES. One resolving name could be a
\   fluke of one record; one name from each of repl.f, debug-watch.f, stepper.f,
\   debug.f and the per-OS repl-term.f says the whole captured set arrived.
\
\   THE NEGATIVE CONTROL IS NOT GARNISH. Without it, "the baked word answered"
\   could be read as "this engine answers anything"; an absent spelling must still
\   exit 70 naming E-UNDEFINED.
\
\   THE GUARD CASE RIDES A REAL DOUBLE ARRIVAL. Every batch boot now reaches
\   "source exhausted" TWICE - once at the end of the engine prefix and once at
\   the end of the program - and only AOT-SEED-DONE-CELL stops the second arrival
\   from seeding again. So counting the dictionary records that carry one baked
\   spelling is a live test of that guard, not a staged one: delete the done-cell
\   test in EM-COMPILE-EXIT and this case reads 2.
\
\ Cost: seven child engine runs of the shipped engine, no builds. Registered as
\ `SUITE aot-seed-batch` in test/gate-stdlib-cases.f (tail-engine tier, since it
\ spawns child engines).
\ Standalone:
\   bin/hb --load lib/errors.f lib/string.f lib/fmt.f lib/test.f lib/memory.f \
\     lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f \
\     lib/process-env.f lib/engine-candidate.f test/aot-seed-batch-suite.f

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package AOT-SEED-BATCH

private

$4000 constant CAP                     \ child stdout/stderr capture
$400 constant SRC-CAP                  \ piped program text
30000 constant TIMEOUT-MS
70 constant UNDEFINED-RC               \ a batch boot that cannot resolve a token

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
create IN SRC-CAP allot  variable IN-U
create EMPTY 1 allot                   \ zero-length stdin
variable RC
variable EXITED

create ROOT-BUF FS-PATH-CAP allot   variable ROOT-U
create PROG-BUF FS-PATH-CAP allot   variable PROG-U

: ROOT$ ( -- ptr u8 n )   ROOT-BUF ROOT-U @ ;
: PROG$ ( -- ptr u8 n )   PROG-BUF PROG-U @ ;
: ERR$ ( -- ptr u8 n )    ERR ERR-U @ ;
: OUT$ ( -- ptr u8 n )    OUT OUT-U @ ;
: SURFACE$ ( -- ptr u8 n ) s" test/aot-seed-surface.f" ;

\ The engine under test: the gate's freshly built candidate when it exported one,
\ else the running engine itself (lib/engine-candidate.f is the tree's one
\ resolver of that question).
: HB$ ( -- ptr u8 n )     ENGINE-CANDIDATE:PATH$ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-aot-seed-batch" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" prog.f" PROG-BUF JOIN-PATH PROG-U ! ;

: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

\ --- the three batch entries, each through its own C-SOURCE path ---------------

: RUN-LOAD ( ptr u8 n -- )             \ hb --load <file>
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   >LEN PROC-ARGV+
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  STORE! ;

: RUN-PLAIN ( ptr u8 n -- )            \ hb <file>
   PROC-ARGV-RESET
   >LEN PROC-ARGV+
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  STORE! ;

: RUN-STDIN ( ptr u8 n -- ) {: a:ptr u:n :}   \ hb < program
   u SRC-CAP > if E-FS-CAPACITY throw then
   a IN u BYTE-COPY  u IN-U !
   PROC-ARGV-RESET
   HB$ >LEN  IN IN-U @ >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  STORE! ;

\ Write a program to the private tree so the two file entries run the same text.
: PROG! ( ptr u8 n -- )
   PROG$ 2swap WRITE-ALL ;

: OK? ( -- )
   EXITED @ TTRUE
   RC @ 0 <> if s" aot-seed-batch: child stderr:" type cr ERR$ type cr then
   RC @ 0 T= ;

: READ-N ( -- n )                      \ the child's single number, or a failed case
   OK?
   OUT$ TRIM STR>NUMBER? MATCH option
     some OF ENDOF
     none OF s" aot-seed-batch: child stdout was not a number:" type cr OUT$ type cr
             T-FAIL 0 ENDOF
   ;MATCH ;

\ --- the probe programs (interpreted by the child) -----------------------------
\ Every one prints exactly one number, so a case is an integer comparison.
\
\ The baked words are called at INTERPRET level, which is what the seed publishes:
\ it registers dictionary records, not checker rows, so a CHECKED definition
\ naming a baked word is still refused. Dot habu-give-baked-records-c97219fb owns
\ that half; until it lands, interpret-level resolution is the whole contract and
\ these cases state exactly it.

: BAKED-ONE$ ( -- ptr u8 n )           \ debug-watch.f: the watch table is empty at boot
   s" BPW-N@ ." ;

\ One name from each captured source, counted in the dictionary; five means every
\ captured file's records arrived. Written as a definition so the walk compiles,
\ with the baked spellings reaching the checker as STRING LITERALS - a checked
\ body may not name them yet.
: BAKED-FIVE$ ( -- ptr u8 n )
   S\" : ASB-SEEN ( ptr u8 n -- n ) {: a:ptr u:n :} 0 0 begin dup ndict@ < while dup XREF-REC XREF-NAME$ a u STR= if swap 1+ swap then 1+ repeat drop ;\n: ASB-FIVE ( -- ) s\" HIST\" ASB-SEEN s\" BPW-N@\" ASB-SEEN + s\" STEPPING\" ASB-SEEN + s\" BP-NULL\" ASB-SEEN + s\" TTY?\" ASB-SEEN + . ;\nASB-FIVE" ;

\ The done-cell guard: one record per baked spelling after BOTH arrivals at
\ "source exhausted".
: ONE-RECORD$ ( -- ptr u8 n )
   S\" : ASB-ONCE ( -- ) 0 0 begin dup ndict@ < while dup XREF-REC XREF-NAME$ s\" BPW-N@\" STR= if swap 1+ swap then 1+ repeat drop . ;\nASB-ONCE" ;

: ABSENT$ ( -- ptr u8 n )
   s" AOT-SEED-NO-SUCH-NAME ." ;

: SURFACE-PROGRAM$ ( -- ptr u8 n )     \ the same fold the interactive half prints
   S\" s\" test/aot-seed-surface.f\" required" ;

\ --- cases ---------------------------------------------------------------------

variable LOAD-SUM

: CASE-BAKED-RESOLVES ( -- )
   BAKED-ONE$ PROG!
   s" a baked-only word answers under --load" T-LABEL
   PROG$ RUN-LOAD  READ-N 0 T=
   s" a baked-only word answers under a piped program" T-LABEL
   BAKED-ONE$ RUN-STDIN  READ-N 0 T=
   s" a baked-only word answers under plain `hb prog.f`" T-LABEL
   PROG$ RUN-PLAIN  READ-N 0 T= ;

: CASE-WHOLE-CAPTURED-SET ( -- )
   s" one name from each of the five captured sources is in the batch dictionary" T-LABEL
   BAKED-FIVE$ RUN-STDIN  READ-N 5 T= ;

: CASE-ABSENT-STILL-FAILS ( -- )
   s" an absent spelling still exits 70 in batch (the cases above are not vacuous)" T-LABEL
   ABSENT$ RUN-STDIN
   EXITED @ TTRUE
   RC @ UNDEFINED-RC T=
   ERR$ s" E-UNDEFINED" CONTAINS? TTRUE ;

: CASE-SEED-RUNS-ONCE ( -- )
   s" the done cell holds: one dictionary record per baked spelling" T-LABEL
   ONE-RECORD$ RUN-STDIN  READ-N 1 T= ;

: CASE-SURFACE-AGREES ( -- )
   s" --load surface fold" T-LABEL
   SURFACE$ RUN-LOAD  READ-N LOAD-SUM !
   LOAD-SUM @ 0 T<>
   s" a piped boot enumerates the same dictionary as --load" T-LABEL
   SURFACE-PROGRAM$ RUN-STDIN  READ-N LOAD-SUM @ T=
   s" a plain `hb prog.f` boot enumerates the same dictionary as --load" T-LABEL
   SURFACE$ RUN-PLAIN  READ-N LOAD-SUM @ T= ;

: BODY ( -- )
   SETUP
   CASE-BAKED-RESOLVES
   CASE-WHOLE-CAPTURED-SET
   CASE-ABSENT-STILL-FAILS
   CASE-SEED-RUNS-ONCE
   CASE-SURFACE-AGREES ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" aot-seed-batch-suite: ok" type cr ;

;package

AOT-SEED-BATCH:RUN
