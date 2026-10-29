\ aot-seed-batch-suite.f - the AOT seed serves batch boots: every boot mode has
\ one dictionary surface. The seed runs at the end of the engine prefix stream
\ on every boot (src/habu/habu2.f EM-COMPILE-EXIT; BOOT-SRC:USER-END in
\ src/habu/layout.f), so the batch dictionary is the interactive one. Row:
\ `SUITE aot-seed-batch` in test/gate-stdlib-cases.f.

\ Standalone:
\   bin/hb --load src/lib/errors.f src/lib/string.f src/lib/fmt.f src/lib/test.f src/lib/memory.f \
\     src/lib/fs.f src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f \
\     src/lib/process-env.f src/lib/engine-candidate.f test/aot-seed-batch-suite.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/test/outcome.f

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

create ROOT-BUF PATH-CAP allot   variable ROOT-U
create PROG-BUF PATH-CAP allot   variable PROG-U

: ROOT$ ( -- ptr u8 n )   ROOT-BUF ROOT-U @ ;
: PROG$ ( -- ptr u8 n )   PROG-BUF PROG-U @ ;
: ERR$ ( -- ptr u8 n )    ERR ERR-U @ ;
: OUT$ ( -- ptr u8 n )    OUT OUT-U @ ;
: SURFACE$ ( -- ptr u8 n ) s" test/aot-seed-surface.f" ;

: HB$ ( -- ptr u8 n )     ENGINE-CANDIDATE:PATH$ ;

: SETUP ( -- )
   FS:CLEANUP-RESET
   s" habu-aot-seed-batch" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" prog.f" PROG-BUF FS:JOIN-PATH PROG-U ! ;

: STORE! ( len len outcome ptr u8 n -- )
   {: outu:len erru:len oc src:ptr srcu:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   oc MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF src srcu OUT$ ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

\ --- the three batch entries, each through its own C-SOURCE path ---------------
\ --load takes C-SOURCE-FILE-PREFIX's load leg, a pipe C-SOURCE-PIPE and
\ `hb prog.f` the plain leg; each publishes the prefix/program boundary at its
\ own store site.

: RUN-LOAD ( ptr u8 n -- )             \ hb --load <file>
   {: p:ptr pu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   p pu >LEN PROC-ARGV+
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  p pu STORE! ;

: RUN-PLAIN ( ptr u8 n -- )            \ hb <file>
   {: p:ptr pu:n :}
   PROC-ARGV-RESET
   p pu >LEN PROC-ARGV+
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  p pu STORE! ;

: RUN-STDIN ( ptr u8 n -- ) {: a:ptr u:n :}   \ hb < program
   u SRC-CAP > if E-FS-CAPACITY throw then
   a IN u BYTE-COPY  u IN-U !
   PROC-ARGV-RESET
   HB$ >LEN  IN IN-U @ >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME  a u STORE! ;

\ Write a program to the private tree so the two file entries run the same text.
: PROG! ( ptr u8 n -- )
   PROG$ 2swap FS:WRITE-ALL ;

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
\ Each prints one number. The baked words run at interpret level, which the seed
\ publishes; a checked definition may name them too, since the engine compiles
\ repl.f after the checker (src/habu/native-runtime.f).

\ repl.f: a batch boot edits no line, so the history ring's low slot is 0 - a
\ baked word with a value the program cannot have set itself.
: BAKED-ONE$ ( -- ptr u8 n )
   s" HLO ." ;

\ One name from each captured source, counted in the dictionary: HIST is
\ repl.f's history ring and HBR-RAWMASK the per-OS repl-term.f's constant. The
\ walk counts seeded records by name, so the spellings are string literals and a
\ definition the child loads itself does not count. A file added to the capture
\ adds a name.
: BAKED-EACH$ ( -- ptr u8 n )
   S\" : ASB-SEEN ( ptr u8 n -- n ) {: a:ptr u:n :} 0 0 begin dup ndict@ < while dup XREF-REC XREF-NAME$ a u STR= if swap 1+ swap then 1+ repeat drop ;\n: ASB-EACH ( -- ) s\" HIST\" ASB-SEEN s\" HBR-RAWMASK\" ASB-SEEN + . ;\nASB-EACH" ;

\ Every batch boot reaches "source exhausted" twice, at the prefix end and the
\ program end; only AOT-SEED-DONE-CELL stops the second seeding. Without its
\ test in EM-COMPILE-EXIT this reads 2.
: ONE-RECORD$ ( -- ptr u8 n )
   S\" : ASB-ONCE ( -- ) 0 0 begin dup ndict@ < while dup XREF-REC XREF-NAME$ s\" HLO\" STR= if swap 1+ swap then 1+ repeat drop . ;\nASB-ONCE" ;

: ABSENT$ ( -- ptr u8 n )
   s" AOT-SEED-NO-SUCH-NAME ." ;

\ test/aot-seed-surface.f folds every record name into one number, so a
\ different set, order or seeding point answers differently. The interactive
\ half is test/proc-pty.f PTY-SEED-SURFACE: that file is the one cross-platform
\ PTY driver.
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
   s" one name from each captured source is in the batch dictionary" T-LABEL
   BAKED-EACH$ RUN-STDIN  READ-N 2 T= ;

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
   FS:CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   FS:CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" aot-seed-batch-suite: ok" type cr ;

;package

AOT-SEED-BATCH:RUN
