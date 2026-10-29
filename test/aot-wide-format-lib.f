\ aot-wide-format-lib.f - the fixture of the capture-window gate row, loaded by
\ test/aot-data-window-suite.f, which reopens package AOT-WIDE-FORMAT and runs
\ its probes. Each probe spawns test/aot-wid-build.f with one HABU_AOT_* mode
\ and a private TMPDIR; the mode compiles inside a window taken by
\ AOT-CAPTURE:CAPTURE, and the builder dies unless its capture tables hold what
\ the mode is for, so the lines a row matches print only past those refusals.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require test/pcap-result.f

package AOT-WIDE-FORMAT

$8000 constant CAP
240000 constant BUILD-TIMEOUT-MS
30000  constant PROBE-TIMEOUT-MS

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
variable RC

create ROOT-BUF PATH-CAP allot    variable ROOT-U
create HB-BUF PATH-CAP allot      variable HB-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: HB$ ( -- ptr u8 n )   HB-BUF HB-U @ ;
: PLAIN$ ( -- ptr u8 n ) s" bin/hb" ;
: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;

\ One tree per build, each registered for cleanup, so "the image exists" is a
\ statement about the build that just ran and never about a leftover.
: SETUP ( -- )
   s" habu-aot-wide" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" hb-pwid" HB-BUF FS:JOIN-PATH HB-U ! ;

: BUILDER-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-wid-build.f" >LEN PROC-ARGV+ ;

\ The builder's environment: TMPDIR names this row's tree, and each knob a probe
\ adds selects a mode or a forge (test/aot-wid-build.f lists them).
: BUILD-OPEN ( -- )
   PROC-ENV-RESET
   s" TMPDIR" >LEN ROOT$ >LEN PROC-ENV+ ;

: KNOB+ ( ptr u8 n ptr u8 n -- ) {: k:ptr ku:n v:ptr vu:n :}
   k ku >LEN v vu >LEN PROC-ENV+ ;

: BUILD-RUN ( -- )
   PROC-ENV-INHERIT-MISSING
   BUILDER-ARGV
   PLAIN$ >LEN  OUT CAP >LEN  ERR CAP >LEN  BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U !
   \ The builder exits PROC-TIMEOUT-RC when a deadline expired in it: replay its
   \ stderr, which names the step, and throw E-PROC-TIMEOUT again.
   RC @ PROC-TIMEOUT-RC = if ERR$ type cr E-PROC-TIMEOUT throw then ;

: BUILD-MODE ( ptr u8 n -- )
   BUILD-OPEN  s" 1" KNOB+  BUILD-RUN ;

\ The seed runs at the end of the engine prefix on every boot, so boot-run entry
\ words report into a batch boot's stdout and a boot dying at the seed or in the
\ boot-run does so before any input is read, on every host. Entry words print
\ whatever fd 0 is; only the REPL they install waits for a terminal
\ (test/proc-pty.f drives it). The program's answer says the image is a working
\ engine; the boot-run lines ahead of it, printed by words that exist only
\ because the seed copied the blob, registered the records and patched this
\ engine's relocation sites, say the capture booted.
: BATCH-RUN ( ptr u8 n -- ) {: p:ptr pu:n :}
   PROC-ARGV-RESET
   HB$ >LEN  p pu >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

\ The program prints its answer after a label of its own, because the boot-run
\ reports ahead of it carry long numbers: awb-ext=6510767442340633178 holds "42",
\ so a bare substring match passed whether or not the program ran. ANSWERED
\ reads the labelled span and compares it exactly.
: BATCH-PROGRAM$ ( -- ptr u8 n )
   S\" : BATCH-ANSWER ( -- ) s\" batch-answer=\" type 7 6 * . cr ;\nBATCH-ANSWER" ;

: BATCH-OK ( -- )
   BATCH-PROGRAM$ BATCH-RUN ;

: DIGIT? ( n -- bool ) {: c:n :}
   c 48 >= c 57 <= and ;

: DIGITS-END ( n -- n )            \ from an index in OUT, the first non-digit at or after it
   begin dup OUT-U @ < while
      dup OUT + c@ DIGIT? 0= if exit then
      1+
   repeat ;

\ The digits a boot-run report printed after its label, as a string. An absent
\ label answers an empty span, which no expectation matches.
: REPORT$ ( ptr u8 n -- ptr u8 n ) {: m:ptr mu:n :}
   OUT$ m mu FIND-SUB MATCH option
     none OF OUT-U @ ENDOF                      \ absent: start at the end -> empty span
     some OF IDX>N mu + ENDOF
   ;MATCH {: st:n :}
   OUT st +  st DIGITS-END st - ;

\ A report's value, asserted against a number the FIXTURE fixed. The two live in
\ different files on purpose - the magic is written in test/aot-wid-build.f and
\ read here - so a fixture that quietly stopped carrying its value cannot also
\ quietly move the expectation.
: REPORT= ( ptr u8 n ptr u8 n -- ) {: m:ptr mu:n w:ptr wu:n :}
   m mu REPORT$ w wu STR= TTRUE ;

: ANSWERED ( -- )
   s" batch-answer=" s" 42" REPORT= ;

: REQUIRE-BUILD ( ptr u8 n -- ) {: m:ptr mu:n :}
   m mu T-LABEL
   RC @ 0 T=
   RC @ 0 <> if s" aot-wid-build: builder stderr:" type cr  ERR$ type cr  RC @ throw then ;

\ A row's probes, with every tree SETUP registered removed whether they pass or
\ throw.
: RUN-PROBES ( [ -- ] -- )
   T-RESET
   FS:CLEANUP-RESET
   catch {: code:n :}
   FS:CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT ;

;package
