\ aot-wide-format-lib.f - the fixture the capture-window gate rows share (dot
\ habu-widen-the-aot-089f5faf). Loaded by test/aot-wide-format-suite.f,
\ test/aot-wide-prefix-suite.f and test/aot-data-window-suite.f: the scratch
\ tree, the builder child, the batch boot of the built engine, the boot-run
\ report reader and the row driver. It runs nothing; each row file reopens
\ package AOT-WIDE-FORMAT and runs the probes it owns.
\
\ Each probe spawns test/aot-wid-build.f in a child process with one HABU_AOT_*
\ mode and a private HB_TMP. The mode compiles its fixture inside a capture window
\ taken by the real AOT-CAPTURE:CAPTURE entry point, and the builder reads its
\ own capture tables and DIES unless the capture holds what the mode is for; the
\ lines a row matches are printed only on the far side of those refusals.
\
\ AND THE BOOT HALF IS HERE TOO, ON EVERY HOST. The seed runs at the end of the
\ engine prefix on EVERY boot (dot habu-decide-arm-the-5234727b), so the boot-run
\ entry words report into an ordinary batch boot's stdout, and a boot that dies
\ at the seed or in the boot-run does so before any batch input is read. Before
\ that the seed was armed at the interactive REPL entry alone and only a PTY -
\ on Linux hosts - could observe a captured word, so on a macOS host nothing
\ asserted that any of this boots. The fixtures' entry words print whatever fd 0
\ is; only the REPL they install waits for a terminal, and test/proc-pty.f drives
\ the shipped engine's captured REPL at one.

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

package AOT-WIDE-FORMAT

$8000 constant CAP
240000 constant BUILD-TIMEOUT-MS
30000  constant PROBE-TIMEOUT-MS

create OUT CAP allot     variable OUT-U
create ERR CAP allot     variable ERR-U
create EMPTY 1 allot                 \ zero-length stdin
variable RC

create ROOT-BUF FS-PATH-CAP allot    variable ROOT-U
create HB-BUF FS-PATH-CAP allot      variable HB-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: HB$ ( -- ptr u8 n )   HB-BUF HB-U @ ;
: PLAIN$ ( -- ptr u8 n ) s" bin/hb" ;
: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;

\ One tree per build, each registered for cleanup, so "the image exists" is a
\ statement about the build that just ran and never about a leftover.
: SETUP ( -- )
   s" habu-aot-wide" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" hb-pwid" HB-BUF JOIN-PATH HB-U ! ;

: BUILDER-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-wid-build.f" >LEN PROC-ARGV+ ;

\ The builder's environment: HB_TMP names this row's tree, and each knob a probe
\ adds selects a mode or a forge (test/aot-wid-build.f lists them).
: BUILD-OPEN ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+ ;

: KNOB+ ( ptr u8 n ptr u8 n -- ) {: k:ptr ku:n v:ptr vu:n :}
   k ku >LEN v vu >LEN PROC-ENV+ ;

: BUILD-RUN ( -- )
   PROC-ENV-INHERIT-MISSING
   BUILDER-ARGV
   PLAIN$ >LEN  OUT CAP >LEN  ERR CAP >LEN  BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: BUILD-MODE ( ptr u8 n -- )
   BUILD-OPEN  s" 1" KNOB+  BUILD-RUN ;

\ Boot the built engine on an ordinary batch program and capture what it wrote.
\ TWO things arrive in that stdout and they are different claims. The program's own
\ answer says the image the widened bake produced is a working engine. The lines
\ AHEAD of it are the boot-run's, printed by words that exist only because the seed
\ copied the blob, registered the records and patched the relocation sites of THIS
\ engine - so those are what say the capture booted.
: BATCH-RUN ( ptr u8 n -- ) {: p:ptr pu:n :}
   PROC-ARGV-RESET
   HB$ >LEN  p pu >LEN  OUT CAP >LEN  ERR CAP >LEN  PROBE-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

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

\ The digits a boot-run report printed after its label, as a string. Taking the
\ SPAN rather than searching for an expected substring is what lets the named-code
\ literal case (test/aot-wide-prefix-suite.f) compare two reports against each
\ other: its value is an address in the engine that printed it, so no fixed text
\ can stand for it. An absent label answers an empty span, which no expectation
\ matches and which the code-literal case rejects outright before it compares.
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
   CLEANUP-RESET
   catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT ;

;package
