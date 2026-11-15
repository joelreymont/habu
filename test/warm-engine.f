\ warm-engine.f - a warm engine build writes the engine a cold build does. A
\ copy of the checkout's sources is built cold, its checker.f gains data at its
\ end, which moves every later file's data, and the copy is built again, warm
\ over the first build's out root and cold into a fresh one. Then its linker.f
\ gains a comment line and is built both ways again: linker.f exports CHECK's
\ private words while the checker files are linked from their objects.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/lib/tree-copy.f
require tools/chain-run.f

package WARM-ENGINE-TEST
private

PATH-CAP BUFFER: ROOT     variable ROOT-U
PATH-CAP BUFFER: JOINED
PATH-CAP BUFFER: JOINED-2

: ROOT$ ( -- ^u8 n ) ROOT ROOT-U @ ;

\ The path of name under the test's directory, in JOINED.
: AT$ ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   ROOT$ a u JOINED FS:JOIN-PATH {: ju:n :}
   JOINED ju ;

\ A failed build shows its status and what it printed on both streams.
: SHOW ( CMD:command n -- CMD:command ) {: rc:n :}
   s" warm-engine: exited " type rc .
   s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr ;

\ The candidate builds the copy's engine to dst, with out as its out root.
: BUILD ( CMD:command ^u8 n ^u8 n -- CMD:command )
   {: out:^ outu:n dst:^ dstu:n :}
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+  s" --" CMD:ARG+
   s" --engine" CMD:ARG+  s" -o" CMD:ARG+  dst dstu AT$ CMD:ARG+
   s" HABU_OUT" out outu AT$ CMD:ENV+
   s" TMPDIR" s" tmp" AT$ CMD:ENV+
   s" tree" AT$ CMD:CWD!
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   rc 0 T= ;

: PAD ( -- )
   s" tree/src/core/checker.f" AT$
   s\" \ncreate WARM-PAD 64 cells allot\n" FS:APPEND-FILE ;

: NOTE ( -- )
   s" tree/src/habu/linker.f" AT$
   s\" \n\\ warm-engine\n" FS:APPEND-FILE ;

: SAME? ( ^u8 n ^u8 n -- bool )
   {: a:^ au:n b:^ bu:n :}
   ROOT$ a au JOINED FS:JOIN-PATH {: ju:n :}
   ROOT$ b bu JOINED-2 FS:JOIN-PATH {: ku:n :}
   JOINED ju JOINED-2 ku CHAIN-RUN:SAME-FILES? ;

: BUILDS ( CMD:command -- CMD:command )
   s" the copy builds cold" T-LABEL
   s" out-warm" s" first-hb" BUILD
   PAD
   s" the padded copy builds warm" T-LABEL
   s" out-warm" s" warm-hb" BUILD
   s" the padded copy builds cold" T-LABEL
   s" out-cold" s" cold-hb" BUILD
   NOTE
   s" the noted copy builds warm" T-LABEL
   s" out-warm" s" warm-hb-2" BUILD
   s" the noted copy builds cold" T-LABEL
   s" out-cold-2" s" cold-hb-2" BUILD ;

: CHECKS ( -- )
   s" tree" AT$ TREE-COPY:BUILD-SOURCES
   s" tmp" AT$ FS:MAKE-DIRS
   [: BUILDS ;] CMD:WITH-COMMAND
   s" the warm build writes the cold build's engine" T-LABEL
   s" warm-hb" s" cold-hb" SAME? TTRUE
   s" the pad changes the engine" T-LABEL
   s" first-hb" s" cold-hb" SAME? 0= TTRUE
   s" the warm build after a linker.f comment writes the cold build's engine" T-LABEL
   s" warm-hb-2" s" cold-hb-2" SAME? TTRUE ;

: REMOVE-ROOT ( -- ) ROOT$ FS:REMOVE-TREE ;

public

: RUN ( -- )
   T-RESET
   s" habu-warm-engine" FS:MKTEMP-DIR {: a:^ u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ['] CHECKS ['] REMOVE-ROOT finally
   T-REPORT ;

;package

WARM-ENGINE-TEST:RUN
