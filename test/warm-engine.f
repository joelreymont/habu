\ warm-engine.f - a warm engine build writes the engine a cold build does. A
\ copy of the checkout's sources is built cold, its checker.f gains data at its
\ end, which moves every later file's data, and the copy is built again, warm
\ over the first build's out root and cold into a fresh one. Then its linker.f
\ gains a comment line and is built both ways again: linker.f exports CHECK's
\ private words while the checker files are linked from their objects.
\ The kernel and engine builds load util.f at startup, where their loads agree,
\ so the first build keeps one object of it; they load icode.f after startup,
\ where their loads differ, so the padded warm build, which reruns the kernel
\ build, rewrites no object of icode.f the first build wrote. Last, the copy's
\ realpath row gains a data cell, and its warm build is rejected: build-time
\ code typed by the tree's row would run hb's body.

require src/lib/error.f
require src/core/exit-status.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/test/assert.f
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
PATH-CAP BUFFER: OBJ-ROOT   variable OBJ-ROOT-U   \ out-warm's obj directory
PATH-CAP BUFFER: KEPT-ROOT  variable KEPT-ROOT-U  \ the first build's icode.f objects
variable SEEN-N                                   \ the walk's paths its word counts
variable ALL-SAME                                 \ SAME-ICODE's copies all match
1 17 lshift constant PRIMS-CAP
create PRIMS PRIMS-CAP allot                      \ the copy's src/habu/prims.f

: ROOT$ ( -- str ) ROOT ROOT-U @ >str ;

\ The path of name under the test's directory, in JOINED.
: AT$ ( str -- str ) {: s:str :}
   ROOT$ s JOINED FS:JOIN-PATH {: ju:n :}
   JOINED ju >str ;

\ A failed build shows its status and what it printed on both streams.
: SHOW ( CMD:command n -- CMD:command ) {: rc:n :}
   s" warm-engine: exited " type rc .
   s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr ;

\ The candidate builds the copy's engine to dst, with out as its out root, and
\ answers its status.
: RUN-BUILD ( CMD:command str str -- CMD:command n )
   {: out:str dst:str :}
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+  s" --" CMD:ARG+
   s" --engine" CMD:ARG+  s" -o" CMD:ARG+  dst AT$ CMD:ARG+
   s" HABU_OUT" out AT$ CMD:ENV+
   s" TMPDIR" s" tmp" AT$ CMD:ENV+
   s" tree" AT$ CMD:CWD!
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

: BUILD ( CMD:command str str -- CMD:command )
   RUN-BUILD {: rc:n :}
   rc 0<> if rc SHOW then
   rc 0 T= ;

\ The build is rejected for realpath's row.
: REJECTED ( CMD:command str str -- CMD:command )
   RUN-BUILD {: rc:n :}
   rc EXIT-STATUS:DATAERR <> if rc SHOW then
   rc EXIT-STATUS:DATAERR T=
   CMD:ERR$ s" E-PRIM-CELLS" STR:CONTAINS? TTRUE
   CMD:ERR$ s" realpath" STR:CONTAINS? TTRUE ;

: PAD ( -- )
   s" tree/src/core/checker.f" AT$
   s\" \ncreate WARM-PAD 64 cells allot\n" FS:APPEND-FILE ;

: NOTE ( -- )
   s" tree/src/habu/linker.f" AT$
   s\" \n\\ warm-engine\n" FS:APPEND-FILE ;

\ The copy's realpath row takes one more data cell.
: WIDEN ( -- )
   s" tree/src/habu/prims.f" AT$ {: p:str :}
   p PRIMS PRIMS-CAP FS:READ-ALL {: u:n :}
   PRIMS u >str {: text:str :}
   s" EPRIM: realpath PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN PE-N PE-IN " {: row:str :}
   text row STR:FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: at:n :}
   s" the copy's prims.f has the realpath row" T-LABEL
   at 0 >= TTRUE
   at 0 < if exit then
   p PRIMS at row STR:LEN + >str FS:WRITE-ALL
   p s" PE-N PE-IN " FS:APPEND-FILE
   p text STR:DATA at + row STR:LEN +  u at - row STR:LEN -  >str FS:APPEND-FILE ;

\ The path below root.
: BELOW$ ( str str -- str ) {: p:str root:str :}
   p STR:DATA root STR:LEN 1 + +  p STR:LEN root STR:LEN - 1 -  >str ;

: OBJ-ROOT$ ( -- str ) OBJ-ROOT OBJ-ROOT-U @ >str ;
: KEPT-ROOT$ ( -- str ) KEPT-ROOT KEPT-ROOT-U @ >str ;

: UTIL-SEEN ( str -- )
   s" /tree/src/core/util.f.hfo" STR:ENDS-WITH? if 1 SEEN-N +! then ;

\ An object of icode.f, copied to its path below KEPT-ROOT.
: KEEP-ICODE ( str -- )
   {: p:str :}
   p s" /tree/src/arch/arm64/icode.f.hfo" STR:ENDS-WITH? 0= if exit then
   1 SEEN-N +!
   KEPT-ROOT$ p OBJ-ROOT$ BELOW$ JOINED FS:JOIN-PATH {: ju:n :}
   JOINED ju >str {: dst:str :}
   JOINED  ju dst FS:BASENAME STR:LEN - 1 -  >str FS:MAKE-DIRS
   p dst FS:COPY-FILE-STREAM ;

\ A copy the object at its path below OBJ-ROOT still matches.
: SAME-ICODE ( str -- )
   {: c:str :}
   1 SEEN-N +!
   OBJ-ROOT$ c KEPT-ROOT$ BELOW$ JOINED FS:JOIN-PATH {: ju:n :}
   c JOINED ju >str CHAIN-RUN:SAME-FILES? 0= if false ALL-SAME ! then ;

\ The files below dir the word counts.
: WALKED ( str [ str -- ] -- n )
   {: d:str q :}
   0 SEEN-N !
   d q FS:WALK-FILES
   SEEN-N @ ;

: SHARED-UTIL ( -- )
   s" the kernel and engine builds share util.f's object" T-LABEL
   OBJ-ROOT$ ['] UTIL-SEEN WALKED 1 T= ;

: KEEP-ICODES ( -- )
   s" the first build writes an object of icode.f" T-LABEL
   OBJ-ROOT$ ['] KEEP-ICODE WALKED 0 > TTRUE ;

\ Run before the noted copy's warm build, which writes into out-warm again.
: SAME-ICODES ( -- )
   s" the padded warm build rewrites no object of icode.f" T-LABEL
   KEPT-ROOT$ FS:EXISTS? 0= if false TTRUE exit then
   true ALL-SAME !
   KEPT-ROOT$ ['] SAME-ICODE WALKED drop
   ALL-SAME @ TTRUE ;

: SAME? ( str str -- bool )
   {: a:str b:str :}
   ROOT$ a JOINED FS:JOIN-PATH {: ju:n :}
   ROOT$ b JOINED-2 FS:JOIN-PATH {: ku:n :}
   JOINED ju >str JOINED-2 ku >str CHAIN-RUN:SAME-FILES? ;

: BUILDS ( CMD:command -- CMD:command )
   s" the copy builds cold" T-LABEL
   s" out-warm" s" first-hb" BUILD
   SHARED-UTIL
   KEEP-ICODES
   PAD
   s" the padded copy builds warm" T-LABEL
   s" out-warm" s" warm-hb" BUILD
   SAME-ICODES
   s" the padded copy builds cold" T-LABEL
   s" out-cold" s" cold-hb" BUILD
   NOTE
   s" the noted copy builds warm" T-LABEL
   s" out-warm" s" warm-hb-2" BUILD
   s" the noted copy builds cold" T-LABEL
   s" out-cold-2" s" cold-hb-2" BUILD
   WIDEN
   s" a row whose cells differ from hb's is rejected" T-LABEL
   s" out-warm" s" wide-hb" REJECTED ;

: CHECKS ( -- )
   ROOT$ s" out-warm/obj" OBJ-ROOT FS:JOIN-PATH OBJ-ROOT-U !
   ROOT$ s" icode" KEPT-ROOT FS:JOIN-PATH KEPT-ROOT-U !
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
   s" habu-warm-engine" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA ROOT d STR:LEN BYTE-COPY  d STR:LEN ROOT-U !
   ['] CHECKS ['] REMOVE-ROOT finally
   T-REPORT ;

;package

WARM-ENGINE-TEST:RUN
