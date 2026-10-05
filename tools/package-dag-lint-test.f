\ package-dag-lint-test.f - the HBR2 package DAG lint CLI over fixture trees
\ and over this tree.
\ Run: bin/hb --load tools/package-dag-lint-test.f
\
\ Each fixture is a root of its own under HB_TMP, holding only the layer files
\ the case needs. The CLI checks the tree in its working directory, so each case
\ runs it in a child engine working in the fixture and asserts on the child's
\ exit status and output. The refused cases show what the lint prints, the
\ accepted ones that an allowed import, a cycle outside the layers, a require of
\ the engine's own files and a root without layers find nothing, and LIVE that
\ this tree is clean.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/engine-candidate.f
require test/suite-budget.f              \ CHILD-MS, the child's hang guard

package PACKAGE-DAG-LINT-TEST
private

$10000 constant OUT-CAP
create OUT-BUF OUT-CAP allot
create ERR-BUF OUT-CAP allot
variable OUT-U
create ROOT FS-PATH-CAP allot
variable ROOT-U
create PATH FS-PATH-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: OUT$ ( -- ptr u8 n ) OUT-BUF OUT-U @ ;

: FIXTURE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u HB-TMP-MKDIR {: r:ptr ru:n :}
   r ROOT ru BYTE-COPY
   ru ROOT-U !
   ROOT$ CLEANUP-TREE+ ;

\ A name under the root, as a path.
: UNDER$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   ROOT$ a u PATH JOIN-PATH {: pu:n :}
   PATH pu ;

: DIR! ( ptr u8 n -- ) UNDER$ MAKE-DIRS ;

: FILE! ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n text:ptr textu:n :}
   a u UNDER$ text textu WRITE-ALL ;

\ A symlink at a name under the root, holding its target as written.
: LINK! ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n target:ptr targetu:n :}
   target targetu a u UNDER$ MAKE-SYMLINK ;

0 constant MODE-NONE

: MODE! ( ptr u8 n n -- ) {: a:ptr u:n mode:n :}
   a u UNDER$ mode CHMOD-MODE ;

\ The CLI in a child working in a root, its output captured and then shown in
\ this log; the child's exit status. The child works elsewhere, so the CLI, this
\ working directory's, and the engine are named absolutely, the engine also as
\ HABU_UNDER_TEST for the engines the child runs.
: CLI ( ptr u8 n -- n )
   {: a:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/package-dag-lint.f" SOURCE-ROOT:CANONICAL drop >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CANONICAL drop {: e:ptr eu:n :}
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN e eu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   e eu >LEN a u >LEN
   OUT-BUF OUT-CAP >LEN ERR-BUF OUT-CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE 0 ENDOF
      err OF PCAP-FAILED:UNMAKE RC>N ENDOF
   ;MATCH
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !
   OUT$ type ERR-BUF erru LEN>N type
   rc ;

\ The count the census ends with, -1 when the child printed none.
: FINDINGS ( -- n )
   OUT$ s" file(s) read, " {: a:ptr u:n k:ptr ku:n :}
   a u k ku FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N ku + ENDOF
   ;MATCH {: at:n :}
   at 0 < if -1 exit then
   0 at begin dup u < while
      dup a + c@ STR-DIGIT? 0= if drop exit then
      swap 10 * over a + c@ STR-DIGIT-VALUE + swap
      1+
   repeat drop ;

\ The lint over a root: its findings, which exit 1 when there is one.
: RUN-AT ( ptr u8 n -- n )
   CLI {: rc:n :}
   FINDINGS {: n:n :}
   rc  n 0 > if 1 else 0 then  T=
   n ;

: RUN ( -- n ) ROOT$ RUN-AT ;

: SAID? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   OUT$ a u CONTAINS? ;

: FORBIDDEN-EDGE ( -- )
   s" pdl-edge" FIXTURE
   s" lib/runtime" DIR!  s" lib/ui" DIR!
   s" lib/runtime/a.f" S\" require lib/ui/b.f\npackage PDLT-EA\n;package\n" FILE!
   s" lib/ui/b.f" S\" package PDLT-EB\n;package\n" FILE!
   RUN 1 T=
   s" RUNTIME may not import UI: lib/runtime/a.f -> lib/ui/b.f" SAID? TTRUE ;

\ The closure, not only the file's own requires: a file of no layer between.
: FORBIDDEN-THROUGH ( -- )
   s" pdl-through" FIXTURE
   s" lib/runtime" DIR!  s" lib/ui" DIR!
   s" lib/runtime/a.f" S\" require lib/glue.f\n" FILE!
   s" lib/glue.f" S\" require lib/ui/b.f\n" FILE!
   s" lib/ui/b.f" S\" package PDLT-TB\n;package\n" FILE!
   RUN 1 T=
   s" RUNTIME may not import UI: lib/runtime/a.f -> lib/glue.f -> lib/ui/b.f"
   SAID? TTRUE ;

: CYCLE ( -- )
   s" pdl-cycle" FIXTURE
   s" lib/runtime" DIR!
   s" lib/runtime/a.f" S\" require lib/runtime/b.f\npackage PDLT-CA\n;package\n" FILE!
   s" lib/runtime/b.f" S\" require lib/runtime/a.f\npackage PDLT-CB\n;package\n" FILE!
   RUN 1 T=
   s" require cycle: lib/runtime/a.f -> lib/runtime/b.f -> lib/runtime/a.f"
   SAID? TTRUE ;

\ s1 and s2 load each other outside the layers, which is not refused.
: CYCLE-THROUGH ( -- )
   s" pdl-through-cycle" FIXTURE
   s" lib/runtime" DIR!
   s" lib/runtime/a.f" S\" require lib/s1.f\n" FILE!
   s" lib/s1.f" S\" require lib/s2.f\n" FILE!
   s" lib/s2.f" S\" require lib/s1.f\n" FILE!
   RUN 0 T= ;

\ A loaded file resolves against the root that resolved it; an entry's is its
\ own directory, so this load reaches lib/ui/b.f.
: RELATIVE ( -- )
   s" pdl-relative" FIXTURE
   s" lib/runtime" DIR!  s" lib/ui" DIR!
   s" lib/runtime/a.f" S\" require ../ui/b.f\n" FILE!
   s" lib/ui/b.f" S\" package PDLT-RB\n;package\n" FILE!
   RUN 1 T=
   s" RUNTIME may not import UI: lib/runtime/a.f -> lib/ui/b.f" SAID? TTRUE ;

\ The same name from the root that requires a.f: util.f there, not lib/runtime's.
: BARE ( -- )
   s" pdl-bare" FIXTURE
   s" lib/runtime" DIR!  s" lib/ui" DIR!
   s" lib/runtime/a.f" S\" require util.f\n" FILE!
   s" lib/runtime/util.f" S\" \n" FILE!
   s" util.f" S\" require lib/ui/b.f\n" FILE!
   s" lib/ui/b.f" S\" package PDLT-BB\n;package\n" FILE!
   RUN 1 T=
   s" RUNTIME may not import UI: lib/runtime/a.f -> util.f -> lib/ui/b.f" SAID? TTRUE ;

\ \x62 is b.
: ESCAPED ( -- )
   s" pdl-escaped" FIXTURE
   s" lib/runtime" DIR!  s" lib/ui" DIR!
   s" lib/runtime/a.f" S\" S\\\" lib/ui/\\x62.f\" required\n" FILE!
   s" lib/ui/b.f" S\" package PDLT-XB\n;package\n" FILE!
   RUN 1 T=
   s" RUNTIME may not import UI: lib/runtime/a.f -> lib/ui/b.f" SAID? TTRUE ;

\ \x6g is no escape: the engine refuses the literal, so the walk names its load
\ unread rather than guess a path, and the file does not load.
: BAD-ESCAPE ( -- )
   s" pdl-bad-escape" FIXTURE
   s" lib/runtime" DIR!
   s" lib/runtime/a.f" S\" S\\\" lib/ui/\\x6g.f\" required\n" FILE!
   RUN 2 T=
   s" lib/runtime/a.f:1: a load the walk cannot read" SAID? TTRUE
   s" lib/runtime/a.f does not load alone, exit 74" SAID? TTRUE ;

\ glue.f loads lib/ui/b.f through a path it computes, which the walk cannot read.
: COMPUTED ( -- )
   s" pdl-computed" FIXTURE
   s" lib/runtime" DIR!  s" lib/ui" DIR!
   s" lib/runtime/a.f" S\" require lib/glue.f\n" FILE!
   s" lib/glue.f"
   S\" : PDLT-TARGET ( -- ptr u8 n ) s\" lib/ui/b.f\" ;\nPDLT-TARGET required\n" FILE!
   s" lib/ui/b.f" S\" package PDLT-CB\n;package\n" FILE!
   RUN 1 T=
   s" lib/glue.f:2: a load the walk cannot read" SAID? TTRUE ;

\ The `s"` on line 3 is the local, so `required` takes a computed path.
: LOCAL-OPENER ( -- )
   s" pdl-local-opener" FIXTURE
   s" lib/runtime" DIR!  s" lib/ui" DIR!
   s" lib/runtime/a.f"
   S\" : LD ( ptr u8 n -- )\n   {: s\":n :}\n   s\" required ;\ns\" lib/ui/b.f\" LD\n" FILE!
   s" lib/ui/b.f" S\" package PDLT-LB\n;package\n" FILE!
   RUN 1 T=
   s" lib/runtime/a.f:3: a load the walk cannot read" SAID? TTRUE ;

\ The engine provides include.f, whose load words compute their paths; a
\ require of it loads nothing, so it is not read.
: ENGINE ( -- )
   s" pdl-engine" FIXTURE
   s" lib/runtime" DIR!
   s" lib/runtime/a.f" S\" require src/core/include.f\n" FILE!
   RUN 0 T= ;

\ An include reads the file again, so the walk reads include.f and names the
\ loads its words compute, which a.f, holding none, cannot name.
: ENGINE-INCLUDE ( -- )
   s" pdl-engine-include" FIXTURE
   s" lib/runtime" DIR!
   s" lib/runtime/a.f" S\" include src/core/include.f\n" FILE!
   RUN 1 > TTRUE
   s" a load the walk cannot read" SAID? TTRUE ;

\ a.f is listed in RUNTIME's directory but is shared/a.f, which imports UI.
: OUTSIDE ( -- )
   s" pdl-outside" FIXTURE
   s" lib/runtime" DIR!  s" lib/ui" DIR!  s" shared" DIR!
   s" shared/a.f" S\" require lib/ui/b.f\n" FILE!
   s" lib/runtime/a.f" s" ../../shared/a.f" LINK!
   s" lib/ui/b.f" S\" package PDLT-OB\n;package\n" FILE!
   RUN 1 T=
   s" layer file resolves outside its layer: lib/runtime/a.f -> shared/a.f"
   SAID? TTRUE ;

\ No require edge to walk: only RUNTIME loading alone refuses the call.
: UNREQUIRED ( -- )
   s" pdl-unrequired" FIXTURE
   s" lib/runtime" DIR!  s" lib/ui" DIR!
   s" lib/runtime/a.f"
   S\" package PDLT-UR\npublic\n: CALL ( -- n ) PDLT-UU:VALUE ;\n;package\n" FILE!
   s" lib/ui/b.f" S\" package PDLT-UU\npublic\n: VALUE ( -- n ) 7 ;\n;package\n" FILE!
   RUN 1 T=
   s" lib/runtime/a.f does not load alone, exit 70" SAID? TTRUE
   s" E-UNDEFINED" SAID? TTRUE
   s" PDLT-UU:VALUE" SAID? TTRUE ;

\ a.f defines the word b.f calls and loads first, but b.f does not require it.
: SIBLING ( -- )
   s" pdl-sibling" FIXTURE
   s" lib/runtime" DIR!
   s" lib/runtime/a.f" S\" package PDLT-SA\npublic\n: VALUE ( -- n ) 7 ;\n;package\n" FILE!
   s" lib/runtime/b.f"
   S\" package PDLT-SB\npublic\n: CALL ( -- n ) PDLT-SA:VALUE ;\n;package\n" FILE!
   RUN 1 T=
   s" lib/runtime/b.f does not load alone, exit 70" SAID? TTRUE
   s" E-UNDEFINED" SAID? TTRUE
   s" PDLT-SA:VALUE" SAID? TTRUE ;

: ALLOWED ( -- )
   s" pdl-allowed" FIXTURE
   s" lib/runtime" DIR!  s" lib/ui" DIR!
   s" lib/runtime/a.f" S\" package PDLT-AR\npublic\n: VALUE ( -- n ) 7 ;\n;package\n" FILE!
   s" lib/ui/b.f"
   S\" require lib/runtime/a.f\npackage PDLT-AU\npublic\n: CALL ( -- n ) PDLT-AR:VALUE ;\n;package\n"
   FILE!
   RUN 0 T=
   s" 2 layer file(s)" SAID? TTRUE ;

\ No layer directory, so no layer file.
: MISSING ( -- )
   s" pdl-missing" FIXTURE
   RUN 0 T=
   s" 0 layer file(s)" SAID? TTRUE ;

\ helper.f's root is the one that resolved it, a.f's directory, not the root.
: DEEP ( -- )
   s" pdl-deep" FIXTURE
   s" lib/runtime" DIR!  s" lib/ui" DIR!  s" helpers/deep" DIR!
   s" lib/runtime/a.f" S\" require ../../helpers/deep/helper.f\n" FILE!
   s" helpers/deep/helper.f" S\" require ../../lib/ui/b.f\n" FILE!
   s" lib/ui/b.f" S\" package PDLT-DB\n;package\n" FILE!
   RUN 1 T=
   s" RUNTIME may not import UI: lib/runtime/a.f -> helpers/deep/helper.f -> lib/ui/b.f"
   SAID? TTRUE ;

\ The lint while the directory a names has mode 0: one finding, that it cannot
\ read host/browser/, or, for a privileged user, host/browser/'s cycle.
: SHUT ( ptr u8 n bool -- ) {: a:ptr u:n privileged:bool :}
   a u MODE-NONE MODE!
   RUN
   a u FS-MUT-MODE-DIR MODE!
   1 T=
   privileged if
      s" require cycle: host/browser/a.f -> host/browser/b.f -> host/browser/a.f"
   else
      s" cannot read layer directory host/browser/"
   then
   SAID? TTRUE ;

\ A layer directory the lint cannot read, under host/ of mode 0 or itself of
\ mode 0, is a finding, not an absent layer. Unix permissions do not stop a
\ privileged user, who reads it and finds its cycle instead: the finding is
\ asserted only when this process cannot stat past mode 0 either, and the log
\ says when it can.
: UNREADABLE ( -- )
   s" pdl-unreadable" FIXTURE
   s" host/browser" DIR!
   s" host/browser/a.f" S\" require host/browser/b.f\npackage PDLT-HA\n;package\n" FILE!
   s" host/browser/b.f" S\" require host/browser/a.f\npackage PDLT-HB\n;package\n" FILE!
   s" host" MODE-NONE MODE!
   s" host/browser/" UNDER$ DIR? {: privileged:bool :}
   s" host" FS-MUT-MODE-DIR MODE!
   privileged if
      S\" privileged: mode 0 stops nothing, so UNREADABLE asserts the cycle\n" type
   then
   s" host" privileged SHUT
   s" host/browser" privileged SHUT ;

\ A file where a layer directory belongs is no absent layer either.
: NOT-DIR ( -- )
   s" pdl-not-dir" FIXTURE
   s" lib" DIR!
   s" lib/runtime" S\" \n" FILE!
   RUN 1 T=
   s" cannot read layer directory lib/runtime/" SAID? TTRUE ;

\ This tree, the working directory.
: LIVE ( -- )
   s" ." RUN-AT 0 T= ;

: MAIN ( -- )
   T-RESET
   CLEANUP-RESET
   LIVE
   FORBIDDEN-EDGE
   FORBIDDEN-THROUGH
   RELATIVE
   BARE
   DEEP
   ESCAPED
   BAD-ESCAPE
   COMPUTED
   LOCAL-OPENER
   ENGINE
   ENGINE-INCLUDE
   OUTSIDE
   CYCLE
   CYCLE-THROUGH
   UNREQUIRED
   SIBLING
   ALLOWED
   MISSING
   UNREADABLE
   NOT-DIR
   CLEANUP-RUN
   T-REPORT ;

MAIN

;package
