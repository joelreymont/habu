\ Named entries own root-relative dependencies; canonical identity deduplicates.
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fmt.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require tools/source-discovery.f
require test/pcap-result.f
require test/fixtures/source-root-count.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process.f

using SOURCE-ROOT
package SOURCE-ROOT-TEST

PATH-CAP 1+ constant CAP
create ROOT CAP allot
create APP-A CAP allot
create APP-B CAP allot
create FALLBACK CAP allot
create SAVED-ROOT CAP allot
create SAVED-TOP CAP allot
create TARGET CAP allot
create COUNT-PATH CAP allot
create LINK CAP allot
create LONG-PATH CAP allot
create LONG-ROOT CAP 2 * allot
variable ROOT-U
variable APP-A-U
variable APP-B-U
variable FALLBACK-U
variable SAVED-ROOT-U
variable TARGET-U
variable COUNT-PATH-U
variable LINK-U
variable SAVED-TOP-U
$4000 constant CHILD-CAP
create CHILD-OUT CHILD-CAP allot
create CHILD-ERR CHILD-CAP allot

private

: COPY! ( str ^u8 ^n -- ) {: s:str dst:^ lenp:^ :}
   s STR:DATA dst s STR:LEN BYTE-COPY s STR:LEN lenp ! ;

\ A fixture requires source-root-count.f by its absolute path and bumps a count.
: REQUIRE-COUNT+ ( -- )
   s" require " SB-APPEND
   COUNT-PATH COUNT-PATH-U @ >str SB-APPEND S\" \n" SB-APPEND ;

: BUMP+ ( n -- ) FMT:SB-U S\"  SOURCE-ROOT-COUNT:BUMP\n" SB-APPEND ;

: BUMP-TEXT ( n -- str ) SB-RESET REQUIRE-COUNT+ BUMP+ SB$ ;

: ROOT$ ( -- str ) ROOT ROOT-U @ >str ;
: A$ ( -- str ) APP-A APP-A-U @ >str ;
: B$ ( -- str ) APP-B APP-B-U @ >str ;
: FALLBACK$ ( -- str ) FALLBACK FALLBACK-U @ >str ;
: A-PATH ( str -- str ) A$ swap JOIN ;
: B-PATH ( str -- str ) B$ swap JOIN ;
: FALLBACK-PATH ( str -- str ) FALLBACK$ swap JOIN ;

: RESTORED ( -- )
   CURRENT$ SAVED-ROOT SAVED-ROOT-U @ >str T$=
   SOURCE-ROOT:ACTIVE-SOURCE drop drop SAVED-TOP SAVED-TOP-U @ >str T$= ;

: PREP ( -- )
   FS:CLEANUP-RESET 0 SOURCE-ROOT-COUNT:LOADED !
   CURRENT$ SAVED-ROOT SAVED-ROOT-U COPY!
   s" test/fixtures/source-root-count.f" RESOLVE TTRUE COUNT-PATH COUNT-PATH-U COPY!
   SOURCE-ROOT:ACTIVE-SOURCE drop drop SAVED-TOP SAVED-TOP-U COPY!
   s" habu-source-root" FS:MKTEMP-DIR CANONICAL TTRUE ROOT ROOT-U COPY!
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" a" JOIN APP-A APP-A-U COPY!
   ROOT$ s" b" JOIN APP-B APP-B-U COPY!
   s" src/nested" A-PATH FS:MAKE-DIRS
   s" src" B-PATH FS:MAKE-DIRS
   s" src/leaf.f" A-PATH 1 BUMP-TEXT FS:WRITE-ALL
   s" src/nested/dep.f" A-PATH s" require src/leaf.f" FS:WRITE-ALL
   s" src/leaf.f" B-PATH 10 BUMP-TEXT FS:WRITE-ALL
   s" main.f" B-PATH s" require src/leaf.f" FS:WRITE-ALL
   s" boom.f" A-PATH s" 9132 throw" FS:WRITE-ALL
   s" ." s" habu-root-fallback" FS:MKTEMP-DIR-IN FALLBACK FALLBACK-U COPY!
   FALLBACK$ FS:CLEANUP-TREE+
   s" leaf.f" FALLBACK-PATH 100 BUMP-TEXT FS:WRITE-ALL
   SB-RESET s" require " SB-APPEND
   s" leaf.f" FALLBACK-PATH SB-APPEND
   s" shared.f" FALLBACK-PATH SB$ FS:WRITE-ALL
   FALLBACK$ A-PATH FS:MAKE-DIRS
   s" leaf.f" FALLBACK-PATH A-PATH 1000 BUMP-TEXT FS:WRITE-ALL
   SB-RESET S\" require src/nested/dep.f\nrequire src/leaf.f\nrequire " SB-APPEND
   s" shared.f" FALLBACK-PATH SB-APPEND
   s" main.f" A-PATH SB$ FS:WRITE-ALL ;

: LOAD-ENTRIES ( -- )
   s" main.f" A-PATH script-required
   SOURCE-ROOT-COUNT:LOADED @ 101 T= RESTORED
   s" main.f" B-PATH script-required
   SOURCE-ROOT-COUNT:LOADED @ 111 T= RESTORED ;

: ALIASES ( -- )
   s" src/leaf.f" A-PATH TARGET TARGET-U COPY!
   s" src/alias.f" A-PATH LINK LINK-U COPY!
   TARGET TARGET-U @ >str LINK LINK-U @ >str FS:MAKE-SYMLINK
   s" src/./nested/../leaf.f" A-PATH required
   LINK LINK-U @ >str required
   TARGET TARGET-U @ >str required
   SOURCE-ROOT-COUNT:LOADED @ 111 T=
   LINK LINK-U @ >str included
   TARGET TARGET-U @ >str included
   SOURCE-ROOT-COUNT:LOADED @ 113 T= RESTORED ;

: PROVIDED-MISSING ( -- )
   s" absent.f" A-PATH provided
   s" ./absent.f" A-PATH required
   RESTORED ;

: THROW-LOAD ( -- ) s" boom.f" A-PATH included ;

: THROW-RESTORES ( -- )
   [: THROW-LOAD ;] 9132 TTHROWSQ
   RESTORED ;

: NUL-REJECTED ( -- )
   s" a path holding a 0 byte is rejected by required" T-LABEL
   REJECTION:CAPTURE
   [: s\" a\zb.f" A-PATH required ;] 70 TTHROWSQ
   REJECTION:BUFFER$
   S\" error[E-PATH-NUL]: `required` got a path with a NUL byte, which no file name holds\n"
   STR:STARTS-WITH? TTRUE
   REJECTION:BUFFER$ s"  --> " STR:CONTAINS? TFALSE
   REJECTION:BUFFER$
   S\" = help: remove the NUL byte from the path\n" STR:CONTAINS? TTRUE
   REJECTION:BUFFER-OFF
   RESTORED ;

\ Quotation bodies that take a str: a quotation literal moves its input as one
\ cell, so each str it takes goes to a word whose row says str.
: NUL-READ ( str -- str ) drop s\" a\zb.f" SOURCE-ROOT:READ-OS ;
: NUL-CANON ( str -- str bool ) drop s\" a\zb.f" true ;
: RELATIVE-CANON ( str -- str bool ) drop s" src/core/include.f" true ;
: LONG-READ ( str -- str ) drop LONG-PATH CAP >str SOURCE-ROOT:READ-OS ;
: LEAF-BYTES ( str str -- )
   drop 1 BUMP-TEXT T$= CURRENT$ A$ T$= 1 SOURCE-ROOT-COUNT:BUMP ;
: SUPPLIED-BYTES ( str str -- )
   drop s" 2 SOURCE-ROOT-COUNT:BUMP" T$=
   CURRENT$ A$ T$= 1 SOURCE-ROOT-COUNT:BUMP ;

: READ-NUL ( -- )
   s" a path holding a 0 byte is rejected by SOURCE-ROOT:READ-OS" T-LABEL
   REJECTION:CAPTURE
   [: [: SOURCE-ROOT:CANON-OS ;]
      [: NUL-READ ;]
      [: s" src/leaf.f" A-PATH included ;] SOURCE-INPUT:WITH ;]
   70 TTHROWSQ
   REJECTION:BUFFER$ s" error[" STR:COUNT-SUB 1 T=
   REJECTION:BUFFER$
   S\" error[E-PATH-NUL]: `SOURCE-ROOT:READ-OS` got a path with a NUL byte" STR:STARTS-WITH? TTRUE
   REJECTION:BUFFER-OFF
   RESTORED ;

: CANON-NUL ( -- )
   s" a supplied canon's answer holding a 0 byte is rejected, naming SOURCE-INPUT:CANON" T-LABEL
   REJECTION:CAPTURE
   [: [: NUL-CANON ;]
      [: SOURCE-ROOT:READ-OS ;]
      [: s" src/leaf.f" A-PATH included ;] SOURCE-INPUT:WITH ;]
   70 TTHROWSQ
   REJECTION:BUFFER$ s" error[" STR:COUNT-SUB 1 T=
   REJECTION:BUFFER$ s" error[E-SOURCE-ROOT]: " STR:STARTS-WITH? TTRUE
   REJECTION:BUFFER$ s" SOURCE-INPUT:CANON" STR:CONTAINS? TTRUE
   REJECTION:BUFFER$ s" NUL byte" STR:CONTAINS? TTRUE
   REJECTION:BUFFER-OFF
   RESTORED ;

: CANON-RELATIVE ( -- )
   s" a supplied canon's answer not beginning with / is rejected, naming SOURCE-INPUT:CANON" T-LABEL
   REJECTION:CAPTURE
   [: [: RELATIVE-CANON ;]
      [: SOURCE-ROOT:READ-OS ;]
      [: s" src/leaf.f" A-PATH required ;] SOURCE-INPUT:WITH ;]
   70 TTHROWSQ
   REJECTION:BUFFER$ s" error[" STR:COUNT-SUB 1 T=
   REJECTION:BUFFER$ s" error[E-SOURCE-ROOT]: " STR:STARTS-WITH? TTRUE
   REJECTION:BUFFER$ s" SOURCE-INPUT:CANON" STR:CONTAINS? TTRUE
   REJECTION:BUFFER$ s" does not begin with" STR:CONTAINS? TTRUE
   REJECTION:BUFFER-OFF
   RESTORED ;

: READ-LONG ( -- )
   s" a path longer than PATH-CAP is rejected by SOURCE-ROOT:READ-OS" T-LABEL
   CAP 0 ?do 97 LONG-PATH i + c! loop
   REJECTION:CAPTURE
   [: [: SOURCE-ROOT:CANON-OS ;]
      [: LONG-READ ;]
      [: s" src/leaf.f" A-PATH included ;] SOURCE-INPUT:WITH ;]
   70 TTHROWSQ
   REJECTION:BUFFER$ s" error[" STR:COUNT-SUB 1 T=
   REJECTION:BUFFER$
   s" error[E-PATH-RANGE]: `SOURCE-ROOT:READ-OS` got a path " STR:STARTS-WITH? TTRUE
   REJECTION:BUFFER-OFF
   RESTORED ;

: WITH-EMPTY ( -- )
   s" an empty path is rejected by SOURCE-ROOT:WITH, which the record names" T-LABEL
   REJECTION:CAPTURE
   [: s" " [: ;] SOURCE-ROOT:WITH ;] 70 TTHROWSQ
   REJECTION:BUFFER$ s" error[" STR:COUNT-SUB 1 T=
   REJECTION:BUFFER$
   s" error[E-PATH-RANGE]: `SOURCE-ROOT:WITH` got an empty path" STR:STARTS-WITH? TTRUE
   REJECTION:BUFFER-OFF
   RESTORED ;

: WITH-OPENS ( -- )
   s" SOURCE-ROOT:WITH-BYTES and WITH-SUPPLIED open a file under the root their caller passes" T-LABEL
   0 SOURCE-ROOT-COUNT:LOADED !
   s" main.f" B-PATH RESOLVE drop drop
   A$ s" src/leaf.f" A-PATH 0
   [: LEAF-BYTES ;] WITH-BYTES
   A$ s" src/leaf.f" A-PATH s" 2 SOURCE-ROOT-COUNT:BUMP" 0
   [: SUPPLIED-BYTES ;] WITH-SUPPLIED
   SOURCE-ROOT-COUNT:LOADED @ 2 T= RESTORED ;

: WITH-LONG ( -- )
   s" a root path of WORK-BYTES is rejected by SOURCE-ROOT:WITH, which the record names" T-LABEL
   CAP 2 * 0 ?do 97 LONG-ROOT i + c! loop
   REJECTION:CAPTURE
   [: LONG-ROOT CAP 2 * >str [: ;] SOURCE-ROOT:WITH ;] 70 TTHROWSQ
   REJECTION:BUFFER$ s" error[" STR:COUNT-SUB 1 T=
   REJECTION:BUFFER$
   s" error[E-PATH-RANGE]: `SOURCE-ROOT:WITH` got a path" STR:STARTS-WITH? TTRUE
   REJECTION:BUFFER-OFF
   RESTORED ;

: DISCOVERY ( -- )
   s" main.f" A-PATH ENTRY-RESOLVE drop RESOLVED-ROOT$
   DISCOVER:READ-IN DISCOVER:RUN-READ
   SOURCE-EVENT:COUNT 3 T=
   0 SOURCE-EVENT:PATH@ s" src/nested/dep.f" A-PATH T$=
   0 SOURCE-EVENT:ROOT@ A$ T$=
   2 SOURCE-EVENT:PATH@ s" shared.f" FALLBACK-PATH CANONICAL TTRUE T$=
   2 SOURCE-EVENT:ROOT@ CWD$ T$=
   2 SOURCE-EVENT:PATH@ 2 SOURCE-EVENT:ROOT@ DISCOVER:READ-IN DISCOVER:RUN-READ
   SOURCE-EVENT:COUNT 1 T=
   0 SOURCE-EVENT:PATH@ s" leaf.f" FALLBACK-PATH CANONICAL TTRUE T$=
   s" src/nested/dep.f" A-PATH A$ DISCOVER:READ-IN DISCOVER:RUN-READ
   SOURCE-EVENT:COUNT 1 T=
   0 SOURCE-EVENT:PATH@ s" src/leaf.f" A-PATH T$=
   0 SOURCE-EVENT:ROOT@ A$ T$=
   RESTORED ;

: INVOCATION$ ( -- str ) ROOT$ s" invoke" JOIN ;

: ENGINE-LINK ( str -- ) {: name:str :}
   CWD$ name JOIN TARGET TARGET-U COPY!
   TARGET TARGET-U @ >str INVOCATION$ name JOIN FS:MAKE-SYMLINK ;

: PREP-ENGINE-ALIASES ( -- )
   INVOCATION$ FS:MAKE-DIR
   s" src" ENGINE-LINK s" test" ENGINE-LINK
   ROOT$ s" other/nested" JOIN FS:MAKE-DIRS
   ROOT$ s" other/src/lib" JOIN FS:MAKE-DIRS
   ROOT$ s" other/src/lib/error.f" JOIN 100 BUMP-TEXT FS:WRITE-ALL
   ROOT$ s" other/nested" JOIN TARGET TARGET-U COPY!
   TARGET TARGET-U @ >str INVOCATION$ s" branch" JOIN FS:MAKE-SYMLINK
   s" src/lib" A-PATH FS:MAKE-DIRS s" src/lib" B-PATH FS:MAKE-DIRS
   s" src/lib/error.f" A-PATH 1 BUMP-TEXT FS:WRITE-ALL
   s" src/lib/error.f" B-PATH 10 BUMP-TEXT FS:WRITE-ALL ;

\ A fresh process must carry its frozen engine facts into a symlinked checkout.
\ The child also keeps owner-local modules and symlink/.. targets distinct.
: ENGINE-ALIASES ( -- )
   s" frozen engine aliases preserve physical source identity" T-LABEL
   PREP-ENGINE-ALIASES
   PROC-ARGV-RESET PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" --load" PROC-ARGV+
   s" test/source-root-alias-child.f" PROC-ARGV+
   s" --" PROC-ARGV+ ROOT$ PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ CANONICAL TTRUE INVOCATION$
   CHILD-OUT CHILD-CAP >LEN CHILD-ERR CHILD-CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   PCAP-RESULT:>N {: outu:n erru:n rc:n :}
   rc 0 <> if CHILD-OUT outu >str type CHILD-ERR erru >str type then
   rc 0 T= erru 0 T=
   CHILD-OUT outu >str s" source root aliases: ok" STR:CONTAINS? TTRUE
   RESTORED ;

\ Each file resumes after its child. Distinct entry/exit ordinals expose
\ overwritten parent source bytes as well as missing or out-of-order loads.
40 constant DEEP-COUNT
variable EVAL-N

public

\ Unlike nested loads, these evaluations have no intervening catch: one throw
\ must unwind every escaped evaluator frame, including changed package/usings.
\ Each text is a closed program, so the level count crosses in EVAL-N, not on
\ the stack.
: EVAL-DOWN ( -- )
   EVAL-N @ 0= if 7 EVAL-N ! exit then
   -1 EVAL-N +!
   s" SOURCE-ROOT-TEST:EVAL-DOWN" evaluate-closed
   1 EVAL-N +! ;

: EVAL-FAIL ( -- )
   EVAL-N @ 0= if
      s" package EVAL-ESCAPED using FMT 9133 throw" evaluate-closed
   then
   -1 EVAL-N +!
   s" SOURCE-ROOT-TEST:EVAL-FAIL" evaluate-closed ;

private

: DEEP-NAME+ ( str n -- ) {: ix:n :}
   SB-APPEND s" /" SB-APPEND ix FMT:SB-U s" .f" SB-APPEND ;

: DEEP-PATH ( str n -- str )
   SB-RESET DEEP-NAME+ SB$ A-PATH ;

: DEEP-MAKE ( str bool -- ) {: prefix:str fail:bool :}
   prefix A-PATH FS:MAKE-DIRS
   DEEP-COUNT 0 ?do
      prefix i DEEP-PATH TARGET TARGET-U COPY!
      SB-RESET REQUIRE-COUNT+ i FMT:SB-U S\"  SOURCE-ROOT-COUNT:ENTER\n" SB-APPEND
      i DEEP-COUNT 1- < if
         S\" s\" " SB-APPEND prefix i 1+ DEEP-NAME+
         fail i 2 mod 0= or if
            S\" \" included\n" SB-APPEND
         else
            S\" \" required\n" SB-APPEND
         then
      else
         fail if S\" 9132 throw\n" SB-APPEND else
            S\" require shared.f\nrequire ./shared.f\n" SB-APPEND
         then
      then
      i FMT:SB-U S\"  SOURCE-ROOT-COUNT:LEAVE-FRAME\n" SB-APPEND
      TARGET TARGET-U @ >str SB$ FS:WRITE-ALL
   loop ;

: DEEP-THROW ( -- )
   A$ [: s" bad/0.f" included ;] WITH ;

: DEEP-GOOD ( -- )
   A$ [: s" good/0.f" required ;] WITH ;

: DEEP-EVALS ( -- )
   40 EVAL-N ! EVAL-DOWN EVAL-N @ 47 T=
   [: 40 EVAL-N ! EVAL-FAIL ;] 9133 TTHROWSQ
   RESTORED
   s" [package] STR:LEN 0 T=" evaluate-closed
   40 EVAL-N ! EVAL-DOWN EVAL-N @ 47 T= ;

: DEEP-LOADS ( -- )
   s" deeply nested throws restore the caller before another dependency chain" T-LABEL
   s" shared.f" A-PATH 7 BUMP-TEXT FS:WRITE-ALL
   s" bad" true DEEP-MAKE
   s" good" false DEEP-MAKE
   A$ SOURCE-ROOT-COUNT:ROOT!
   0 SOURCE-ROOT-COUNT:ACTIVE ! 0 SOURCE-ROOT-COUNT:VISITS !
   0 SOURCE-ROOT-COUNT:LOADED ! 0 SOURCE-ROOT-COUNT:WRONG !
   [: DEEP-THROW ;] 9132 TTHROWSQ
   SOURCE-ROOT-COUNT:VISITS @ DEEP-COUNT T=
   SOURCE-ROOT-COUNT:WRONG @ 0 T= RESTORED
   0 SOURCE-ROOT-COUNT:ACTIVE ! 0 SOURCE-ROOT-COUNT:VISITS !
   DEEP-GOOD
   SOURCE-ROOT-COUNT:ACTIVE @ 0 T= SOURCE-ROOT-COUNT:VISITS @ DEEP-COUNT T=
   SOURCE-ROOT-COUNT:LOADED @ 7 T= SOURCE-ROOT-COUNT:WRONG @ 0 T= RESTORED
   DEEP-GOOD
   SOURCE-ROOT-COUNT:VISITS @ DEEP-COUNT T=
   SOURCE-ROOT-COUNT:LOADED @ 7 T= RESTORED ;

\ A file larger than a header and 1 MiB loads through require and include and
\ resumes after the small file it includes midway: each load frame is sized to
\ its own file's bytes.
$1000 constant LARGE-CHUNK
192 constant LARGE-HALF                  \ chunks each side of the include: 1.5 MiB
create CHUNK LARGE-CHUNK allot

: LARGE-BYTE ( n -- n )
   64 mod
   {: col:n :}
   col 0= if $5c exit then
   col 1 = if $20 exit then
   col 63 = if $0a exit then
   $78 ;

: LARGE-CHUNKS+ ( n -- )
   {: count:n :}
   count 0 ?do TARGET TARGET-U @ >str CHUNK LARGE-CHUNK >str FS:APPEND-FILE loop ;

: LARGE-MAKE ( -- )
   s" large" A-PATH FS:MAKE-DIRS
   LARGE-CHUNK 0 ?do i LARGE-BYTE CHUNK i + c! loop
   s" large/small.f" A-PATH 1 BUMP-TEXT FS:WRITE-ALL
   s" large/big.f" A-PATH TARGET TARGET-U COPY!
   TARGET TARGET-U @ >str CHUNK LARGE-CHUNK >str FS:WRITE-ALL
   LARGE-HALF 1- LARGE-CHUNKS+
   TARGET TARGET-U @ >str S\" include large/small.f\n" FS:APPEND-FILE
   LARGE-HALF LARGE-CHUNKS+
   TARGET TARGET-U @ >str 10 BUMP-TEXT FS:APPEND-FILE
   SB-RESET REQUIRE-COUNT+
   S\" require large/big.f\nrequire large/big.f\ninclude large/big.f\n" SB-APPEND 100 BUMP+
   s" large/outer.f" A-PATH SB$ FS:WRITE-ALL ;

\ require loads big.f once (its small.f and its tail: 11), the second require
\ skips it, include loads it again (11), and outer.f's own tail adds 100.
: LARGE-LOADS ( -- )
   s" a file past 1 MiB loads through require and include around a small one" T-LABEL
   LARGE-MAKE
   0 SOURCE-ROOT-COUNT:LOADED !
   A$ [: s" large/outer.f" included ;] WITH
   SOURCE-ROOT-COUNT:LOADED @ 122 T= RESTORED ;

\ JOIN joins a root longer than PATH-CAP whole, and rejects a root length below 0
\ or one whose sum with the name's length wraps.
PATH-CAP 1+ 2 * constant JOIN-WORK
-1 1 rshift constant MAX-CELL
create JOIN-SRC JOIN-WORK allot

: JOIN-ROOT$ ( n -- str ) JOIN-SRC swap >str ;

: JOIN-ROOM ( -- )
   JOIN-WORK 0 ?do 97 JOIN-SRC i + c! loop
   47 JOIN-SRC c!
   s" a root twice PATH-CAP long joins whole" T-LABEL
   JOIN-WORK 3 - JOIN-ROOT$ s" x" JOIN STR:LEN JOIN-WORK 1- T=
   s" a root length of -1 or the maximum cell is rejected" T-LABEL
   [: -1 JOIN-ROOT$ s" x" JOIN drop ;] E-PATH-RANGE TTHROWSQ
   [: MAX-CELL JOIN-ROOT$ s" x" JOIN drop ;] E-PATH-RANGE TTHROWSQ ;

: DIRNAMES ( -- )
   s" a bare relative name's directory is the current directory" T-LABEL
   s" stamp" DIRNAME s" ." T$=
   s" a directory part, rooted or relative, is kept and / is its own parent" T-LABEL
   s" out/stamp" DIRNAME s" out" T$=
   s" /stamp" DIRNAME s" /" T$=
   s" /" DIRNAME s" /" T$= ;

public
: RUN ( -- )
   T-RESET PREP
   LOAD-ENTRIES ALIASES PROVIDED-MISSING THROW-RESTORES NUL-REJECTED READ-NUL
   CANON-NUL CANON-RELATIVE READ-LONG WITH-EMPTY WITH-OPENS WITH-LONG DISCOVERY DEEP-LOADS
   LARGE-LOADS DEEP-EVALS
   ENGINE-ALIASES JOIN-ROOM DIRNAMES
   FS:CLEANUP-RUN T-REPORT ;

;package
SOURCE-ROOT-TEST:RUN
;using
