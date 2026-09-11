\ Named entries own root-relative dependencies; canonical identity deduplicates.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/fmt.f
require tools/source-discovery.f
require tools/event-closure-lib.f

using SOURCE-ROOT
using DISCOVER
using EC
package SOURCE-ROOT-TEST

FS-PATH-CAP 1+ constant CAP
create ROOT CAP allot
create APP-A CAP allot
create APP-B CAP allot
create FALLBACK CAP allot
create SAVED-ROOT CAP allot
create TARGET CAP allot
create LINK CAP allot
variable ROOT-U
variable APP-A-U
variable APP-B-U
variable FALLBACK-U
variable SAVED-ROOT-U
variable TARGET-U
variable LINK-U
variable LOADED
variable SAVED-DEPTH
variable SAVED-NAMED

public
: BUMP ( n -- ) LOADED +! ;
private

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY u lenp ! ;

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: A$ ( -- ptr u8 n ) APP-A APP-A-U @ ;
: B$ ( -- ptr u8 n ) APP-B APP-B-U @ ;
: FALLBACK$ ( -- ptr u8 n ) FALLBACK FALLBACK-U @ ;
: A-PATH ( ptr u8 n -- ptr u8 n ) A$ 2swap JOIN ;
: B-PATH ( ptr u8 n -- ptr u8 n ) B$ 2swap JOIN ;
: FALLBACK-PATH ( ptr u8 n -- ptr u8 n ) FALLBACK$ 2swap JOIN ;

: RESTORED ( -- )
   CURRENT$ SAVED-ROOT SAVED-ROOT-U @ T$=
   INCLUDE-DEPTH @ SAVED-DEPTH @ T=
   SCRIPT-NAMED-LOAD? if 1 else 0 then SAVED-NAMED @ T= ;

: PREP ( -- )
   CLEANUP-RESET 0 LOADED !
   CURRENT$ SAVED-ROOT SAVED-ROOT-U COPY!
   INCLUDE-DEPTH @ SAVED-DEPTH !
   SCRIPT-NAMED-LOAD? if 1 else 0 then SAVED-NAMED !
   s" habu-source-root" TMPDIR-MKDIR CANONICAL TTRUE ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" a" JOIN APP-A APP-A-U COPY!
   ROOT$ s" b" JOIN APP-B APP-B-U COPY!
   s" src/nested" A-PATH MAKE-DIRS
   s" src" B-PATH MAKE-DIRS
   s" src/leaf.f" A-PATH s" 1 SOURCE-ROOT-TEST:BUMP" WRITE-ALL
   s" src/nested/dep.f" A-PATH s" require src/leaf.f" WRITE-ALL
   s" src/leaf.f" B-PATH s" 10 SOURCE-ROOT-TEST:BUMP" WRITE-ALL
   s" main.f" B-PATH s" require src/leaf.f" WRITE-ALL
   s" boom.f" A-PATH s" 9132 throw" WRITE-ALL
   s" ." s" habu-root-fallback" MAKE-TEMP-DIR FALLBACK FALLBACK-U COPY!
   FALLBACK$ CLEANUP-TREE+
   s" leaf.f" FALLBACK-PATH s" 100 SOURCE-ROOT-TEST:BUMP" WRITE-ALL
   SB-RESET s" require " SB-APPEND
   s" leaf.f" FALLBACK-PATH SB-APPEND
   s" shared.f" FALLBACK-PATH SB$ WRITE-ALL
   FALLBACK$ A-PATH MAKE-DIRS
   s" leaf.f" FALLBACK-PATH A-PATH s" 1000 SOURCE-ROOT-TEST:BUMP" WRITE-ALL
   SB-RESET S\" require src/nested/dep.f\nrequire src/leaf.f\nrequire " SB-APPEND
   s" shared.f" FALLBACK-PATH SB-APPEND
   s" main.f" A-PATH SB$ WRITE-ALL ;

: LOAD-ENTRIES ( -- )
   s" main.f" A-PATH script-required
   LOADED @ 101 T= RESTORED
   s" main.f" B-PATH script-required
   LOADED @ 111 T= RESTORED ;

: ALIASES ( -- )
   s" src/leaf.f" A-PATH TARGET TARGET-U COPY!
   s" src/alias.f" A-PATH LINK LINK-U COPY!
   TARGET TARGET-U @ LINK LINK-U @ MAKE-SYMLINK
   s" src/./nested/../leaf.f" A-PATH required
   LINK LINK-U @ required
   TARGET TARGET-U @ required
   LOADED @ 111 T=
   LINK LINK-U @ included
   TARGET TARGET-U @ included
   LOADED @ 113 T= RESTORED ;

: PROVIDED-MISSING ( -- )
   s" absent.f" A-PATH provided
   s" ./absent.f" A-PATH required
   RESTORED ;

: THROW-LOAD ( -- ) s" boom.f" A-PATH included ;

: THROW-RESTORES ( -- )
   [: THROW-LOAD ;] 9132 TTHROWSQ
   RESTORED ;

: DISCOVERY ( -- )
   s" main.f" A-PATH DISCOVER:RUN
   EVENT-COUNT 3 T=
   0 EVENT-PATH@ s" src/nested/dep.f" A-PATH T$=
   0 SOURCE-EVENT:ROOT@ A$ T$=
   2 EVENT-PATH@ s" shared.f" FALLBACK-PATH CANONICAL TTRUE T$=
   2 SOURCE-EVENT:ROOT@ CWD$ T$=
   s" src/nested/dep.f" A-PATH A$ RUN-IN
   EVENT-COUNT 1 T=
   0 EVENT-PATH@ s" src/leaf.f" A-PATH T$=
   0 SOURCE-EVENT:ROOT@ A$ T$=
   RESTORED ;

: CLOSURE ( -- )
   s" main.f" A-PATH BUILD
   EC:COUNT 5 T=
   0 PATH$ s" main.f" A-PATH T$=
   4 PATH$ s" leaf.f" FALLBACK-PATH CANONICAL TTRUE T$=
   RESTORED ;

\ Each file resumes after its child. Distinct entry/exit ordinals expose
\ overwritten parent source bytes as well as missing or out-of-order loads.
40 constant DEEP-COUNT
variable ACTIVE
variable VISITS

public

: ENTER ( n -- )
   ACTIVE @ T= 1 ACTIVE +! 1 VISITS +!
   CURRENT$ A$ T$=
   SCRIPT-NAMED-LOAD? TFALSE ;

: LEAVE-FRAME ( n -- )
   -1 ACTIVE +! ACTIVE @ T=
   CURRENT$ A$ T$= ;

\ Unlike nested loads, these evaluations have no intervening catch: one throw
\ must unwind every escaped evaluator frame, including changed package/usings.
: EVAL-DOWN ( n -- n )
   dup 0= if drop 7 exit then
   s" 1- SOURCE-ROOT-TEST:EVAL-DOWN 1+" INCLUDE-EVALUATE ;

: EVAL-FAIL ( n -- n )
   dup 0= if
      s" ;package package EVAL-ESCAPED using FMT 9133 throw" INCLUDE-EVALUATE
   then
   s" 1- SOURCE-ROOT-TEST:EVAL-FAIL" INCLUDE-EVALUATE ;

private

: DEEP-NAME+ ( ptr u8 n n -- ) {: ix:n :}
   SB-APPEND s" /" SB-APPEND ix FMT:SB-U s" .f" SB-APPEND ;

: DEEP-PATH ( ptr u8 n n -- ptr u8 n )
   SB-RESET DEEP-NAME+ SB$ A-PATH ;

: DEEP-MAKE ( ptr u8 n bool -- ) {: prefix:ptr prefixu:n fail:bool :}
   prefix prefixu A-PATH MAKE-DIRS
   DEEP-COUNT 0 ?do
      prefix prefixu i DEEP-PATH TARGET TARGET-U COPY!
      SB-RESET i FMT:SB-U S\"  SOURCE-ROOT-TEST:ENTER\n" SB-APPEND
      i DEEP-COUNT 1- < if
         S\" s\" " SB-APPEND prefix prefixu i 1+ DEEP-NAME+
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
      i FMT:SB-U S\"  SOURCE-ROOT-TEST:LEAVE-FRAME\n" SB-APPEND
      TARGET TARGET-U @ SB$ WRITE-ALL
   loop ;

: DEEP-THROW ( -- )
   A$ [: s" bad/0.f" included ;] WITH ;

: DEEP-GOOD ( -- )
   A$ [: s" good/0.f" required ;] WITH ;

: DEEP-EVALS ( -- )
   40 EVAL-DOWN 47 T=
   [: 40 EVAL-FAIL drop ;] 9133 TTHROWSQ
   RESTORED
   s" public : AFTER-DEEP-EVAL ( -- n ) 17 ;" INCLUDE-EVALUATE
   s" SOURCE-ROOT-TEST:AFTER-DEEP-EVAL 17 T=" INCLUDE-EVALUATE
   40 EVAL-DOWN 47 T= ;

: DEEP-LOADS ( -- )
   s" deeply nested throws restore the caller before another dependency chain" T-LABEL
   s" shared.f" A-PATH s" 7 SOURCE-ROOT-TEST:BUMP" WRITE-ALL
   s" bad" true DEEP-MAKE
   s" good" false DEEP-MAKE
   0 ACTIVE ! 0 VISITS ! 0 LOADED !
   [: DEEP-THROW ;] 9132 TTHROWSQ
   VISITS @ DEEP-COUNT T= RESTORED
   0 ACTIVE ! 0 VISITS !
   DEEP-GOOD
   ACTIVE @ 0 T= VISITS @ DEEP-COUNT T= LOADED @ 7 T= RESTORED
   DEEP-GOOD
   VISITS @ DEEP-COUNT T= LOADED @ 7 T= RESTORED ;

: RUN ( -- )
   T-RESET PREP
   LOAD-ENTRIES ALIASES PROVIDED-MISSING THROW-RESTORES DISCOVERY CLOSURE DEEP-LOADS DEEP-EVALS
   CLEANUP-RUN T-REPORT ;

RUN
;package
;using
;using
;using
