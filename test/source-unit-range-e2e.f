\ Run as stdin: bin/hb < test/source-unit-range-e2e.f
\ Execute one owned file in two byte ranges through the real require loader.
\ Stdin leaves no outer source-unit callback when the escaped q is checked.
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/core/include.f
require src/habu/source-exec.f
require src/habu/interpret.f

package SOURCE-UNIT-RANGE-TEST
private

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
15000 constant LONG-SIZE
create LONG-SOURCE LONG-SIZE allot
variable PARENT-RUNS
variable CHILD-RUNS
variable LONG-PARENT-RUNS
variable LONG-CHILD-RUNS
variable NESTED-RUNS
variable HELPER-RUNS
variable READ-PARENT-RUNS
variable READ-CHILD-RUNS
variable READ-ATTEMPTS
variable OVERRIDE-RUNS
variable PREFIX-END
variable NEXT-OFF
variable BALANCED
variable SEEN
variable SEEN-KIND
variable RETURN-CUT
variable CUT-U
variable PRIVATE-HIT
variable PRIVATE-RUNS
variable TICK-SEEN
variable BAD-MODE
variable DIRECT-VALUE
variable DIRECT-MODE
TYPED-VARIABLE RANGE-Q [ n n -- ]

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n bytes:ptr size:n :}
   ROOT$ name nameu PATH JOIN-PATH PATH swap bytes size WRITE-ALL ;

public
: PARENT-BUMP ( -- )
   SOURCE-ROOT:ACTIVE-SOURCE {: path:ptr pathu:n root:ptr rootu:n raw:ptr rawu:n :}
   path pathu s" /parent.f" ENDS-WITH? if raw 2 s" #!" T$= then
   1 PARENT-RUNS +! ;
: CHILD-BUMP ( -- )
   SOURCE-ROOT:ACTIVE-SOURCE {: path:ptr pathu:n root:ptr rootu:n raw:ptr rawu:n :}
   raw 2 s" #!" T$=
   1 CHILD-RUNS +! ;
: LONG-PARENT-BUMP ( -- ) 1 LONG-PARENT-RUNS +! ;
: LONG-CHILD-BUMP ( -- ) 1 LONG-CHILD-RUNS +! ;
: HELPER-BUMP ( -- ) 1 HELPER-RUNS +! ;
: READ-PARENT-BUMP ( -- ) 1 READ-PARENT-RUNS +! ;
: READ-CHILD-BUMP ( -- ) 1 READ-CHILD-RUNS +! ;
: OVERRIDE-BUMP ( -- )
   SOURCE-ROOT:ACTIVE-SOURCE {: path:ptr pathu:n root:ptr rootu:n raw:ptr rawu:n :}
   raw 2 s" #!" T$=
   1 OVERRIDE-RUNS +! ;
: DIRECT-BUMP ( -- ) 1 DIRECT-VALUE +! ;
: PRIVATE-BUMP ( -- ) 1 PRIVATE-RUNS +! ;
: DIRECT-RANGE ( ptr u8 n n n -- )
   DIRECT-MODE @ 1 = if INCLUDE-EVALUATE-RANGE else OUTER:INTERPRET-RANGE then ;
: DIRECT-INNER ( -- )
   s" SOURCE-UNIT-RANGE-TEST:DIRECT-BUMP" dup >r 0 r>
      DIRECT-RANGE ;
\ Deliberately carry an undeclared return-stack cell through a loader token.
TRUSTED: PUSH-R ( -- ) 1 >r ;
TRUSTED: POP-R ( -- ) r> drop ;
private

: READ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n root:ptr rootu:n :}
   path pathu s" /override.f" ENDS-WITH? if
      S\" #! custom source\nSOURCE-UNIT-RANGE-TEST:OVERRIDE-BUMP\n" exit
   then
   path pathu s" /acquire.f" ENDS-WITH? if
      1 READ-ATTEMPTS +!
      s" read-child.f" required
      7136 throw
   then
   path pathu root rootu SOURCE-ROOT:READ-OS ;

\ A top-level resolution row on the open package's private leg that holds a
\ record.
: PRIVATE-LEG ( ptr u8 n -- )
   drop {: row:ptr :}
   row CHECKER-OWNER-ABI:RES-ROLE cells + CELL-VIEW @
      CHECKER-OWNER-ABI:RES-ROLE-OPEN-PRIVATE = if
      row CHECKER-OWNER-ABI:RES-RECORD cells + CELL-VIEW @ 0< 0= if
         -1 PRIVATE-HIT !
      then
   then ;

: BEFORE ( n n n n ptr u8 n -- n bool ) {: unit:n op:n rec:n entry:n spelling:ptr spelling-u:n :}
   op rec entry SOURCE-UNIT:LOADER-KIND {: kind:n :}
   unit 20 = if
      op SOURCE-EXEC:TICK = if
         spelling spelling-u s" SOURCE-UNIT-RANGE-TEST:HELPER-BUMP" CORE-STR= TTRUE
         1 TICK-SEEN +!
      then
   then
   unit 19 = if
      op SOURCE-EXEC:WORD = if
         spelling spelling-u s" HELPER" CORE-STR= if
            spelling spelling-u ['] PRIVATE-LEG CHECKER-RESOLUTION:TOP
         then
      then
   then
   kind SOURCE-UNIT:PARSED-PATH = if
      spelling spelling-u s" require" CORE-STR= if true else
         spelling spelling-u s" include" CORE-STR=
      then TTRUE
   then
   kind SEEN-KIND !
   kind SOURCE-UNIT:NONE <> if
      unit 18 = if
         SOURCE-EXEC:LOADER-CUT? 0=
         SOURCE-EXEC:BOUNDARY? 0= and RETURN-CUT !
      then
   then
   0 true ;
TRUSTED: AFTER ( n n n -- ) {: unit:n ticket:n rc:n :}
   rc 0= SEEN @ 0= and if
      SOURCE-UNIT:ACTION$ {: mode:n path:ptr pathu:n :}
      mode SOURCE-UNIT:LOAD-REQUIRE T=
      path pathu s" child.f" T$=
      SOURCE-EXEC:POSITION {: off:n balanced:bool :}
      off NEXT-OFF ! balanced BALANCED !
      1 SEEN !
   then ;

: EXEC-PREFIX ( -- ) 0 PREFIX-END @ RANGE-Q @ execute ;
: EXEC-CUT ( -- ) 0 CUT-U @ RANGE-Q @ execute ;

TRUSTED: LOAD ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr size:n q :}
   path pathu s" /parent.f" ENDS-WITH? if
      q RANGE-Q !
      S\" #! parent\nrequire child.f\n" nip PREFIX-END !
      17 source PREFIX-END @ [: BEFORE ;] [: AFTER ;]
         [: EXEC-PREFIX ;] SOURCE-EXEC:RUN {: rc:n :}
      rc 0<> if rc throw then
      NEXT-OFF @ size q execute
      exit
   then
   path pathu s" /child.f" ENDS-WITH? if
      0 size RANGE-Q @ execute
      1 NESTED-RUNS +!
      exit
   then
   path pathu s" /return-cut.f" ENDS-WITH? if
      q RANGE-Q ! size CUT-U !
      18 source size [: BEFORE ;] [: AFTER ;]
         [: EXEC-CUT ;] SOURCE-EXEC:RUN {: rc:n :}
      rc 0<> if rc throw then
      exit
   then
   path pathu s" /private-public.f" ENDS-WITH? if
      q RANGE-Q ! size CUT-U !
      19 source size [: BEFORE ;] [: 2drop drop ;]
         [: EXEC-CUT ;] SOURCE-EXEC:RUN {: rc:n :}
      rc 0<> if rc throw then
      exit
   then
   path pathu s" /tick.f" ENDS-WITH? if
      q RANGE-Q ! size CUT-U !
      20 source size [: BEFORE ;] [: 2drop drop ;]
         [: EXEC-CUT ;] SOURCE-EXEC:RUN {: rc:n :}
      rc 0<> if rc throw then
      exit
   then
   path pathu s" /long-parent.f" ENDS-WITH? if
      s" long.f" required
      [: s" long-throw.f" required ;] 7136 TTHROWSQ
      0 size q execute
      exit
   then
   path pathu s" /trigger.f" ENDS-WITH? if
      SOURCE-UNIT:LOAD-REQUIRE s" helper.f" SOURCE-UNIT:LOAD-PATH
      0 size q execute
      exit
   then
   path pathu s" /trigger-include.f" ENDS-WITH? if
      SOURCE-UNIT:LOAD-INCLUDE s" helper.f" SOURCE-UNIT:LOAD-PATH
      0 size q execute
      exit
   then
   path pathu s" /read-parent.f" ENDS-WITH? if
      [: s" acquire.f" included ;] 7136 TTHROWSQ
      [: s" acquire.f" included ;] 7136 TTHROWSQ
      0 size q execute
      exit
   then
   path pathu s" /bad.f" ENDS-WITH? if
      BAD-MODE @ 1 = if -1 0 q execute exit then
      BAD-MODE @ 2 = if 0 size 1+ q execute exit then
      BAD-MODE @ 3 = if 1 0 q execute exit then
      0 0 q execute
   then
   0 size q execute ;

: CHECK-PARENT ( -- )
   0 NEXT-OFF ! 0 BALANCED ! 0 SEEN ! 0 SEEN-KIND !
   s" parent.f" required
   SEEN @ 1 T=
   SEEN-KIND @ SOURCE-UNIT:PARSED-PATH T=
   \ EMIT-TOK leaves INP on the delimiter after child.f; resume there so the
   \ range evaluator consumes that newline before the following word.
   NEXT-OFF @ PREFIX-END @ 1- T=
   BALANCED @ TTRUE
   PARENT-RUNS @ 1 T=
   CHILD-RUNS @ 1 T=
   NESTED-RUNS @ 1 T=
   s" SOURCE-UNIT-RANGE-CHILD:NUMBER 317 T=" evaluate-closed
   s" parent.f" required
   PARENT-RUNS @ 1 T=
   CHILD-RUNS @ 1 T=
   s" SOURCE-UNIT-RANGE-CHILD:NUMBER 317 T=" evaluate-closed ;

: CHECK-RETURN-CUT ( -- )
   0 RETURN-CUT !
   s" return-cut.f" included
   RETURN-CUT @ TTRUE
   s" SOURCE-UNIT-RANGE-INCLUDE:NUMBER 419 T=" evaluate-closed ;

: CHECK-PRIVATE-PUBLIC ( -- )
   0 PRIVATE-HIT !  0 PRIVATE-RUNS !
   [: 2drop ;] CHECKER-RESOLUTION:NS-INSTALL
   [: s" private-public.f" included ;]
      [: CHECKER-RESOLUTION:NS-DEFAULT ;] finally
   PRIVATE-RUNS @ 1 T=
   PRIVATE-HIT @ TTRUE ;

: CHECK-TICK ( -- )
   0 TICK-SEEN !
   s" tick.f" included
   TICK-SEEN @ 1 T= ;

: LOAD-BAD ( -- ) s" bad.f" included ;
: CHECK-BOUNDS ( -- )
   1 BAD-MODE !
   [: LOAD-BAD ;] INCLUDE-RANGE-RC TTHROWSQ
   PARENT-RUNS @ 1 T=
   2 BAD-MODE !
   [: LOAD-BAD ;] INCLUDE-RANGE-RC TTHROWSQ
   PARENT-RUNS @ 1 T=
   3 BAD-MODE !
   [: LOAD-BAD ;] INCLUDE-RANGE-RC TTHROWSQ
   PARENT-RUNS @ 1 T=
   0 BAD-MODE !
   LOAD-BAD LOAD-BAD
   PARENT-RUNS @ 3 T= ;

\ Direct callers have no enclosing loader to close the program stack or
\ validate the span. Their checked renderer must supply both boundaries.
: DIRECT-LEAK ( -- ) s" 123 " 0 4 DIRECT-RANGE ;
: DIRECT-DROP ( -- ) s" drop" 0 4 DIRECT-RANGE ;
: CHECK-DIRECT ( -- )
   0 DIRECT-VALUE !
   s" 7136 throw SOURCE-UNIT-RANGE-TEST:DIRECT-BUMP" dup >r 11 r>
      DIRECT-RANGE
   DIRECT-VALUE @ 1 T=
   s" SOURCE-UNIT-RANGE-TEST:DIRECT-INNER SOURCE-UNIT-RANGE-TEST:DIRECT-BUMP"
      dup >r 0 r> SOURCE-ROOT:RUN-EVAL-RANGE
   DIRECT-VALUE @ 3 T=
   [: s" 42 . " drop 0 0 5 DIRECT-RANGE ;]
      INCLUDE-RANGE-RC TTHROWSQ
   [: s" 9 . " 3 1 DIRECT-RANGE ;]
      INCLUDE-RANGE-RC TTHROWSQ
   [: s" 9 . " -1 1 DIRECT-RANGE ;]
      INCLUDE-RANGE-RC TTHROWSQ
   s" " 0 0 DIRECT-RANGE
   [: DIRECT-LEAK ;] E-EVAL-RESIDUE TTHROWSQ
   913 [: DIRECT-DROP ;] 70 TTHROWSQ 913 T=
   s" SOURCE-UNIT-RANGE-TEST:DIRECT-BUMP" dup >r 0 r>
      DIRECT-RANGE
   DIRECT-VALUE @ 4 T= ;

: LOAD-FAIL ( -- ) s" fail.f" included ;
: ESCAPED ( -- ) 0 0 RANGE-Q @ execute ;
: CHECK-PATH ( -- )
   s" trigger.f" required s" trigger.f" required
   HELPER-RUNS @ 1 T=
   s" trigger-include.f" included
   HELPER-RUNS @ 2 T= ;
: CHECK-FAIL ( -- )
   [: LOAD-FAIL ;] 7136 TTHROWSQ
   CHILD-RUNS @ 1 T=
   s" fail.f"
      S\" require child.f\nSOURCE-UNIT-RANGE-TEST:PARENT-BUMP\n" PUT
   LOAD-FAIL LOAD-FAIL
   CHILD-RUNS @ 1 T=
   PARENT-RUNS @ 5 T= ;

: CHECK-READ-FAIL ( -- )
   s" read-parent.f" required
   READ-PARENT-RUNS @ 1 T=
   READ-CHILD-RUNS @ 1 T=
   READ-ATTEMPTS @ 2 T=
   s" read-parent.f" required
   READ-PARENT-RUNS @ 1 T= ;

: CHECK-LONG ( -- )
   s" long-parent.f" required
   LONG-CHILD-RUNS @ 1 T=
   LONG-PARENT-RUNS @ 1 T= ;

: CHECK-OVERRIDE ( -- )
   s" override.f" required
   OVERRIDE-RUNS @ 1 T= ;

: CHECK-SHEBANG-EOF ( -- )
   s" bang.f" required ;

: LONG-FILES ( -- )
   LONG-SIZE 0 ?do 10 LONG-SOURCE i + c! loop
   S\" SOURCE-UNIT-RANGE-TEST:LONG-CHILD-BUMP\n" LONG-SOURCE swap BYTE-COPY
   s" long.f" LONG-SOURCE LONG-SIZE PUT
   S\" 7136 throw\n" LONG-SOURCE swap BYTE-COPY
   s" long-throw.f" LONG-SOURCE LONG-SIZE PUT ;

: FILES ( -- )
   s" child.f" S\" #! child\nSOURCE-UNIT-RANGE-TEST:CHILD-BUMP\npackage SOURCE-UNIT-RANGE-CHILD public : NUMBER ( -- n ) 317 ; ;package\n" PUT
   s" cut-child.f" S\" package SOURCE-UNIT-RANGE-INCLUDE public : NUMBER ( -- n ) 419 ; ;package\n" PUT
   s" return-cut.f" S\" SOURCE-UNIT-RANGE-TEST:PUSH-R include cut-child.f SOURCE-UNIT-RANGE-TEST:POP-R\n" PUT
   s" private-public.f" S\" package SOURCE-UNIT-RANGE-PRIVATE private : HELPER ( -- ) SOURCE-UNIT-RANGE-TEST:PRIVATE-BUMP ; public HELPER ;package\n" PUT
   s" tick.f" S\" ' SOURCE-UNIT-RANGE-TEST:HELPER-BUMP drop\n" PUT
   s" parent.f"
      S\" #! parent\nrequire child.f\nSOURCE-UNIT-RANGE-TEST:PARENT-BUMP\n" PUT
   s" bad.f" S\" SOURCE-UNIT-RANGE-TEST:PARENT-BUMP\n" PUT
   s" trigger.f" S\" \n" PUT
   s" trigger-include.f" S\" \n" PUT
   s" helper.f" S\" SOURCE-UNIT-RANGE-TEST:HELPER-BUMP\n" PUT
   s" read-parent.f" S\" SOURCE-UNIT-RANGE-TEST:READ-PARENT-BUMP\n" PUT
   s" read-child.f" S\" SOURCE-UNIT-RANGE-TEST:READ-CHILD-BUMP\n" PUT
   s" acquire.f" S\" \n" PUT
   s" override.f" S\" NO-SUCH-OVERRIDE-WORD\n" PUT
   s" bang.f" s" #!" PUT
   s" fail.f" S\" require child.f\n7136 throw\n" PUT
   s" long-parent.f" S\" SOURCE-UNIT-RANGE-TEST:LONG-PARENT-BUMP\n" PUT
   LONG-FILES ;

: LOADER ( -- )
   ['] LOAD SOURCE-UNIT:USE
   [: SOURCE-ROOT:CANON-OS ;] [: READ ;] SOURCE-INPUT:USE
   ROOT$ [: CHECK-PARENT CHECK-RETURN-CUT CHECK-PRIVATE-PUBLIC CHECK-TICK CHECK-BOUNDS CHECK-FAIL CHECK-PATH CHECK-READ-FAIL CHECK-LONG CHECK-OVERRIDE CHECK-SHEBANG-EOF ;]
      SOURCE-ROOT:WITH ;

: RESET ( -- ) SOURCE-UNIT:RESET SOURCE-INPUT:RESET ;

public
TRUSTED: CHECK-ABSENT ( -- )
   SOURCE-EXEC:POSITION {: off:n balanced:bool :}
   off 0 T= balanced TFALSE ;

TRUSTED: RUN ( -- )
   T-RESET
   s" source-unit-range" FS:MKTEMP-DIR {: dir:ptr size:n :}
   dir ROOT size BYTE-COPY size ROOT-U !
   FILES
   1 DIRECT-MODE ! CHECK-DIRECT
   2 DIRECT-MODE ! CHECK-DIRECT
   [: LOADER ;] [: RESET ;] finally
   CHECK-ABSENT
   [: ESCAPED ;] INCLUDE-RANGE-RC TTHROWSQ
   ROOT$ s" result.txt" PATH JOIN-PATH
   PATH swap S\" source-unit-range: ok\n" WRITE-ALL
   s" source unit range tree: " type ROOT$ type cr
   T-REPORT ;
;package

SOURCE-UNIT-RANGE-TEST:RUN
