\ Build the suite's compiler bundle from one target source load. Its source
\ tail checks the early origins of a split unit and the newly installed target
\ checker, whose temporary definitions are forgotten before WINDOW-CLOSE.
require src/lib/test.f
require src/lib/string.f
require src/lib/source.f
require src/habu/source-exec.f
require src/compiler/native/host.f
require tools/native-build-core.f

package NATIVE-BUILD
private

DYNAMIC-BUFFER EARLY-BYTES u8
variable EARLY-U
TYPED-VARIABLE SPLIT-PRIOR [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ]
TYPED-VARIABLE SPLIT-Q [ n n -- ]
variable SPLIT-AT
variable SPLIT-END
variable SPLIT-SEEN
variable SPLIT-EVENTS
variable SPLIT-POS-OK
CAST: ORIGIN-AT-XT ( n -- [ ptr u8 n n -- n bool ] )

: EARLY-ROOM ( n -- ptr u8 ) EARLY-BYTES-RESERVE 0 EARLY-BYTES ;

: EARLY-TOKEN ( ptr u8 n n -- n )
   {: needle:ptr needleu:n skip:n :}
   0 EARLY-BYTES EARLY-U @ needle needleu FIND-SUB MATCH option
      none OF s" native bundle: early token missing" 76 die ENDOF
      some OF IDX>N skip + ENDOF
   ;MATCH ;

: EARLY-ORIGIN ( ptr u8 n n ptr u8 n -- )
   {: needle:ptr needleu:n skip:n path:ptr pathu:n :}
   needle needleu skip EARLY-TOKEN
      path pathu rot
      s" ENGINE-ORIGIN:SYMBOL-AT" OPEN-TARGET-XT ORIGIN-AT-XT execute
      {: sym:n found:bool :}
   found TTRUE  sym 0 > TTRUE ;

: EARLY-ORIGINS ( -- )
   T-RESET
   s" src/core/util.f" SOURCE-ROOT:CANONICAL
      {: path:ptr pathu:n found:bool :}
   found TTRUE
   path pathu [: EARLY-ROOM ;] SOURCE:READ-WHOLE-FILE EARLY-U !
   s" prefix variables, create and constant keep their name coordinates" T-LABEL
   s" variable IMK-NDICT0" 9 path pathu EARLY-ORIGIN
   s" create REG-PROT-IDX" 7 path pathu EARLY-ORIGIN
   s" 5 constant DICT-WORDLIST-SLOT" 11 path pathu EARLY-ORIGIN
   SPLIT-SEEN @ 0 > TTRUE
   SPLIT-EVENTS @ 0 > TTRUE
   SPLIT-POS-OK @ 0 > TTRUE
   T-REPORT
   EARLY-BYTES-RELEASE ;

: SPLIT-BEFORE ( n n n n ptr u8 n -- n bool )
   {: unit:n op:n rec:n entry:n spelling:ptr spelling-u:n :}
   unit drop rec drop entry drop spelling drop spelling-u drop
   op SOURCE-EXEC:WORD = if 1 SPLIT-EVENTS +! then
   -1 false ;

: SPLIT-AFTER ( n n n -- )
   {: unit:n ticket:n rc:n :}
   unit drop ticket drop
   rc 0= if
      SOURCE-EXEC:POSITION {: pos:n balanced:bool :}
      balanced pos 0 >= and pos SPLIT-END @ SPLIT-AT @ - <= and if
         1 SPLIT-POS-OK +!
      then
   then ;

: SPLIT-REST ( -- ) SPLIT-AT @ SPLIT-END @ SPLIT-Q @ execute ;

: FIRST-LINE-END ( ptr u8 n -- n )
   {: source:ptr sourceu:n :}
   0 begin dup sourceu < while
      source over + c@ 10 = if 1+ exit then
      1+
   repeat ;

: SPLIT-UNIT ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   path pathu s" /src/core/util.f" ENDS-WITH? 0= if
      path pathu root rootu source sourceu q SPLIT-PRIOR @ execute exit
   then
   1 SPLIT-SEEN +!
   source sourceu FIRST-LINE-END {: cut:n :}
   cut sourceu >= if s" native bundle: no source split" 76 die then
   q SPLIT-Q !  cut SPLIT-AT !  sourceu SPLIT-END !
   0 cut q execute
   63 source cut + sourceu cut - ['] SPLIT-BEFORE ['] SPLIT-AFTER
      [: SPLIT-REST ;] SOURCE-EXEC:RUN {: rc:n :}
   rc 0<> if rc throw then ;

: RESTORE-FULL-ORIGIN ( -- )
   s" ENGINE-ORIGIN:FILE-DRAIN" OPEN-TARGET-XT SOURCE-RESET-XT execute
   s" ENGINE-ORIGIN:INSTALL" OPEN-TARGET-XT SOURCE-RESET-XT execute ;

: ORIGIN-INSTALL-CASE ( -- )
   s" ENGINE-ORIGIN-EARLY:INSTALL" OPEN-TARGET-XT SOURCE-RESET-XT execute
   s" ENGINE-ORIGIN-EARLY:INSTALL" OPEN-TARGET-XT SOURCE-RESET-XT execute
   [: s" package EARLY-INSTALL-PROBE private CAST: ID-N ( n -- n ) public : VALUE ( -- n ) 42 ID-N ; ;package : EARLY-INSTALL-CHECK ( -- ) EARLY-INSTALL-PROBE:VALUE 42 <> if 70 throw then ; EARLY-INSTALL-CHECK"
      evaluate-closed ;]
      [: RESTORE-FULL-ORIGIN ;] finally ;

: SPLIT-BUILD ( -- )
   BUNDLE-DIR @ BUNDLE-DIR-U @ RUN-BUNDLE-READY ;

: SPLIT-CLOSE ( -- ) SPLIT-PRIOR @ SOURCE-UNIT:USE ;

: BUNDLE-HANDLE ( ptr u8 n -- n n )
   XREF-FIND DEF-OCC:SELECT ;

: BUNDLE-TARGET-ENTRY ( -- n )
   s" NATIVE-RUNTIME:BUNDLE-HOST-ABS" XREF-FIND XREF-START ;

CAST: TARGET-PATH-XT ( n -- [ ptr u8 n -- ] )

: BUNDLE-HOST-CHECK ( -- )
   T-RESET
   \ This is compiled by the target compiler installed at the end of
   \ native-runtime.f, while `abs` belongs to the retained host producer.
   s" package NATIVE-RUNTIME public : BUNDLE-HOST-MARK ( -- ) ; : BUNDLE-HOST-ABS ( n -- n ) abs ; ;package"
      evaluate-closed
   s" fresh target facts associate with a retained host implementation" T-LABEL
   BUNDLE-TARGET-ENTRY AOT-ARM:B0 @ >= TTRUE
   BUNDLE-TARGET-ENTRY cp@ < TTRUE
   BUNDLE-TARGET-ENTRY NHOST:ID-OF 0 T=
   s" abs" XREF-FIND XREF-START NHOST:ID-OF 0 > TTRUE
   s" NATIVE-RUNTIME:BUNDLE-HOST-ABS" BUNDLE-HANDLE
   s" abs" BUNDLE-HANDLE NHOST:ASSOCIATE
   ORIGIN-INSTALL-CASE
   s" NATIVE-RUNTIME:BUNDLE-HOST-MARK"
      s" FORGET-DEFS-FROM" OPEN-TARGET-XT TARGET-PATH-XT execute
   s" NATIVE-RUNTIME:BUNDLE-HOST-ABS" XREF-FIND XREF-FOUND? 0= TTRUE
   T-REPORT ;

\ native-donor-map reads these fixtures' words from the built engine.
: MAP-ORIGIN-TAIL ( -- )
   EARLY-ORIGINS
   s" test/compiler/native-map-b.f" included
   s" test/compiler/native-map-c.f" included
   BUNDLE-HOST-CHECK ;

public

: BUILD-BUNDLE ( ptr u8 n -- )
   BUILD-TARGET:IDLE-CK
   DEFAULT-SOURCE-POLICY
   ['] MAP-ORIGIN-TAIL SOURCE-TAIL!
   SOURCE-UNIT:CURRENT SPLIT-PRIOR !
   ['] SPLIT-UNIT SOURCE-UNIT:USE
   BUNDLE-DIR-U ! BUNDLE-DIR !
   [: SPLIT-BUILD ;] [: SPLIT-CLOSE ;] finally ;

;package
