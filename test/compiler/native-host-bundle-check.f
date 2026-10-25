\ Exercise the newly installed target checker while the suite's one compiler
\ source bundle is loading. The temporary target definition is forgotten before
\ WINDOW-CLOSE, so no test definition enters any emitted engine image.
require src/lib/test.f
require src/lib/fs.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-id.f
require src/lib/fmt.f
require src/habu/aot-file.f
require src/lib/source.f
require src/habu/source-exec.f
require src/compiler/native/host.f
require tools/native-build-core.f

package NATIVE-BUILD
private

create MAP-ORIGIN-PATH FS-PATH-CAP allot
DYNAMIC-BUFFER MAP-ORIGIN-TEXT u8
create SEED-IMAGE FS-PATH-CAP allot
create SEED-ART FS-PATH-CAP allot
create SEED-KEY 32 allot
create SEED-HASH SHA256-FILE-CTX-BYTES allot
$10000 constant SEED-OUTPUT-CAP
create SEED-OUT SEED-OUTPUT-CAP allot
create SEED-ERR SEED-OUTPUT-CAP allot
variable SEED-CAP-OFF
variable SEED-PTR-OFF
variable SEED-ROW-BYTES
variable SEED-CAP
variable SEED-LINK-OFF
variable SEED-LINK-CELL
variable SEED-LINK-SHIFT
variable SEED-HIGH
variable SEED-FULL
variable SEED-AT
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

: MAP-ORIGIN-CHECK ( ptr u8 n -- )
   {: dir:ptr diru:n :}
   T-RESET
   dir diru s" hb-root.map" MAP-ORIGIN-PATH JOIN-PATH {: pathu:n :}
   MAP-ORIGIN-PATH pathu FILE-SIZE {: size:n :}
   size MAP-ORIGIN-TEXT-RESERVE
   MAP-ORIGIN-PATH pathu 0 MAP-ORIGIN-TEXT BYTE-VIEW size READ-ALL size T=
   0 MAP-ORIGIN-TEXT BYTE-VIEW size {: map:ptr mapu:n :}
   s" retired records do not shift the next definitions' source paths" T-LABEL
   map mapu s" MAP-ORIGIN-A " CONTAINS? TFALSE
   map mapu s" MAP-ORIGIN-B word baked test/compiler/native-map-b.f" CONTAINS? TTRUE
   map mapu s" MAP-ORIGIN-C word baked test/compiler/native-map-c.f" CONTAINS? TTRUE
   map mapu s" MAP-ORIGIN-B alias baked test/compiler/native-map-c.f" CONTAINS? TTRUE
   map mapu S\" MAP-ORIGIN-D word baked -\n" CONTAINS? TTRUE
   T-REPORT ;

: BUNDLE-HANDLE ( ptr u8 n -- n n )
   XREF-FIND DEF-OCC:SELECT ;

: BUNDLE-TARGET-ENTRY ( -- n )
   s" NATIVE-RUNTIME:BUNDLE-HOST-ABS" XREF-FIND XREF-START ;

CAST: TARGET-PATH-XT ( n -- [ ptr u8 n -- ] )
CAST: TARGET-N-XT ( n -- [ -- n ] )

: TARGET-N ( ptr u8 n -- n )
   OPEN-TARGET-XT TARGET-N-XT execute ;

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

: MAP-ORIGIN-TAIL ( -- )
   EARLY-ORIGINS
   s" test/compiler/native-map-a.f" included
   s" test/compiler/native-map-b.f" included
   s" test/compiler/native-map-c.f" included
   s" package NATIVE-MAP-SEED-OFFSET public RECORD-LINK-CAP BYTE-VIEW data-base BYTE-VIEW - constant CAP-OFF RECORD-LINK-P BYTE-VIEW data-base BYTE-VIEW - constant PTR-OFF RECORD-LINK-ROW constant ROW-BYTES ;package"
      evaluate-closed
   s" NATIVE-MAP-SEED-OFFSET:CAP-OFF" TARGET-N SEED-CAP-OFF !
   s" NATIVE-MAP-SEED-OFFSET:PTR-OFF" TARGET-N SEED-PTR-OFF !
   s" NATIVE-MAP-SEED-OFFSET:ROW-BYTES" TARGET-N SEED-ROW-BYTES !
   BUNDLE-HOST-CHECK ;

: RUN-SEED-CHECK ( ptr u8 n ptr u8 n bool -- )
   {: dir:ptr diru:n image:ptr imageu:n growth:bool :}
   T-RESET
   dir diru image imageu SEED-IMAGE JOIN-PATH {: pathu:n :}
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" test/compiler/native-map-seed-check.f" >LEN PROC-ARGV+
   growth if
      s" --" >LEN PROC-ARGV+
      SB-RESET SEED-CAP @ FMT:SB-INT SB$ >LEN PROC-ARGV+
      SB-RESET SEED-HIGH @ FMT:SB-INT SB$ >LEN PROC-ARGV+
   then
   SEED-IMAGE pathu >LEN
   SEED-OUT SEED-OUTPUT-CAP >LEN
   SEED-ERR SEED-OUTPUT-CAP >LEN
   60000 >MS RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
      {: outu:len erru:len rc:n :}
   s" fresh bundle image keeps exact checker contracts" T-LABEL
   rc 0<> if SEED-OUT outu LEN>N type SEED-ERR erru LEN>N type then
   rc 0 T=
   T-REPORT ;

: MAP-SEED-CHECK ( ptr u8 n ptr u8 n -- )
   false RUN-SEED-CHECK ;

: WINDOW-OFF ( n -- n )
   AOT-ARM:D0 @ DATA-VA - - ;

: SCAN-VALUES ( [ n n -- ] -- )
   {: take :}
   0 SEED-AT !
   AOT-WINDOW:BM-LEN @ AOT-WINDOW:CELL-BITS * 0 ?do
      AOT-WINDOW:BM-BUF@ i AOT-WINDOW:CELL-BITS / + c@
      i AOT-WINDOW:CELL-BITS mod rshift 1 and 0<> if
         AOT-WINDOW:VAL-BUF@ SEED-AT @ +
         AOT-WINDOW:VAL-LEN @ SEED-AT @ -
         AOT-WINDOW:CELL-V@ {: value:n bytes:n :}
         bytes 0 <= if s" seed merge: malformed captured cell" 76 die then
         SEED-AT @ bytes + SEED-AT !
         i AOT-WINDOW:CELL-BYTES * value take execute
      then
   loop
   SEED-AT @ AOT-WINDOW:VAL-LEN @ <> if
      s" seed merge: captured cell values remain" 76 die then ;

: CAP-TAKE ( n n -- )
   {: off:n value:n :}
   off SEED-CAP-OFF @ WINDOW-OFF = if value SEED-CAP ! then ;

: LINK-TAKE ( n n -- )
   {: off:n value:n :}
   off SEED-LINK-CELL @ - {: rel:n :}
   rel 0 < rel SEED-CAP @ SEED-ROW-BYTES @ * >= or if exit then
   rel SEED-ROW-BYTES @ mod {: slot:n :}
   \ REG-PERSIST-BUF can start the table between grid cells. An occurrence
   \ then straddles two saved cells; inspect only its eight bytes.
   slot 0= if
      value SEED-LINK-SHIFT @ rshift 0= if exit then
   else
      slot AOT-WINDOW:CELL-BYTES <>
      SEED-LINK-SHIFT @ 0= or if exit then
      value AOT-WINDOW:CELL-BYTES AOT-WINDOW:CELL-BITS *
         SEED-LINK-SHIFT @ - lshift 0= if exit then
   then
   rel SEED-ROW-BYTES @ / SEED-HIGH ! ;

: LINK-BASE ( -- )
   \ The persisted pointer cell is excluded from the DATA value bitmap.
   \ Its XTOFF row names the captured table's DATA offset instead.
   -1 SEED-LINK-OFF !
   AOT-WINDOW:XTOFF-N @ 0 ?do
      AOT-WINDOW:XTOFF-BUF@ i AOT-WINDOW:XTOFF-ROW * +
         CELL-VIEW @ {: pair:n :}
      pair $FFFFFFFF and {: loc:n :}
      loc AOT-WINDOW:XTOFF-WINDOW-TAG and 0<>
      loc AOT-WINDOW:XTOFF-LOC-MASK and SEED-PTR-OFF @ WINDOW-OFF = and if
         pair 32 rshift $FFFFFFFF and {: meta:n :}
         meta AOT-WINDOW:XTOFF-KIND-MASK and
            AOT-WINDOW:XTOFF-DATA-TAG <> if
            s" seed merge: record link target is not DATA" 76 die then
         meta AOT-WINDOW:XTOFF-VALUE-MASK and dup 0= if
            s" seed merge: record link pointer is null" 76 die then
         1- {: target:n :}
         SEED-LINK-OFF @ 0 >= if
            s" seed merge: duplicate record link pointer" 76 die then
         target SEED-LINK-OFF !
      then
   loop
   SEED-LINK-OFF @ 0 < if
      s" seed merge: no record link pointer" 76 die then
   SEED-LINK-OFF @ AOT-WINDOW:CELL-BYTES 1- invert and SEED-LINK-CELL !
   SEED-LINK-OFF @ AOT-WINDOW:CELL-BYTES 1- and
      AOT-WINDOW:CELL-BITS * SEED-LINK-SHIFT ! ;

: CAPTURED-LINKS ( -- )
   \ Writer source loads may grow the live target table after sealing. Decode
   \ the saved capacity and canonical occurrence rows from the capture itself.
   AOT-BUF:AOT-REC-N @ SEED-FULL !
   -1 SEED-CAP !
   ['] CAP-TAKE SCAN-VALUES
   SEED-CAP @ 0 < if s" seed merge: no captured link capacity" 76 die then
   SEED-CAP @ AOT-BUF:AOT-REC-MAX > if
      s" seed merge: captured link cap " type SEED-CAP @ FMT:.INT
      s"  exceeds AOT record max " type AOT-BUF:AOT-REC-MAX FMT:.INT cr
      s" seed merge: no legal prefix can cross saved link capacity" 76 die then
   SEED-FULL @ SEED-CAP @ >= if
      s" seed merge: no legal prefix can cross saved link capacity" 76 die then
   LINK-BASE
   -1 SEED-HIGH !
   ['] LINK-TAKE SCAN-VALUES
   SEED-HIGH @ SEED-FULL @ 1- <> if
      s" seed merge: cap " type SEED-CAP @ FMT:.INT
      s"  records " type SEED-FULL @ FMT:.INT
      s"  highwater " type SEED-HIGH @ FMT:.INT cr
      s" seed merge: full capture link highwater differs from last row" 76 die then ;

: SAVE-FULL ( ptr u8 n -- )
   {: dir:ptr diru:n :}
   dir diru s" seed-runtime.aot" SEED-ART JOIN-PATH {: pathu:n :}
   AOT-IDENT:RESET
   s" test/compiler/native-host-bundle-check.f" AOT-IDENT:PATH+
   SEED-HASH ENGINE-ID:PATH$ SEED-KEY SHA256-FILE-IN 0<> if
      s" seed merge: cannot hash bundle producer" 76 die then
   SEED-KEY SEED-ART pathu AOT-FILE:WRITE ;

: GROWTH-CHILD ( ptr u8 n -- )
   {: dir:ptr diru:n :}
   dir diru s" seed-runtime.aot" SEED-ART JOIN-PATH {: pathu:n :}
   dir diru s" hb-root" SEED-IMAGE JOIN-PATH {: child-u:n :}
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" test/compiler/native-map-seed-merge.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   SEED-ART pathu >LEN PROC-ARGV+
   ENGINE-ID:PATH$ >LEN PROC-ARGV+
   dir diru >LEN PROC-ARGV+
   SB-RESET SEED-CAP @ SEED-FULL @ - FMT:SB-INT SB$ >LEN PROC-ARGV+
   SB-RESET SEED-CAP @ FMT:SB-INT SB$ >LEN PROC-ARGV+
   SEED-IMAGE child-u >LEN
   SEED-OUT SEED-OUTPUT-CAP >LEN
   SEED-ERR SEED-OUTPUT-CAP >LEN
   60000 >MS RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
      {: outu:len erru:len rc:n :}
   rc 0<> if
      SEED-OUT outu LEN>N type SEED-ERR erru LEN>N type
      s" seed merge: prefix child failed" rc die
   then ;

: SEED-GROWTH-CHECK ( ptr u8 n -- )
   {: dir:ptr diru:n :}
   CAPTURED-LINKS
   dir diru SAVE-FULL
   dir diru GROWTH-CHILD
   dir diru s" hb-seed-merged" true RUN-SEED-CHECK ;

public

: RUN-BUNDLE-HOST-CHECK ( ptr u8 n -- )
   2dup
   BUILD-TARGET:IDLE-CK
   DEFAULT-SOURCE-POLICY
   ['] MAP-ORIGIN-TAIL SOURCE-TAIL!
   SOURCE-UNIT:CURRENT SPLIT-PRIOR !
   ['] SPLIT-UNIT SOURCE-UNIT:USE
   BUNDLE-DIR-U ! BUNDLE-DIR !
   [: SPLIT-BUILD ;] [: SPLIT-CLOSE ;] finally
   2dup s" hb-whitebox-source" MAP-SEED-CHECK
   2dup s" hb-root" MAP-SEED-CHECK
   2dup SEED-GROWTH-CHECK
   MAP-ORIGIN-CHECK ;

;package
