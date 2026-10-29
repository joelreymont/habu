\ lsp-source-index.f - locate an image word in current engine source. The verifier
\ child scans the supplied bytes (CHECK:INDEX-BYTES) and returns surviving
\ declaration identities; this file matches them to the image's compiled origins.
\ An origin row is a binding locator (symbol, code entry, root-relative path, name
\ span) with no file bytes, so lookups read the file's current text, an open
\ document's first (tools/lsp-definition.f). Primitives come from PRIM-SOURCE.

require src/lib/string.f
require src/lib/json-read.f
require src/habu/engine-origin.f
require src/habu/verify-source.f
require tools/check-verify-core.f

package SOURCE-INDEX
private

60000 constant INDEX-MS

TYPED-VARIABLE JR-ST JR:cursor
DYNAMIC-BUFFER ID-PKG u8
DYNAMIC-BUFFER ID-TAIL u8
variable ID-PKG-U
variable ID-TAIL-U
variable ID-VIS
DYNAMIC-BUFFER ROW-PKG u8
DYNAMIC-BUFFER ROW-TAIL u8
variable ROW-PKG-U
variable ROW-TAIL-U
variable ROW-VIS
variable ROW-START
variable ROW-END
variable ROW-FIELDS
variable MODE                          \ 0: locate a symbol, 1: select at byte
variable CURSOR
variable COUNT
variable START
variable END
variable RESULT-SYM

: ID! ( ptr u8 n ptr u8 n n -- )
   {: pkg:ptr pkgu:n tail:ptr tailu:n vis:n :}
   pkgu 1 max ID-PKG-RESERVE
   tailu 1 max ID-TAIL-RESERVE
   pkg 0 ID-PKG pkgu BYTE-COPY  pkgu ID-PKG-U !
   tail 0 ID-TAIL tailu BYTE-COPY  tailu ID-TAIL-U !
   vis ID-VIS ! ;

: ID=? ( ptr u8 n ptr u8 n n -- bool )
   {: pkg:ptr pkgu:n tail:ptr tailu:n vis:n :}
   vis ID-VIS @ = pkgu ID-PKG-U @ = and tailu ID-TAIL-U @ = and
   pkg pkgu 0 ID-PKG ID-PKG-U @ CORE-STR= and
   tail tailu 0 ID-TAIL ID-TAIL-U @ CORE-STR= and ;

: STRING>PKG ( JR:reader -- JR:reader )
   JR:SPAN$ nip {: raw:n :}
   raw 1 max ROW-PKG-RESERVE
   0 ROW-PKG raw JR:STR ROW-PKG-U ! ;

: STRING>TAIL ( JR:reader -- JR:reader )
   JR:SPAN$ nip {: raw:n :}
   raw 1 max ROW-TAIL-RESERVE
   0 ROW-TAIL raw JR:STR ROW-TAIL-U ! ;

: VISIBILITY ( JR:reader -- JR:reader )
   s" global" JR:STR-EQ? if SYM-GLOBAL ROW-VIS ! exit then
   s" private" JR:STR-EQ? if SYM-PRIVATE ROW-VIS ! exit then
   s" public" JR:STR-EQ? if SYM-PUBLIC ROW-VIS ! then ;

: MEMBER ( JR:reader -- JR:reader )
   s" package" JR:STR-EQ? if
      JR:NEXT drop STRING>PKG 1 ROW-FIELDS +! exit
   then
   s" decl_name" JR:STR-EQ? if
      JR:NEXT drop STRING>TAIL 2 ROW-FIELDS +! exit
   then
   s" visibility" JR:STR-EQ? if
      JR:NEXT drop VISIBILITY 4 ROW-FIELDS +! exit
   then
   s" byte_start" JR:STR-EQ? if
      JR:NEXT drop JR:INT ROW-START ! 8 ROW-FIELDS +! exit
   then
   s" byte_end" JR:STR-EQ? if
      JR:NEXT drop JR:INT ROW-END ! 16 ROW-FIELDS +! exit
   then
   JR:NEXT drop JR:SKIP-VALUE ;

: DECODE ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   0 ROW-PKG-U !  0 ROW-TAIL-U !
   -1 ROW-VIS !  0 ROW-START !  0 ROW-END !  0 ROW-FIELDS !
   JR-ST a u JR:INIT
   JR:NEXT drop
   begin JR:NEXT JR:T-KEY = while MEMBER repeat
   JR:CLOSE
   ROW-FIELDS @ 31 = ROW-VIS @ 0 >= and
   ROW-TAIL-U @ 0 > and ROW-START @ 0 >= and
   ROW-END @ ROW-START @ > and ;

: ROW-ID ( -- ptr u8 n ptr u8 n n )
   0 ROW-PKG ROW-PKG-U @ 0 ROW-TAIL ROW-TAIL-U @ ROW-VIS @ ;

: ROW-SELECT ( -- )
   MODE @ 0= if
      ROW-ID ID=? if
         1 COUNT +!
         ROW-START @ START !  ROW-END @ END !
      then
      exit
   then
   CURSOR @ ROW-START @ >= CURSOR @ ROW-END @ < and if
      1 COUNT +!
      ROW-ID ID!
   then ;

: LINE ( ptr u8 n -- )
   DECODE if ROW-SELECT then ;

: ROWS ( -- )
   CHECK:VERIFY-INDEX$ {: a:ptr u:n :}
   0 begin dup u < while
      {: at:n :}
      a at + u at - 10 INDEX-OF MATCH option
         some OF IDX>N ENDOF
         none OF u at - ENDOF
      ;MATCH {: len:n :}
      a at + len LINE
      at len + 1+
   repeat drop ;

: SCAN ( ptr u8 n ptr u8 n -- )
   0 COUNT !
   INDEX-MS >MS
   CHECK:INDEX-BYTES MATCH CHECK:verdict
      verified OF ROWS ENDOF
      refused OF ROWS ENDOF
      engine-provided OF ENDOF
      held OF ENDOF
      incomplete OF drop ENDOF
      deferred OF ENDOF
   ;MATCH ;

\ The path must be in the image's own source tree; return its relative part.
: RELATIVE ( ptr u8 n -- ptr u8 n bool )
   {: path:ptr pathu:n :}
   SOURCE-ROOT:ENGINE$ {: root:ptr rootu:n :}
   pathu rootu 1+ <= if NULL$ false exit then
   path rootu root rootu CORE-STR= 0= if NULL$ false exit then
   path rootu + c@ 47 <> if NULL$ false exit then
   path rootu 1+ + pathu rootu 1+ - true ;

: MATCH-ORIGIN ( n -- )
   {: sym:n :}
   sym VERIFY:SYM-IDENTITY ID=? if
      1 COUNT +!  sym RESULT-SYM !
   then ;

public

\ A declaration matches when its canonical package, tail and visibility equal the
\ image symbol's; a cursor selects a symbol only when exactly one live origin in
\ the file has that identity. A refused scan keeps the declarations it accepted; a
\ deferred, held, engine-provided or incomplete scan gives no Location, even for
\ declarations reported before the stop. The scan defers a whole file at a
\ dictionary call (a loader, `undefine`, a replaceable library definer), whose
\ execution could change any binding, and at a trusted does> definition, whose
\ clause body the scanner skips; structural native declarations and literal
\ openers still scan. A prefix file with a top-level dictionary call has no
\ source locations.
: TARGET-IN ( n ptr u8 n ptr u8 n -- n n bool )
   {: sym:n path:ptr pathu:n src:ptr srcu:n :}
   sym ENGINE-ORIGIN:TARGET
   {: rel:ptr relu:n old:n old-end:n name:ptr nameu:n live:bool :}
   live 0= if 0 0 false exit then
   path pathu RELATIVE {: got:ptr gotu:n in-root:bool :}
   in-root 0= if 0 0 false exit then
   got gotu rel relu CORE-STR= 0= if 0 0 false exit then
   sym VERIFY:SYM-IDENTITY ID!
   0 MODE !
   src srcu path pathu SCAN
   COUNT @ 1 = if START @ END @ true else 0 0 false then ;

: SYMBOL-IN ( ptr u8 n n ptr u8 n -- n bool )
   {: path:ptr pathu:n byte:n src:ptr srcu:n :}
   path pathu RELATIVE {: rel:ptr relu:n in-root:bool :}
   in-root 0= if 0 false exit then
   1 MODE !  byte CURSOR !
   src srcu path pathu SCAN
   COUNT @ 1 <> if 0 false exit then
   0 COUNT !
   rel relu ['] MATCH-ORIGIN ENGINE-ORIGIN:EACH-IN
   COUNT @ 1 = if RESULT-SYM @ true else 0 false then ;

;package
