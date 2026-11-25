\ lsp-source-index.f - locate an image word in current engine source. The verifier
\ child scans the supplied bytes (LSP-CHECK:INDEX-BYTES) and returns surviving
\ declaration identities; this file matches them to the path and name span
\ ENGINE-ORIGIN:TARGET gives an engine word, and reads the file's current text, an
\ open document's first (tools/lsp-definition.f). Primitives come from PRIM-SOURCE.

require src/lib/string.f
require src/lib/string-option.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/json.f
require src/habu/engine-origin.f
require src/habu/verify-source.f
require tools/check-verify-core.f

package SOURCE-INDEX
private

60000 constant INDEX-MS

TYPED-VARIABLE JSON-ST JSON:cursor
DYNAMIC-BUFFER ID-PKG u8
DYNAMIC-BUFFER ID-TAIL u8
variable ID-PKG-U
variable ID-TAIL-U
variable ID-VIS
TYPED-VARIABLE ROW-PKG ^VEC:vector<u8> SCRATCH-ONLY
TYPED-VARIABLE ROW-TAIL ^VEC:vector<u8> SCRATCH-ONLY
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

: ID! ( str str n -- )
   {: pkg:str tail:str vis:n :}
   pkg STR:LEN 1 max ID-PKG-RESERVE
   tail STR:LEN 1 max ID-TAIL-RESERVE
   pkg STR:DATA 0 ID-PKG pkg STR:LEN BYTE-COPY  pkg STR:LEN ID-PKG-U !
   tail STR:DATA 0 ID-TAIL tail STR:LEN BYTE-COPY  tail STR:LEN ID-TAIL-U !
   vis ID-VIS ! ;

: ID=? ( str str n -- bool )
   {: pkg:str tail:str vis:n :}
   vis ID-VIS @ = pkg STR:LEN ID-PKG-U @ = and tail STR:LEN ID-TAIL-U @ = and
   pkg 0 ID-PKG ID-PKG-U @ >str STR:= and
   tail 0 ID-TAIL ID-TAIL-U @ >str STR:= and ;

: VISIBILITY ( JSON:reader -- JSON:reader )
   s" global" JSON:READ-STR-EQ? if SYM-GLOBAL ROW-VIS ! exit then
   s" private" JSON:READ-STR-EQ? if SYM-PRIVATE ROW-VIS ! exit then
   s" public" JSON:READ-STR-EQ? if SYM-PUBLIC ROW-VIS ! then ;

: MEMBER ( JSON:reader -- JSON:reader )
   s" package" JSON:READ-STR-EQ? if
      JSON:READ-NEXT drop ROW-PKG @ JSON:READ-STR drop 1 ROW-FIELDS +! exit
   then
   s" decl_name" JSON:READ-STR-EQ? if
      JSON:READ-NEXT drop ROW-TAIL @ JSON:READ-STR drop 2 ROW-FIELDS +! exit
   then
   s" visibility" JSON:READ-STR-EQ? if
      JSON:READ-NEXT drop VISIBILITY 4 ROW-FIELDS +! exit
   then
   s" byte_start" JSON:READ-STR-EQ? if
      JSON:READ-NEXT drop JSON:READ-INT ROW-START ! 8 ROW-FIELDS +! exit
   then
   s" byte_end" JSON:READ-STR-EQ? if
      JSON:READ-NEXT drop JSON:READ-INT ROW-END ! 16 ROW-FIELDS +! exit
   then
   JSON:READ-NEXT drop JSON:READ-SKIP-VALUE ;

: MADE ( ^^VEC:vector<u8> -- ^VEC:vector<u8> )
   {: c:^ :}
   c @ NULL-PTR = if HEAP:ALLOCATOR 0 [type] u8 VEC:NEW c ! then
   c @ ;

: DECODE ( str -- bool )
   {: s:str :}
   ROW-PKG MADE 0 VEC:RESIZE  ROW-TAIL MADE 0 VEC:RESIZE
   -1 ROW-VIS !  0 ROW-START !  0 ROW-END !  0 ROW-FIELDS !
   JSON-ST s JSON:READ-INIT
   JSON:READ-NEXT drop
   begin JSON:READ-NEXT JSON:T-KEY = while MEMBER repeat
   JSON:READ-CLOSE
   ROW-FIELDS @ 31 = ROW-VIS @ 0 >= and
   ROW-TAIL @ VEC:COUNT 0 > and ROW-START @ 0 >= and
   ROW-END @ ROW-START @ > and ;

: ROW-ID ( -- str str n )
   ROW-PKG @ VEC:BYTES ROW-TAIL @ VEC:BYTES ROW-VIS @ ;

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

: LINE ( str -- )
   DECODE if ROW-SELECT then ;

: ROWS ( -- )
   LSP-CHECK:VERIFY-INDEX$ {: s:str :}
   0 begin dup s STR:LEN < while
      {: at:n :}
      s STR:DATA at + s STR:LEN at - >str 10 STR:INDEX-OF MATCH option
         some OF IDX>N ENDOF
         none OF s STR:LEN at - ENDOF
      ;MATCH {: len:n :}
      s STR:DATA at + len >str LINE
      at len + 1+
   repeat drop ;

: SCAN ( str str -- )
   0 COUNT !
   INDEX-MS >MS
   LSP-CHECK:INDEX-BYTES MATCH LSP-CHECK:verdict
      verified OF ROWS ENDOF
      refused OF ROWS ENDOF
      engine-provided OF ENDOF
      held OF ENDOF
      incomplete OF drop ENDOF
      deferred OF ENDOF
   ;MATCH ;

\ The path must be in the image's own source tree; return its relative part.
: RELATIVE ( str -- str bool )
   {: path:str :}
   SOURCE-ROOT:ENGINE$ {: root:str :}
   path STR:LEN root STR:LEN 1+ <= if NULL$ false exit then
   path STR:DATA root STR:LEN >str root STR:= 0= if NULL$ false exit then
   path STR:DATA root STR:LEN + c@ 47 <> if NULL$ false exit then
   path STR:DATA root STR:LEN 1+ + path STR:LEN root STR:LEN 1+ - >str true ;

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
: TARGET-IN ( n str str -- n n bool )
   {: sym:n path:str src:str :}
   sym ENGINE-ORIGIN:TARGET
   {: rel:str old:n old-end:n name:str live:bool :}
   live 0= if 0 0 false exit then
   path RELATIVE {: got:str in-root:bool :}
   in-root 0= if 0 0 false exit then
   got rel STR:= 0= if 0 0 false exit then
   sym VERIFY:SYM-IDENTITY ID!
   0 MODE !
   src path SCAN
   COUNT @ 1 = if START @ END @ true else 0 0 false then ;

: SYMBOL-IN ( str n str -- n bool )
   {: path:str byte:n src:str :}
   path RELATIVE {: rel:str in-root:bool :}
   in-root 0= if 0 false exit then
   1 MODE !  byte CURSOR !
   src path SCAN
   COUNT @ 1 <> if 0 false exit then
   0 COUNT !
   rel ['] MATCH-ORIGIN ENGINE-ORIGIN:EACH-IN
   COUNT @ 1 = if RESULT-SYM @ true else 0 false then ;

;package
