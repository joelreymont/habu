\ primitive-source.f - locate engine primitive names in the raw source of the
\ specification the running checker retained. The lexer keeps registry rows
\ whole, so the name is read from each row's header after matching the ordered
\ source rows to PRIM-SPEC's kind, package and name.

require src/habu/prims.f
require src/lib/source-lex.f
require src/lib/string.f

package PRIM-SOURCE

private

65536 constant READ-STEP
create PATH PATH-CAP 1+ allot
variable PATH-U
create PATHZ PATH-CAP 1+ allot
DYNAMIC-BUFFER SOURCE-B u8
variable SOURCE-U
DYNAMIC-BUFFER STARTS n
DYNAMIC-BUFFER ENDS n
variable ROW
variable READY
variable VALID

: PATH$ ( -- ptr u8 n ) PATH PATH-U @ ;

: ENGINE-PATH ( -- bool )
   0 PATH-U !
   SOURCE-ROOT:ENGINE$ {: root:ptr rootu:n :}
   s" /src/habu/prims.f" {: tail:ptr tailu:n :}
   rootu tailu + PATH-CAP > if false exit then
   root PATH rootu BYTE-COPY
   tail PATH rootu + tailu BYTE-COPY
   rootu tailu + PATH-U !
   true ;

: READ-BODY ( n -- bool )
   {: fd:n :}
   0 SOURCE-U !
   begin
      SOURCE-U @ READ-STEP + SOURCE-B-RESERVE
      fd SOURCE-U @ SOURCE-B READ-STEP read {: got:n :}
      got 0 < if false exit then
      got 0= if true exit then
      got SOURCE-U +!
   again ;

: READ-FILE ( ptr u8 n -- bool )
   {: path:ptr pathu:n :}
   pathu 0 <= pathu PATH-CAP > or if false exit then
   path PATHZ pathu BYTE-COPY
   0 PATHZ pathu + c!
   PATHZ open-rd {: fd:n :}
   fd 0 < if false exit then
   fd READ-BODY {: read-ok:bool :}
   fd close
   read-ok ;

\ The next blank-delimited field in a registry token, as parse-name reads it.
: FIELD ( ptr u8 n n -- n n )
   {: a:ptr u:n from:n :}
   from
   begin dup u < if a over + c@ LINT-LEX:ENGINE-DELIM? else false then while 1+ repeat
   dup {: start:n :}
   begin dup u < if a over + c@ LINT-LEX:ENGINE-DELIM? 0= else false then while 1+ repeat
   start swap ;

: SPEC-NEXT ( n -- n )
   begin dup PRIM-SPEC:COUNT < while
      dup PRIM-SPEC:KIND@ {: kind:n :}
      kind PRIM-SPEC:K-PRIM = kind PRIM-SPEC:K-PKG-PRIVATE = or
      kind PRIM-SPEC:K-ELAB = or if exit then
      1+
   repeat ;

\ A registry token must be the next retained primitive row; its header gives
\ the raw source offset of the name without interpreting its body.
: MATCH-NAME ( n n n -- bool )
   {: token:n row:n after:n :}
   token LINT-LEX:TOKEN {: a:ptr u:n :}
   a u after FIELD {: name:n name-end:n :}
   name-end name <= if false exit then
   a name + name-end name - row PRIM-SPEC:NAME$ STR= 0= if false exit then
   token LINT-LEX:BYTE@ name + row STARTS !
   token LINT-LEX:BYTE@ name-end + row ENDS !
   true ;

: MATCH-ROW ( n n -- bool )
   {: token:n row:n :}
   token LINT-LEX:TOKEN {: a:ptr u:n :}
   a u 0 FIELD {: opener:n after:n :}
   after opener <= if false exit then
   a opener + after opener - {: op:ptr opu:n :}
   row PRIM-SPEC:KIND@ {: kind:n :}
   kind PRIM-SPEC:K-PRIM = if
      op opu s" EPRIM:" STR=CI 0= if false exit then
      token row after MATCH-NAME exit
   then
   kind PRIM-SPEC:K-PKG-PRIVATE <> if false exit then
   op opu s" EPPRIM:" STR=CI 0= if false exit then
   a u after FIELD {: pkg:n pkg-end:n :}
   a pkg + pkg-end pkg - row PRIM-SPEC:PKG$ STR= 0= if false exit then
   token row pkg-end MATCH-NAME ;

: MATCH-ELAB ( n n -- bool )
   {: token:n row:n :}
   row PRIM-SPEC:KIND@ PRIM-SPEC:K-ELAB <> if false exit then
   token 1+ LINT-LEX:COUNT >= if false exit then
   token 1+ LINT-LEX:KIND@ LINT-LEX:WORD <> if false exit then
   token 1+ LINT-LEX:TOKEN row PRIM-SPEC:NAME$ STR= 0= if false exit then
   token 1+ LINT-LEX:BYTE@ {: start:n :}
   token 1+ LINT-LEX:TOKEN nip start + {: end:n :}
   start row STARTS !
   end row ENDS !
   true ;

: SCAN-IN ( ptr u8 n -- bool )
   LINT-LEX:SOURCE
   LINT-LEX:ERROR? if false exit then
   PRIM-SPEC:COUNT STARTS-RESERVE
   PRIM-SPEC:COUNT ENDS-RESERVE
   PRIM-SPEC:COUNT 0 ?do 0 i STARTS ! 0 i ENDS ! loop
   0 ROW !
   LINT-LEX:COUNT 0 ?do
      i LINT-LEX:KIND@ LINT-LEX:REGISTRY = if
         ROW @ SPEC-NEXT {: row:n :}
         row PRIM-SPEC:COUNT >= if false unloop exit then
         i row MATCH-ROW 0= if false unloop exit then
         row 1+ ROW !
      else
         ROW @ 0 > if
            i LINT-LEX:TOKEN s" ELAB:" STR=CI if
               ROW @ SPEC-NEXT {: row:n :}
               row PRIM-SPEC:COUNT >= if false unloop exit then
               i row MATCH-ELAB 0= if false unloop exit then
               row 1+ ROW !
            then
         then
      then
   loop
   ROW @ SPEC-NEXT PRIM-SPEC:COUNT = ;

: AT ( n -- n n bool )
   {: row:n :}
   row 0 < row PRIM-SPEC:COUNT >= or if 0 0 false exit then
   row STARTS @ row ENDS @ {: start:n end:n :}
   start end end start > ;

: CANON-ROW ( n -- n )
   {: row:n :}
   row PRIM-SPEC:PKG$ {: pkg:ptr pkgu:n :}
   row PRIM-SPEC:NAME$ {: name:ptr nameu:n :}
   row 0 ?do
      i PRIM-SPEC:KIND@ {: kind:n :}
      kind PRIM-SPEC:K-PRIM = kind PRIM-SPEC:K-PKG-PRIVATE = or
      kind PRIM-SPEC:K-ELAB = or if
         i PRIM-SPEC:PKG$ pkg pkgu STR=CI
         i PRIM-SPEC:NAME$ name nameu STR=CI and if i unloop exit then
      then
   loop
   row ;

public

\ Raw source for a caller that validates an image-recorded name token. The
\ bytes are borrowed until another read through this package.
: FILE-BYTES ( ptr u8 n -- ptr u8 n bool )
   READ-FILE if 0 SOURCE-B SOURCE-U @ true exit then
   NULL$ false ;

: ENGINE-PATH$ ( -- ptr u8 n bool )
   ENGINE-PATH
   PATH$ rot ;

\ Match the entire raw specification before trusting a target in it.
: TARGET-IN ( ptr u8 n n -- n n bool )
   {: a:ptr u:n row:n :}
   0 READY ! 0 VALID !
   a u SCAN-IN VALID !
   VALID @ 0= if 0 0 false exit then
   row AT ;

: TARGET-ROW ( n -- n n bool )
   VALID @ 0= if drop 0 0 false exit then
   AT ;

\ Reverse lookup in raw or supplied source, after validating the whole table.
\ Overload names map to the first row, the same marker a checker use carries.
: MARKER-IN ( ptr u8 n n -- n n n bool )
   {: a:ptr u:n byte:n :}
   0 READY ! 0 VALID !
   a u SCAN-IN 0= if 0 0 0 false exit then
   PRIM-SPEC:COUNT 0 ?do
      i AT {: start:n end:n found:bool :}
      found byte start >= and byte end <= and if
         i CANON-ROW {: canonical:n :}
         canonical AT {: at:n to:n located:bool :}
         -2 canonical - at to located unloop exit
      then
   loop
   0 0 0 false ;

\ The engine path and its matched row offsets are read once per verifier
\ child. A missing or changed source has no location for any primitive.
: TARGET ( n -- ptr u8 n n n bool )
   {: row:n :}
   READY @ 0= if
      1 READY !
      ENGINE-PATH if
         PATH$ READ-FILE if
            0 SOURCE-B SOURCE-U @ SCAN-IN VALID !
         then
      then
   then
   VALID @ 0= if PATH$ 0 0 false exit then
   PATH$ row AT ;

;package
