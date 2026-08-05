\ source-lex.f — package LINT-LEX: the one shared source lexer for self-hosted
\ lint, checker, and codegen tooling.
\ Load after lib/memory.f, lib/vector.f, tools/lint/text.f, tools/lint/token.f, and tools/lint/lib.f.
\
\ Public surface (all reads; the package owns every cell):
\   WORD COMMENT REGISTRY              token kinds returned by KIND@
\   UNTERMINATED-QUOTE MALFORMED-REGISTRY   diagnostic kinds from ERROR-KIND@
\   SOURCE ( ptr u8 n -- )             scan a buffer; clears all prior state first
\   COUNT ( -- n )                     tokens produced by the last SOURCE
\   TOKEN CONTENT ( n -- ptr u8 n )    token span / paren-comment body or
\                                      string-literal payload span
\   KIND@ BYTE@ LINE@ COL@ ( n -- n )  kind, 0-based byte, 1-based line, 1-based column
\   ERROR? ( -- bool )                 the last scan hit malformed input
\   ERROR-KIND@ ERROR-BYTE@ ERROR-LINE@ ERROR-COL@ ( -- n )
\
\ A complete `PRIM: ... PRIM;` or `PPRIM: pkg ... PPRIM;` primitive-axiom row is
\ one REGISTRY token spanning the whole row, positioned at its opener; its fields
\ never appear as separate tokens and CONTENT is empty for it. An incomplete row
\ is the MALFORMED-REGISTRY diagnostic at the opener site.
\
\ The diagnostic is one generic record, not a quote-specific flag. A scan writes
\ it at most once: the writer runs at the malformed site and stops the scan, so
\ no token after that site is exposed. Consumers only read it back, so no caller
\ can mutate lexer state. A consumer that requires valid source must reject when
\ ERROR? is true, and should read ERROR-KIND@ to name which defect it hit.

package LINT-LEX
private

1024 constant MIN-CAP
0 constant NO-ERROR

0 constant ROW-BARE             \ FAM: bare `PRIM:` row, closed by `PRIM;`
1 constant ROW-PKG              \ FAM: `PPRIM:` row, closed by `PPRIM;` or `CLOSE-PRIVATE`

variable INIT-N
variable TOK-N   variable SRC-A   variable SRC-U   variable POS
variable LINE-N  variable COL-N  variable START  variable START-LINE  variable START-COL
variable CSTART
variable CLEN
variable ERR-KIND
variable ERR-BYTE
variable ERR-LINE
variable ERR-COL
variable HALTED                 \ a diagnostic ended the scan; expose no later token
variable FAM                    \ family of the row being scanned
variable FOFF   variable FLEN   \ current row field: offset into the source, length
variable R-BYTE variable R-LINE variable R-COL   \ opener site of the row being scanned

create KIND-V VEC:HEADER-CELLS cells allot
create ADDR-V VEC:HEADER-CELLS cells allot
create LEN-V VEC:HEADER-CELLS cells allot
create BYTE-V VEC:HEADER-CELLS cells allot
create LINE-V VEC:HEADER-CELLS cells allot
create COL-V VEC:HEADER-CELLS cells allot
create CADDR-V VEC:HEADER-CELLS cells allot
create CLEN-V VEC:HEADER-CELLS cells allot

\ Lift validated lexer counts and indexes into VEC roles.
: N>ITEM ( n -- CAD-NUM:item-count )
   CAD-NUM:ITEM-COUNT
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;
: N>INDEX ( n -- CAD-NUM:index )
   CAD-NUM:INDEX
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

: SRC-FIELD ( -- ptr ptr u8 )
   SRC-A 0 ptr-field ;

: SRC@ ( -- ptr u8 )
   SRC-FIELD @ ;

: SRC! ( ptr u8 -- )
   SRC-FIELD ! ;

: INIT-ONE ( ptr h n -- ) {: vec:ptr step:n :}
   step INIT-N @ < if exit then
   vec MIN-CAP N>ITEM VEC:INIT
   step 1+ INIT-N ! ;

: CLEAR-ONE ( ptr h -- )
   VEC:CLEAR ;

: INIT-VECTORS ( -- )
   KIND-V  0 INIT-ONE
   ADDR-V  1 INIT-ONE
   LEN-V   2 INIT-ONE
   BYTE-V  3 INIT-ONE
   LINE-V  4 INIT-ONE
   COL-V   5 INIT-ONE
   CADDR-V 6 INIT-ONE
   CLEN-V  7 INIT-ONE ;

: CLEAR-VECTORS ( -- )
   KIND-V CLEAR-ONE
   ADDR-V CLEAR-ONE
   LEN-V CLEAR-ONE
   BYTE-V CLEAR-ONE
   LINE-V CLEAR-ONE
   COL-V CLEAR-ONE
   CADDR-V CLEAR-ONE
   CLEN-V CLEAR-ONE ;

: RESET-TABLES ( -- )
   INIT-N @ 8 = if CLEAR-VECTORS else INIT-VECTORS then
   0 TOK-N ! ;

: RESERVE-ONE ( ptr h n -- )  N>ITEM VEC:ENSURE ;

: RESERVE-ROW ( -- )
   TOK-N @ 1+ {: need:n :}
   KIND-V  need RESERVE-ONE
   ADDR-V  need RESERVE-ONE
   LEN-V   need RESERVE-ONE
   BYTE-V  need RESERVE-ONE
   LINE-V  need RESERVE-ONE
   COL-V   need RESERVE-ONE
   CADDR-V need RESERVE-ONE
   CLEN-V  need RESERVE-ONE ;

: ADD ( n ptr u8 n n n n ptr u8 n -- ) {: kind:n a:ptr u:n byte:n line:n col:n ca:ptr cu:n :}
   RESERVE-ROW
   kind KIND-V VEC:PUSH drop
   a ADDR-V VEC:PUSH drop
   u LEN-V VEC:PUSH drop
   byte BYTE-V VEC:PUSH drop
   line LINE-V VEC:PUSH drop
   col COL-V VEC:PUSH drop
   ca CADDR-V VEC:PUSH drop
   cu CLEN-V VEC:PUSH drop
   TOK-N @ 1+ TOK-N ! ;

public

1 constant WORD                 \ KIND@: whitespace-delimited word token
2 constant COMMENT              \ KIND@: `( ... )` or `.( ... )` comment, body via CONTENT
3 constant REGISTRY             \ KIND@: one complete PRIM:/PPRIM: primitive-axiom row

1 constant UNTERMINATED-QUOTE   \ ERROR-KIND@: a string literal ran past end of input
2 constant MALFORMED-REGISTRY   \ ERROR-KIND@: a primitive-axiom row lacked a header or its closer

: COUNT ( -- n )
   TOK-N @ ;

: TOKEN ( n -- ptr u8 n ) {: k:n :}
   ADDR-V k N>INDEX VEC:@
   LEN-V k N>INDEX VEC:@ ;

: CONTENT ( n -- ptr u8 n ) {: k:n :}
   CADDR-V k N>INDEX VEC:@
   CLEN-V k N>INDEX VEC:@ ;

: KIND@ ( n -- n ) {: k:n :}
   KIND-V k N>INDEX VEC:@ ;

: BYTE@ ( n -- n ) {: k:n :}
   BYTE-V k N>INDEX VEC:@ ;

: LINE@ ( n -- n ) {: k:n :}
   LINE-V k N>INDEX VEC:@ ;

: COL@ ( n -- n ) {: k:n :}
   COL-V k N>INDEX VEC:@ ;

: ERROR? ( -- bool )
   ERR-KIND @ NO-ERROR = if LINT-FALSE else LINT-TRUE then ;

: ERROR-KIND@ ( -- n )
   ERR-KIND @ ;

\ The three position readers below describe the site named by ERROR-KIND@ and
\ are meaningful only while ERROR? is true; a clean scan leaves them zeroed.
: ERROR-BYTE@ ( -- n )
   ERR-BYTE @ ;

: ERROR-LINE@ ( -- n )
   ERR-LINE @ ;

: ERROR-COL@ ( -- n )
   ERR-COL @ ;

private

: END? ( -- bool )
   POS @ SRC-U @ >= ;

: CUR ( -- n )
   SRC@ POS @ + c@ ;

: ADV ( -- n )
   CUR
   POS @ 1+ POS !
   dup 10 = if LINE-N @ 1+ LINE-N ! 1 COL-N ! else COL-N @ 1+ COL-N ! then ;

: SKIP-QUOTE ( -- bool )
   begin END? 0= while ADV DQUOTE = if LINT-TRUE exit then repeat
   LINT-FALSE ;

: SKIP-ESC-QUOTE ( -- bool )
   begin END? 0= while
      ADV dup 92 = if
         drop END? 0= if ADV drop then
      else
         DQUOTE = if LINT-TRUE exit then
      then
   repeat
   LINT-FALSE ;

: CLEAR-ERROR ( -- )
   NO-ERROR ERR-KIND !
   0 ERR-BYTE !
   0 ERR-LINE !
   0 ERR-COL !
   LINT-FALSE HALTED ! ;

: MARK-UNTERM ( n -- ) {: k:n :}
   UNTERMINATED-QUOTE ERR-KIND !
   k BYTE@ ERR-BYTE !
   k LINE@ ERR-LINE !
   k COL@ ERR-COL ! ;

: LINE-COMMENT ( -- )
   begin END? 0= CUR 10 <> and while ADV drop repeat ;

: BODY-A ( -- ptr u8 )
   SRC@ CSTART @ + ;

: BODY-U ( -- n )
   CLEN @ ;

: TO-PAREN ( -- )
   begin END? 0= CUR 41 <> and while ADV drop repeat ;

\ The span from the token start the main loop recorded to the current position.
: CUR$ ( -- ptr u8 n )
   SRC@ START @ + POS @ START @ - ;

\ `parse-name` and the engine token loop delimit on every byte at or below space.
: ENGINE-DELIM? ( n -- bool )
   $20 <= ;

\ Engine parity: `(` opens a comment only as a standalone token (followed by
\ an engine delimiter or EOF). A `(`-initial token such as `(CMP)` is one word.
: PAREN-STANDALONE? ( -- bool )
   POS @ 1+ SRC-U @ >= if LINT-TRUE exit then
   SRC@ POS @ 1+ + c@ ENGINE-DELIM? ;

\ Emit one COMMENT token for a paren-delimited inert span whose opener is already
\ consumed, so POS sits on the first body byte. The token spans from the opener
\ site the caller recorded in START through the closing `)`; an opener that never
\ closes ends the span at end of input, which is not a diagnostic because the
\ unread text is inert either way.
: PAREN-BODY ( -- )
   POS @ CSTART !
   TO-PAREN
   POS @ CSTART @ - CLEN !
   END? 0= if ADV drop then
   COMMENT CUR$ START @ START-LINE @ START-COL @
   BODY-A BODY-U ADD ;

: PAREN-COMMENT ( -- )
   ADV drop PAREN-BODY ;

\ `.( ... )` is the printing comment. The engine reads its opener with
\ `parse-name`, so the opener is the whole token `.(` rather than the `(` inside
\ it: it arrives on the word path and is recognised there by exact spelling. That
\ is what makes `.(X)` an ordinary word, for the same reason `(CMP)` is one, and
\ it needs no separate standalone test because a word IS the standalone unit.
\ The body is inert source exactly like a `( ... )` body, so it becomes a COMMENT
\ token whose CONTENT is that body; the printing is a runtime effect no lexer
\ models. Without this rule a word-at-a-time reader hands the body out as
\ ordinary tokens, and a consumer that counts declarations reads a declaration
\ the engine never performs - test/bootstrap-wide-memory.fs really does open a
\ file with one.
: PRINT-OPEN? ( ptr u8 n -- bool )
   s" .(" LINT-STR= ;

\ ---- primitive-axiom rows (`PRIM: ... PRIM;`, `PPRIM: pkg ... PPRIM;`) ---------
\ The engine reads a row's name (and a package row's package) with `parse-name`,
\ so a primitive may be NAMED `s"`, `c"`, `."`, `s\"`, `c\"`, `.\"`, `[']` or
\ `[char]` (src/core/checker.f does name all eight). A word-at-a-time lexer treats
\ those names as live string openers and swallows real source: before this row
\ scanner, `PRIM: s"     PE-PTR-U8 PE-OUT PE-N PE-OUT PRIM;` on checker.f line
\ 5093 consumed everything through the quote in the NEXT row, losing five tokens
\ of its own row plus the following opener from the table with no diagnostic.
\ So each complete row becomes one REGISTRY token and its fields never reach the
\ word path. The row model is src/habu/verify-source.f RECORD-PRIM-ROW, the
\ authoritative source replay of the same engine text: header fields raw, then
\ body fields where comments are inert and a parsing word consumes its operand.
\ Row fields share the engine delimiter rule with top-level tokenization because
\ `parse-name` is what the engine runs.

: SKIP-RAW-WS ( -- )
   begin END? 0= CUR ENGINE-DELIM? and while ADV drop repeat ;

: F$ ( -- ptr u8 n )
   SRC@ FOFF @ + FLEN @ ;

\ FLEN 0 means end of input: no field was left to read.
: NEXT-FIELD ( -- )
   SKIP-RAW-WS
   POS @ FOFF !
   begin END? 0= CUR ENGINE-DELIM? 0= and while ADV drop repeat
   POS @ FOFF @ - FLEN ! ;

: ROW-PAREN ( -- )
   ADV drop
   TO-PAREN
   END? 0= if ADV drop then ;

\ A row body is interpreted text, so `\` and `( ... )` inside it are comments and
\ a closer spelled inside one is not a closer.
: SKIP-INERT ( -- )
   begin
      SKIP-RAW-WS
      END? if exit then
      CUR 92 = if LINE-COMMENT else
         CUR 40 = PAREN-STANDALONE? and if ROW-PAREN else exit then
      then
   again ;

: BODY-FIELD ( -- )
   SKIP-INERT
   NEXT-FIELD ;

: PRIM-OPEN? ( ptr u8 n -- bool )
   s" PRIM:" LINT-STR=CI ;

: PPRIM-OPEN? ( ptr u8 n -- bool )
   s" PPRIM:" LINT-STR=CI ;

: ROW-OPEN? ( ptr u8 n -- bool )
   2dup PPRIM-OPEN? if 2drop LINT-TRUE exit then
   PRIM-OPEN? ;

: PRIM-CLOSE? ( ptr u8 n -- bool )
   s" PRIM;" LINT-STR=CI ;

: PPRIM-CLOSE? ( ptr u8 n -- bool )
   s" PPRIM;" LINT-STR=CI ;

: PRIVATE-CLOSE? ( ptr u8 n -- bool )
   s" CLOSE-PRIVATE" LINT-STR=CI ;

\ `PRIM;` closes a bare row; a package row closes with `PPRIM;` (public wordlist)
\ or `CLOSE-PRIVATE`
\ (package private wordlist). A bare row has no package wordlist, so
\ `CLOSE-PRIVATE` there is an ordinary effect field, not a closer.
: ROW-CLOSE? ( ptr u8 n -- bool )
   FAM @ ROW-PKG = if
      2dup PPRIM-CLOSE? if 2drop LINT-TRUE exit then
      PRIVATE-CLOSE? exit
   then
   PRIM-CLOSE? ;

: WRONG-CLOSE? ( ptr u8 n -- bool )
   FAM @ ROW-PKG = if PRIM-CLOSE? exit then
   PPRIM-CLOSE? ;

\ `[']` and `[char]` parse one raw operand, so a closer-shaped token there is that
\ operand and not a closer.
\
\ The bracket-less `char` is deliberately NOT here, and this is a known divergence
\ from src/habu/verify-source.f PARSE-NEXT?, which treats `char` and `[char]`
\ alike. Two reasons. The row grammar this file implements names exactly eight
\ operand-parsing labels, and `char` is not one of them. And leaving it out is the
\ safe direction: a hostile `PRIM: FOO char PRIM; create LEAK PRIM;` then closes
\ at the first closer and hands `create LEAK` to the consumer as ordinary tokens
\ instead of hiding it inside the row span. Real source is unaffected, because
\ src/core/checker.f only ever writes `char` in header position, where every field
\ is raw anyway. This rule is the canonical one; verify-source.f conforms to it
\ when it starts consuming REGISTRY tokens (dot habu-consume-registry-events-
\ efe7fe5e), and that leaf owns the differential fixture for `char`.
: PARSE-NEXT? ( ptr u8 n -- bool )
   2dup s" [']" LINT-STR= if 2drop LINT-TRUE exit then
   s" [char]" LINT-STR=CI ;

: MARK-BAD ( -- )
   MALFORMED-REGISTRY ERR-KIND !
   R-BYTE @ ERR-BYTE !
   R-LINE @ ERR-LINE !
   R-COL @ ERR-COL !
   LINT-TRUE HALTED ! ;

\ A header field names the primitive (and, for a package row, its package). An
\ opener or a closer of this row's family there is never a name: the row is
\ missing its header.
: HDR-BAD? ( -- bool )
   FLEN @ 0= if LINT-TRUE exit then
   F$ ROW-OPEN? if LINT-TRUE exit then
   F$ ROW-CLOSE? if LINT-TRUE exit then
   F$ WRONG-CLOSE? ;

: HDR-FIELD ( -- )
   NEXT-FIELD
   HDR-BAD? if MARK-BAD then ;

: HDR ( -- )
   HDR-FIELD
   HALTED @ if exit then
   FAM @ ROW-PKG = if HDR-FIELD then ;

\ A row body is interpreted, so `.( ... )` there parses its own text exactly like
\ `s" ... "` does: a closer spelled inside a print body is that text and not this
\ row's closer. false = the print body ran past end of input.
: SKIP-PRINT ( -- bool )
   TO-PAREN
   END? if LINT-FALSE exit then
   ADV drop LINT-TRUE ;

\ false = the operand ran past end of input, so the row can never close.
: FIELD-OPERAND ( -- bool )
   F$ LINT-ESC-STRING-OPENER? if SKIP-ESC-QUOTE exit then
   F$ LINT-NORMAL-STRING-OPENER? if SKIP-QUOTE exit then
   F$ PRINT-OPEN? if SKIP-PRINT exit then
   F$ PARSE-NEXT? if NEXT-FIELD FLEN @ 0 <> exit then
   LINT-TRUE ;

: ROW-BODY ( -- )
   begin
      BODY-FIELD
      FLEN @ 0= if MARK-BAD exit then
      F$ ROW-CLOSE? if exit then
      F$ WRONG-CLOSE? if MARK-BAD exit then
      F$ ROW-OPEN? if MARK-BAD exit then
      FIELD-OPERAND 0= if MARK-BAD exit then
   again ;

: EMIT-ROW ( -- )
   REGISTRY SRC@ R-BYTE @ + POS @ R-BYTE @ -
   R-BYTE @ R-LINE @ R-COL @ SRC@ 0 ADD ;

\ The opener field is already consumed; POS sits just past it.
: SCAN-ROW ( n -- ) {: kind:n :}
   kind FAM !
   START @ R-BYTE !  START-LINE @ R-LINE !  START-COL @ R-COL !
   HDR
   HALTED @ if exit then
   ROW-BODY
   HALTED @ if exit then
   EMIT-ROW ;

: PREV$ ( -- ptr u8 n )
   COUNT 1- TOKEN ;

: PREV-WORD? ( -- bool )
   COUNT 0= if LINT-FALSE exit then
   COUNT 1- KIND@ WORD = ;

\ After one of these tokens the engine consumes the next word as a parsed name and never executes
\ it, so `: PRIM: ( -- ) parse-name PE-OPEN ;` in src/core/checker.f declares the
\ opener rather than opening a row.
: NAME-POS? ( -- bool )
   PREV-WORD? 0= if LINT-FALSE exit then
   PREV$ s" :" LINT-STR= if LINT-TRUE exit then
   PREV$ s" '" LINT-STR= if LINT-TRUE exit then
   PREV$ s" [']" LINT-STR= if LINT-TRUE exit then
   PREV$ s" postpone" LINT-STR=CI if LINT-TRUE exit then
   PREV$ s" undefine" LINT-STR=CI ;

: ROW-START? ( -- bool )
   CUR$ ROW-OPEN? 0= if LINT-FALSE exit then
   NAME-POS? LINT-NOT ;

\ The same name-position rule a row opener obeys: after `:` or `'` the engine
\ parses the next word as a name and never executes it, so `: .( ( -- ) ;`
\ DEFINES a word spelled `.(` instead of opening a printing comment.
: PRINT-START? ( -- bool )
   CUR$ PRINT-OPEN? 0= if LINT-FALSE exit then
   NAME-POS? LINT-NOT ;

\ Swallow the literal and answer the bytes it held, together with whether it
\ closed. The payload starts one byte past the opener, because the opener is
\ followed by exactly one delimiter; it ends at the byte before the closing
\ quote, which is where POS now sits minus one. An unterminated literal has no
\ payload to report.
: STRING-PAYLOAD ( bool -- ptr u8 n bool ) {: esc:bool :}
   POS @ 1+ {: pstart:n :}
   esc if SKIP-ESC-QUOTE else SKIP-QUOTE then {: closed:bool :}
   closed 0= if SRC@ 0 closed exit then
   SRC@ pstart + POS @ 1- pstart - closed ;

: STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u LINT-ESC-STRING-OPENER? if LINT-TRUE exit then
   a u LINT-NORMAL-STRING-OPENER? ;

\ A string-literal token carries its payload in CONTENT, exactly as a paren
\ comment carries its body there. The payload is deliberately never tokenized -
\ that is what stops a quoted word being mistaken for code - so without this a
\ consumer that has to reason about a quoted NAME has no route to it but
\ substring search over the raw source, which is the evasion route this lexer
\ exists to close. The checker's own concrete type table is written that way
\ (`s" n" CC-N CT-INT 64 CS-GENERIC CT-SET`), and the type names in it are only
\ reachable here.
: SCAN-WORD ( -- )
   begin END? 0= CUR ENGINE-DELIM? 0= and while ADV drop repeat
   ROW-START? if
      CUR$ PPRIM-OPEN? if ROW-PKG else ROW-BARE then SCAN-ROW
      exit
   then
   \ `.(` is already consumed by the word scan, so the print body starts at POS.
   PRINT-START? if PAREN-BODY exit then
   CUR$ {: a:ptr u:n :}
   START @ START-LINE @ START-COL @ {: byte:n line:n col:n :}
   a u STRING-OPENER? 0= if
      WORD a u byte line col SRC@ 0 ADD exit
   then
   a u LINT-ESC-STRING-OPENER? STRING-PAYLOAD {: pa:ptr pu:n closed:bool :}
   WORD a u byte line col pa pu ADD
   closed 0= if COUNT 1- MARK-UNTERM then ;

public

: SOURCE ( ptr u8 n -- ) {: a:ptr u:n :}
   a SRC! u SRC-U ! 0 POS ! 1 LINE-N ! 1 COL-N !
   CLEAR-ERROR
   RESET-TABLES
   begin END? 0= HALTED @ 0= and while
      CUR ENGINE-DELIM? if ADV drop
      else
         POS @ START !  LINE-N @ START-LINE !  COL-N @ START-COL !
         CUR 92 = if LINE-COMMENT
         else CUR 40 = PAREN-STANDALONE? and if PAREN-COMMENT
         else SCAN-WORD then then
      then
   repeat ;

;package
