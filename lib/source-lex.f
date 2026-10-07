\ source-lex.f — package LINT-LEX: the one shared lexer for Habu source, used by
\ the lint, checker and codegen tooling and by applications that read source by
\ its tokens. Storage class: process-wide. The token table and the scan state
\ are one set for the image, and SOURCE replaces the last scan's tokens.
\ It states its own dependencies below rather than naming them in a load order:
\ lib/vector.f is no longer in the engine, so a consumer that loaded this file
\ without it would fail at VEC-HEADER-CELLS.
\
\ Public surface (the package owns every cell):
\   WORD COMMENT REGISTRY              token kinds returned by KIND@
\   UNTERMINATED-QUOTE MALFORMED-REGISTRY   diagnostic kinds from ERROR-KIND@
\   SOURCE ( ptr u8 n -- )             scan a buffer; clears all prior state first
\   OPERAND ( n -- )                   token n parses the next token: rescan
\                                      from it by parse-name's rule
\   OPERAND? ( n -- bool )             token n is a raw operand, read as data
\   COUNT ( -- n )                     tokens produced by the last SOURCE
\   TOKEN CONTENT ( n -- ptr u8 n )    token span / paren-comment body or
\                                      string-literal payload span
\   LITERAL? ( n -- bool )             token n is a string literal
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
\ no token after that site is exposed. Consumers only read it back; a rescan by
\ OPERAND writes it afresh, as SOURCE does. A consumer that requires valid source
\ must reject when ERROR? is true, and should read ERROR-KIND@ to name which
\ defect it hit.

require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/source.f

package LINT-LEX
private

1024 constant MIN-CAP
0 constant NO-ERROR
$22 constant DQUOTE             \ the byte that closes a string literal

0 constant ROW-BARE             \ FAM: bare `PRIM:` row, closed by `PRIM;`
1 constant ROW-PKG              \ FAM: `PPRIM:` row, closed by `PPRIM;` or `CLOSE-PRIVATE`

variable CAP
TYPED-VARIABLE SRC-A ptr u8   \ start of the source span being lexed
variable TOK-N   variable SRC-U   variable POS
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

create KIND-V VEC-HEADER-CELLS cells allot
\ The two address columns hold starts into the linted source, so their vector
\ headers are declared pointer storage and carry the element type.
VEC-HEADER-CELLS TYPED-BUFFER ADDR-V ptr u8
create LEN-V VEC-HEADER-CELLS cells allot
create BYTE-V VEC-HEADER-CELLS cells allot
create LINE-V VEC-HEADER-CELLS cells allot
create COL-V VEC-HEADER-CELLS cells allot
VEC-HEADER-CELLS TYPED-BUFFER CADDR-V ptr u8
create CLEN-V VEC-HEADER-CELLS cells allot
create OPND-V VEC-HEADER-CELLS cells allot   \ the token is a raw operand
\ The locals live after the tokens below STEPPED, read from those tokens (STEP):
\ the token of each live local's name and the block depth that declared it.
create LOCAL-V VEC-HEADER-CELLS cells allot
create LDEPTH-V VEC-HEADER-CELLS cells allot
variable STEPPED
variable DEPTH                  \ blocks open, counted while a local lives
variable IN-GROUP               \ those tokens end inside a `{: … :}` group

\ ---- raw table cell -> NUM role bridges for the typed VEC surface ---------
\ The lexer's parallel record columns store raw cells (token / content addresses,
\ lengths, kinds, byte/line/col positions). The typed VEC surface (package VEC)
\ reads a validated NUM role - a capacity is a `NUM:item-count`, a record
\ position is a `NUM:index` - so a count/index role swap at a VEC call is a
\ checker reject. These lift a nonnegative cell to its role through the PUBLIC
\ NUM validators (no laundering back to n, no reopened package); the refusal
\ arms are unreachable invariants (MIN-CAP and a live record index are
\ nonnegative), an impossible negative surfaces the vector's own capacity / bounds
\ code. This is the maki/sched-key.f SK>ITEM / SK>INDEX idiom, kept lexer-local.
: N>ITEM ( n -- NUM:item-count )
   NUM:ITEM-COUNT
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-CAPACITY throw ENDOF
      zero OF E-VEC-CAPACITY throw ENDOF        overflow OF E-VEC-CAPACITY throw ENDOF
      underflow OF E-VEC-CAPACITY throw ENDOF   bad-alignment OF E-VEC-CAPACITY throw ENDOF
      misaligned OF E-VEC-CAPACITY throw ENDOF
   ;MATCH ;
: N>INDEX ( n -- NUM:index )
   NUM:INDEX
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-VEC-BOUNDS throw ENDOF
      zero OF E-VEC-BOUNDS throw ENDOF          overflow OF E-VEC-BOUNDS throw ENDOF
      underflow OF E-VEC-BOUNDS throw ENDOF     bad-alignment OF E-VEC-BOUNDS throw ENDOF
      misaligned OF E-VEC-BOUNDS throw ENDOF
   ;MATCH ;

: SRC@ ( -- ptr u8 )
   SRC-A @ ;

: SRC! ( ptr u8 -- )
   SRC-A ! ;

: INIT-ONE ( ptr a -- )
   MIN-CAP N>ITEM VEC:INIT ;

: CLEAR-ONE ( ptr a -- )
   VEC:CLEAR ;

: INIT-VECTORS ( -- )
   KIND-V INIT-ONE
   0 ADDR-V INIT-ONE
   LEN-V INIT-ONE
   BYTE-V INIT-ONE
   LINE-V INIT-ONE
   COL-V INIT-ONE
   0 CADDR-V INIT-ONE
   CLEN-V INIT-ONE
   OPND-V INIT-ONE
   LOCAL-V INIT-ONE
   LDEPTH-V INIT-ONE
   MIN-CAP CAP ! ;

: CLEAR-VECTORS ( -- )
   KIND-V CLEAR-ONE
   0 ADDR-V CLEAR-ONE
   LEN-V CLEAR-ONE
   BYTE-V CLEAR-ONE
   LINE-V CLEAR-ONE
   COL-V CLEAR-ONE
   0 CADDR-V CLEAR-ONE
   CLEN-V CLEAR-ONE
   OPND-V CLEAR-ONE ;

\ Forget the locals state: the next question reads the tokens again from the
\ first (LOCALS-SYNC).
: LOCALS-RESET ( -- )
   LOCAL-V CLEAR-ONE
   LDEPTH-V CLEAR-ONE
   0 STEPPED !
   0 DEPTH !
   false IN-GROUP ! ;

: RESET-TABLES ( -- )
   CAP @ 0= if INIT-VECTORS else CLEAR-VECTORS then
   0 TOK-N !
   LOCALS-RESET ;

\ RAW residual (maki/sched-key.f SK-N precedent): VEC:LEN@ yields a
\ NUM:item-count and the checker correctly refuses to launder it back to n, but
\ TOK-N is a raw n cache that drives the lexer's raw token arithmetic
\ (COUNT 1- ...), so the count is read through the raw VEC-LEN@ accessor for this
\ word alone.
: SYNC-COUNT ( -- )
   KIND-V VEC-LEN@ LEN>N TOK-N ! ;

: ADD ( n ptr u8 n n n n ptr u8 n -- ) {: kind:n a:ptr u:n byte:n line:n col:n ca:ptr cu:n :}
   kind KIND-V VEC:PUSH drop
   a 0 ADDR-V VEC:PUSH drop
   u LEN-V VEC:PUSH drop
   byte BYTE-V VEC:PUSH drop
   line LINE-V VEC:PUSH drop
   col COL-V VEC:PUSH drop
   ca 0 CADDR-V VEC:PUSH drop
   cu CLEN-V VEC:PUSH drop
   false OPND-V VEC:PUSH drop
   SYNC-COUNT ;

\ The locals state reads the marks, so a mark on a token it has read forgets it.
: MARK-OPERAND ( n -- ) {: k:n :}
   true OPND-V k N>INDEX VEC:!
   k STEPPED @ < if LOCALS-RESET then ;

public

1 constant WORD                 \ KIND@: whitespace-delimited word token
2 constant COMMENT              \ KIND@: `( ... )` or `.( ... )` comment, body via CONTENT
3 constant REGISTRY             \ KIND@: one complete PRIM:/PPRIM: primitive-axiom row

1 constant UNTERMINATED-QUOTE   \ ERROR-KIND@: a string literal ran past end of input
2 constant MALFORMED-REGISTRY   \ ERROR-KIND@: a primitive-axiom row lacked a header or its closer

: COUNT ( -- n )
   TOK-N @ ;

: TOKEN ( n -- ptr u8 n ) {: k:n :}
   0 ADDR-V k N>INDEX VEC:@
   LEN-V k N>INDEX VEC:@ ;

: CONTENT ( n -- ptr u8 n ) {: k:n :}
   0 CADDR-V k N>INDEX VEC:@
   CLEN-V k N>INDEX VEC:@ ;

: KIND@ ( n -- n ) {: k:n :}
   KIND-V k N>INDEX VEC:@ ;

: BYTE@ ( n -- n ) {: k:n :}
   BYTE-V k N>INDEX VEC:@ ;

: LINE@ ( n -- n ) {: k:n :}
   LINE-V k N>INDEX VEC:@ ;

: COL@ ( n -- n ) {: k:n :}
   COL-V k N>INDEX VEC:@ ;

\ Token k is a raw operand: data the source never runs, so it starts nothing,
\ ends nothing and makes no name of the token after it.
: OPERAND? ( n -- bool ) {: k:n :}
   OPND-V k N>INDEX VEC:@ ;

: ERROR? ( -- bool )
   ERR-KIND @ NO-ERROR <> ;

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

\ A scan guard tests the byte at POS only when there is one: `and` evaluates
\ both operands, so `END? 0=` beside `CUR` reads the byte past the end, and a
\ buffer sized to the source ends there (a source whose last token ended on a
\ 64 KiB boundary faulted in the reserved-name lint).
: CUR-NOT? ( n -- bool ) {: c:n :}
   END? if false exit then
   CUR c <> ;

: SKIP-QUOTE ( -- bool )
   begin END? 0= while ADV DQUOTE = if true exit then repeat
   false ;

: SKIP-ESC-QUOTE ( -- bool )
   begin END? 0= while
      ADV dup 92 = if
         drop END? 0= if ADV drop then
      else
         DQUOTE = if true exit then
      then
   repeat
   false ;

: CLEAR-ERROR ( -- )
   NO-ERROR ERR-KIND !
   0 ERR-BYTE !
   0 ERR-LINE !
   0 ERR-COL !
   false HALTED ! ;

: MARK-UNTERM ( n -- ) {: k:n :}
   UNTERMINATED-QUOTE ERR-KIND !
   k BYTE@ ERR-BYTE !
   k LINE@ ERR-LINE !
   k COL@ ERR-COL ! ;

: LINE-COMMENT ( -- )
   begin 10 CUR-NOT? while ADV drop repeat ;

: BODY-A ( -- ptr u8 )
   SRC@ CSTART @ + ;

: BODY-U ( -- n )
   CLEN @ ;

: TO-PAREN ( -- )
   begin 41 CUR-NOT? while ADV drop repeat ;

\ The span from the token start the main loop recorded to the current position.
: CUR$ ( -- ptr u8 n )
   SRC@ START @ + POS @ START @ - ;

\ `parse-name` and the engine token loop delimit on every byte at or below space.
: ENGINE-DELIM? ( n -- bool )
   $20 <= ;

: GAP? ( -- bool )
   END? if false exit then
   CUR ENGINE-DELIM? ;

: INK? ( -- bool )
   END? if false exit then
   CUR ENGINE-DELIM? 0= ;

\ Engine parity: `(` opens a comment only as a standalone token (followed by
\ an engine delimiter or EOF). A `(`-initial token such as `(CMP)` is one word.
: PAREN-STANDALONE? ( -- bool )
   POS @ 1+ SRC-U @ >= if true exit then
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
   s" .(" STR= ;

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
   begin GAP? while ADV drop repeat ;

: F$ ( -- ptr u8 n )
   SRC@ FOFF @ + FLEN @ ;

\ FLEN 0 means end of input: no field was left to read.
: NEXT-FIELD ( -- )
   SKIP-RAW-WS
   POS @ FOFF !
   begin INK? while ADV drop repeat
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
   s" PRIM:" STR=CI ;

: PPRIM-OPEN? ( ptr u8 n -- bool )
   s" PPRIM:" STR=CI ;

: ROW-OPEN? ( ptr u8 n -- bool )
   2dup PPRIM-OPEN? if 2drop true exit then
   PRIM-OPEN? ;

: PRIM-CLOSE? ( ptr u8 n -- bool )
   s" PRIM;" STR=CI ;

: PPRIM-CLOSE? ( ptr u8 n -- bool )
   s" PPRIM;" STR=CI ;

: PRIVATE-CLOSE? ( ptr u8 n -- bool )
   s" CLOSE-PRIVATE" STR=CI ;

\ `PRIM;` closes a bare row; a package row closes with `PPRIM;` (public wordlist)
\ or `CLOSE-PRIVATE`
\ (package private wordlist). A bare row has no package wordlist, so
\ `CLOSE-PRIVATE` there is an ordinary effect field, not a closer.
: ROW-CLOSE? ( ptr u8 n -- bool )
   FAM @ ROW-PKG = if
      2dup PPRIM-CLOSE? if 2drop true exit then
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
\ from src/habu/verify-source.f BODY-PARSER?, which treats `char` and `[char]`
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
   2dup s" [']" STR= if 2drop true exit then
   s" [char]" STR=CI ;

: MARK-BAD ( -- )
   MALFORMED-REGISTRY ERR-KIND !
   R-BYTE @ ERR-BYTE !
   R-LINE @ ERR-LINE !
   R-COL @ ERR-COL !
   true HALTED ! ;

\ A header field names the primitive (and, for a package row, its package). An
\ opener or a closer of this row's family there is never a name: the row is
\ missing its header.
: HDR-BAD? ( -- bool )
   FLEN @ 0= if true exit then
   F$ ROW-OPEN? if true exit then
   F$ ROW-CLOSE? if true exit then
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
   END? if false exit then
   ADV drop true ;

\ false = the operand ran past end of input, so the row can never close.
: FIELD-OPERAND ( -- bool )
   F$ SOURCE:ESC-STRING-OPENER? if SKIP-ESC-QUOTE exit then
   F$ SOURCE:NORMAL-STRING-OPENER? if SKIP-QUOTE exit then
   F$ PRINT-OPEN? if SKIP-PRINT exit then
   F$ PARSE-NEXT? if NEXT-FIELD FLEN @ 0 <> exit then
   true ;

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

\ After `:` or `undefine` the engine consumes the next word as a parsed name and
\ never executes it, so `: PRIM: ( -- ) parse-name PE-OPEN ;` in
\ src/core/checker.f declares the opener rather than opening a row.
: NAMER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" :" STR= if true exit then
   a u s" undefine" STR=CI ;

\ Token k is a name: the word before it is a namer. A `:` that is itself an
\ operand (`' :`) is data and names nothing.
: NAMED-AT? ( n -- bool ) {: k:n :}
   k 0= if false exit then
   k 1- KIND@ WORD <> if false exit then
   k 1- OPERAND? if false exit then
   k 1- TOKEN NAMER? ;

\ The word the scan reads now is a name.
: NAME-POS? ( -- bool )
   COUNT NAMED-AT? ;

\ ---- the locals a body declares -----------------------------------------------
\ The engine and the checker look a body token up among the live locals before
\ every keyword but `;` (src/habu/habu2.f EM-COMPILE-LOCAL, src/core/checker.f
\ LOC-REF?), byte for byte, so a local named `char`, `[']` or `s"` is that local:
\ it takes no operand and opens no string. A `{: … :}` group reads its names raw,
\ and a name ends at its first `:`. A local lives to the end of the control
\ block that declared it (SOURCE:BLOCK-OPENER?) and `else` drops the true arm's;
\ `:` and `;` drop them all. An enclosing local is still that local inside a
\ quotation, where the engine refuses it at that token. The state is read from
\ the tokens the scan has made, never kept beside them, so a rescan (OPERAND)
\ reads it again as it was at its token. Blocks are counted only while a local
\ lives: one opened with none live drops none, and a local only needs the count
\ to change from its own group on, so a body with no locals pays no block test.

\ The name the group word k declares.
: LOCAL$ ( n -- ptr u8 n ) {: k:n :}
   k TOKEN {: a:ptr u:n :}
   0 begin dup u < if a over + c@ $3A <> else false then while 1+ repeat
   a swap ;

: LIVE ( -- n )
   LOCAL-V VEC-LEN@ LEN>N ;

: LOCAL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   LIVE 0 ?do
      a u LOCAL-V i N>INDEX VEC:@ LOCAL$ STR= if true unloop exit then
   loop
   false ;

: LOCAL-PUSH ( n -- )
   LOCAL-V VEC:PUSH drop
   DEPTH @ LDEPTH-V VEC:PUSH drop ;

: LOCALS-DROP ( -- )
   LOCAL-V CLEAR-ONE
   LDEPTH-V CLEAR-ONE ;

\ Drop the locals the innermost open block declared.
: BLOCK-DROP ( -- )
   begin LIVE 0 > if LDEPTH-V LIVE 1- N>INDEX VEC:@ DEPTH @ >= else false then while
      LIVE 1- VEC-LEN {: l :}
      l LOCAL-V VEC-LEN!
      l LDEPTH-V VEC-LEN!
   repeat ;

\ Block word k acts unless it is a name or names a live local.
: BLOCK-WORD? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k NAMED-AT? if false exit then
   a u LOCAL? 0= ;

: BLOCK-STEP ( n ptr u8 n -- ) {: k:n a:ptr u:n :}
   a u SOURCE:BLOCK-OPENER? if
      k a u BLOCK-WORD? if DEPTH @ 1+ DEPTH ! then exit
   then
   a u s" else" STR=CI if
      k a u BLOCK-WORD? if BLOCK-DROP then exit
   then
   a u SOURCE:BLOCK-CLOSER? if
      k a u BLOCK-WORD? if BLOCK-DROP DEPTH @ 1- DEPTH ! then
   then ;

\ Read token k into the state that holds after the tokens before it.
: STEP ( n -- ) {: k:n :}
   k KIND@ WORD <> if exit then
   k TOKEN {: a:ptr u:n :}
   IN-GROUP @ if
      a u s" :}" STR= if false IN-GROUP ! exit then
      k LOCAL-PUSH exit
   then
   k OPERAND? if exit then
   a u s" ;" STR= if LOCALS-DROP exit then
   a u s" :" STR= if LOCALS-DROP exit then
   a u s" {:" STR= if
      k NAMED-AT? 0= if true IN-GROUP ! then exit
   then
   LIVE 0= if exit then
   k a u BLOCK-STEP ;

\ Bring the state up to the tokens the scan has made.
: LOCALS-SYNC ( -- )
   begin STEPPED @ COUNT < while
      STEPPED @ STEP
      STEPPED @ 1+ STEPPED !
   repeat ;

\ The word the scan reads now names a live local, so a spelling that would
\ steer the scan is a plain word.
: LOCAL-START? ( -- bool )
   LOCALS-SYNC
   CUR$ LOCAL? ;

\ A group opener; named after `:` or `undefine`, it is a name. No local is
\ spelled `{:`, nor a row opener, since a local's name ends before its `:`.
: GROUP-START? ( -- bool )
   CUR$ s" {:" STR= 0= if false exit then
   NAME-POS? 0= ;

: ROW-START? ( -- bool )
   CUR$ ROW-OPEN? 0= if false exit then
   NAME-POS? 0= ;

\ The same name-position rule a row opener obeys: after `:` or `undefine` the
\ engine parses the next word as a name and never executes it, so
\ `: .( ( -- ) ;` DEFINES a word spelled `.(` instead of opening a printing
\ comment.
: PRINT-START? ( -- bool )
   CUR$ PRINT-OPEN? 0= if false exit then
   NAME-POS? if false exit then
   LOCAL-START? 0= ;

\ A parsing keyword the engine runs reads the next token raw, whatever it
\ spells, so `char \` hides nothing after it and `['] (` opens no comment.
\ Named after `:` or `undefine`, the keyword is a name and takes nothing, and a
\ live local of its name is that local.
: PARSER-START? ( -- bool )
   CUR$ SOURCE:PARSING-KEYWORD? 0= if false exit then
   NAME-POS? if false exit then
   LOCAL-START? 0= ;

\ The span from START to POS is one WORD token.
: ADD-WORD ( -- )
   WORD CUR$ START @ START-LINE @ START-COL @ SRC@ 0 ADD ;

\ Read the next whitespace-delimited token, across line ends, as one WORD token;
\ false at end of input, where there is none.
: RAW-WORD ( -- bool )
   SKIP-RAW-WS
   END? if false exit then
   POS @ START !  LINE-N @ START-LINE !  COL-N @ START-COL !
   begin INK? while ADV drop repeat
   ADD-WORD
   true ;

: RAW-OPERAND ( -- )
   RAW-WORD if COUNT 1- MARK-OPERAND then ;

\ The names of a `{: … :}` group, read raw and marked, up to its plain closer.
: GROUP ( -- )
   begin RAW-WORD while
      CUR$ s" :}" STR= if exit then
      COUNT 1- MARK-OPERAND
   repeat ;

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
   a u SOURCE:ESC-STRING-OPENER? if true exit then
   a u SOURCE:NORMAL-STRING-OPENER? ;

\ A string opener opens a literal unless a live local has its spelling.
: LITERAL-START? ( ptr u8 n -- bool )
   STRING-OPENER? 0= if false exit then
   LOCAL-START? 0= ;

\ A string-literal token carries its payload in CONTENT, exactly as a paren
\ comment carries its body there. The payload is deliberately never tokenized -
\ that is what stops a quoted word being mistaken for code - so without this a
\ consumer that has to reason about a quoted NAME has no route to it but
\ substring search over the raw source, which is the evasion route this lexer
\ exists to close. The checker's own concrete type table is written that way
\ (`s" n" CC-N CT-INT 64 CS-GENERIC CT-SET`), and the type names in it are only
\ reachable here.
: SCAN-WORD ( -- )
   begin INK? while ADV drop repeat
   GROUP-START? if ADD-WORD GROUP exit then
   ROW-START? if
      CUR$ PPRIM-OPEN? if ROW-PKG else ROW-BARE then SCAN-ROW
      exit
   then
   \ `.(` is already consumed by the word scan, so the print body starts at POS.
   PRINT-START? if PAREN-BODY exit then
   PARSER-START? if ADD-WORD RAW-OPERAND exit then
   CUR$ {: a:ptr u:n :}
   START @ START-LINE @ START-COL @ {: byte:n line:n col:n :}
   a u LITERAL-START? 0= if
      WORD a u byte line col SRC@ 0 ADD exit
   then
   a u SOURCE:ESC-STRING-OPENER? STRING-PAYLOAD {: pa:ptr pu:n closed:bool :}
   WORD a u byte line col pa pu ADD
   closed 0= if COUNT 1- MARK-UNTERM then ;

\ The engine's token loop over the rest of the source, from POS.
: SCAN ( -- )
   begin END? 0= HALTED @ 0= and while
      CUR ENGINE-DELIM? if ADV drop
      else
         POS @ START !  LINE-N @ START-LINE !  COL-N @ START-COL !
         CUR 92 = if LINE-COMMENT
         else CUR 40 = PAREN-STANDALONE? and if PAREN-COMMENT
         else SCAN-WORD then then
      then
   repeat ;

\ ---- an operand taken by parse-name -------------------------------------------
\ The scan reads a parsing keyword's operand raw as it meets the keyword
\ (PARSER-START?). A definer reads its name with `parse-name` as well: the next
\ whitespace-delimited token, whatever it spells, so `package (` names the
\ package `(` and `: \` defines the word `\`. Which token is such a definer is a
\ grammar question this lexer cannot answer: `DEFLINEAR` parses its name at top
\ level but is an ordinary call inside a body, where a `(` after it opens a
\ comment. So the consumer that knows the grammar names the token, and OPERAND
\ reads what follows it again. OPERAND? answers true for either operand.

\ The first byte at or after a byte index that is not an engine delimiter, or
\ the end of the source.
: INK-FROM ( n -- n )
   begin dup SRC-U @ < if SRC@ over + c@ ENGINE-DELIM? else false then while
      1+
   repeat ;

\ The first engine delimiter at or after a byte index, or the end of the source.
: GAP-FROM ( n -- n )
   begin dup SRC-U @ < if SRC@ over + c@ ENGINE-DELIM? 0= else false then while
      1+
   repeat ;

\ Token k is the raw token at [a, a+u) when the scan read it there as a plain
\ word. A string opener of that spelling is not one: the scan swallowed its
\ literal as well.
: PLAIN-AT? ( n n n -- bool ) {: k:n a:n u:n :}
   k COUNT >= if false exit then
   k KIND@ WORD <> if false exit then
   k BYTE@ a <> if false exit then
   k TOKEN {: ta:ptr tu:n :}
   tu u <> if false exit then
   ta tu STRING-OPENER? 0= ;

\ Keep the first n tokens and drop the rest, and the locals state when it read
\ a dropped one.
: KEEP ( n -- )
   dup STEPPED @ < if LOCALS-RESET then
   VEC-LEN {: l :}
   l KIND-V VEC-LEN!
   l 0 ADDR-V VEC-LEN!
   l LEN-V VEC-LEN!
   l BYTE-V VEC-LEN!
   l LINE-V VEC-LEN!
   l COL-V VEC-LEN!
   l 0 CADDR-V VEC-LEN!
   l CLEN-V VEC-LEN!
   l OPND-V VEC-LEN!
   SYNC-COUNT ;

\ A token whose spelling steers how the scan reads the token after it.
: STEERS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u SOURCE:PARSING-KEYWORD? if true exit then
   a u s" {:" STR= if true exit then
   a u NAMER? ;

public

\ Token k is a string literal: an opener whose payload CONTENT holds, one
\ delimiter past it (STRING-PAYLOAD). A word spelled as an opener that opened
\ nothing, such as a live local `s"`, holds none.
: LITERAL? ( n -- bool )
   {: k:n :}
   k KIND@ WORD <> if false exit then
   k TOKEN {: a:ptr u:n :}
   a u STRING-OPENER? 0= if false exit then
   k CONTENT drop a u + 1+ = ;

: SOURCE ( ptr u8 n -- ) {: a:ptr u:n :}
   a SRC! u SRC-U ! 0 POS ! 1 LINE-N ! 1 COL-N !
   CLEAR-ERROR
   RESET-TABLES
   SCAN ;

\ Token k is a word that takes the next whitespace-delimited token as its
\ operand; the caller matched its spelling, so it is a plain word. Token k+1
\ becomes that raw operand. When the scan read it as anything else (a comment,
\ a string literal, a row, a print body, or a token past a dropped `\` line),
\ or its spelling steered the scan of the token after it (`create char` names a
\ word that takes nothing), the rest of the source is scanned again from the
\ end of token k. Otherwise only the mark changes, so asking costs a span
\ compare and a spelling test.
: OPERAND ( n -- ) {: k:n :}
   k BYTE@ k TOKEN nip + {: end:n :}
   end INK-FROM {: a:n :}
   a GAP-FROM a - {: u:n :}
   u 0= if exit then
   k 1+ a u PLAIN-AT? if
      k 1+ TOKEN STEERS? 0= if k 1+ MARK-OPERAND exit then
   then
   k 1+ KEEP
   end POS !
   k LINE@ LINE-N !
   k COL@ end k BYTE@ - + COL-N !
   CLEAR-ERROR
   RAW-OPERAND
   SCAN ;

;package
