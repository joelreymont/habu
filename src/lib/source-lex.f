\ source-lex.f - package LINT-LEX: the one shared lexer for Habu source, for the
\ lint, checker and codegen tooling and applications that read source by its
\ tokens. Tokens point into the span SOURCE was given, so that span stays in
\ place while they are read.
\ STORAGE CLASS. PROCESS-WIDE: one token table and scan state for the image;
\ SOURCE replaces the last scan's tokens. SOURCE makes the tables on the heap
\ when none are made; RELEASE deletes them and forgets the span.

require src/lib/string.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/heap.f
require src/lib/source.f
require src/habu/escape.f

package LINT-LEX
private

0 constant NO-ERROR
$22 constant DQUOTE             \ the byte that closes a string literal

0 constant ROW-BARE             \ FAM: an `EPRIM:` row
1 constant ROW-PKG              \ FAM: an `EPPRIM:` row

\ partial: the tables possibly part made.
ENUM tables released partial made ;ENUM
TYPED-VARIABLE TABLES tables
TYPED-VARIABLE SRC-A ^u8   \ start of the source span being lexed
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

\ Each table is a vector the cell holds, NULL-PTR while it is not made.
TYPED-VARIABLE KIND-V ^VEC:vector<n>
\ The two address columns hold starts into the linted source.
TYPED-VARIABLE ADDR-V ^VEC:vector<^u8>
TYPED-VARIABLE LEN-V ^VEC:vector<n>
TYPED-VARIABLE BYTE-V ^VEC:vector<n>
TYPED-VARIABLE LINE-V ^VEC:vector<n>
TYPED-VARIABLE COL-V ^VEC:vector<n>
TYPED-VARIABLE CADDR-V ^VEC:vector<^u8>
TYPED-VARIABLE CLEN-V ^VEC:vector<n>
TYPED-VARIABLE OPND-V ^VEC:vector<n>   \ the token is a raw operand
\ The locals live after the tokens below STEPPED, read from those tokens (STEP):
\ the token of each live local's name and the block depth that declared it.
TYPED-VARIABLE LOCAL-V ^VEC:vector<n>
TYPED-VARIABLE LDEPTH-V ^VEC:vector<n>
variable STEPPED
variable DEPTH                  \ blocks open, counted while a local lives
variable IN-GROUP               \ those tokens end inside a `{: … :}` group
variable IN-BODY                \ those tokens end inside a definition body

: SRC@ ( -- ^u8 )
   SRC-A @ ;

: SRC! ( ^u8 -- )
   SRC-A ! ;

\ NULL-PTR goes in before NEW: a refused NEW leaves no vector to delete twice.
: INIT-ONE ( ^^VEC:vector<n> -- )
   {: c:^ :}
   c @ VEC:DELETE  NULL-PTR c !
   HEAP:ALLOCATOR 0 [type] n VEC:NEW c ! ;

: INIT-ADDR ( ^^VEC:vector<^u8> -- )
   {: c:^ :}
   c @ VEC:DELETE  NULL-PTR c !
   HEAP:ALLOCATOR 0 [type] ^u8 VEC:NEW c ! ;

: CLEAR-ONE ( ^^VEC:vector<t> -- )
   {: c:^ :}
   c @ NULL-PTR = if exit then
   c @ 0 VEC:RESIZE ;

\ The vector a table's cell holds; one not made is E-VEC-BOUNDS.
: TABLE@ ( ^^VEC:vector<t> -- ^VEC:vector<t> )
   @ dup NULL-PTR = if E-VEC-BOUNDS throw then ;

: CLEAR-ERROR ( -- )
   NO-ERROR ERR-KIND !
   0 ERR-BYTE !
   0 ERR-LINE !
   0 ERR-COL !
   false HALTED ! ;

TABLES-RELEASED TABLES !

: INIT-VECTORS ( -- )
   KIND-V INIT-ONE
   ADDR-V INIT-ADDR
   LEN-V INIT-ONE
   BYTE-V INIT-ONE
   LINE-V INIT-ONE
   COL-V INIT-ONE
   CADDR-V INIT-ADDR
   CLEN-V INIT-ONE
   OPND-V INIT-ONE
   LOCAL-V INIT-ONE
   LDEPTH-V INIT-ONE
   construct tables made TABLES ! ;

: CLEAR-VECTORS ( -- )
   KIND-V CLEAR-ONE
   ADDR-V CLEAR-ONE
   LEN-V CLEAR-ONE
   BYTE-V CLEAR-ONE
   LINE-V CLEAR-ONE
   COL-V CLEAR-ONE
   CADDR-V CLEAR-ONE
   CLEN-V CLEAR-ONE
   OPND-V CLEAR-ONE ;

\ Forget the locals state: the next question reads the tokens again from the
\ first (LOCALS-SYNC).
: LOCALS-RESET ( -- )
   LOCAL-V CLEAR-ONE
   LDEPTH-V CLEAR-ONE
   0 STEPPED !
   0 DEPTH !
   false IN-GROUP !
   false IN-BODY ! ;

: DISPOSE-VECTORS ( -- )
   KIND-V @ VEC:DELETE  NULL-PTR KIND-V !
   ADDR-V @ VEC:DELETE  NULL-PTR ADDR-V !
   LEN-V @ VEC:DELETE  NULL-PTR LEN-V !
   BYTE-V @ VEC:DELETE  NULL-PTR BYTE-V !
   LINE-V @ VEC:DELETE  NULL-PTR LINE-V !
   COL-V @ VEC:DELETE  NULL-PTR COL-V !
   CADDR-V @ VEC:DELETE  NULL-PTR CADDR-V !
   CLEN-V @ VEC:DELETE  NULL-PTR CLEN-V !
   OPND-V @ VEC:DELETE  NULL-PTR OPND-V !
   LOCAL-V @ VEC:DELETE  NULL-PTR LOCAL-V !
   LDEPTH-V @ VEC:DELETE  NULL-PTR LDEPTH-V ! ;

\ The state is partial before any table is made: a refused NEW leaves partial,
\ and the retry makes each table again.
: RESET-TABLES ( -- )
   TABLES @
   MATCH tables
      released OF TABLES-PARTIAL TABLES ! INIT-VECTORS ENDOF
      partial OF INIT-VECTORS ENDOF
      made OF CLEAR-VECTORS ENDOF
   ;MATCH
   0 TOK-N !
   LOCALS-RESET ;

: SYNC-COUNT ( -- )
   KIND-V TABLE@ VEC:COUNT TOK-N ! ;

: ADD ( n ^u8 n n n n ^u8 n -- ) {: kind:n a:^ u:n byte:n line:n col:n ca:^ cu:n :}
   kind KIND-V TABLE@ VEC:PUSH !
   a ADDR-V TABLE@ VEC:PUSH !
   u LEN-V TABLE@ VEC:PUSH !
   byte BYTE-V TABLE@ VEC:PUSH !
   line LINE-V TABLE@ VEC:PUSH !
   col COL-V TABLE@ VEC:PUSH !
   ca CADDR-V TABLE@ VEC:PUSH !
   cu CLEN-V TABLE@ VEC:PUSH !
   false bool>n OPND-V TABLE@ VEC:PUSH !
   SYNC-COUNT ;

\ The locals state reads the marks, so a mark on a token it has read forgets it.
: MARK-OPERAND ( n -- ) {: k:n :}
   true bool>n OPND-V TABLE@ k VEC:AT !
   k STEPPED @ < if LOCALS-RESET then ;

public

1 constant WORD                 \ KIND@: whitespace-delimited word token
2 constant COMMENT              \ KIND@: `( ... )` or `.( ... )` comment, body via CONTENT
3 constant REGISTRY             \ KIND@: one complete primitive-axiom row

1 constant BAD-STRING           \ ERROR-KIND@: a string literal has no closing quote or holds a bad escape
2 constant MALFORMED-REGISTRY   \ ERROR-KIND@: a primitive-axiom row lacked a header or its closer

: COUNT ( -- n )
   TOK-N @ ;

: TOKEN ( n -- ^u8 n ) {: k:n :}
   ADDR-V TABLE@ k VEC:AT @
   LEN-V TABLE@ k VEC:AT @ ;

: CONTENT ( n -- ^u8 n ) {: k:n :}
   CADDR-V TABLE@ k VEC:AT @
   CLEN-V TABLE@ k VEC:AT @ ;

: KIND@ ( n -- n ) {: k:n :}
   KIND-V TABLE@ k VEC:AT @ ;

\ A token's byte offset is 0-based; its line and column are 1-based.
: BYTE@ ( n -- n ) {: k:n :}
   BYTE-V TABLE@ k VEC:AT @ ;

: LINE@ ( n -- n ) {: k:n :}
   LINE-V TABLE@ k VEC:AT @ ;

: COL@ ( n -- n ) {: k:n :}
   COL-V TABLE@ k VEC:AT @ ;

\ Token k is a raw operand: data the source never runs, so it starts nothing,
\ ends nothing and makes no name of the token after it.
: OPERAND? ( n -- bool ) {: k:n :}
   OPND-V TABLE@ k VEC:AT @ 0<> ;

\ The diagnostic is one record, written at most once per scan: the writer stops
\ the scan at the malformed site, so no token after it is exposed. A rescan by
\ OPERAND writes it afresh, as SOURCE does. A consumer that requires valid source
\ rejects when ERROR? is true and reads ERROR-KIND@ to name the defect.
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

\ `parse-name` and the engine token loop delimit on every byte at or below space.
: ENGINE-DELIM? ( n -- bool )
   $20 <= ;

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

: TO-END ( -- )
   begin END? 0= while ADV drop repeat ;

\ The load's reader finds the closing quote, so a bad escape stops the scan
\ as a missing quote does.
: SKIP-ESC-QUOTE ( -- bool )
   END? if false exit then
   SRC@ POS @ + 1 +  SRC@ SRC-U @ +  ESCAPE:SPAN nip
   {: q:^ ok:bool :}
   ok 0= if TO-END false exit then
   q SRC@ - 1 + {: stop:n :}
   begin POS @ stop < while ADV drop repeat
   true ;

: MARK-UNTERM ( n -- ) {: k:n :}
   BAD-STRING ERR-KIND !
   k BYTE@ ERR-BYTE !
   k LINE@ ERR-LINE !
   k COL@ ERR-COL ! ;

: LINE-COMMENT ( -- )
   begin 10 CUR-NOT? while ADV drop repeat ;

: BODY-A ( -- ^u8 )
   SRC@ CSTART @ + ;

: BODY-U ( -- n )
   CLEN @ ;

: TO-PAREN ( -- )
   begin 41 CUR-NOT? while ADV drop repeat ;

\ The span from the token start the main loop recorded to the current position.
: CUR$ ( -- ^u8 n )
   SRC@ START @ + POS @ START @ - ;

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
\ `parse-name`, so `.(` arrives on the word path as a whole token and `.(X)` is
\ an ordinary word, as `(CMP)` is. Its body becomes a COMMENT token whose CONTENT
\ is the body: read word at a time, the body would yield a declaration the
\ engine never performs.
: PRINT-OPEN? ( ^u8 n -- bool )
   s" .(" STR= ;

\ Primitive-axiom rows. The engine reads a row's name (and a package row's
\ package) with `parse-name`, so a row may name a primitive `s"`, `c"`, `."`,
\ `s\"`, `c\"`, `.\"`, `[']` or `[char]`, which a word-at-a-time lexer would
\ read as a string opener or a parsing word. The engine's table
\ (src/habu/prims.f) writes `EPRIM: name ... EPRIM;` and `EPPRIM: pkg name ...
\ ECLOSE-PRIVATE`, and names a definer: `EPRIM: create EPRIM;` read word at a
\ time defines `EPRIM;`. A complete row is one REGISTRY token spanning the row,
\ at its opener, with empty CONTENT; an incomplete row is MALFORMED-REGISTRY at
\ the opener. Header fields are raw; in body fields comments are inert and a
\ parsing word consumes its operand. Row fields share the engine delimiter rule.

: SKIP-RAW-WS ( -- )
   begin GAP? while ADV drop repeat ;

: F$ ( -- ^u8 n )
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

\ The family of the row a token opens, or -1 for a token that opens none.
: ROW-FAMILY ( ^u8 n -- n ) {: a:^ u:n :}
   a u s" EPRIM:" STR=CI if ROW-BARE exit then
   a u s" EPPRIM:" STR=CI if ROW-PKG exit then
   -1 ;

: ROW-OPEN? ( ^u8 n -- bool )
   ROW-FAMILY -1 <> ;

\ A package row names its package before the primitive.
: PKG-ROW? ( -- bool )
   FAM @ ROW-PKG = ;

: EPRIM-CLOSE? ( ^u8 n -- bool )
   s" EPRIM;" STR=CI ;

: EPRIVATE-CLOSE? ( ^u8 n -- bool )
   s" ECLOSE-PRIVATE" STR=CI ;

\ Both closers write the open row, so the engine ends either row at the first
\ `EPRIM;` or `ECLOSE-PRIVATE`.
: ROW-CLOSE? ( ^u8 n -- bool ) {: a:^ u:n :}
   a u EPRIM-CLOSE? if true exit then
   a u EPRIVATE-CLOSE? ;

\ `[']` and `[char]` parse one raw operand, so a closer-shaped token there is
\ that operand and not a closer. Bracket-less `char` is not here: `EPRIM: FOO
\ char EPRIM; create LEAK EPRIM;` closes at the first closer and hands `create
\ LEAK` to the consumer as tokens.
: PARSE-NEXT? ( ^u8 n -- bool )
   2dup s" [']" STR= if 2drop true exit then
   s" [char]" STR=CI ;

: MARK-BAD ( -- )
   MALFORMED-REGISTRY ERR-KIND !
   R-BYTE @ ERR-BYTE !
   R-LINE @ ERR-LINE !
   R-COL @ ERR-COL !
   true HALTED ! ;

\ A header field names the primitive (and, for a package row, its package). An
\ opener or a closer there is never a name: the row is missing its header.
: HDR-BAD? ( -- bool )
   FLEN @ 0= if true exit then
   F$ ROW-OPEN? if true exit then
   F$ ROW-CLOSE? ;

: HDR-FIELD ( -- )
   NEXT-FIELD
   HDR-BAD? if MARK-BAD then ;

: HDR ( -- )
   HDR-FIELD
   HALTED @ if exit then
   PKG-ROW? if HDR-FIELD then ;

\ A row body is interpreted, so `.( ... )` there parses its own text exactly like
\ `s" ... "` does: a closer spelled inside a print body is that text and not this
\ row's closer. false = the print body ran past end of input.
: SKIP-PRINT ( -- bool )
   TO-PAREN
   END? if false exit then
   ADV drop true ;

\ false = the operand holds a bad escape or runs past end of input, so the
\ row can never close.
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

\ After a body opener or `undefine` the engine consumes the next word as a parsed
\ name and never executes it, so `: EPRIM: ( -- )` declares the opener rather
\ than opening a row.
: NAMER? ( ^u8 n -- bool ) {: a:^ u:n :}
   a u SOURCE-SYNTAX:BODY-OPENER? if true exit then
   a u s" undefine" STR=CI ;

\ Token k is a name: the word before it is a namer. A `:` that is itself an
\ operand (`' :`) is data and names nothing.
: NAMED-AT? ( n -- bool ) {: k:n :}
   k 0= if false exit then
   k 1- KIND@ WORD <> if false exit then
   k 1- OPERAND? if false exit then
   k 1- TOKEN NAMER? ;

: NAME-POS? ( -- bool )
   COUNT NAMED-AT? ;

\ Before any keyword but `;`, a body token spelling a live local byte for byte
\ is that local (src/habu/interpreter.f BODY-LOCAL?); a rescan reads locals
\ from tokens.

: LOCAL$ ( n -- ^u8 n ) {: k:n :}
   k TOKEN {: a:^ u:n :}
   0 begin dup u < if a over + c@ $3A <> else false then while 1+ repeat
   a swap ;

: LIVE ( -- n )
   LOCAL-V @ {: v:^ :}
   v NULL-PTR = if 0 exit then
   v VEC:COUNT ;

: LOCAL? ( ^u8 n -- bool ) {: a:^ u:n :}
   LIVE 0 ?do
      a u LOCAL-V TABLE@ i VEC:AT @ LOCAL$ STR= if true unloop exit then
   loop
   false ;

: LOCAL-PUSH ( n -- )
   LOCAL-V TABLE@ VEC:PUSH !
   DEPTH @ LDEPTH-V TABLE@ VEC:PUSH ! ;

: LOCALS-DROP ( -- )
   LOCAL-V CLEAR-ONE
   LDEPTH-V CLEAR-ONE ;

: BLOCK-DROP ( -- )
   begin
      LIVE 0 > if LDEPTH-V TABLE@ LIVE 1- VEC:AT @ DEPTH @ >= else false then
   while
      LIVE 1- {: l:n :}
      LOCAL-V TABLE@ l VEC:RESIZE
      LDEPTH-V TABLE@ l VEC:RESIZE
   repeat ;

\ Block word k acts unless it is a name or names a live local.
: BLOCK-WORD? ( n ^u8 n -- bool ) {: k:n a:^ u:n :}
   k NAMED-AT? if false exit then
   a u LOCAL? 0= ;

: BLOCK-STEP ( n ^u8 n -- ) {: k:n a:^ u:n :}
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
   k TOKEN {: a:^ u:n :}
   IN-GROUP @ if
      a u s" :}" STR= if false IN-GROUP ! exit then
      k LOCAL-PUSH exit
   then
   k OPERAND? if exit then
   a u s" ;" STR= if LOCALS-DROP false IN-BODY ! exit then
   a u SOURCE-SYNTAX:BODY-OPENER? if
      LOCALS-DROP k NAMED-AT? 0= if true IN-BODY ! then exit
   then
   a u s" {:" STR= if
      k NAMED-AT? 0= if true IN-GROUP ! then exit
   then
   LIVE 0= if exit then
   k a u BLOCK-STEP ;

\ Read the state that holds after the tokens below k, from the first token again
\ when the state has read past k.
: LOCALS-TO ( n -- )
   {: k:n :}
   k STEPPED @ < if LOCALS-RESET then
   begin STEPPED @ k < while
      STEPPED @ STEP
      STEPPED @ 1+ STEPPED !
   repeat ;

: LOCALS-SYNC ( -- )
   COUNT LOCALS-TO ;

\ Token k sits inside a definition, where only `[char]` and `[']` parse.
: BODY-AT? ( n -- bool )
   LOCALS-TO IN-BODY @ ;

public

\ Name a u is a live local at token k: declared in the tokens before k, so a
\ group's own name is live from the token after it.
: LIVE-LOCAL? ( n ^u8 n -- bool )
   {: k:n a:^ u:n :}
   k LOCALS-TO
   a u LOCAL? ;

private

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

\ The same name-position rule a row opener obeys: after a body opener or
\ `undefine` the engine parses the next word as a name and never executes it, so
\ `: .( ( -- ) ;` DEFINES a word spelled `.(` instead of opening a printing
\ comment.
: PRINT-START? ( -- bool )
   CUR$ PRINT-OPEN? 0= if false exit then
   NAME-POS? if false exit then
   LOCAL-START? 0= ;

\ A parsing keyword the engine runs reads the next token raw, so `['] (` opens no
\ comment; a name after a body opener or `undefine`, or a live local, takes
\ nothing.
: PARSER-START? ( -- bool )
   CUR$ SOURCE:PARSING-KEYWORD? 0= if false exit then
   NAME-POS? if false exit then
   LOCAL-START? if false exit then
   COUNT BODY-AT? 0= if true exit then
   CUR$ SOURCE-SYNTAX:BODY-PARSING-KEYWORD? ;

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
: STRING-PAYLOAD ( bool -- ^u8 n bool ) {: esc:bool :}
   POS @ 1+ {: pstart:n :}
   esc if SKIP-ESC-QUOTE else SKIP-QUOTE then {: closed:bool :}
   closed 0= if SRC@ 0 closed exit then
   SRC@ pstart + POS @ 1- pstart - closed ;

: STRING-OPENER? ( ^u8 n -- bool ) {: a:^ u:n :}
   a u SOURCE:ESC-STRING-OPENER? if true exit then
   a u SOURCE:NORMAL-STRING-OPENER? ;

\ A string opener opens a literal unless a live local has its spelling.
: LITERAL-START? ( ^u8 n -- bool )
   STRING-OPENER? 0= if false exit then
   LOCAL-START? 0= ;

\ A string-literal token carries its payload in CONTENT, as a paren comment
\ carries its body. The payload is never tokenized, which stops a quoted word
\ being mistaken for code; a consumer that reasons about a quoted name reads it
\ here, not by substring search over the raw source. The checker's concrete type
\ table (`s" n" CC-N CT-INT 64 CS-GENERIC CT-SET`) is reachable only this way.
: SCAN-WORD ( -- )
   begin INK? while ADV drop repeat
   GROUP-START? if ADD-WORD GROUP exit then
   ROW-START? if
      CUR$ ROW-FAMILY SCAN-ROW
      exit
   then
   \ `.(` is already consumed by the word scan, so the print body starts at POS.
   PRINT-START? if PAREN-BODY exit then
   PARSER-START? if ADD-WORD RAW-OPERAND exit then
   CUR$ {: a:^ u:n :}
   START @ START-LINE @ START-COL @ {: byte:n line:n col:n :}
   a u LITERAL-START? 0= if
      WORD a u byte line col SRC@ 0 ADD exit
   then
   a u SOURCE:ESC-STRING-OPENER? STRING-PAYLOAD {: pa:^ pu:n closed:bool :}
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
   k TOKEN {: ta:^ tu:n :}
   tu u <> if false exit then
   ta tu STRING-OPENER? 0= ;

\ Keep the first n tokens and drop the rest, and the locals state when it read
\ a dropped one.
: KEEP ( n -- )
   dup STEPPED @ < if LOCALS-RESET then
   {: l:n :}
   KIND-V TABLE@ l VEC:RESIZE
   ADDR-V TABLE@ l VEC:RESIZE
   LEN-V TABLE@ l VEC:RESIZE
   BYTE-V TABLE@ l VEC:RESIZE
   LINE-V TABLE@ l VEC:RESIZE
   COL-V TABLE@ l VEC:RESIZE
   CADDR-V TABLE@ l VEC:RESIZE
   CLEN-V TABLE@ l VEC:RESIZE
   OPND-V TABLE@ l VEC:RESIZE
   SYNC-COUNT ;

\ Token k, read as a plain word, steers how the scan reads the token after it.
: STEERS? ( n -- bool ) {: k:n :}
   k TOKEN {: a:^ u:n :}
   a u s" {:" STR= if true exit then
   a u NAMER? if true exit then
   a u SOURCE:PARSING-KEYWORD? 0= if false exit then
   k BODY-AT? 0= if true exit then
   a u SOURCE-SYNTAX:BODY-PARSING-KEYWORD? ;

public

\ Token k is a string literal: an opener whose payload CONTENT holds, one
\ delimiter past it (STRING-PAYLOAD). A word spelled as an opener that opened
\ nothing, such as a live local `s"`, holds none.
: LITERAL? ( n -- bool )
   {: k:n :}
   k KIND@ WORD <> if false exit then
   k TOKEN {: a:^ u:n :}
   a u STRING-OPENER? 0= if false exit then
   k CONTENT drop a u + 1+ = ;

: SOURCE ( ^u8 n -- ) {: a:^ u:n :}
   a SRC! u SRC-U ! 0 POS ! 1 LINE-N ! 1 COL-N !
   CLEAR-ERROR
   RESET-TABLES
   SCAN ;

\ Deletes the tables and forgets the span, so no cell holds an address SOURCE
\ was given or made; the next SOURCE makes them again.
: RELEASE ( -- )
   DISPOSE-VECTORS
   TABLES-RELEASED TABLES !
   NULL-PTR SRC!  0 SRC-U !  0 TOK-N !  0 POS !
   LOCALS-RESET ;

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
      k 1+ STEERS? 0= if k 1+ MARK-OPERAND exit then
   then
   k 1+ KEEP
   end POS !
   k LINE@ LINE-N !
   k COL@ end k BYTE@ - + COL-N !
   CLEAR-ERROR
   RAW-OPERAND
   SCAN ;

;package
