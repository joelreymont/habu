\ native-source-fixture.f - the front half of the native chain: a line of
\ straight-line Habu source becomes a sealed tape and the module the elaborator
\ writes into. One lexer serves every caller, so harnesses cannot disagree about
\ a token. Each token's span is its real range in the text, which the module
\ registers as its source for the selector to check. The fixture asserts
\ nothing.

require src/compiler/native/elaborate.f

package NSRC

private

32 constant SP-C
48 constant ZERO-C
57 constant NINE-C
45 constant MINUS-C
256 constant TXT-CAP
128 constant TAPE-CAP

variable LX-I                        \ how many tokens are on the tape
variable LX-P                        \ the byte the scan stands on

create TXT TXT-CAP allot
variable TXT-U

: SLICE ( n n -- ptr u8 n )
   {: st:n ln:n :}
   st TXT + ln ;

\ ---- the tape writer ---------------------------------------------------------
\ The module, the tape and the registered source the current fixture appends to.
1 TYPED-BUFFER W-B IR-BUILD:builder
1 TYPED-BUFFER W-TP IR-ARENA:arena
1 TYPED-BUFFER W-S0 IR-ID:ir-source-id

: WRITER! ( IR-BUILD:builder IR-ARENA:arena IR-ID:ir-source-id -- )
   {: b:IR-BUILD:builder tp:IR-ARENA:arena s0:IR-ID:ir-source-id :}
   b 0 W-B !
   tp 0 W-TP !
   s0 0 W-S0 ! ;

: SPAN ( n n -- IR-SOURCE:span )
   {: st:n ln:n :}
   0 W-B @  0 W-S0 @ st ln IR-BUILD:ADD-SPAN ;

: SYM ( IR-CTX:ctx n n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx st:n ln:n :}
   c 0 W-B @ st ln SLICE IR-BUILD:INTERN-SYMBOL ;

public

\ ---- the source text ---------------------------------------------------------
: TEXT+ ( ptr u8 n -- )
   {: a u:n :}
   TXT-U @ u + TXT-CAP > if E-NSRC-CAP throw then
   0 begin dup u < while
      dup a + c@  over TXT-U @ + TXT + c!
      1+
   repeat drop
   TXT-U @ u + TXT-U ! ;

: TEXT! ( ptr u8 n -- )
   0 TXT-U ! TEXT+ ;

: TEXT$ ( -- ptr u8 n )
   TXT TXT-U @ ;

\ ---- the tape tokens ---------------------------------------------------------
\ A name token whose spelling is exactly the bytes its span covers.
: NAME, ( IR-CTX:ctx n n NTAPE:mode -- )
   {: c:IR-CTX:ctx st:n ln:n m:NTAPE:mode :}
   c 0 W-B @ 0 W-TP @
      st ln SPAN  c st ln SYM  m NTAPE:NAME-TOKEN
   NTAPE:PUSH-INTO drop ;

: INT, ( IR-CTX:ctx n n NTAPE:mode n -- )
   {: c:IR-CTX:ctx st:n ln:n m:NTAPE:mode val:n :}
   c 0 W-B @ 0 W-TP @
      st ln SPAN  c st ln SYM  m val NTAPE:INT-TOKEN
   NTAPE:PUSH-INTO drop ;

: STR, ( IR-CTX:ctx n n NTAPE:mode -- )
   {: c:IR-CTX:ctx st:n ln:n m:NTAPE:mode :}
   c 0 W-B @ 0 W-TP @
      st ln SPAN  c st ln SYM  m NTAPE:STRING-TOKEN
   NTAPE:PUSH-INTO drop ;

\ A character literal, a kind the straight-line subset has no model for: a
\ fixture that needs an unmodelled kind has one unmodelled for a reason.
: CHAR, ( IR-CTX:ctx n n NTAPE:mode n -- )
   {: c:IR-CTX:ctx st:n ln:n m:NTAPE:mode v:n :}
   c 0 W-B @ 0 W-TP @
      st ln SPAN  c st ln SYM  m v NTAPE:CHAR-TOKEN
   NTAPE:PUSH-INTO drop ;

private

\ ---- the lexer ---------------------------------------------------------------
: DIGIT? ( n -- bool )
   {: ch:n :}
   ch ZERO-C >= ch NINE-C <= and ;

: DIGITS? ( n n -- bool )
   {: st:n ln:n :}
   ln 0= if false exit then
   true
   ln 0 ?do
      st i + TXT + c@ DIGIT? 0= if drop false leave then
   loop ;

: VALUE-OF ( n n -- n )
   {: st:n ln:n :}
   0
   ln 0 ?do
      10 *  st i + TXT + c@ ZERO-C -  +
   loop ;

\ A minus sign in front of a digit run makes the token an integer literal and not
\ a name, which is what the engine's own reader makes of it. The sign alone is
\ the word `-`, so a run of one byte is still a name.
: SIGNED? ( n n -- bool )
   {: st:n ln:n :}
   ln 2 < if false exit then
   st TXT + c@ MINUS-C <> if false exit then
   st 1+ ln 1- DIGITS? ;

\ The text is a definition without its frame, as a produced tape holds it: the
\ engine hands the checker's reader the reconstructed name, signature and body,
\ `:` and `;` consumed, so a caller states `NAME body…`. `:` reads the name
\ before switching the parser to compiling, so the first token is marked
\ interpreting and the rest compiling: src/compiler/native/elaborate.f reads the
\ frame from that alone.
: TOKEN, ( IR-CTX:ctx n n n -- )
   {: c:IR-CTX:ctx ix:n st:n ln:n :}
   ix 0= if c st ln NTAPE-MODE:INTERPRETING NAME, exit then
   st ln DIGITS? if
      c st ln NTAPE-MODE:COMPILING st ln VALUE-OF INT, exit
   then
   st ln SIGNED? if
      c st ln NTAPE-MODE:COMPILING  0 st 1+ ln 1- VALUE-OF -  INT, exit
   then
   c st ln NTAPE-MODE:COMPILING NAME, ;

: AT-SP? ( -- bool )
   LX-P @ TXT-U @ >= if false exit then
   LX-P @ TXT + c@ SP-C = ;

: SKIP-SP ( -- )
   begin AT-SP? while
      LX-P @ 1+ LX-P !
   repeat ;

: END? ( n -- bool )
   {: k:n :}
   k TXT-U @ >= if true exit then
   k TXT + c@ SP-C = ;

: RUN-END ( -- n )
   LX-P @
   begin dup END? 0= while
      1+
   repeat ;

public

: LEX ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   0 LX-I !
   0 LX-P !
   begin
      SKIP-SP
      LX-P @ TXT-U @ <
   while
      c LX-I @ LX-P @  RUN-END LX-P @ -  TOKEN,
      RUN-END LX-P !
      LX-I @ 1+ LX-I !
   repeat ;

\ ---- the module a fixture compiles into --------------------------------------
: HIR-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   b ;

\ The dialect's source-word model: which Habu word means which operation, and
\ which words are renames that stage nothing.
: MODEL ( IR-CTX:ctx IR-BUILD:builder -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY HIR-WORD:WORDS HIR-WORD:PICK-CELLS HIR-WORD:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   p r ;

\ The same model with room for `extra` rows: src/compiler/native/elaborate.f
\ RESOLVE-SCAN adds a callable row for every name the engine and the checker
\ both answer, so a body that calls needs room, sized by the caller.
: MODEL-ROOM ( IR-CTX:ctx IR-BUILD:builder n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder extra:n :}
   c b IR-BUILD:MODULE-KEY  HIR-WORD:WORDS extra +  HIR-WORD:PICK-CELLS
   HIR-WORD:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   p r ;

\ The same model with one `create`d data word declared by its spelling beside
\ the subset's vocabulary: the table is committed to one row more than
\ REGISTER-WORDS writes. The engine answers the word's address, so the caller
\ must have created it.
: MODEL-DATA ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n :}
   c b IR-BUILD:MODULE-KEY HIR-WORD:WORDS 1+ HIR-WORD:PICK-CELLS HIR-WORD:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   c b r  c b a u IR-BUILD:INTERN-SYMBOL  HIR-WORD:DECLARE-FIXED
   p r ;

\ A tape of this module, bound to the text now in TXT and registered as the
\ module's source, ready for LEX to write into.
: TAPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   b tp  c b TXT TXT-U @ IR-BUILD:ADD-SOURCE  WRITER!
   tp ;

;package
