\ native-source-fixture.f - the front half of the native chain, for everything
\ that needs to run it. One concern: turning a line of straight-line Habu source
\ into a sealed tape and the module the elaborator writes into.
\
\ WHY THIS IS ITS OWN FILE. Two callers turn source text into HIR the same way -
\ test/compiler/native-elaborate.f, which proves the operations a definition
\ becomes, and tools/codegen-compare-new.f, which measures the machine code a
\ real corpus word compiles to. A second copy of the lexer would be a second
\ opinion about what a token is, and the two harnesses would then disagree for a
\ reason that is about them rather than about the compiler.
\
\ WHAT A FIXTURE STATES AND WHAT THIS FILE DERIVES. The caller states the source
\ text. This file splits it on spaces and appends one tape token per word: a run
\ of digits is an integer literal carrying its value, everything else is a name,
\ and every token's byte span is its real range in the text - so spans and
\ spellings cannot disagree with the source, and the module registers those same
\ bytes as its source, which is what lets the selector check later that the text
\ presented to it is the text the module was compiled from.
\
\ THE TEXT IS A DEFINITION WITHOUT ITS FRAME, BECAUSE THAT IS WHAT A REAL TAPE
\ HOLDS. The tape a real compilation produces is filled by the checker's own
\ reader, and the engine hands that reader the definition it RECONSTRUCTED: the
\ name, the declared signature and the body, with the opening `:` and the closing
\ `;` already consumed. There is no frame token on a produced tape and there
\ never will be, so a caller here states `NAME body…` rather than
\ `: NAME body… ;`. What draws the name/body boundary is the recorded parser
\ mode, exactly as it does on a produced tape: `:` parses the defined name from
\ the outer interpreter before it switches the parser to compiling, so the FIRST
\ token is marked interpreting and every later one compiling. That is the one
\ fact src/compiler/native/elaborate.f reads the frame from, so a fixture that
\ marked two tokens interpreting would be describing a tape no producer can make.
\ test/compiler/native-feed.f measures the same grid on a definition the engine
\ really compiled.
\
\ NOTHING IN THIS FILE ASSERTS. It defines no case and prints nothing: it is a
\ fixture, not a test, so it never names the harness verdict word and no gate
\ schedules it on its own.

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
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
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

\ The mode is the token's place in the definition: `:` reads the defined name
\ before it switches the parser to compiling, so the first token - the name - was
\ read while interpreting and everything after it while compiling.
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

\ Append one tape token per word of the text now in TXT.
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

\ The same model with one `create`d data word declared beside the subset's own
\ vocabulary: the word's spelling and the address it pushes. A definition that
\ mentions a data word cannot be compiled without this, because the dialect's
\ vocabulary is the dialect's and which data words a program names is the
\ program's; the table is therefore committed to one row more than
\ REGISTER-WORDS writes. The address is stated rather than looked up, which is
\ dot habu-resolve-a-data-a1c8067f.
: MODEL-DATA ( IR-CTX:ctx IR-BUILD:builder ptr u8 n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n v:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   c b IR-BUILD:MODULE-KEY HIR-WORD:WORDS 1+ HIR-WORD:PICK-CELLS HIR-WORD:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   c b r  c b a u IR-BUILD:INTERN-SYMBOL  v HIR-WORD:DECLARE-FIXED
   p r ;

\ A tape of this module, bound to the text now in TXT and registered as the
\ module's source, ready for LEX to write into.
: TAPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   b tp  c b TXT TXT-U @ IR-BUILD:ADD-SOURCE  WRITER!
   tp ;

;package
