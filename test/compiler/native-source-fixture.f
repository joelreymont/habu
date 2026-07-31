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
\ text. This file splits it on spaces and appends one tape token per word: the
\ first two tokens are read while interpreting, because `:` runs from the outer
\ interpreter and parses the defined name before it switches the parser to
\ compiling, and every later token is read while compiling. A run of digits is an
\ integer literal carrying its value; everything else is a name. Every token's
\ byte span is its real range in the text, so spans and spellings cannot disagree
\ with the source, and the module registers those same bytes as its source - which
\ is what lets the selector check later that the text presented to it is the text
\ the module was compiled from.
\
\ WHAT STAYS WITH THE CALLER. The immediate-word contract table is a parameter,
\ not a fixed part of the rig: the elaboration suite needs tables that are wrong
\ on purpose - a `:` declared an unmodeled boundary, a `;` never declared, a `;`
\ declared compile-time rather than intrinsic - and those belong to the suite that
\ refuses them. ORDINARY-IMM below builds the one honest table, for callers that
\ want a definition to compile rather than to be refused.
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

\ The mode is the token's place in the definition: `:` reads the defined name
\ before it switches the parser to compiling, so the first two tokens were read
\ while interpreting and everything after them while compiling.
: TOKEN, ( IR-CTX:ctx n n n -- )
   {: c:IR-CTX:ctx ix:n st:n ln:n :}
   ix 2 < if c st ln NTAPE-MODE:INTERPRETING NAME, exit then
   st ln DIGITS? if
      c st ln NTAPE-MODE:COMPILING st ln VALUE-OF INT, exit
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

\ The honest immediate-word contract table: both frame words declared the
\ front-end intrinsics they are.
: ORDINARY-IMM ( IR-CTX:ctx IR-BUILD:builder -- IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY 4 NIMM:NEW {: im:IR-ARENA:arena :}
   c b im  c b s" :" IR-BUILD:INTERN-SYMBOL  NIMM-CLASS:INTRINSIC NIMM:DECLARE-INTO
   c b im  c b s" ;" IR-BUILD:INTERN-SYMBOL  NIMM-CLASS:INTRINSIC NIMM:DECLARE-INTO
   im ;

\ A tape of this module, bound to the text now in TXT and registered as the
\ module's source, ready for LEX to write into.
: TAPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY TAPE-CAP NTAPE:NEW {: tp:IR-ARENA:arena :}
   b tp  c b TXT TXT-U @ IR-BUILD:ADD-SOURCE  WRITER!
   tp ;

;package
