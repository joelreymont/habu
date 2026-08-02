\ release-inventory.f — structural inventory of the sealed production release
\ definitions of the generated-declaration transaction.
\
\ Why this exists. src/core/declaration-transaction.f runs its RELEASE phase
\ after every reversible commit has already published, so a release callback has
\ no error channel: it is executed directly, never caught, never diagnosed, and
\ can never poison the coordinator. That contract is a property of the SOURCE,
\ not of any run. No declaration a test can write reaches a release callback in a
\ state where it would want to reject, so swapping a total release word for a
\ validating one survives every behavioural suite in the repository. Nine
\ E-PF-TX-class and E-DEV-TX-class throw sites were reachable from the callbacks
\ this phase used to run; this tool makes reintroducing any of them fail closed.
\
\ What it proves, reading source only:
\   1. Exactly the expected number of participant registrations exist, each
\      passing exactly five `[: WORD ;]` callback quotations. The word in the
\      fifth (release) slot is a root of the inventory, so reordering the
\      quotations inventories whatever actually landed in the release slot.
\   2. From each root, every word the definition reaches is closed over: it must
\      be another definition in the scanned sources, or a member of the
\      total-word allowlist below. Roots enter that closure exactly the way an
\      interior reference does, so a root that is itself a deferred word is
\      followed rather than waved through. A deferred word is followed through
\      every `[: WORD ;] is VECTOR` and `['] WORD is VECTOR` binding, and a
\      deferred word reached with no binding at all, or with one whose target this
\      scan cannot name, is a finding rather than silence. A name that resolves to
\      two definitions in one package is ambiguous and is a finding on its own.
\   3. Nothing in that closure signals errors (throw, catch, die, abort),
\      allocates, looks a name up, publishes, or reports. Those words are simply
\      absent from the allowlist; the ones whose reappearance is the actual
\      regression are additionally named, so the report says which class came
\      back instead of only "unrecognised word".
\
\ Known limit, stated so nobody mistakes it for coverage: an execution token that
\ reaches a release callback without passing through any `is` at all - stored in a
\ cell and run by `execute`, say - is outside what a source scan can follow. What
\ the scan does guarantee is that no deferred word in the closure is bound by a
\ route it ignored: every `is` is recorded, and an unnameable target is reported.
\
\ Comments and string bodies can neither satisfy nor break any rule: the scan
\ runs through tools/lint/source-lex.f, which drops `\` comments, exposes
\ `( ... )` comments as their own token kind, consumes string bodies as opaque
\ spans, and folds a whole `PRIM:`/`PPRIM:` axiom row into one token.
\
\ Load: bin/hb --load tools/lint/text.f tools/lint/token.f tools/lint/lib.f \
\              tools/lint/source-lex.f tools/release-inventory.f
\ The tool defines no main; test/declaration-release-inventory.f drives it
\ against the repository sources and against hostile fixtures.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

package RELEASE-INV

public

\ Capacity and lexer failures are named, never a bare status.
88 constant E-INV-CAP
89 constant E-INV-LEX

private

s" E-INV-CAP" E-INV-CAP LINT-CODE-NAME+
s" E-INV-LEX" E-INV-LEX LINT-CODE-NAME+

$C0000 constant SRC-CAP
create SRC-BUF SRC-CAP allot
variable SRC-END

$20000 constant NAME-CAP
create NAMES NAME-CAP allot
variable NAME-END

16 constant FILE-MAX
create F-LOFF FILE-MAX cells allot
create F-LLEN FILE-MAX cells allot
create F-TOFF FILE-MAX cells allot
create F-TLEN FILE-MAX cells allot
variable FILE-N

8192 constant DEF-MAX
0 constant KIND-COLON            \ has a body the closure walks
1 constant KIND-DATA             \ variable/constant/create/value: no body
2 constant KIND-DEFER            \ followed through its recorded bindings
create D-PKG   DEF-MAX cells allot
create D-PKGU  DEF-MAX cells allot
create D-NAME  DEF-MAX cells allot
create D-NAMEU DEF-MAX cells allot
create D-FILE  DEF-MAX cells allot
create D-KIND  DEF-MAX cells allot
create D-TS    DEF-MAX cells allot
create D-TE    DEF-MAX cells allot
create D-REACH DEF-MAX cells allot
create D-DONE  DEF-MAX cells allot
variable DEF-N

32 constant ROOT-MAX
create R-PKG   ROOT-MAX cells allot
create R-PKGU  ROOT-MAX cells allot
create R-NAME  ROOT-MAX cells allot
create R-NAMEU ROOT-MAX cells allot
variable ROOT-N

128 constant BIND-MAX
0 constant BIND-NAMED            \ `[: W ;] is V` or `['] W is V`: W is nameable
1 constant BIND-OPAQUE           \ any other shape: the bound token has no name here
create B-PKG   BIND-MAX cells allot
create B-PKGU  BIND-MAX cells allot
create B-VEC   BIND-MAX cells allot
create B-VECU  BIND-MAX cells allot
create B-WORD  BIND-MAX cells allot
create B-WORDU BIND-MAX cells allot
create B-FORM  BIND-MAX cells allot
variable BIND-N
variable FB-I
variable FB-HITS

64 constant LOCAL-MAX
create L-OFF LOCAL-MAX cells allot
create L-LEN LOCAL-MAX cells allot
variable LOCAL-N

8 constant PKG-DEPTH-MAX
create P-OFF PKG-DEPTH-MAX cells allot
create P-LEN PKG-DEPTH-MAX cells allot
variable PKG-DEPTH

variable FINDINGS
variable REG-SITES
variable REG-QUOTED
variable ROOT-EXPECT
variable CUR-FILE
variable SWEPT
variable ROOT-A
variable ROOT-U
variable IDX-I
variable SITE-I
variable WB-I
variable WB-END
variable WB-DEF
variable SEED-I
variable FILE-I
variable DEF-I
create PATH-BUF 1024 allot
variable PATH-U

\ ---- byte arenas ----------------------------------------------------------

: NAMES+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   NAME-END @ u + NAME-CAP > IF
      s" release-inventory: name arena full" E-INV-CAP die
   THEN
   a NAMES NAME-END @ + u LINT-BMOVE
   NAME-END @
   NAME-END @ u + NAME-END ! ;

: NAME$ ( n n -- ptr u8 n ) {: off:n u:n :}
   NAMES off + u ;

: EMPTY$ ( -- ptr u8 n )
   NAMES 0 ;

: SRC$ ( n n -- ptr u8 n ) {: off:n u:n :}
   SRC-BUF off + u ;

\ ---- scanned source set ---------------------------------------------------

: FILE-LABEL$ ( n -- ptr u8 n ) {: f:n :}
   f cells F-LOFF + @ f cells F-LLEN + @ NAME$ ;

: FILE-TEXT$ ( n -- ptr u8 n ) {: f:n :}
   f cells F-TOFF + @ f cells F-TLEN + @ SRC$ ;

: SRC-ROOM ( n -- ) {: u:n :}
   SRC-END @ u + SRC-CAP > IF
      s" release-inventory: source arena full" E-INV-CAP die
   THEN ;

: FILE-ROOM ( -- )
   FILE-N @ FILE-MAX >= IF
      s" release-inventory: source table full" E-INV-CAP die
   THEN ;

: FILE-RECORD ( ptr u8 n n -- ) {: la:ptr lu:n tu:n :}
   la lu NAMES+ FILE-N @ cells F-LOFF + !
   lu FILE-N @ cells F-LLEN + !
   SRC-END @ FILE-N @ cells F-TOFF + !
   tu FILE-N @ cells F-TLEN + !
   SRC-END @ tu + SRC-END !
   FILE-N @ 1 + FILE-N ! ;

public

\ Add one source under a label. The text is copied, because the closure walk
\ re-reads every scanned source several times.
: SOURCE+ ( ptr u8 n ptr u8 n -- ) {: la:ptr lu:n ta:ptr tu:n :}
   FILE-ROOM
   tu SRC-ROOM
   ta SRC-BUF SRC-END @ + tu LINT-BMOVE
   la lu tu FILE-RECORD ;

: RESET ( -- )
   0 SRC-END !   0 NAME-END !   0 FILE-N !
   0 DEF-N !     0 ROOT-N !     0 BIND-N !
   0 FINDINGS !  0 REG-SITES !  0 REG-QUOTED !
   0 ROOT-A !    0 ROOT-U !
   4 ROOT-EXPECT ! ;

: ROOTS-EXPECT! ( n -- )
   ROOT-EXPECT ! ;

private

\ ---- path building --------------------------------------------------------

: PATH-BYTE+ ( n -- ) {: c:n :}
   PATH-U @ 1023 >= IF
      s" release-inventory: path too long" E-INV-CAP die
   THEN
   c PATH-BUF PATH-U @ + c!
   PATH-U @ 1 + PATH-U ! ;

: PATH+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0
   BEGIN dup u < WHILE
      dup a + c@ PATH-BYTE+
      1 +
   REPEAT
   drop ;

: PATH$ ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: PATH-BUILD ( ptr u8 n -- ) {: a:ptr u:n :}
   0 PATH-U !
   ROOT-U @ 0 > IF
      ROOT-A @ ROOT-U @ NAME$ PATH+
      47 PATH-BYTE+
   THEN
   a u PATH+ ;

public

\ Source root prefix for READ+, so the same inventory can run against a checkout
\ other than the working tree. Empty means paths are used as given.
: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NAMES+ ROOT-A !
   u ROOT-U ! ;

: READ+ ( ptr u8 n -- ) {: a:ptr u:n :}
   FILE-ROOM
   a u PATH-BUILD
   SRC-BUF SRC-END @ + {: dst:ptr :}
   PATH$ dst SRC-CAP SRC-END @ - READ-FILE {: got:ptr gotu:n :}
   got dst = 0= IF
      s" release-inventory: reader did not fill the source arena" E-INV-CAP die
   THEN
   a u gotu FILE-RECORD ;

private

\ ---- token predicates -----------------------------------------------------

: WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: TOK$ ( n -- ptr u8 n )
   LINT-LEX:TOKEN ;

: TOK= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k WORD? 0= IF LINT-FALSE EXIT THEN
   k TOK$ a u LINT-STR=CI ;

: DIGIT? ( n -- bool ) {: c:n :}
   c 48 >= c 57 <= and ;

: HEX-DIGIT? ( n -- bool ) {: c:n :}
   c DIGIT? IF LINT-TRUE EXIT THEN
   c LINT-FOLD {: f:n :}
   f 97 >= f 102 <= and ;

: DIGITS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= IF LINT-FALSE EXIT THEN
   0
   BEGIN dup u < WHILE
      dup a + c@ DIGIT? 0= IF drop LINT-FALSE EXIT THEN
      1 +
   REPEAT
   drop LINT-TRUE ;

: HEX-DIGITS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= IF LINT-FALSE EXIT THEN
   0
   BEGIN dup u < WHILE
      dup a + c@ HEX-DIGIT? 0= IF drop LINT-FALSE EXIT THEN
      1 +
   REPEAT
   drop LINT-TRUE ;

\ A numeric literal names nothing and executes nothing.
: LITERAL$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= IF LINT-FALSE EXIT THEN
   a c@ 45 = IF a 1 + u 1 - DIGITS? EXIT THEN
   a c@ 36 = IF a 1 + u 1 - HEX-DIGITS? EXIT THEN
   a u DIGITS? ;

\ ---- the total-word allowlist ---------------------------------------------
\ Membership here is the whole admission rule for a word with no definition in
\ the scanned sources: stack shuffles, integer arithmetic, comparisons, plain
\ cell reads and writes, structured control flow. Nothing that can reject,
\ allocate, look a name up, or emit is listed, so `throw`, `catch`, `die`,
\ `allot`, `search-wl`, `type` and every other such word fails the closure by
\ being absent rather than by being named.

: SHUFFLE$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" dup" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" drop" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" swap" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" over" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" nip" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" tuck" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" rot" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" -rot" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" 2drop" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" 2dup" LINT-STR=CI ;

: ARITH$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" +" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" -" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" *" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" 1+" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" 1-" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" cells" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" cell+" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" max" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" min" LINT-STR=CI ;

: COMPARE$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" =" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" <>" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" <" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" >" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" <=" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" >=" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" 0=" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" 0<>" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" and" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" or" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" invert" LINT-STR=CI ;

: CELL-ACCESS$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" @" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" !" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" ptr-field" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" execute" LINT-STR=CI ;

: CONTROL$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" IF" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" ELSE" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" THEN" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" EXIT" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" BEGIN" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" WHILE" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" REPEAT" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" AGAIN" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" UNTIL" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" DO" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" ?DO" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" LOOP" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" I" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" J" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" UNLOOP" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" LEAVE" LINT-STR=CI ;

: TOTAL-WORD$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u SHUFFLE$? IF LINT-TRUE EXIT THEN
   a u ARITH$? IF LINT-TRUE EXIT THEN
   a u COMPARE$? IF LINT-TRUE EXIT THEN
   a u CELL-ACCESS$? IF LINT-TRUE EXIT THEN
   a u CONTROL$? ;

\ ---- named regression classes --------------------------------------------
\ Each of these already fails the closure by absence. Naming them turns the
\ report from "unrecognised word" into the contract class that came back.

: SIGNAL$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" throw" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" catch" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" die" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" abort" LINT-STR=CI ;

: ALLOCATE$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" allot" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" allocate" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" ARENA-BYTES-GROW" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" REG-GROW1" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" DEV-REG-GROW1" LINT-STR=CI ;

: LOOKUP$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" find" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" search-wl" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" XREF-FIND" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" SYM-INTERN" LINT-STR=CI ;

: PUBLISH$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" prot-wid-add" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" ndict!" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" cp!" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" reveal" LINT-STR=CI ;

: REPORT$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" type" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" cr" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" emit" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" ." LINT-STR= ;

\ The coordinator's own failure machinery: no error path may be rebuilt inside
\ release out of words that are individually free of `throw`.
: MACHINERY$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" POISON" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" REMEMBER-CLEANUP" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" CALL-PARTICIPANT" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" REPORT-FAILURE" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" CATCH-DIAGNOSTIC" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" ENTER-PHASE" LINT-STR=CI IF LINT-TRUE EXIT THEN
   a u s" FAIL" LINT-STR=CI ;

: CLASS$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u SIGNAL$? IF s" error signalling" EXIT THEN
   a u ALLOCATE$? IF s" allocation" EXIT THEN
   a u LOOKUP$? IF s" name lookup" EXIT THEN
   a u PUBLISH$? IF s" publication" EXIT THEN
   a u REPORT$? IF s" diagnostic reporting" EXIT THEN
   a u MACHINERY$? IF s" coordinator failure machinery" EXIT THEN
   s" unrecognised word" ;

\ ---- definition table -----------------------------------------------------

: DEF-NAME$ ( n -- ptr u8 n ) {: d:n :}
   d cells D-NAME + @ d cells D-NAMEU + @ NAME$ ;

: DEF-PKG$ ( n -- ptr u8 n ) {: d:n :}
   d cells D-PKG + @ d cells D-PKGU + @ NAME$ ;

: FINDING-HEAD ( n -- ) {: d:n :}
   s" release-inventory: " type
   d cells D-FILE + @ FILE-LABEL$ type
   s"  " type
   d DEF-PKG$ dup 0 > IF type s" :" type ELSE 2drop THEN
   d DEF-NAME$ type ;

: FINDING ( n ptr u8 n -- ) {: d:n a:ptr u:n :}
   d FINDING-HEAD
   s"  reaches `" type a u type s" ` (" type a u CLASS$ type s" )" type cr
   FINDINGS @ 1 + FINDINGS ! ;

: SITE-FINDING ( n ptr u8 n -- ) {: f:n a:ptr u:n :}
   s" release-inventory: " type f FILE-LABEL$ type s"  " type a u type cr
   FINDINGS @ 1 + FINDINGS ! ;

\ ---- package scope while scanning one file --------------------------------

: PKG-RESET ( -- )
   0 PKG-DEPTH ! ;

: PKG$ ( -- n n )
   PKG-DEPTH @ 0= IF 0 0 EXIT THEN
   PKG-DEPTH @ 1 - cells P-OFF + @
   PKG-DEPTH @ 1 - cells P-LEN + @ ;

: PKG-PUSH ( ptr u8 n -- ) {: a:ptr u:n :}
   PKG-DEPTH @ PKG-DEPTH-MAX >= IF
      s" release-inventory: package nesting too deep" E-INV-CAP die
   THEN
   a u NAMES+ PKG-DEPTH @ cells P-OFF + !
   u PKG-DEPTH @ cells P-LEN + !
   PKG-DEPTH @ 1 + PKG-DEPTH ! ;

: PKG-POP ( -- )
   PKG-DEPTH @ 0 > IF PKG-DEPTH @ 1 - PKG-DEPTH ! THEN ;

: DEF-ROOM ( -- )
   DEF-N @ DEF-MAX >= IF
      s" release-inventory: definition table full" E-INV-CAP die
   THEN ;

: DEF+ ( ptr u8 n n n n -- ) {: a:ptr u:n kind:n ts:n te:n :}
   DEF-ROOM
   PKG$ {: poff:n plen:n :}
   poff DEF-N @ cells D-PKG + !
   plen DEF-N @ cells D-PKGU + !
   a u NAMES+ DEF-N @ cells D-NAME + !
   u DEF-N @ cells D-NAMEU + !
   CUR-FILE @ DEF-N @ cells D-FILE + !
   kind DEF-N @ cells D-KIND + !
   ts DEF-N @ cells D-TS + !
   te DEF-N @ cells D-TE + !
   0 DEF-N @ cells D-REACH + !
   0 DEF-N @ cells D-DONE + !
   DEF-N @ 1 + DEF-N ! ;

: DEF-MATCH? ( n ptr u8 n ptr u8 n -- bool ) {: d:n pa:ptr pu:n na:ptr nu:n :}
   d DEF-PKG$ pa pu LINT-STR=CI 0= IF LINT-FALSE EXIT THEN
   d DEF-NAME$ na nu LINT-STR=CI ;

\ A second definition of the same package-qualified name makes every reference
\ to it ambiguous, so the count matters as much as the index.
: DEF-COUNT ( ptr u8 n ptr u8 n -- n ) {: pa:ptr pu:n na:ptr nu:n :}
   0 0
   BEGIN dup DEF-N @ < WHILE
      dup pa pu na nu DEF-MATCH? IF swap 1 + swap THEN
      1 +
   REPEAT
   drop ;

: DEF-FIND ( ptr u8 n ptr u8 n -- n ) {: pa:ptr pu:n na:ptr nu:n :}
   0
   BEGIN dup DEF-N @ < WHILE
      dup pa pu na nu DEF-MATCH? IF EXIT THEN
      1 +
   REPEAT
   drop -1 ;

\ ---- definer recognition --------------------------------------------------

: COLON-DEFINER? ( n -- bool ) {: k:n :}
   k s" :" TOK= IF LINT-TRUE EXIT THEN
   k s" TRUSTED:" TOK= IF LINT-TRUE EXIT THEN
   k s" CHECKED:" TOK= ;

: DATA-DEFINER? ( n -- bool ) {: k:n :}
   k s" variable" TOK= IF LINT-TRUE EXIT THEN
   k s" 2variable" TOK= IF LINT-TRUE EXIT THEN
   k s" fvariable" TOK= IF LINT-TRUE EXIT THEN
   k s" constant" TOK= IF LINT-TRUE EXIT THEN
   k s" 2constant" TOK= IF LINT-TRUE EXIT THEN
   k s" fconstant" TOK= IF LINT-TRUE EXIT THEN
   k s" create" TOK= IF LINT-TRUE EXIT THEN
   k s" value" TOK= IF LINT-TRUE EXIT THEN
   k s" PTR-VARIABLE" TOK= IF LINT-TRUE EXIT THEN
   k s" TYPED-VARIABLE" TOK= ;

\ A word after one of these is parsed as a name and never executed, so it must
\ not be read as a definition, a callback, or a call.
: NAME-CONSUMER? ( n -- bool ) {: k:n :}
   k s" undefine" TOK= IF LINT-TRUE EXIT THEN
   k s" postpone" TOK= IF LINT-TRUE EXIT THEN
   k s" '" TOK= IF LINT-TRUE EXIT THEN
   k s" [']" TOK= IF LINT-TRUE EXIT THEN
   k s" is" TOK= IF LINT-TRUE EXIT THEN
   k s" defer" TOK= ;

: BODY-END ( n -- n ) {: start:n :}
   start
   BEGIN dup LINT-LEX:COUNT < WHILE
      dup s" ;" TOK= IF EXIT THEN
      1 +
   REPEAT
   drop LINT-LEX:COUNT ;

\ ---- registration sites ---------------------------------------------------

: REGISTER-CALL? ( n -- bool ) {: k:n :}
   k s" DECLARATION-TRANSACTION:REGISTER" TOK= IF LINT-TRUE EXIT THEN
   k s" GENERATED-DECL-OWNER:REGISTER" TOK= IF LINT-TRUE EXIT THEN
   k s" GENERATED-DECL-OWNER:REGISTER-LAST" TOK= ;

\ One `[: WORD ;]` group ending at k: opener, exactly one word, closer.
: QUOTE-GROUP? ( n -- bool ) {: k:n :}
   k 2 < IF LINT-FALSE EXIT THEN
   k s" ;]" TOK= 0= IF LINT-FALSE EXIT THEN
   k 1 - WORD? 0= IF LINT-FALSE EXIT THEN
   k 2 - s" [:" TOK= ;

: QUOTE-WORD$ ( n -- ptr u8 n ) {: k:n :}
   k 1 - TOK$ ;

: GROUP-BACK? ( n n -- bool ) {: k:n q:n :}
   k 1 - q 3 * - QUOTE-GROUP? ;

: FIVE-QUOTES? ( n -- bool ) {: k:n :}
   0
   BEGIN dup 5 < WHILE
      dup k swap GROUP-BACK? 0= IF drop LINT-FALSE EXIT THEN
      1 +
   REPEAT
   drop
   k 16 - 0 < IF LINT-TRUE EXIT THEN
   k 16 - QUOTE-GROUP? LINT-NOT ;   \ a sixth group means the arity moved

: ROOT-ROOM ( -- )
   ROOT-N @ ROOT-MAX >= IF
      s" release-inventory: root table full" E-INV-CAP die
   THEN ;

: ROOT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   ROOT-ROOM
   PKG$ {: poff:n plen:n :}
   poff ROOT-N @ cells R-PKG + !
   plen ROOT-N @ cells R-PKGU + !
   a u NAMES+ ROOT-N @ cells R-NAME + !
   u ROOT-N @ cells R-NAMEU + !
   ROOT-N @ 1 + ROOT-N ! ;

: NOTE-REGISTRATION ( n -- ) {: k:n :}
   REG-SITES @ 1 + REG-SITES !
   k 1 - 0 < IF EXIT THEN
   k 1 - s" ;]" TOK= 0= IF EXIT THEN     \ a forwarder passing locals, not a site
   REG-QUOTED @ 1 + REG-QUOTED !
   k FIVE-QUOTES? 0= IF
      CUR-FILE @
      s" registration does not pass exactly five [: WORD ;] callbacks"
      SITE-FINDING
      EXIT
   THEN
   k 1 - QUOTE-WORD$ ROOT+ ;

\ ---- deferred-word bindings ----------------------------------------------

: BIND-ROOM ( -- )
   BIND-N @ BIND-MAX >= IF
      s" release-inventory: binding table full" E-INV-CAP die
   THEN ;

: BIND+ ( ptr u8 n ptr u8 n n -- ) {: va:ptr vu:n wa:ptr wu:n form:n :}
   BIND-ROOM
   PKG$ {: poff:n plen:n :}
   poff BIND-N @ cells B-PKG + !
   plen BIND-N @ cells B-PKGU + !
   va vu NAMES+ BIND-N @ cells B-VEC + !
   vu BIND-N @ cells B-VECU + !
   wa wu NAMES+ BIND-N @ cells B-WORD + !
   wu BIND-N @ cells B-WORDU + !
   form BIND-N @ cells B-FORM + !
   BIND-N @ 1 + BIND-N ! ;

\ `['] WORD` and `' WORD` push the same execution token a `[: WORD ;]` quotation
\ does, and both are ordinary checked ways to feed `is`. k names the WORD.
: TICK-TARGET? ( n -- bool ) {: k:n :}
   k 1 < IF LINT-FALSE EXIT THEN
   k WORD? 0= IF LINT-FALSE EXIT THEN
   k 1 - s" [']" TOK= IF LINT-TRUE EXIT THEN
   k 1 - s" '" TOK= ;

\ Every `is` is recorded. A shape whose target this scan cannot name - an xt read
\ out of a cell, say - is recorded as opaque rather than dropped, so a deferred
\ word in the release closure can never be bound by a route the scan ignored.
: NOTE-BINDING ( n -- ) {: k:n :}
   k 1 + LINT-LEX:COUNT >= IF EXIT THEN
   k 1 + WORD? 0= IF EXIT THEN
   k 1 < IF EXIT THEN
   k 1 - QUOTE-GROUP? IF
      k 1 + TOK$ k 1 - QUOTE-WORD$ BIND-NAMED BIND+
      EXIT
   THEN
   k 1 - TICK-TARGET? IF
      k 1 + TOK$ k 1 - TOK$ BIND-NAMED BIND+
      EXIT
   THEN
   k 1 + TOK$ EMPTY$ BIND-OPAQUE BIND+ ;

\ ---- pass one: index every scanned source ---------------------------------

: LEX-FILE ( n -- ) {: f:n :}
   f FILE-TEXT$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? IF
      s" release-inventory: " type f FILE-LABEL$ type
      s"  lexer rejected the source at line " type LINT-LEX:ERROR-LINE@ . cr
      s" release-inventory: unscannable source" E-INV-LEX die
   THEN ;

: INDEX-COLON ( -- )
   IDX-I @ 1 + LINT-LEX:COUNT >= IF IDX-I @ 1 + IDX-I ! EXIT THEN
   IDX-I @ 1 + WORD? 0= IF IDX-I @ 1 + IDX-I ! EXIT THEN
   IDX-I @ 1 + TOK$ KIND-COLON
      IDX-I @ 2 + IDX-I @ 2 + BODY-END DEF+
   IDX-I @ 1 + IDX-I ! ;

: INDEX-NAMED ( n -- ) {: kind:n :}
   IDX-I @ 1 + LINT-LEX:COUNT >= IF IDX-I @ 1 + IDX-I ! EXIT THEN
   IDX-I @ 1 + WORD? 0= IF IDX-I @ 1 + IDX-I ! EXIT THEN
   IDX-I @ 1 + TOK$ kind 0 0 DEF+
   IDX-I @ 1 + IDX-I ! ;

: PKG-OPEN ( -- )
   IDX-I @ 1 + LINT-LEX:COUNT < IDX-I @ 1 + WORD? and IF
      IDX-I @ 1 + TOK$ PKG-PUSH
   THEN
   IDX-I @ 2 + IDX-I ! ;

: INDEX-TOKEN ( -- )
   IDX-I @ WORD? 0= IF IDX-I @ 1 + IDX-I ! EXIT THEN
   IDX-I @ s" package" TOK= IF PKG-OPEN EXIT THEN
   IDX-I @ s" ;package" TOK= IF PKG-POP IDX-I @ 1 + IDX-I ! EXIT THEN
   IDX-I @ s" defer" TOK= IF KIND-DEFER INDEX-NAMED EXIT THEN
   IDX-I @ NAME-CONSUMER? IF IDX-I @ 2 + IDX-I ! EXIT THEN
   IDX-I @ COLON-DEFINER? IF INDEX-COLON EXIT THEN
   IDX-I @ DATA-DEFINER? IF KIND-DATA INDEX-NAMED EXIT THEN
   IDX-I @ 1 + IDX-I ! ;

: INDEX-FILE ( n -- ) {: f:n :}
   f CUR-FILE !
   f LEX-FILE
   PKG-RESET
   0 IDX-I !
   BEGIN IDX-I @ LINT-LEX:COUNT < WHILE
      INDEX-TOKEN
   REPEAT ;

\ Registrations and bindings get their own walk, so the definition index is
\ already complete when a callback name is recorded.
: SITE-PKG-OPEN ( -- )
   SITE-I @ 1 + LINT-LEX:COUNT < SITE-I @ 1 + WORD? and IF
      SITE-I @ 1 + TOK$ PKG-PUSH
   THEN
   SITE-I @ 2 + SITE-I ! ;

: SITE-TOKEN ( -- )
   SITE-I @ WORD? 0= IF SITE-I @ 1 + SITE-I ! EXIT THEN
   SITE-I @ s" package" TOK= IF SITE-PKG-OPEN EXIT THEN
   SITE-I @ s" ;package" TOK= IF PKG-POP SITE-I @ 1 + SITE-I ! EXIT THEN
   SITE-I @ s" is" TOK= IF
      SITE-I @ NOTE-BINDING
      SITE-I @ 2 + SITE-I !
      EXIT
   THEN
   SITE-I @ NAME-CONSUMER? IF SITE-I @ 2 + SITE-I ! EXIT THEN
   SITE-I @ REGISTER-CALL? IF SITE-I @ NOTE-REGISTRATION THEN
   SITE-I @ 1 + SITE-I ! ;

: SITES-FILE ( n -- ) {: f:n :}
   f CUR-FILE !
   f LEX-FILE
   PKG-RESET
   0 SITE-I !
   BEGIN SITE-I @ LINT-LEX:COUNT < WHILE
      SITE-TOKEN
   REPEAT ;

: INDEX-ALL ( -- )
   0 FILE-I !
   BEGIN FILE-I @ FILE-N @ < WHILE
      FILE-I @ INDEX-FILE
      FILE-I @ 1 + FILE-I !
   REPEAT
   0 FILE-I !
   BEGIN FILE-I @ FILE-N @ < WHILE
      FILE-I @ SITES-FILE
      FILE-I @ 1 + FILE-I !
   REPEAT ;

\ ---- pass two: closure over the release definitions -----------------------

: LOCALS-RESET ( -- )
   0 LOCAL-N ! ;

: LOCAL+ ( ptr u8 n -- ) {: a:ptr u:n :}
   LOCAL-N @ LOCAL-MAX >= IF
      s" release-inventory: locals table full" E-INV-CAP die
   THEN
   a u NAMES+ LOCAL-N @ cells L-OFF + !
   u LOCAL-N @ cells L-LEN + !
   LOCAL-N @ 1 + LOCAL-N ! ;

: LOCAL$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0
   BEGIN dup LOCAL-N @ < WHILE
      dup cells L-OFF + @ over cells L-LEN + @ NAME$ a u LINT-STR=CI
         IF drop LINT-TRUE EXIT THEN
      1 +
   REPEAT
   drop LINT-FALSE ;

\ `state:ptr` declares the local `state`; a bare `state` declares itself.
: LOCAL-HEAD$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0
   BEGIN dup u < WHILE
      dup a + c@ 58 = IF a swap EXIT THEN
      1 +
   REPEAT
   drop a u ;

: COLON-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   BEGIN dup u < WHILE
      dup a + c@ 58 = IF EXIT THEN
      1 +
   REPEAT
   drop -1 ;

: MARK-REACH ( n -- ) {: d:n :}
   -1 d cells D-REACH + ! ;

: BIND-MATCH? ( n ptr u8 n -- bool ) {: b:n va:ptr vu:n :}
   b cells B-VEC + @ b cells B-VECU + @ NAME$ va vu LINT-STR=CI ;

: VECTOR-FINDING ( n ptr u8 n ptr u8 n -- ) {: d:n va:ptr vu:n ra:ptr ru:n :}
   d FINDING-HEAD
   s"  reaches deferred `" type va vu type s" ` " type ra ru type cr
   FINDINGS @ 1 + FINDINGS ! ;

: FOLLOW-ONE-BINDING ( n n -- ) {: b:n d:n :}
   b cells B-FORM + @ BIND-OPAQUE = IF
      d b cells B-VEC + @ b cells B-VECU + @ NAME$
      s" bound to an execution token this scan cannot name" VECTOR-FINDING
      EXIT
   THEN
   b cells B-PKG + @ b cells B-PKGU + @ NAME$
   b cells B-WORD + @ b cells B-WORDU + @ NAME$ DEF-FIND {: t:n :}
   t 0 < IF
      d b cells B-WORD + @ b cells B-WORDU + @ NAME$ FINDING
      EXIT
   THEN
   t MARK-REACH ;

\ A deferred word with no binding here is not "nothing to follow": it is a hole
\ the scan cannot see the other side of, so it is a finding in its own right.
: FOLLOW-BINDINGS ( n ptr u8 n -- ) {: d:n va:ptr vu:n :}
   0 FB-HITS !
   0 FB-I !
   BEGIN FB-I @ BIND-N @ < WHILE
      FB-I @ va vu BIND-MATCH? IF
         FB-HITS @ 1 + FB-HITS !
         FB-I @ d FOLLOW-ONE-BINDING
      THEN
      FB-I @ 1 + FB-I !
   REPEAT
   FB-HITS @ 0= IF
      d va vu s" with no binding in the scanned sources" VECTOR-FINDING
   THEN ;

: RESOLVE-DEF ( n n -- ) {: d:n t:n :}
   t MARK-REACH
   t cells D-KIND + @ KIND-DEFER = IF d t DEF-NAME$ FOLLOW-BINDINGS THEN ;

: RESOLVE-QUALIFIED ( n ptr u8 n -- ) {: d:n a:ptr u:n :}
   a u COLON-AT {: c:n :}
   a c a c 1 + + u c 1 + - {: pa:ptr pu:n na:ptr nu:n :}
   pa pu na nu DEF-COUNT 1 <> IF d a u FINDING EXIT THEN
   d pa pu na nu DEF-FIND RESOLVE-DEF ;

: RESOLVE-BARE ( n ptr u8 n -- ) {: d:n a:ptr u:n :}
   d DEF-PKG$ a u DEF-COUNT {: own:n :}
   own 1 > IF d a u FINDING EXIT THEN
   own 1 = IF d d DEF-PKG$ a u DEF-FIND RESOLVE-DEF EXIT THEN
   EMPTY$ a u DEF-COUNT 1 <> IF d a u FINDING EXIT THEN
   d EMPTY$ a u DEF-FIND RESOLVE-DEF ;

: RESOLVE-WORD ( n ptr u8 n -- ) {: d:n a:ptr u:n :}
   a u LITERAL$? IF EXIT THEN
   a u LOCAL$? IF EXIT THEN
   a u TOTAL-WORD$? IF EXIT THEN
   a u COLON-AT 0 >= IF d a u RESOLVE-QUALIFIED EXIT THEN
   d a u RESOLVE-BARE ;

\ Quotation openers and closers are structure, not calls; the word between them
\ is resolved like any other reference.
: STRUCTURE$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" [:" LINT-STR= IF LINT-TRUE EXIT THEN
   a u s" ;]" LINT-STR= ;

: WB-LOCALS ( -- )               \ WB-I sits on `{:`; consume through `:}`
   WB-I @ 1 + WB-I !
   BEGIN WB-I @ WB-END @ < WHILE
      WB-I @ WORD? IF
         WB-I @ TOK$ s" :}" LINT-STR= IF EXIT THEN
         WB-I @ TOK$ LOCAL-HEAD$ LOCAL+
      THEN
      WB-I @ 1 + WB-I !
   REPEAT ;

: WB-TOKEN ( -- )
   WB-I @ WORD? 0= IF EXIT THEN
   WB-I @ TOK$ s" {:" LINT-STR= IF WB-LOCALS EXIT THEN
   WB-I @ TOK$ STRUCTURE$? IF EXIT THEN
   WB-DEF @ WB-I @ TOK$ RESOLVE-WORD ;

: WALK-BODY ( n -- ) {: d:n :}
   LOCALS-RESET
   d WB-DEF !
   d cells D-TE + @ WB-END !
   d cells D-TS + @ WB-I !
   BEGIN WB-I @ WB-END @ < WHILE
      WB-TOKEN
      WB-I @ 1 + WB-I !
   REPEAT ;

: PENDING? ( n -- bool ) {: d:n :}
   d cells D-REACH + @ 0 <>
   d cells D-DONE + @ 0= and
   d cells D-KIND + @ KIND-COLON = and ;

: FILE-PENDING? ( n -- bool ) {: f:n :}
   0
   BEGIN dup DEF-N @ < WHILE
      dup PENDING? over cells D-FILE + @ f = and IF drop LINT-TRUE EXIT THEN
      1 +
   REPEAT
   drop LINT-FALSE ;

: WALK-FILE-PENDING ( n -- ) {: f:n :}
   f CUR-FILE !
   f LEX-FILE
   0 DEF-I !
   BEGIN DEF-I @ DEF-N @ < WHILE
      DEF-I @ PENDING? DEF-I @ cells D-FILE + @ f = and IF
         DEF-I @ WALK-BODY
         -1 DEF-I @ cells D-DONE + !
         -1 SWEPT !
      THEN
      DEF-I @ 1 + DEF-I !
   REPEAT ;

: SWEEP ( -- )
   0 SWEPT !
   0 FILE-I !
   BEGIN FILE-I @ FILE-N @ < WHILE
      FILE-I @ FILE-PENDING? IF FILE-I @ WALK-FILE-PENDING THEN
      FILE-I @ 1 + FILE-I !
   REPEAT ;

: CLOSURE ( -- )
   BEGIN SWEEP SWEPT @ 0= UNTIL ;

\ ---- roots ----------------------------------------------------------------

\ A root enters the closure exactly the way an interior reference does. Marking
\ it reachable and stopping would admit a root that is itself a deferred word
\ without ever following its bindings, which is the whole hole the closure
\ exists to close.
: ADMIT-ROOT ( n -- ) {: d:n :}
   d cells D-KIND + @ KIND-DATA = IF
      0 s" release callback names data, not a definition this scan can walk"
      SITE-FINDING
      EXIT
   THEN
   d d RESOLVE-DEF ;

: SEED-ROOT ( n -- ) {: r:n :}
   r cells R-PKG + @ r cells R-PKGU + @ NAME$
   r cells R-NAME + @ r cells R-NAMEU + @ NAME$ DEF-COUNT 1 <> IF
      0 s" registered release callback is missing or defined twice" SITE-FINDING
      EXIT
   THEN
   r cells R-PKG + @ r cells R-PKGU + @ NAME$
   r cells R-NAME + @ r cells R-NAMEU + @ NAME$ DEF-FIND ADMIT-ROOT ;

public

\ Extra root: a definition the coordinator reaches through its row table rather
\ than by name, so the registration walk cannot see it.
: PIN-ROOT ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n na:ptr nu:n :}
   pa pu na nu DEF-COUNT 1 <> IF
      0 s" pinned release root is missing or defined twice" SITE-FINDING
      EXIT
   THEN
   pa pu na nu DEF-FIND ADMIT-ROOT ;

: INDEX ( -- )
   INDEX-ALL ;

: SEED ( -- )
   0 SEED-I !
   BEGIN SEED-I @ ROOT-N @ < WHILE
      SEED-I @ SEED-ROOT
      SEED-I @ 1 + SEED-I !
   REPEAT ;

: RUN ( -- n )
   CLOSURE
   ROOT-N @ ROOT-EXPECT @ <> IF
      0 s" wrong number of participant release callbacks discovered" SITE-FINDING
   THEN
   FINDINGS @ ;

: ROOTS ( -- n ) ROOT-N @ ;
: SITES ( -- n ) REG-SITES @ ;
: QUOTED-SITES ( -- n ) REG-QUOTED @ ;
: DEFS ( -- n ) DEF-N @ ;
: BINDINGS ( -- n ) BIND-N @ ;

private

;package
