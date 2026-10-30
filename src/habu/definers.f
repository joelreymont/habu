\ definers.f - the interpret loop's definition heads in Habu: `:`, `kernel:`,
\ `trusted:`, `create`, `variable`, `cast:`, `linear:`, `immediate`, `constant`
\ and `defer`, twins of habu2.f EM-INTERPRET-COLON, C-TRUSTED, C-CREATE and kin.
\ STEP (src/habu/interpret.f) asks COMPILING? after a comment and DEFINE? after
\ the package keywords. The head refuses in the engine's order and with its text:
\ a definition writer's own refusal (src/habu/prims.f) exits without a word.

require src/lib/prelude.f
require src/lib/string.f
require src/lib/source-syntax.f
require src/core/checker.f
require src/core/does-clause.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/dict.f
require src/compiler/native/compiler.f
require src/habu/outer.f
require src/habu/packages.f

package OUTER

private

\ Tier 1's capture reads locals before syntax. These cells describe lexical
\ lifetime only; LOCF is the JIT's physical frame and CF its patch stack.
LOC-RECS TYPED-BUFFER BODY-LOCAL-DEPTH n
variable BODY-DEPTH
variable BODY-RAW-NEXT
variable TOK-BODY-DEPTH
variable TOK-BODY-RAW

: BODY-LEX-RESET ( -- )
   0 BODY-DEPTH !  0 BODY-RAW-NEXT ! ;

76 constant DEF-RC-TIER-0        \ the code of the engine head's first refusal, a pass-2 nesting
76 constant DEF-RC-BAD-SIG       \ habu2.f C-SIG-BAD: `trusted:` or `does>` with no signature
71 constant DEF-RC-BODY-FULL     \ habu2.f EM-BODY-CAP-DIE: the body capture is full
72 constant DEF-RC-PD-FULL       \ habu2.f C-PD-DIE-FULL: no pending slot holds the defer

\ def-open, body-append, created-sig!, def-close, def-abort, def-create,
\ def-cast, imm-mark and min-in-mark are OUTER's own rows (src/habu/prims.f),
\ called below by name. trust-sig! has none: it is the `trusted:`, `cast:`,
\ `linear:` and `defer` heads' alone.
TRUSTED: DEF-TRUST-SIG ( ptr u8 n -- ) trust-sig! ;

\ Ending or abandoning a definition also ends the capture's lexical state.
\ No local outlives its definition, as habu2.f C-CLEAR-LOCALS ends them on
\ arm64: x86-64's def-close (kernel-x64.f DEF-CLOSE-BODY) leaves the locals
\ count, so `;` clears it here. def-abort clears it on both.
: DEF-CLOSE ( -- ) def-close  0 LOCN-CELL CELL!  BODY-LEX-RESET ;
: DEF-ABORT ( -- ) def-abort BODY-LEX-RESET ;

\ The compiler entry, the preflight, the def hook and the checker's
\ declaration operations, which take a name and a signature, are raw execution
\ tokens in the engine's cells; these views state the signatures they are
\ called with here.
CAST: AS-COMPILE ( n -- [ ptr u8 n -- ] )
CAST: AS-PREFLIGHT ( n -- [ ptr u8 n ptr u8 n bool -- ] )
CAST: AS-DEF-HOOK ( n -- [ ptr u8 n -- n ] )
CAST: AS-DECLARE ( n -- [ ptr u8 n ptr u8 n -- ] )

\ Tier 0 is T0, a native compiler, on an engine whose NCOMP-DISPATCH:COMP-CELL
\ holds its entry (arm64), and the JIT when that cell is zero; the engine's
\ head latches which in DEF-TIER-CELL, 0 for the JIT. This loop has no tier-0
\ head, so a head at tier 0 (TIER-CELL 0) refuses. A body the engine's head
\ latched to T0 is captured here as a tier-1 body is; one latched to the JIT
\ refuses at its first token, since the JIT compiles each token as it reads
\ it, which this loop cannot call. Each refusal names the token, where the
\ engine's head makes its first refusal. `cast:`, `linear:`, `immediate` and
\ the definers compile no body the JIT reads, so they are read at any tier.
: DEF-TIER-0 ( -- )
   s" hb: tier 0 is not in the Habu loop: " SAY
   DEF-RC-TIER-0 PKG-FAIL ;

\ The engine's twins: habu1.f EMIT-BCAP and habu2.f EM-BODY-CAP-DIE.
\ The capture's first token, the definition's name, which a head seeds: the
\ bytes up to a space, a zero byte or BODYLEN (habu2.f EMIT-DIAGDEF).
: DEF-CAPTURED-NAME ( -- ptr u8 n )
   data-base BODYBUF-OFF + BYTE-VIEW BODYLEN-CELL CELL@ {: b:ptr len:n :}
   0 begin
      dup len < if b over + c@ dup BLANK <> swap 0<> and else false then
   while 1+ repeat
   {: u:n :}
   b u ;

\ The checker declaration xt given takes the name as the capture spells it,
\ qualified, and the signature given.
: DEF-DECLARE ( ptr u8 n n -- )
   {: sig:ptr len:n xt:n :}
   DEF-CAPTURED-NAME sig len xt AS-DECLARE execute ;

\ A capture that would take the body past BODYBUF-CAP ends the definition,
\ naming the ceiling, the definition and the size it needed.
: DEF-BODY-FULL ( n -- ) {: need:n :}
   s" hb: definition body text full at " SAY
   BODYBUF-CAP DIGITS$ SAY
   s"  bytes: " SAY
   DEF-CAPTURED-NAME SAY
   s"  needs " SAY
   need DIGITS$ SAY
   DEF-RC-BODY-FULL THROW-AT ;

\ The source ended inside the pending definition: refused, named and located
\ at its name token (layout.f PENDTKA-CELL), as habu2.f LSRCEND refuses.
: DEF-ENDED ( -- )
   PENDTKA-CELL CELL@ INP-CELL CELL!
   s" hb: source ended inside definition: " SAY
   DEF-CAPTURED-NAME SAY
   RC-UNCLOSED THROW-AT ;

: DEF-CAPTURE ( ptr u8 n -- ) {: a:ptr u:n :}
   BODYLEN-CELL CELL@ u + 1 + {: need:n :}
   need BODYBUF-CAP > if need DEF-BODY-FULL then
   a u body-append ;

\ The engine's twins: habu2.f EM-INTERPRET-COLON and C-TRUSTED.
\ CP at the code ceiling and no record slot left each refuse, naming the
\ keyword. Both comparisons are signed, as the engine's head makes them.
: DEF-CODE-ROOM ( -- )
   cp@ dbase@ REGION + PKG-CODE-RESERVE - >= if
      s" hb: code space full at: " SAY PKG-RC-CODE-FULL PKG-FAIL
   then ;

: DEF-ROOM ( -- )
   DEF-CODE-ROOM
   ndict@ DICT-CAP >= if
      s" hb: dictionary full at: " SAY PKG-RC-DICT-FULL PKG-FAIL
   then ;

\ The name. With the input at its end `:` and `kernel:` refuse with the
\ engine's colon text, `trusted:` as the reader keyword it is (habu2.f
\ C-DIE-KEYWORD-NAME).
: DEF-NAME ( bool -- ) {: trusted:bool :}
   trusted if s" trusted:" OPERAND exit then
   TOKEN if exit then
   s" hb: : missing definition name after " SAY RC-NO-NAME THROW-AT ;

\ The engine's twin: habu2.f EMIT-QUALIFY-DEF.
\ The wordlist NAME:tail goes into: NAME's public one, from its namespace row.
\ With no row a new one is made, as a qualified definition makes it: a public
\ wid and no private one.
: DEF-NAMESPACE ( ptr u8 n -- n ) {: a:ptr q:n :}
   a q XREF-NAMESPACE-WL WL-PROBE {: row:ptr :}
   row XREF-FOUND? if row XREF-PKG-PUBLIC exit then
   PKG-DICT-ROOM
   a q PKG-CODE-ROOM
   a q false namespace-record XREF-REC XREF-PKG-PUBLIC ;

\ A selected unit owns its definitions: a name with a colon anywhere in it,
\ at an edge too, refuses before the qualifier could send it to another
\ package (habu2.f C-UNIT-DEF-NAME-GUARD).
: DEF-UNIT-GUARD ( -- )
   UNIT? 0= if exit then
   TOKEN$ 0 COLON-AT 0 >= if UNIT-REFUSE then ;

\ The tail the record is named and the wordlist it goes into. DEF-TKA and
\ DEF-TKL take the name token, which the refusals after them name, as TKA and
\ TKL still do: the engine moves those to the tail and back, and this head
\ names the tail where the engine names it. A bare name goes into the current
\ wordlist; the qualifier is the first colon at neither edge (one at either
\ edge leaves the whole token a bare name: `:a:b` is bare), and a second colon
\ after it refuses.
: DEF-QUALIFY ( -- ptr u8 n n )
   OVERLAY-GUARD
   DEF-UNIT-GUARD
   SEAL-GUARD
   TKA-CELL CELL@ DEF-TKA-CELL CELL!
   TKL-CELL CELL@ DEF-TKL-CELL CELL!
   TOKEN$ {: a:ptr u:n :}
   a u 0 COLON-AT {: q:n :}
   q 1 <  q 1+ u >=  or if a u get-current exit then
   a u q 1+ COLON-AT 0 >= if PKG-RC-CONTEXT PKG-FAIL then
   a q 1+ ZPTR+  u q - 1-  a q DEF-NAMESPACE ;

\ The engine's twin: habu2.f EMIT-DEF-KW-GUARD.
\ The keywords a body's compiler reads before it looks a word up
\ (EM-COMPILE-KEYWORDS): a word named by one could never be called from a body.
: DEF-KEYWORDS ( -- ptr u8 n )
   S\" if then else begin until again while repeat case of endof endcase construct match s\q c\q .\q s\\\q c\\\q .\\\q ['] [char] does> [: is ;] do loop i >r r> r@ exit recurse ?do +loop j leave unloop {:" ;

\ A design may define none of the engine's dispatch-row names. The twelve
\ admitted syntax rows keep the ordinary reserved-name error; every other
\ row is outside the vocabulary before dictionary duplicate checks run.
: DEF-DESIGN-KEYWORDS ( -- ptr u8 n )
   S\" if else then {: :} s\q package public private ;package using ;using" ;

\ The interpret dispatch rows, plus kernel: and ;match, which the ARM row walk
\ compares inside their constructs. Ordinary words such as require and include
\ may be published by a package even though a definition cannot take their
\ names at this checked head boundary.
: DEF-INTERPRET-KEYWORDS ( -- ptr u8 n )
   S\" kernel: trusted: cast: linear: immediate create variable constant defer export char ' ;match" ;

: DEF-NAME-ONLY-KEYWORDS ( -- ptr u8 n )
   s" : 2constant to require include sumtype variant ;variant ;sumtype" ;

: DEF-OP-KEYWORDS ( -- ptr u8 n )
   s" + - * / mod and or xor lshift rshift dup drop swap over nip = <> < > <= >= 1+ 1- 0= 0< negate invert f+ f- f* f/" ;

: DEF-P2-KEYWORDS ( -- ptr u8 n )
   s" tuck rot -rot 2dup 2drop 2swap 2over 2>r 2r> 2r@ @ !" ;

: DEF-KEYWORD-END ( ptr u8 n n -- n ) {: t:ptr tu:n s:n :}
   s begin dup tu < if t over + c@ BLANK <> else false then while 1+ repeat ;

: DEF-IN-TABLE? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n t:ptr tu:n :}
   0 begin dup tu < while
      {: s:n :}
      t tu s DEF-KEYWORD-END {: e:n :}
      a u  t s ZPTR+ e s -  FOLDED= if true exit then
      e 1+
   repeat
   drop false ;

: DEF-KEYWORD? ( ptr u8 n -- bool )
   DEF-KEYWORDS DEF-IN-TABLE? ;

: DEF-DESIGN-KEYWORD? ( ptr u8 n -- bool )
   DEF-DESIGN-KEYWORDS DEF-IN-TABLE? ;

: DEF-ROW-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u DEF-KEYWORD? if true exit then
   a u DEF-INTERPRET-KEYWORDS DEF-IN-TABLE? if true exit then
   a u DEF-OP-KEYWORDS DEF-IN-TABLE? if true exit then
   a u DEF-P2-KEYWORDS DEF-IN-TABLE? ;

: DEF-NAME-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u DEF-ROW-KEYWORD? if true exit then
   a u DEF-NAME-ONLY-KEYWORDS DEF-IN-TABLE? ;

\ The x86-64 policy writer asks this captured interpreter once per package,
\ before setting its admission bit. Use the same dispatch-row table that guards
\ names in a sealed definition, including pass-2 rows.
public
: POLICY-KEYWORD ( n -- ptr u8 n bool ) {: wid:n :}
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-WORDLIST wid = if
         rec XREF-NAME$ {: a:ptr u:n :}
         a u DEF-DESIGN-KEYWORD? a u DEF-ROW-KEYWORD? or if
            a u true unloop exit
         then
      then
   loop
   s" " false ;

: INSTALL-POLICY ( -- )
   ['] POLICY-KEYWORD data-base POLICY-ABI:KEYWORD-CELL + xt! ;
private

: DEF-KEYWORD-REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" hb: compile keyword cannot be a definition name: " SAY
   a u SAY RC-REJECT THROW-AT ;

: DEF-POLICY-REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" hb: not in vocabulary: " SAY
   a u SAY ENGINE-ERROR:POLICY THROW-AT ;

: DEF-RESERVED-REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" hb: reserved word cannot be a definition name: " SAY
   a u SAY RC-REJECT THROW-AT ;

\ The engine's twin: habu2.f C-RESERVED-DEF-GUARD. Generated bodies bind
\ STRUCTURE-MAKE:PROJECT-NAME$ by name, so once sealed no definition takes it.
: DEF-RESERVED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SEAL-NDICT@ 0= if false exit then
   a u STRUCTURE-MAKE:PROJECT-NAME$ FOLDED= ;

\ Matched as the engine matches its keywords, with only the tail's A-Z folded.
: DEF-WALL ( ptr u8 n -- ) {: a:ptr u:n :}
   POLICY-NDICT-CELL CELL@ 0<> if
      a u DEF-DESIGN-KEYWORD? if a u DEF-KEYWORD-REFUSE then
      a u DEF-NAME-KEYWORD? if a u DEF-POLICY-REFUSE then
   else
      a u DEF-KEYWORD? if a u DEF-KEYWORD-REFUSE then
   then
   a u DEF-RESERVED? if a u DEF-RESERVED-REFUSE then ;

\ The engine's twins: habu2.f C-SIG-START, C-SIG-END and C-SIG-CAPTURE-TSIG.
\ A signature starts at the first byte past the blanks when that byte is `(`,
\ whole token or not: where it starts, and whether one does.
: DEF-SIG-START ( -- ptr u8 bool )
   INP-CELL ADDR@ INE-CELL ADDR@ {: p:ptr e:ptr :}
   p e BLANKS-END {: s:ptr :}
   s e < if s c@ OPEN-COMMENT = else false then {: open:bool :}
   s open ;

\ The byte past the `)` that closes the signature at s, and whether one does;
\ with none, the input's end.
: DEF-SIG-END ( ptr u8 -- ptr u8 bool ) {: s:ptr :}
   INE-CELL ADDR@ {: e:ptr :}
   s begin dup e < if dup c@ CLOSE-COMMENT <> else false then while 1 + repeat
   {: c:ptr :}
   c e < if c 1 + true exit then
   e false ;

: DEF-SIG-PASS ( ptr u8 ptr u8 -- ) {: s:ptr end:ptr :}
   end INP-CELL ADDR!
   s end s - DEF-CAPTURE ;

\ trust-sig! takes the text inside the parentheses. The inner length is the
\ engine's, two less than the whole: one less than the text for a signature
\ the input ends inside.
: DEF-SIG-INNER ( ptr u8 ptr u8 -- ) {: s:ptr end:ptr :}
   s 1 + end s - 2 - DEF-TRUST-SIG ;

: DEF-SIG-TAKE ( ptr u8 ptr u8 -- ) {: s:ptr end:ptr :}
   s end DEF-SIG-INNER
   s end DEF-SIG-PASS ;

\ `:` takes one if it is there, to the input's end if it is not closed.
: DEF-MAYBE-SIG ( -- )
   DEF-SIG-START {: s:ptr open:bool :}
   open 0= if exit then
   s DEF-SIG-END drop {: end:ptr :}
   s end DEF-SIG-TAKE ;

\ A signature that must be there, opened and closed, or the refusal names the
\ token: where it starts and the byte past it. A head's signature that the
\ input ends before or inside is the source ending inside the definition
\ (habu2.f C-PARSE-TRUST-SIG).
: DEF-SIG-SPAN ( bool -- ptr u8 ptr u8 ) {: head:bool :}
   DEF-SIG-START {: s:ptr open:bool :}
   head s INE-CELL ADDR@ = and if DEF-ENDED then
   open 0= if DEF-RC-BAD-SIG PKG-FAIL then
   s DEF-SIG-END {: end:ptr closed:bool :}
   closed 0= if head if DEF-ENDED then DEF-RC-BAD-SIG PKG-FAIL then
   s end ;

: DEF-REQUIRED-SIG ( -- )
   true DEF-SIG-SPAN DEF-SIG-TAKE ;

\ Tier 1 compiles through the entry the AOT seed installed; with none the
\ boot is broken, and the process ends (habu2.f NCOMP-EMIT:LOAD).
: DEF-DISPATCH ( -- )
   NCOMP-DISPATCH:XT-CELL CELL@ 0<> if exit then
   s" hb: native compiler dispatch unset" ENGINE-ERROR:AOT-SEED FAIL-CLOSED ;

\ The tail and its wordlist pass the wall, the room, the duplicate test and the
\ seal's protected wordlists, then the name's code room, before def-open
\ writes anything (habu2.f EMIT-QUALIFY-DEF, EMIT-STORE-DEF-NAME). The
\ refusals name the token, but the wall and the code room name the tail. The
\ kind is the record's: 0 for a body the compiler compiles, DKIND:ADDR for a
\ created word, DKIND:VAL for a constant, DKIND:CAST for a retype.
: DEF-RECORD ( ptr u8 n n n -- ) {: a:ptr u:n wid:n kind:n :}
   a u DEF-WALL
   PKG-DICT-ROOM
   a u wid WL-PROBE XREF-FOUND? if
      s" duplicate definition: " SAY PKG-RC-DUPLICATE PKG-FAIL
   then
   wid PKG-OPEN-WID
   a u PKG-CODE-ROOM
   a u wid kind def-open ;

\ The capture seeded with the name, the spelling the checker is told.
: DEF-SEED ( -- )
   0 BODYLEN-CELL CELL!
   TOKEN$ DEF-CAPTURE ;

: DEF-START ( n -- )
   {: kind:n :}
   DEF-SEED
   DEF-QUALIFY kind DEF-RECORD ;

: DEF-HEAD ( bool -- ) {: trusted:bool :}
   NCOMP-DISPATCH:TIER-CELL CELL@ 0= if DEF-TIER-0 then
   TASK-GUARD
   DEF-ROOM
   trusted DEF-NAME
   0 DEF-START
   0 LOCN-CELL CELL!  0 LOCF-CELL CELL!
   BODY-LEX-RESET
   trusted if
      1 TRUSTED-CELL CELL!
      DEF-REQUIRED-SIG
   else
      DEF-MAYBE-SIG
   then
   DEF-DISPATCH ;

\ The engine's twin: habu2.f NCOMP-EMIT:CAPTURE-STRING.
\ After a string keyword the body takes the literal's text as written, from one
\ byte past the keyword through its closing quote, in one capture. The scan
\ comes first, so a literal with no closing quote or with a bad escape refuses
\ as the top-level keyword does, before any of it is captured. A counted
\ string's length waits for `;`, which compiles the body.
: DEF-TEXT ( -- )
   TEXT {: a:ptr u:n :}
   a u 1+ DEF-CAPTURE ;

: DEF-ESC-TEXT ( -- )
   ESC-SCAN drop {: s:ptr q:ptr :}
   q PAST
   s q s - 1+ DEF-CAPTURE ;

\ The engine's six string keywords, matched as LITERAL? matches them.
: DEF-STRING-TEXT ( -- )
   S\" s\q" TOKEN-IS? if DEF-TEXT exit then
   S\" c\q" TOKEN-IS? if DEF-TEXT exit then
   S\" .\q" TOKEN-IS? if DEF-TEXT exit then
   S\" s\\\q" TOKEN-IS? if DEF-ESC-TEXT exit then
   S\" c\\\q" TOKEN-IS? if DEF-ESC-TEXT exit then
   S\" .\\\q" TOKEN-IS? if DEF-ESC-TEXT then ;

\ A body admits a local before looking up a dictionary word. Tier 0's JIT
\ fills LOCNAMES while compiling `{:`; tier 1 captures each declaration here,
\ then the native compiler reads it from BODYBUF at `;`.
: BODY-LOCAL? ( -- bool )
   TOKEN$ {: a:ptr u:n :}
   LOCN-CELL CELL@ 0 ?do
      data-base LOCNAMES + i LOC-REC * + BYTE-VIEW {: row:ptr :}
      row CELL-VIEW @ u = if
         a u row 8 + u STR= if true unloop exit then
      then
   loop
   false ;

: BODY-LOCAL-ONE ( -- )
   TOKEN$ {: a:ptr u:n :}
   LOCN-CELL CELL@ {: idx:n :}
   idx LOC-RECS >= if
      s" hb: more than 64 locals in one definition: " SAY
      TOKEN$ SAY RC-REJECT THROW-AT
   then
   a u 0 COLON-AT {: colon:n :}
   colon 0 < if u else colon then {: len:n :}
   len LOC-NAME-CAP > if
      s" hb: local name over 16 bytes: " SAY
      TOKEN$ SAY RC-REJECT THROW-AT
   then
   data-base LOCNAMES + idx LOC-REC * + BYTE-VIEW {: row:ptr :}
   len row CELL-VIEW !
   a row 8 + len BYTE-COPY
   BODY-DEPTH @ idx BODY-LOCAL-DEPTH !
   idx 1+ LOCN-CELL CELL! ;

: BODY-LOCAL-GROUP ( -- )
   begin TOKEN while
      TOKEN$ DEF-CAPTURE
      s" :}" TOKEN-IS? if exit then
      BODY-LOCAL-ONE
   repeat ;

\ A closer discards exactly the locals declared in its block. A later rejected
\ token can restore the count without reconstructing any row.
: BODY-BLOCK-DROP ( -- )
   begin
      LOCN-CELL CELL@ 0 > if
         LOCN-CELL CELL@ 1- BODY-LOCAL-DEPTH @ BODY-DEPTH @ >=
      else false then
   while
      LOCN-CELL CELL@ 1- LOCN-CELL CELL!
   repeat ;

: BODY-BLOCK-STEP ( -- )
   LOCN-CELL CELL@ 0= if exit then
   TOKEN$ SOURCE-SYNTAX:BLOCK-OPENER? if
      BODY-DEPTH @ 1+ BODY-DEPTH ! exit
   then
   s" else" TOKEN-IS? if BODY-BLOCK-DROP exit then
   TOKEN$ SOURCE-SYNTAX:BLOCK-CLOSER? if
      BODY-BLOCK-DROP  BODY-DEPTH @ 1- BODY-DEPTH !
   then ;

\ The JIT's sealed row and the tier-1 capture admit the same body vocabulary.
\ `;` is handled before the capture. A miss reaches the five design body
\ keywords only after local, dictionary and numeric lookup, as the native
\ capture does.
: BODY-POLICY ( -- )
   BODY-LOCAL? if exit then
   TOKEN$ NUMBER {: v:n flt:bool num:bool range:bool :}
   range if UNDEFINED then
   POLICY-NDICT-CELL CELL@ 0= if exit then
   num if exit then
   TOKEN$ FIND-SCOPE {: rec:ptr :}
   rec XREF-FOUND? if rec POLICY-CHECK-REC exit then
   TOKEN$ FIND {: used:ptr :}
   used XREF-FOUND? if used POLICY-CHECK-REC exit then
   s" if" TOKEN-IS? if exit then
   s" else" TOKEN-IS? if exit then
   s" then" TOKEN-IS? if exit then
   S\" s\q" TOKEN-IS? if exit then
   s" {:" TOKEN-IS? if exit then
   POLICY-REFUSE ;

\ An armed checker's preflight gets the body so far, the token and the trusted
\ cell first; one armed without a preflight is refused (habu2.f C-PREFLIGHT,
\ its twin).
: DEF-PREFLIGHT ( -- )
   HOOK-CELL CELL@ 0= if exit then
   COMPILE-PREFLIGHT-CELL CELL@ {: xt:n :}
   xt 0= if
      S\" hb: compile preflight hook missing\n" SAY RC-REJECT throw
   then
   data-base BODYBUF-OFF + BYTE-VIEW BODYLEN-CELL CELL@ TOKEN$ TRUSTED-CELL CELL@ 0<>
   xt AS-PREFLIGHT execute ;

\ The engine's twin: habu2.f NCOMP-EMIT:CAPTURE-IMMEDIATE.
\ A captured token LFIND resolves to an immediate word. One the checker calls a
\ stack-neutral parsing immediate (parse-imm) may read the input after it or
\ end the definition, and the compiler passes over it at `;`: it meets an armed
\ checker's preflight in DEF-RUN (C-CALL-COMPILE-IMMEDIATE), then runs now. One
\ spelled does> is the keyword (DEF-DOES?), as the JIT reads it. Any other
\ immediate meets the preflight here, which refuses an unmodeled one as the JIT
\ does before it would run one, and then waits in the capture for the
\ compiler. The live checker owner answers (CHECKER-OWNER:PARSE-IMM?), as the
\ compiler asks at `;` (elaborate.f): a binding fixed at this file's load would
\ ask the retained checker, which never saw a `parse-imm` a build window's
\ replacement checker recorded.
: DEF-IMMEDIATE ( -- ptr n bool )
   TOKEN$ FIND-SCOPE {: rec:ptr :}
   rec XREF-FOUND? 0= if NULL-PTR false exit then
   rec XREF-FLAGS DNAME-IMM and 0= if NULL-PTR false exit then
   rec POLICY-CHECK-REC
   rec TOKEN$ CHECKER-OWNER:PARSE-IMM?
   dup 0= s" does>" TOKEN-IS? 0= and if DEF-PREFLIGHT then ;

\ Keep the original occurrence across the unit hook and the preflight, then
\ select its host body. A preflight that retires it cannot substitute a
\ reused dictionary slot. The stack's floor holds after the word, as after a
\ word the loop runs.
: DEF-RUN ( ptr n -- )
   DEF-OCC:SELECT {: slot:n occurrence:n :}
   0 UNIT-EV-IMMEDIATE UNIT-EVENT drop
   DEF-PREFLIGHT
   slot occurrence DEF-OCC:RESOLVE NHOST:RUN-REC
   -1 PEND-OPAQUE-CELL CELL!
   execute-floor FLOORED ;

: DEF-IMMEDIATE? ( -- bool )
   DEF-IMMEDIATE if DEF-RUN true exit then
   drop false ;

\ The engine's twin: habu2.f NCOMP-EMIT:CAPTURE-DOES.
\ `does>` ends a defining word's own part: DOESB takes the capture's length
\ with `does>` in it, where `;` splits the body, and the signature after it,
\ which must be there, opened and closed, is the effect of each word the
\ definer creates. The signature joins no capture: INP passes it and
\ created-sig! takes a copy of its inside at here, where it outlives the input
\ (C-PARSE-CREATED-SIG). A second `does>` refuses naming `does>` (C-DIE-DOES),
\ and a missing or open signature names the token as spelled (C-SIG-BAD).

: DEF-CREATED ( -- )
   false DEF-SIG-SPAN {: s:ptr end:ptr :}
   end INP-CELL ADDR!
   s 1 + end s - 2 - COPY-HERE created-sig! ;

\ The pending parent is record NDICT. Its clause will be published under the
\ parent's name plus ;does in the same wordlist. Check the live records before
\ the signature copy or split changes any state, including a folded-case hit.
: DEF-DOES-NAME ( -- )
   ndict@ XREF-REC {: parent:ptr :}
   parent XREF-NAME$ {: stem:ptr stemu:n :}
   parent XREF-WORDLIST {: wid:n :}
   DOES-CLAUSE:SUFFIX$ {: suffix:ptr sufu:n :}
   ndict@ 0 ?do
      i XREF-REC {: row:ptr :}
      row XREF-WORDLIST wid = if
         row XREF-NAME$ {: name:ptr size:n :}
         size stemu sufu + = if
            name stemu stem stemu STR=CI
            name stemu + sufu suffix sufu STR=CI and if
               s" duplicate definition: " SAY
               stem stemu SAY suffix sufu SAY
               PKG-RC-DUPLICATE THROW-AT
            then
         then
      then
   loop ;

: DEF-DOES ( -- )
   DOESB-CELL CELL@ 0<> if s" does>" SAY RC-REJECT THROW-AT then
   DEF-DOES-NAME
   DEF-CREATED
   BODYLEN-CELL CELL@ DOESB-CELL CELL!
   1 CLAUSE-CELL CELL!
   \ The head's locals end here, as habu2.f J-DOES ends them: the clause
   \ cannot name one, and a local it declares is captured from an empty scope.
   0 LOCN-CELL CELL!  0 BODY-DEPTH ! ;

\ The engine reads `does>` as its keyword, with the token's A-Z folded, unless
\ LFIND finds a word of that spelling that is not immediate, which the body
\ calls instead.
: DEF-CALL? ( -- bool )
   TOKEN$ FIND-SCOPE {: rec:ptr :}
   rec XREF-FOUND? 0= if false exit then
   rec XREF-FLAGS DNAME-IMM and 0= ;

: DEF-DOES? ( -- bool )
   s" does>" TOKEN-IS? 0= if false exit then
   DEF-CALL? if false exit then
   DEF-DOES true ;

\ The engine's twins: habu2.f C-DEF-SOURCE-CLOSE and C-DEF-SOURCE-END.
\ A definition closes in the buffer that opened it. ENTRY-PEND is the record
\ pending as the buffer OUTER:INTERPRET reads began (habu1.f EMIT-EVAL-BODY,
\ layout.f EVAL-FRAME:PEND); a pending record other than it is the buffer's
\ own. A throw out of the buffer, these refusals included, puts the entry
\ record back in PEND-CELL (OUTER:INTERPRET): the buffer's own definition is
\ abandoned, so no later `;` publishes it, and an outer one stays pending, as
\ habu2.f EM-EVAL-THROW-RECOVER leaves them. Unlike the engine's, this loop
\ puts no CP, NDICT or DP back.
variable ENTRY-PEND

\ A `;` that would close the record pending as the buffer began, one an outer
\ buffer opened, is refused at the `;`, naming the definition.
: DEF-SOURCE-CLOSE ( -- )
   PEND-CELL CELL@ ENTRY-PEND @ <> if exit then
   s" hb: source closed a definition it did not open: " SAY
   DEF-CAPTURED-NAME SAY
   RC-UNCLOSED THROW-AT ;

: DEF-SOURCE-END ( -- )
   PEND-CELL CELL@ ENTRY-PEND @ = if exit then
   DEF-ENDED ;

\ An evaluate begun inside this native definition owns only its new tokens.
\ The checker reads the body prefix for binding, leaving the definition's
\ effect, open scopes and publication to its eventual `;`. A refusal lands in
\ evaluate's existing frame, whose token mark removes only the failed token.
TRUSTED: DEF-PREFIX-BIND ( -- )
   ENTRY-PEND @ 0= if exit then
   data-base BODYBUF-OFF + BYTE-VIEW
   BODYLEN-CELL CELL@ TOKBODY-CELL CELL@
   CHECKER-PREFIX:BIND? {: rollback:n ok:bool :}
   ok 0= if
      \ A parser operand can roll back its opener as well as the operand.
      \ The token mark must then restore the state before that opener.
      rollback TOKBODY-CELL CELL@ < if 0 TOK-BODY-RAW ! then
      rollback TOKBODY-CELL CELL! UNDEFINED
   then ;

\ The engine's twin: habu2.f NCOMP-EMIT:EM-COMPILE.
\ `;`, the one byte, ends the definition and joins no capture. The body goes
\ to the compiler entry the AOT seed installed, as the engine's tier-1 `;`
\ hands it (NCOMP-EMIT:LOAD), checked again here since a body immediate can
\ have cleared it. The compiler checks, compiles and publishes the record, or
\ throws with the definition still pending, as the engine's does. On its
\ return def-close clears what def-open set. This `;` recovers: it declares
\ so in RECOVER-CELL for the compile, and abandons the definition the compiler
\ refused with E-NCOMP-RECOVERY, which only a multi-error run throws, and
\ reads on.
: DEF-COMPILE ( ptr u8 n -- )
   DEF-DISPATCH NCOMP-DISPATCH:XT-CELL CELL@ AS-COMPILE execute ;

: DEF-SEMI? ( -- bool )
   s" ;" TOKEN-IS? 0= if false exit then
   DEF-SOURCE-CLOSE
   NCOMP-DISPATCH:RECOVER-CELL CELL@ {: prior:n :}
   -1 NCOMP-DISPATCH:RECOVER-CELL CELL!
   [: data-base BODYBUF-OFF + BYTE-VIEW BODYLEN-CELL CELL@ DEF-COMPILE ;] catch
   {: rc:n :}
   prior NCOMP-DISPATCH:RECOVER-CELL CELL!
   rc E-NCOMP-RECOVERY = if DEF-ABORT true exit then
   rc 0<> if rc throw then
   DEF-CLOSE
   true ;

\ While a definition is pending every token is its body's (habu2.f EM-COMMENT):
\ tier 1 ends it at `;`, or captures the token, runs it if it is a neutral
\ immediate, splits the body at `does>`, or takes a string keyword's text
\ (NCOMP-EMIT:EM-COMPILE).
: COMPILING? ( -- bool )
   PEND-CELL CELL@ 0= if false exit then
   NCOMP-DISPATCH:DEF-TIER-CELL CELL@ 0= if DEF-TIER-0 then
   BODY-RAW-NEXT @ if
      0 BODY-RAW-NEXT !
      TOKEN$ DEF-CAPTURE  DEF-PREFIX-BIND  true exit
   then
   DEF-SEMI? if true exit then
   BODY-LOCAL? if
      TOKEN$ DEF-CAPTURE  DEF-PREFIX-BIND  true exit
   then
   BODY-POLICY
   TOKEN$ DEF-CAPTURE
   s" {:" TOKEN-IS? if BODY-LOCAL-GROUP DEF-PREFIX-BIND true exit then
   BODY-BLOCK-STEP
   DEF-IMMEDIATE? if true exit then
   DEF-DOES? if true exit then
   DEF-STRING-TEXT
   TOKEN$ SOURCE-SYNTAX:PARSING-KEYWORD? if -1 BODY-RAW-NEXT ! then
   DEF-PREFIX-BIND
   true ;

\ The engine's twins: habu2.f C-CREATE, EMIT-CREATE and C-VARIABLE.
\ A created word is published whole where its keyword is read: def-create
\ writes the code that pushes its data address and appends the record. The
\ checker then reads it as the engine hands it over. `variable` is `create`
\ and one cell more, and the engine names `create` for both: in the missing
\ name's refusal and in the def hook's text. x86-64 has no def-create body
\ (src/habu/kernel-x64.f refuses the row): there NCOMP compiles the body that
\ pushes the address, as it compiles every definer's, and def-close ends it.

\ The keyword joins the capture, and an armed check hook reads it, `NAME
\ create` for a created word, its verdict dropped: a definer's word always
\ publishes (habu2.f C-DEFHOOK). The hook runs on the program's stack, so
\ neither this word nor its callers keep locals.
: DEF-HOOK ( ptr u8 n -- )
   DEF-CAPTURE
   HOOK-CELL CELL@ 0= if exit then
   data-base BODYBUF-OFF + BYTE-VIEW BODYLEN-CELL CELL@
   HOOK-CELL CELL@ AS-DEF-HOOK execute drop ;

: DEF-ADDR-HOOK ( -- )
   s" create" DEF-HOOK ;

\ The active owner's trust-raw. With none and a hook set the target owner's,
\ and with no target's either the process ends naming it; with neither an
\ owner's nor a hook, 0 (LASTC-TRUST:FIND-ACTIVE, FIND-RAW).
: DEF-RAW-ACTIVE ( -- n )
   NCOMP-DISPATCH:DECL-A NCOMP-DISPATCH:DECL-RAW-OFF PKG-OPERATION {: own:n :}
   own 0<> if own exit then
   HOOK-CELL CELL@ 0= if 0 exit then
   NCOMP-DISPATCH:DECL-RAW-OFF PKG-TARGET {: target:n :}
   target 0= if s" trust-raw" RC-REJECT FAIL-CLOSED then
   target ;

\ A definer's effect, raw dictionary storage stated by sig, goes to the active
\ owner's trust-raw, then to the target owner's while an active owner record
\ exists, unless the two are the same (LASTC-TRUST:FIND-TARGET); the checker
\ seals it through trust-raw (habu2.f LASTC-TRUST:PUBLISH-PTR-A).
: DEF-RAW-PUBLISH ( ptr u8 n -- ) {: sig:ptr su:n :}
   DEF-RAW-ACTIVE {: own:n :}
   own 0= if exit then
   sig su own DEF-DECLARE
   NCOMP-DISPATCH:DECL-CELL CELL@ 0= if exit then
   NCOMP-DISPATCH:DECL-RAW-OFF PKG-TARGET {: target:n :}
   target 0= if exit then
   NCOMP-DISPATCH:DECL-A NCOMP-DISPATCH:DECL-RAW-OFF PKG-OPERATION target = if exit then
   sig su target DEF-DECLARE ;

\ A created word's effect is a cell of raw dictionary storage.
: DEF-ADDR-PUBLISH ( -- )
   s" -- ptr a" DEF-RAW-PUBLISH ;

\ The DATA address a created word's body pushes, as the number NCOMP takes.
TRUSTED: DEF-HERE ( -- n ) here ;

\ The pending DKIND:ADDR record published: def-create's tail, or on x86-64
\ the body NCOMP compiles at a cell-aligned here and def-close.
: DEF-ADDR-TAIL ( -- )
   HB-TARGET-LINUX-X86-64? if
      align
      DEF-HERE NDICT:FIXED-ADDR NCOMP:COMPILE-FIXED
      DEF-CLOSE
      exit
   then
   def-create ;

\ The head refuses as `:`'s does, at any tier, the tail publishes the word,
\ DP rounded up to a cell. No record slot left is refused after the name,
\ naming it, as the engine's `create` refuses it (C-QUALIFY-CAP, here
\ DEF-RECORD). The engine's `create` writes at CP at the code ceiling, which
\ def-open refuses, so the code room refuses first, naming the keyword, as
\ `:`'s does.
: DEF-ADDR-ROW ( bool -- ) {: cell?:bool :}
   TASK-GUARD
   DEF-CODE-ROOM
   s" create" OPERAND
   DKIND:ADDR cell? if DNAME-CELL or then DEF-START
   DEF-ADDR-TAIL ;

\ A compiled defining word reaches `create` through the engine primitive,
\ which calls the row through CREATEP-CELL (INSTALL-CREATE below) without the
\ keyword's hook and raw-effect registration: those belong to the
\ interpreting `create` only.
: DEF-CREATE-ROW ( -- )
   false DEF-ADDR-ROW ;

: DEF-ADDR ( bool -- )
   DEF-ADDR-ROW
   DEF-ADDR-HOOK
   DEF-ADDR-PUBLISH ;

\ The engine's twin: habu2.f C-CONSTANT.
\ A constant's record opens as DKIND:VAL, the stamp a mention of it folds to,
\ and NCOMP compiles the body that pushes the value, at either tier, as the
\ engine's definers are native at both. It takes the program's top cell after
\ its record opens, where the engine's pops it. With none the engine reads
\ below its stack and faults (rc 102); this loop names the underflow, as
\ after a word. Its effect is registered as raw storage, `-- a`.
variable DEF-VALUE

: DEF-CONSTANT ( -- )
   TASK-GUARD
   s" constant" OPERAND
   DKIND:VAL DEF-START
   depth 0= if s" E-UNDERFLOW: " REFUSE then
   [: DEF-VALUE ! ;] CELL-TAKE RUN-LITERAL
   DEF-VALUE @ NDICT:FIXED-VAL NCOMP:COMPILE-FIXED
   DEF-CLOSE
   s" constant" DEF-HOOK
   s" -- a" DEF-RAW-PUBLISH ;

\ The engine's twins: habu2.f C-CAST and C-IDENTITY.
\ `cast: NAME ( in -- out )` declares a retype, and `linear: NAME ( in -- out )`
\ a linear one. Neither has a body or a `;`, so each completes where its
\ keyword is read, at any tier: the checker's registrar proves the row legal
\ and records it, def-cast publishes the empty checked body as the identity,
\ and the record facts the registrar latched go into the record. Nothing calls
\ the def hook.

\ With the input at its end the refusal names the keyword as spelled, at the
\ end's line (habu2.f C-CAST-DIE-NO-NAME).
: DEF-CAST-NAME ( -- )
   TOKEN if exit then
   s" hb: cast: missing name after " SAY
   TOKEN$ SAY RC-NO-NAME THROW-AT ;

\ The signature must be there, opened and closed, before the qualifier's
\ refusals, and INP passes it first, so those name the line it ends on
\ (C-PARSE-REQUIRED-SIG precedes C-QUALIFY-DEF). def-open clears TSIG, so
\ trust-sig! takes the signature once the record is open.
: DEF-CAST-HEAD ( -- )
   DEF-SEED
   false DEF-SIG-SPAN {: s:ptr end:ptr :}
   s end DEF-SIG-PASS
   DEF-QUALIFY DKIND:CAST DEF-RECORD
   s end DEF-SIG-INNER ;

\ A signature's inside goes to the active owner's registrar at off, then to
\ the target owner's unless the active owner holds the same one; a missing
\ owner or registrar is skipped
\ (habu2.f DEF-TRUST:REGISTER-IDENTITY). A row the checker refuses throws
\ before def-cast counts the record, so no callable name is left.
: DEF-REGISTER-IDENTITY ( ptr u8 n n -- ) {: sig:ptr su:n off:n :}
   NCOMP-DISPATCH:DECL-A off PKG-OPERATION {: own:n :}
   own 0<> if sig su own DEF-DECLARE then
   off PKG-TARGET {: target:n :}
   target 0= if exit then
   NCOMP-DISPATCH:DECL-A off PKG-OPERATION target = if exit then
   sig su target DEF-DECLARE ;

: DEF-REGISTER-CAST ( -- )
   TSIG-A-CELL ADDR@ TSIG-U-CELL CELL@ NCOMP-DISPATCH:DECL-CAST-OFF
   DEF-REGISTER-IDENTITY ;

\ With a hook set, the published record takes the facts the registrar latched,
\ through the owner the registrar asked, as the native publisher drains them
\ (src/compiler/native/publish.f PENDING-FACTS): the wide mark, then the
\ minimum input arity, which the interpret dispatch refuses an underdepth
\ call on. With none they wait, as the engine's do (habu2.f
\ EM-REC-WIDE-PUBLISH).
: DEF-CAST-FACTS ( -- )
   HOOK-CELL CELL@ 0= if exit then
   CHECKER-OWNER:WIDE-PUBLISH
   CHECKER-OWNER:MIN-IN {: mi:n :}
   mi 0<> if ndict@ 1- mi min-in-mark then ;

\ The keyword refuses a task's thread, then a full code space or dictionary,
\ naming itself, then a missing name, as the engine's does. Its name folds
\ apart from the def-cast row: a word spelled DEF-CAST here would shadow the
\ row for every later body, and DEF-LINEAR's def-cast would read a cast head.
: DEF-CAST-ROW ( -- )
   TASK-GUARD
   DEF-ROOM
   DEF-CAST-NAME
   DEF-CAST-HEAD
   DEF-REGISTER-CAST
   def-cast
   DEF-CAST-FACTS ;

\ A qualified linear name is refused by the certifier before DEF-QUALIFY can
\ create its namespace. A second colon is left to DEF-QUALIFY's syntax refusal.
: LINEAR-QUALIFIED? ( -- bool )
   TOKEN$ {: a:ptr u:n :}
   a u 0 COLON-AT {: q:n :}
   q 0 <= q 1+ u >= or if false exit then
   a u q 1+ COLON-AT 0< ;

: DEF-LINEAR ( -- )
   TASK-GUARD
   DEF-ROOM
   s" linear:" OPERAND
   DEF-SEED
   false DEF-SIG-SPAN {: s:ptr end:ptr :}
   s end DEF-SIG-PASS
   LINEAR-QUALIFIED? if
      s 1 + end s - 2 - NCOMP-DISPATCH:DECL-LINEAR-OFF DEF-REGISTER-IDENTITY
      TOKEN$ SAY RC-REJECT THROW-AT
   then
   DEF-QUALIFY DKIND:CAST DEF-RECORD
   s end DEF-SIG-INNER
   TSIG-A-CELL ADDR@ TSIG-U-CELL CELL@ NCOMP-DISPATCH:DECL-LINEAR-OFF
   DEF-REGISTER-IDENTITY
   def-cast
   DEF-CAST-FACTS ;

\ The engine's twin: habu2.f C-DEFER.
\ `defer NAME ( in -- out )` declares a word that calls the execution token in
\ its dispatch cell, an aligned DATA cell that starts with `defer-unset`'s and
\ that `is` in a body re-points. It refuses in the engine's order and with its
\ text: a task's thread, the room, the name, then the signature, which must be
\ there, opened and closed, then `defer-unset`, whose cell is allotted before
\ the qualifier, as the engine's is. The checker gets the effect before NCOMP
\ compiles the body, which asks it whether the defer returns; NPUB lays the
\ trailer after the routine and def-close ends the definition. The engine
\ registers after it publishes, which nothing observes: trust-decl never
\ consults the dictionary (habu2.f DEF-TRUST).

\ With the input at its end the token cells still hold the keyword, which the
\ refusal names (DEFER-DIAG:DIE-NO-NAME, rc $4A).
: DEFER-NAME ( -- )
   TOKEN if exit then
   s" hb: defer: missing name after " SAY TOKEN$ SAY RC-NO-NAME THROW-AT ;

\ `defer-unset`'s execution token, found as LFIND finds it; with none the
\ refusal is the bare name token (C-DEFER-FIND-UNSET, rc $46).
: DEFER-UNSET-XT ( -- n )
   s" defer-unset" FIND-SCOPE {: rec:ptr :}
   rec XREF-FOUND? 0= if TOKEN$ SAY RC-REJECT THROW-AT then
   rec XREF-START ;

\ The cell holds an execution token, so xt! declares it one, as the engine's
\ C-DEFER-CELL marks it: a snapshot moves it with the code region.
TRUSTED: DEF-XT! ( n n -- ) xt! ;

: DEFER-CELL ( -- n )
   DEFER-UNSET-XT {: xt:n :}
   align
   DEF-HERE {: cell:n :}
   1 cells allot
   xt cell DEF-XT!
   cell ;

\ The engine's twins: habu2.f C-PRETRUST-READY? and C-PD-CAPTURE.
\ A target owner without both trust-decl and checker-defer cannot register a
\ defer yet: a build's src/core/checker.f declares defers before `: TRUST`, and
\ the build reads it through this loop. The defer's name and signature wait in
\ the next pending slot (src/habu/layout.f PD-*) until checker.f's
\ DRAIN-PRETRUST replays both registrations to the target owner.
\ With both owner cells zero the engine's loop still holds the defer, while
\ NCOMP below refuses it ("ncomp: cannot compile"): no checker knows its
\ effect. No build reaches that state: native-runtime.f BIND-OWNER sets both
\ cells at boot, and checker.f CHECKER-RESET-SOURCE zeroes only the target's.
: DEFER-READY? ( -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-OFF PKG-TARGET 0<>
   NCOMP-DISPATCH:DECL-DEFER-OFF PKG-TARGET 0<> and ;

\ A full table, or a name or signature past its slot field, ends the process
\ naming the defer.
: PD-FULL ( -- )
   s" hb: pre-trust defer table full: " SAY TOKEN$ SAY
   NL 1 DEF-RC-PD-FULL FAIL-CLOSED ;

\ The u bytes at a into the slot field at DATA offset off, their count into
\ the cell at lenoff.
: PD-PUT ( ptr u8 n n n n -- ) {: a:ptr u:n cap:n lenoff:n off:n :}
   u cap > if PD-FULL then
   u lenoff CELL!
   a data-base off + BYTE-VIEW u BYTE-COPY ;

: PD-HOLD ( ptr u8 n -- ) {: sig:ptr su:n :}
   PD-TABLE-OFF CELL@ {: count:n :}
   count PD-CAP >= if PD-FULL then
   PD-TABLE-OFF PD-SLOTS-REL + count PD-SLOT * + {: slot:n :}
   DEF-CAPTURED-NAME PD-NAME-CAP slot PD-NLEN-OFF + slot PD-NAME-OFF + PD-PUT
   sig su PD-SIG-CAP slot PD-SLEN-OFF + slot PD-SIG-OFF + PD-PUT
   count 1 + PD-TABLE-OFF CELL! ;

\ The active owner's operation at off, then the target owner's, 0 when the
\ active owner's field holds the same one (DECL-OWNER:FIND, SKIP-SAME).
: DEF-OWNERS ( n -- n n ) {: off:n :}
   NCOMP-DISPATCH:DECL-A off PKG-OPERATION {: own:n :}
   off PKG-TARGET {: target:n :}
   own  target own = if 0 else target then ;

\ The name the capture holds and signature sig to each trust-decl
\ (DEF-TRUST:REGISTER), then the name to each checker-defer
\ (C-CALL-CHECKER-DEFER). Before the target owner is ready only the active
\ owner's two are called, and the pending slot holds the defer for the
\ target's.
: DEFER-REGISTER ( ptr u8 n -- ) {: sig:ptr su:n :}
   DEFER-READY? {: ready:bool :}
   NCOMP-DISPATCH:DECL-EFFECT-OFF DEF-OWNERS {: own:n target:n :}
   own 0<> if sig su own DEF-DECLARE then
   ready target 0<> and if sig su target DEF-DECLARE then
   NCOMP-DISPATCH:DECL-DEFER-OFF DEF-OWNERS {: down:n dtarget:n :}
   down 0<> if DEF-CAPTURED-NAME down PKG-AS-NAME-ACTION execute then
   ready dtarget 0<> and if DEF-CAPTURED-NAME dtarget PKG-AS-NAME-ACTION execute then
   ready 0= if sig su PD-HOLD then ;

: DEF-DEFER ( -- )
   TASK-GUARD
   DEF-ROOM
   DEFER-NAME
   DEF-SEED
   false DEF-SIG-SPAN {: s:ptr end:ptr :}
   s end DEF-SIG-PASS
   DEFER-CELL {: cell:n :}
   DEF-QUALIFY 0 DEF-RECORD
   s end DEF-SIG-INNER
   s 1 + end s - 2 - DEFER-REGISTER
   cell NCOMP:FIXED-DEFER NCOMP:COMPILE-FIXED
   DEF-CLOSE ;

\ The engine's `:` is the one byte, `kernel:` its synonym; `trusted:`,
\ `create`, `variable`, `constant`, `defer`, `cast:`, `linear:` and
\ `immediate` are matched as LITERAL? matches its keywords. `variable` then
\ allots one cell more and stores nothing. `immediate` marks the newest
\ record, whatever it is, and checks nothing (habu2.f C-IMMEDIATE).
: DEFINE? ( -- bool )
   s" :" TOKEN-IS? if false DEF-HEAD true exit then
   s" kernel:" TOKEN-IS? if false DEF-HEAD true exit then
   s" trusted:" TOKEN-IS? if true DEF-HEAD true exit then
   s" create" TOKEN-IS? if false DEF-ADDR true exit then
   s" variable" TOKEN-IS? if true DEF-ADDR 1 cells allot true exit then
   s" constant" TOKEN-IS? if DEF-CONSTANT true exit then
   s" defer" TOKEN-IS? if DEF-DEFER true exit then
   s" cast:" TOKEN-IS? if DEF-CAST-ROW true exit then
   s" linear:" TOKEN-IS? if DEF-LINEAR true exit then
   s" immediate" TOKEN-IS? if imm-mark true exit then
   false ;

public

: INSTALL-CREATE ( -- )
   ['] DEF-CREATE-ROW data-base CREATEP-CELL + xt! ;

;package
