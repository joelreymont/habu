\ stdin-closure-lint-core.f - fail-closed drift gate for the stdin driver closure.
\
\ Gate 17e. The stdin driver closure (tools/stdin-closure-lib.f) is the set of
\ engine-side files that shape the piped/stdin engine that becomes bin/hb. Every
\ consumer is either wired to that manifest or checked against it here, so the set
\ cannot drift. Consumers and how each is bound:
\   tools/build-fixpoint.f - stdin emit + stamp digest name the paths through the
\                            SDC-INCLUDE$/SDC-AOT$/SDC-DRIVER$ accessors.
\   tools/srclist.f        - canonical stdin order names the same accessors, and
\                            emits src/habu/driver-io.f, which has no accessor.
\   tools/bootstrap.sh     - audited launcher: the emission it builds for the
\                            stdin driver must compile every SDC-HOST file.
\   tools/hb-build-lib.f   - proven outside the per-file closure: its maker key
\                            hashes the whole engine binary via BF-ENGINE$.
\
\ HOW A CONSUMER IS READ. Not by searching its text - that is what this lint used
\ to do, and it could not tell a call from a mention. `CONTAINS?` over the whole
\ file was satisfied by the accessor's name in a `\` comment or inside a string,
\ so deleting the real `SDC-DECL$ type space` from tools/srclist.f and leaving the
\ name in a comment kept the gate green (measured on master, 0 findings), and so
\ did commenting out `cat src/habu/aot-arm.f` in the launcher.
\
\ A Forth consumer is LEXED by the shared source lexer (package LINT-LEX), and the
\ requirement is that the accessor appears as a live WORD token: a name in a
\ comment is a COMMENT token, and a name inside a literal is that literal's
\ payload, which the lexer never tokenizes. The comparison is case-INSENSITIVE
\ because the dictionary is (docs/forth.md § Naming), and a token that ends
\ `:<name>` counts too, so a consumer that qualifies instead of importing with
\ `using` is still calling it. A path with no accessor is required as one WORD of
\ a string literal's PAYLOAD, since emitting a path is exactly writing it into a
\ literal.
\
\ The launcher is read structurally too, by package BOOTSTRAP-SRC
\ (tools/bootstrap-src-lib.f), which parses the script's SRC_COMMON array and its
\ emit_src cat rows by field role for a named driver. Asking it about
\ SDC-DRIVER$ is what makes the four rows guarded by
\ `if [[ "$driver" == "src/habu/stdin.f" ]]` count, which no driver-independent
\ read of the script can see.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/source-lex.f,
\ tools/bootstrap-src-lib.f and tools/stdin-closure-lib.f.

package STDIN-CLOSURE-LINT
using STDIN-CLOSURE

public

\ ---- source-defect codes ----------------------------------------------------
\ A lexer diagnostic truncates the token table at the defect, so a consumer's
\ later calls are unreadable and every accessor after it would read absent. That
\ is a finding this lint must not invent and must not miss, so the scan refuses
\ instead. -4828..-4830 continue the unclaimed lint-tool gap that holds
\ E-SHADOW-UNTERM (-4800) through E-NS-NONAME (-4827); the gap ends before
\ lib/errors.f's reserved E-REPORT block at -4900.
-4828 constant E-SDCL-QUOTE  \ a string literal ran past end of input
-4829 constant E-SDCL-ROW    \ a `PRIM:`/`PPRIM:` axiom row lacked a header or its closer
-4830 constant E-SDCL-LEX    \ a lexer diagnostic or token kind this lint was never taught

private

$80 constant NAME-CAP
$4A constant NAME-RC

create SLAB LINT-SLAB:CELLS cells allot
create NAME NAME-CAP allot

variable NAME-U
variable BAD
variable PW-I                       \ payload cursor: the word scan is not nested
variable PW-S

: NL ( -- ) 10 emit ;

: NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u NAME-CAP > if s" stdin-closure-lint: consumer name too long" NAME-RC die then
   a NAME u LINT-BMOVE
   u NAME-U ! ;

: NAME$ ( -- ptr u8 n ) NAME NAME-U @ ;

: WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

\ Kinds this scan understands. A WORD is code; a comment and a complete
\ primitive-axiom row are inert spans that call nothing. Any other kind is one
\ this lint was never taught, and stepping over it in silence would let the token
\ span the very call the scan is looking for.
: KNOWN-KIND? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ {: kind:n :}
   kind LINT-LEX:WORD = if LINT-TRUE exit then
   kind LINT-LEX:COMMENT = if LINT-TRUE exit then
   kind LINT-LEX:REGISTRY = ;

: STRING-TOKEN? ( n -- bool ) {: k:n :}
   k LINT-LEX:TOKEN LINT-NORMAL-STRING-OPENER? if LINT-TRUE exit then
   k LINT-LEX:TOKEN LINT-ESC-STRING-OPENER? ;

: UNKNOWN-KIND ( n -- ) {: k:n :}
   s" stdin-closure-lint: " type NAME$ type
   s" : unknown lexer token kind " type k LINT-LEX:KIND@ . NL
   E-SDCL-LEX throw ;

: LEX-DEFECT ( -- )
   s" stdin-closure-lint: " type NAME$ type
   s" :" type LINT-LEX:ERROR-LINE@ .
   s" : " type
   LINT-LEX:ERROR-KIND@ {: kind:n :}
   kind LINT-LEX:UNTERMINATED-QUOTE = if
      s" unterminated string literal" type NL  E-SDCL-QUOTE throw
   then
   kind LINT-LEX:MALFORMED-REGISTRY = if
      s" malformed primitive-axiom row" type NL  E-SDCL-ROW throw
   then
   s" unknown lexer diagnostic" type NL  E-SDCL-LEX throw ;

: LEX-CHECK ( -- )
   LINT-LEX:ERROR? if LEX-DEFECT then
   0 begin dup LINT-LEX:COUNT < while
      dup KNOWN-KIND? 0= if dup UNKNOWN-KIND then
      1+
   repeat drop ;

\ A token names the word when it IS the word, or when it is that word qualified
\ by a package (`PKG:NAME`). Case-insensitive, because the dictionary is.
: TOKEN-NAMES? ( n ptr u8 n -- bool ) {: k:n na:ptr nu:n :}
   k LINT-LEX:TOKEN {: ta:ptr tu:n :}
   ta tu na nu LINT-STR=CI if LINT-TRUE exit then
   tu nu 1 + <= if LINT-FALSE exit then
   ta tu nu - + nu na nu LINT-STR=CI 0= if LINT-FALSE exit then
   ta tu nu - 1 - + c@ $3A = ;

public

\ Does the lexed source call this word: some live WORD token names it.
: NAMES? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n na:ptr nu:n :}
   a u LINT-LEX:SOURCE
   LEX-CHECK
   0 begin dup LINT-LEX:COUNT < while
      dup WORD? if dup na nu TOKEN-NAMES? if drop LINT-TRUE exit then then
      1+
   repeat drop LINT-FALSE ;

private

: PW-SKIP-WS ( ptr u8 n -- ) {: a:ptr u:n :}
   begin PW-I @ u < a PW-I @ + c@ LINT-WS? and while PW-I @ 1+ PW-I ! repeat ;

: PW-SKIP-WORD ( ptr u8 n -- ) {: a:ptr u:n :}
   begin PW-I @ u < a PW-I @ + c@ LINT-WS? 0= and while PW-I @ 1+ PW-I ! repeat ;

\ Every whitespace-delimited word of a literal's payload. A path is one word of a
\ literal that usually holds several (`s" src/habu/driver-io.f " type`), which is
\ the rule tools/lint/schedule-lint.f reads paths by.
: PAYLOAD-HAS? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n pa:ptr pu:n :}
   0 PW-I !
   begin PW-I @ u < while
      a u PW-SKIP-WS
      PW-I @ u < if
         PW-I @ PW-S !
         a u PW-SKIP-WORD
         a PW-S @ +  PW-I @ PW-S @ -  pa pu LINT-STR= if LINT-TRUE exit then
      then
   repeat
   LINT-FALSE ;

public

\ Does the lexed source EMIT this path: some string literal's payload carries it
\ as one whole word.
: QUOTES? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n pa:ptr pu:n :}
   a u LINT-LEX:SOURCE
   LEX-CHECK
   0 begin dup LINT-LEX:COUNT < while
      dup WORD? over STRING-TOKEN? and if
         dup LINT-LEX:CONTENT pa pu PAYLOAD-HAS? if drop LINT-TRUE exit then
      then
      1+
   repeat drop LINT-FALSE ;

private

: SRC$ ( -- ptr u8 n )
   SLAB LINT-SLAB:TEXT ;

: CONSUMER! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NAME!
   a u SLAB LINT-SLAB:LOAD ;

: MISS ( ptr u8 n ptr u8 n -- ) {: what:ptr wu:n na:ptr nu:n :}
   s" stdin-closure-lint: " type NAME$ type
   s"  missing closure " type what wu type
   s"  " type na nu type NL
   1 BAD +! ;

\ the accessor must be CALLED here, not merely mentioned
: NEED-WORD ( ptr u8 n -- ) {: na:ptr nu:n :}
   SRC$ na nu NAMES? if exit then
   s" call to" na nu MISS ;

\ the path has no accessor, so it must be EMITTED here as literal text
: NEED-PATH ( ptr u8 n -- ) {: pa:ptr pu:n :}
   SRC$ pa pu QUOTES? if exit then
   s" emitted path" pa pu MISS ;

: CHECK-BUILD-FIXPOINT ( -- )
   s" tools/build-fixpoint.f" CONSUMER!
   \ build-fixpoint names this one by its accessor too, though the file is not
   \ an SDC-HOST source: it is compiled into every engine, not only the stdin one.
   s" SDC-INCLUDE$" NEED-WORD
   s" SDC-AOT$" NEED-WORD
   s" SDC-DECL$" NEED-WORD
   s" SDC-ARM$" NEED-WORD
   s" SDC-IDENT$" NEED-WORD
   s" SDC-FILE$" NEED-WORD
   s" SDC-DRIVER$" NEED-WORD ;

: CHECK-SRCLIST ( -- )
   s" tools/srclist.f" CONSUMER!
   s" SDC-AOT$" NEED-WORD
   s" SDC-DECL$" NEED-WORD
   s" SDC-ARM$" NEED-WORD
   s" SDC-IDENT$" NEED-WORD
   s" SDC-FILE$" NEED-WORD
   s" SDC-DRIVER$" NEED-WORD
   s" src/habu/driver-io.f" NEED-PATH ;

: CHECK-HB-BUILD ( -- )
   s" tools/hb-build-lib.f" CONSUMER!
   s" BF-ENGINE$" NEED-WORD ;

\ typed-local-lint: allow-bare-local - callback fires per manifest row.
: HOST-ROW ( n ptr u8 n n -- ) {: ix:n pa:ptr pu:n fl:n :}
   fl SDC-HOST SDC-ROLE? 0= if exit then
   pa pu BOOTSTRAP-SRC:HAS? if exit then
   s" compiled source" pa pu MISS ;

: NO-EMISSION ( ptr u8 n -- ) {: what:ptr wu:n :}
   s" stdin-closure-lint: " type NAME$ type
   s" : " type what wu type NL
   1 BAD +! ;

public

\ The launcher's STDIN emission, read by field role for that driver: every
\ SDC-HOST file has to be one of the sources it compiles. Takes the script TEXT
\ so the suite can put a script with one row removed through the same check the
\ gate runs, rather than through a copy of it.
: EMISSION-CK ( ptr u8 n ptr u8 n -- ) {: la:ptr lu:n a:ptr u:n :}
   la lu NAME!
   a u SDC-DRIVER$ BOOTSTRAP-SRC:PARSE
   BOOTSTRAP-SRC:DRIVER-CALLED? 0= if
      s" no emit_src call site builds the stdin driver, so the emission does not exist"
      NO-EMISSION exit then
   BOOTSTRAP-SRC:ARRAY-USED? 0= if
      s" emit_src no longer expands SRC_COMMON, so no array entry reaches a stage source"
      NO-EMISSION then
   [: HOST-ROW ;] SDC-WALK ;

private

: CHECK-BOOTSTRAP ( -- )
   s" tools/bootstrap.sh" {: pa:ptr pu:n :}
   pa pu CONSUMER!
   pa pu SRC$ EMISSION-CK ;

: SCAN ( -- )
   CHECK-BUILD-FIXPOINT
   CHECK-SRCLIST
   CHECK-BOOTSTRAP
   CHECK-HB-BUILD ;

public

: RESET ( -- )
   0 BAD ! ;

: FINDINGS ( -- n )
   BAD @ ;

: LINT ( -- )
   RESET
   SCAN
   s" stdin-closure-lint: drift finding(s)=" type BAD @ . NL ;

: STRICT ( -- )
   LINT
   BAD @ 0 > if 1 throw then ;

;package
