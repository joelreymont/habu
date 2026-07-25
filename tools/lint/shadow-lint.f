\ shadow-lint.f — toolchain definitions must not shadow engine PRIM names.
\ Self-hosted shadow lint. The engine dict is later-wins and the checker
\ records later sigs over PTAB, so a GLOBAL toolchain word named like a prim
\ silently replaces it for every program the toolchain-loaded engine compiles.
\ Two classes of false alarm are avoided so the report only names real clobbers.
\ First, a definer keyword written inside a string literal — for example
\ `s" declare it with variable or create"` — is prose, not a definition, so
\ per-file scanning runs through the string-aware lexer in tools/lint/source-lex.f,
\ which consumes s" / s\" / ." / c" bodies as opaque spans exactly like the real
\ parser. Second, a definition inside a `package ... ;package` block is a scoped
\ tail (docs/forth.md § Packages: unqualified global lookup never finds it), so it
\ cannot clobber a global prim and is not flagged; only true global-scope shadows
\ are reported.
\ Run: bin/hb --load tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/lint/source-lex.f tools/lint/shadow-lint.f
\ Prim-name extraction still tokenizes habu1.f: SCAN-PRIMS reads prim names out of
\ its `s" NAME"` literals, so that pass keeps the plain whitespace tokenizer.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

package SHADOW-LINT-TOOL

$80000 constant SL-FB-CAP                    \ largest linted source + headroom (checker.f is the largest and crossed $60000 with the field-projection window)
create FB SL-FB-CAP allot                    \ one file at a time

\ ---- prim-name store: copied out of habu1.f so FB can be reused per file ----
create PNAMES 8192 allot   variable PEND
$200 constant PMAX
create POFF PMAX cells allot   create PRIM-LEN PMAX cells allot   variable PN#
: ADD-PRIM  ( ptr u8 n -- ) {: a:ptr u :}
   a  PNAMES PEND @ +  u LINT-BMOVE
   PNAMES PEND @ +  POFF PN# @ cells + !   u PRIM-LEN PN# @ cells + !
   PEND @ u + PEND !   PN# @ 1+ PN# ! ;
: PRIM?  ( ptr u8 n -- bool ) {: a:ptr u :}   \ case-insensitive membership
   0 begin dup PN# @ < while
      dup cells POFF + @  over cells PRIM-LEN + @  a u LINT-STR=CI IF drop LINT-TRUE exit THEN  1+
   repeat  drop  LINT-FALSE ;

\ token "NAME"" (trailing quote from s" NAME") -> NAME
: TRIMQ  ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u 0 <= IF a u exit THEN
   a u 1- + c@ 34 = IF a u 1- exit THEN
   a u ;

\ ---- extract prim names: s" NAME" ['] X FPRIM[-L] -> NAME (habu1.f tokenized) ----
variable SI
: SCAN-PRIMS  ( -- )
   0 PN# !  0 PEND !  3 SI !
   begin SI @ TN# @ < while
      SI @ TOK s" FPRIM" LINT-PREFIX?  SI @ 2 - TOK s" [']" LINT-STR= and IF
         SI @ 3 - TOK TRIMQ ADD-PRIM THEN
      SI @ 1+ SI !
   repeat ;

\ ---- lint one toolchain source: a GLOBAL-scope defining word whose name is a
\ prim is a SHADOW. Definitions inside a `package ... ;package` block are scoped
\ tails and cannot clobber a global prim, so they are not flagged. String-literal
\ bodies are opaque to the lexer, so a definer keyword written in prose is never
\ mistaken for a definition. ----
\ Negative so error-code-lint keeps it globally unique — a positive code is a
\ shared sysexits status, and 83 collided with the engine's SEAL-VIOLATION.
\ -4800 sits in the unclaimed gap between lib/codegen's E-CG codes (-4700/-4701)
\ and the -5000 cluster; lib/errors.f owns -2000..-4499 and the verdict policy
\ blocks E-PV / E-TRV own -4500..-4699. It claims no reserved range.
-4800 constant E-SHADOW-UNTERM
\ The second lexer defect: a `PRIM:`/`PPRIM:` axiom row with no header or no
\ closer. It hides definitions the same way an open string does, but a scan that
\ names it "unterminated string literal" sends the reader hunting for a quote, so
\ it gets its own code in the same unclaimed gap.
-4802 constant E-SHADOW-REGISTRY

variable BAD  variable LI  variable IN-PACKAGE

\ Lexed token k equals word a/u, case-insensitively; comment tokens never match.
: LEX-WORD= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD <> IF LINT-FALSE exit THEN
   k LINT-LEX:TOKEN a u LINT-STR=CI ;

\ Offset from a definer token to the name it defines, 0 when k is not a definer.
: DEF-NAME-OFFSET ( n -- n )
   dup s" :" LEX-WORD= IF drop 1 exit THEN
   dup s" constant" LEX-WORD= IF drop 1 exit THEN
   dup s" variable" LEX-WORD= IF drop 1 exit THEN
   dup s" create" LEX-WORD= IF drop 1 exit THEN
   dup s" LAYOUT-BUFFER" LEX-WORD= IF drop 1 exit THEN
   drop 0 ;

: LINT-DEFINITION ( ptr u8 n n -- ) {: pa:ptr pu:n k:n :}
   k DEF-NAME-OFFSET dup 0= IF drop exit THEN
   k + dup LINT-LEX:COUNT >= IF drop exit THEN
   LINT-LEX:TOKEN 2dup PRIM? IF
      s" SHADOW " type pa pu type s" : `" type type s" ` hides a prim" type cr
      BAD @ 1+ BAD !
   ELSE 2drop THEN ;

\ package/;package toggle scope; the language rejects nesting, so one flag is
\ enough. Only global-scope tokens reach the prim-shadow check.
: LINT-TOKEN ( ptr u8 n n -- ) {: pa:ptr pu:n k:n :}
   k s" package" LEX-WORD= IF LINT-TRUE IN-PACKAGE ! exit THEN
   k s" ;package" LEX-WORD= IF LINT-FALSE IN-PACKAGE ! exit THEN
   IN-PACKAGE @ LINT-NOT IF pa pu k LINT-DEFINITION THEN ;

\ Fail-closed: an unterminated string means the lexer consumed the rest of the
\ file as string body, hiding any later definitions. Name the file and throw the
\ named code (catchable, so the gate reports a clean per-file failure) instead of
\ silently skipping real definitions.
: SL-UNTERM-FAIL ( ptr u8 n -- ) {: pa:ptr pu:n :}
   s" shadow-lint: unterminated string literal in " type pa pu type
   s"  (line " type LINT-LEX:ERROR-LINE@ . s" )" type cr
   E-SHADOW-UNTERM throw ;

\ Same fail-closed rule, different defect: an incomplete primitive-axiom row also
\ stops the scan, and the reader needs to be sent to the row opener rather than to
\ a missing quote.
: SL-REGISTRY-FAIL ( ptr u8 n -- ) {: pa:ptr pu:n :}
   s" shadow-lint: malformed primitive registry row in " type pa pu type
   s"  (line " type LINT-LEX:ERROR-LINE@ . s" )" type cr
   E-SHADOW-REGISTRY throw ;

\ The lexer reports more than one defect, so name the one it actually hit.
: SL-LEX-FAIL ( ptr u8 n -- ) {: pa:ptr pu:n :}
   LINT-LEX:ERROR-KIND@ LINT-LEX:MALFORMED-REGISTRY = IF
      pa pu SL-REGISTRY-FAIL
   ELSE
      pa pu SL-UNTERM-FAIL
   THEN ;

: LINT-SCAN ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n a:ptr u:n :}
   a u LINT-LEX:SOURCE
   LINT-LEX:ERROR? IF pa pu SL-LEX-FAIL THEN
   LINT-FALSE IN-PACKAGE !
   0 LI !
   begin LI @ LINT-LEX:COUNT < while
      pa pu LI @ LINT-TOKEN
      LI @ 1+ LI !
   repeat ;

: LINT-FILE  ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu FB SL-FB-CAP READ-FILE {: a:ptr u:n :}
   pa pu a u LINT-SCAN ;

\ prims live in habu1.f; lint every snap-toolchain file against them.
: SHADOW-LINT
   0 BAD !
   s" src/habu/habu1.f" FB SL-FB-CAP READ-FILE  TOKENIZE  SCAN-PRIMS
   s" tools/lint/text.f" LINT-FILE   s" tools/lint/token.f" LINT-FILE s" tools/lint/lib.f" LINT-FILE
   s" tools/lint/shadow-lint.f" LINT-FILE
   s" src/core/util.f"      LINT-FILE   s" src/core/cell.f"      LINT-FILE
   s" src/core/pointer-storage.f" LINT-FILE
   s" src/core/pointer-storage-effects.f" LINT-FILE
   s" src/core/engine-error.f" LINT-FILE
   s" src/core/engine-error-effects.f" LINT-FILE
   s" src/core/checker.f"   LINT-FILE
   s" src/core/lower-cert-base.f" LINT-FILE
   s" src/core/type-schema.f" LINT-FILE s" src/core/type-family.f" LINT-FILE
   s" src/core/render.f"    LINT-FILE   s" src/core/roles.f"     LINT-FILE
   s" src/core/layout-valid.f" LINT-FILE
   s" src/core/lower-cert-seal.f" LINT-FILE
   s" src/core/bytes.f"     LINT-FILE
   s" src/core/exec-vector.f" LINT-FILE s" src/core/sha256.f"    LINT-FILE
   s" src/core/type-family-sha.f" LINT-FILE
   s" src/arch/arm64/asm.f" LINT-FILE   s" src/arch/arm64/icode.f" LINT-FILE
   s" src/arch/arm64/mnem.f" LINT-FILE  s" src/os/macos/layout.f" LINT-FILE
   s" src/os/macos/sys.f"   LINT-FILE
   s" src/os/macos/repl-term.f" LINT-FILE
   s" src/os/linux/layout.f" LINT-FILE   s" src/os/linux/sys.f"   LINT-FILE
   s" src/os/env-base.f"    LINT-FILE   s" src/os/script-argv.f" LINT-FILE
   s" src/habu/bundle-argv.f" LINT-FILE
   s" src/habu/layout.f"    LINT-FILE
   s" src/os/linux/repl-term.f" LINT-FILE
   s" src/habu/treeshake.f" LINT-FILE
   s" src/habu/rt.f"        LINT-FILE   s" src/habu/crash.f"     LINT-FILE
   s" src/os/image-bytes.f" LINT-FILE
   s" src/os/macos/macho.f" LINT-FILE   s" src/os/macos/sign2.f" LINT-FILE
   s" src/os/linux/elf.f"   LINT-FILE   s" src/os/linux/sign.f"  LINT-FILE
   s" src/habu/habu1.f"     LINT-FILE   s" src/habu/prof.f"      LINT-FILE
   s" src/habu/regalloc.f"  LINT-FILE   s" src/habu/jit.f"       LINT-FILE
   s" src/habu/habu2.f"     LINT-FILE   s" src/habu/snap-lib.f"  LINT-FILE
   s" src/habu/snap.f"      LINT-FILE
   \ `1 die` alone left die's message operands to the CALLER's stack — a
   \ latent below-base read the certified-underdepth gate now rejects (dot
   \ habu-habu-certified-words-84e84eaf); die carries its own message.
   BAD @ 0 > IF  s" shadow-lint: " type BAD @ . s"  collision(s)" type cr
      s" shadow-lint: prim-name collision(s)" 1 die
   ELSE  s" shadow-lint: clean (" type PN# @ . s"  prims checked)" type cr  THEN ;
SHADOW-LINT
;package
