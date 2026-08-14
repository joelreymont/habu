\ create-axiom-test.f - what `create` is to the checker, and what every reader of
\ that answer does with it. One concern: the `PRIM: create` axiom row in
\ src/core/checker.f (dot habu-fix-the-create-75d82809).
\
\ WHAT THE ROW SAID AND WHY IT WAS WRONG. It said `-- ptr a` from 2026-06-11,
\ and the commit that wrote it said why in its own comment: "defining-word kinds
\ ... create/variable are addresses". That is the effect of the word `create`
\ DEFINES, filed in a table keyed by the token a BODY names, so the row asserted
\ that CALLING `create` pushes an address. It does not: the primitive parses a
\ name out of the input and defines it (src/habu/habu1.f BCREATE, `( "name" -- )`),
\ and the name it defines gets `-- ptr a` from its own definer at the moment it is
\ made (src/habu/habu2.f LASTC-TRUST:PUBLISH-PTR-A). Two authorities for one fact,
\ and the row was the one with no claim to it.
\
\ WHY IT SURVIVED SO LONG, WHICH IS THE PART A SUITE HAS TO COVER. Inside a body
\ that declares a signature the row is never read - src/core/checker.f DEFINER-TOK
\ models `create` directly and runs before TRY-PRIMS in DO-TOK - so the one reader
\ anybody exercises gave the right answer for the wrong reason, and every fixture
\ written against a signed body passed either way. The readers that saw the lie
\ ask the effect store BY NAME:
\
\   1. the checker's own inference, for a body with NO declared signature. That
\      body's rendered effect is recorded under its name, so `: MK create ;`
\      published `-- ptr a` and a CHECKED caller declaring one output then
\      certified against a word the machine leaves nothing behind. A certificate
\      the machine contradicts is the whole failure this file exists to keep out.
\   2. src/compiler/native/dict.f NDICT:SPELL-ARITY, which answered 0-in/1-out, so
\      the native chain refused every definer body - `dup create , 1 +`, the shape
\      src/core/enums.f ENUM+ ships - as E-NELAB-ARITY.
\
\ HOW THIS SUITE WOULD CATCH THE ROW COMING BACK. Restore `PE-PTR-A PE-OUT` on the
\ row and eight assertions red (measured, 2026-08-14): the arity pair reads 0/1,
\ the inferred effect reads 0/1, the bad caller certifies instead of being
\ refused, the honest caller is refused instead of certifying, all three chain
\ measurements come back E-NELAB-ARITY, and the arity-refusal control stops
\ answering E-NELAB-ARITY because the body it names is refused earlier.
\
\ AND WHAT KEEPS IT FROM PASSING FOR A LAZY REASON. Zeroing every storage row
\ would pass a `create`-only suite, so `here`, `allot` and `,` are measured
\ beside it and one of them reds. Reading the row as the created word's authority
\ would pass too, so a word `create` MADE is measured and keeps its `-- ptr a`.
\ And `create` written inside a string and inside a comment must stay a string and
\ a comment, so a body holding either certifies as if the token were not there.
\
\ Every verdict here comes off the production load path: the source is handed to
\ the engine, the engine compiles it, and the check hook certifies or throws -
\ which is what loading a file does to every definition in it.
\
\ WHY THE ROW ITSELF CARRIES NO PROSE, which is where a reader would look first.
\ src/core/checker.f is 524245 bytes on the master this landed against and three
\ lint tools read it whole into a $80000 (524288) fixed buffer - repo, maki-dep
\ and error-code, each dying `lint: file exceeds buffer: src/core/checker.f`
\ 44 bytes later. So the file has room for the corrected row (16 bytes shorter
\ than the wrong one) and for nothing else, and this header is where the
\ reasoning went. An empty row is not anomalous there in any case: `cr` and
\ `space` two lines above it are empty rows too. The cliff itself is a finding
\ of this lane, not a property anybody chose; tools/lint/text.f already carries
\ the runtime-sized slab those three lints want in place of a fixed buffer.
\
\ Run: bin/hb --load test/create-axiom-test.f

require lib/test.f
require src/compiler/native/migrate.f

package CREATE-AXIOM-TEST
private

18 constant REGS
0 constant LOADED               \ what `catch` answers for a source that loaded
70 constant REJECT-RC           \ src/core/check-hook.f LOWER-CERT-HOOK CHECK-RC (private there)

\ `evaluate` is the metaprogramming boundary the checker does not model, and it is
\ how this suite loads a definition the way a source file loads one. The suite
\ runs at top level, so what these publish is global - the position an ordinary
\ program's definitions occupy, and the one the inference case needs.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

TRUSTED: EV-STR ( ptr u8 n -- ptr u8 n )
   evaluate ;

\ ---- what the row says, read the way the native chain reads it ----------------
\ NDICT:SPELL-ARITY is the bridge the dot named: it resolves a spelling's active
\ effect through the checker's effect-read export API and answers the pair in
\ stack cells. Asking it is asking the row.
: ARITY-CASE ( -- )
   s" the checker's effect for `create` moves no stack cell in either direction" T-LABEL
   s" create" NDICT:SPELL-ARITY {: din:n dout:n :}
   din 0 T=  dout 0 T=

   s" while the storage words that DO move one keep saying so" T-LABEL
   s" here" NDICT:SPELL-ARITY {: hin:n hout:n :}
   hin 0 T=  hout 1 T=
   s" allot" NDICT:SPELL-ARITY {: ain:n aout:n :}
   ain 1 T=  aout 0 T=
   s" ," NDICT:SPELL-ARITY {: cin:n cout:n :}
   cin 1 T=  cout 0 T= ;

\ ---- and the word `create` MAKES is unaffected, because the row was never its
\ ---- authority. The definer publishes `-- ptr a` against the new name as it
\ ---- emits the body, so emptying the definer's own row leaves this untouched.
: MADE-WORD ( -- )
   s" create CAX-DAT 8 allot" EV ;

: MADE-USE ( -- )
   s" : CAX-USE ( -- n ) CAX-DAT @ ;" EV ;

: MADE-CASE ( -- )
   s" a word `create` made still pushes one address" T-LABEL
   [: MADE-WORD ;] LOADED TTHROWSQ
   s" CAX-DAT" NDICT:SPELL-ARITY {: din:n dout:n :}
   din 0 T=  dout 1 T=

   s" and a checked body may still take it as one" T-LABEL
   [: MADE-USE ;] LOADED TTHROWSQ
   s" 41 CAX-DAT !  CAX-USE" EV-N 41 T= ;

\ ---- the load path: bodies that must load, and bodies that must not -----------
: LOAD-EMPTY ( -- )      s" : CAX-B1 ( -- ) create ;" EV ;
: LOAD-PTR-A ( -- )      s" : CAX-B2 ( -- ptr a ) create ;" EV ;
: LOAD-N ( -- )          s" : CAX-B3 ( -- n ) create ;" EV ;
: LOAD-ENUM-HALF ( -- )  s" : CAX-B4 ( n -- n ) dup create , 1 + ;" EV ;
: LOAD-IN-STRING ( -- )  s\" : CAX-B5 ( -- ptr u8 n ) s\q create\q ;" EV ;
: LOAD-IN-COMMENT ( -- ) s" : CAX-B6 ( -- ) ( create ) ;" EV ;
: LOAD-HERE ( -- )       s" : CAX-B7 ( -- ptr a ) here ;" EV ;

: BODY-CASE ( -- )
   s" a body whose whole content is `create` declares nothing and loads" T-LABEL
   [: LOAD-EMPTY ;] LOADED TTHROWSQ

   s" and the machine agrees: running it leaves the cell under it on top" T-LABEL
   s" 99 CAX-B1 CAX-B1-NAME" EV-N 99 T=
   s" CAX-B1-NAME" EV-N 0 T<>

   s" declaring the address it used to claim is refused at the token" T-LABEL
   [: LOAD-PTR-A ;] REJECT-RC TTHROWSQ

   s" and so is any other output - the row states an effect, not a wildcard" T-LABEL
   [: LOAD-N ;] REJECT-RC TTHROWSQ

   s" the definer half src/core/enums.f ENUM+ ships balances" T-LABEL
   [: LOAD-ENUM-HALF ;] LOADED TTHROWSQ

   s" and running it defines the name and stores the counter in it" T-LABEL
   s" 5 CAX-B4 CAX-B4-CELL" EV-N 6 T=
   s" CAX-B4-CELL @" EV-N 5 T=

   s" `create` inside a string is a string" T-LABEL
   [: LOAD-IN-STRING ;] LOADED TTHROWSQ
   s" CAX-B5" EV-STR s" create" T$=

   s" `create` inside a comment is a comment" T-LABEL
   [: LOAD-IN-COMMENT ;] LOADED TTHROWSQ
   s" CAX-B6" NDICT:SPELL-ARITY {: b6in:n b6out:n :}
   b6in 0 T=  b6out 0 T=

   s" and `here`, which really does push an address, still declares one" T-LABEL
   [: LOAD-HERE ;] LOADED TTHROWSQ
   s" CAX-B7" NDICT:SPELL-ARITY {: b7in:n b7out:n :}
   b7in 0 T=  b7out 1 T= ;

\ ---- the leg DEFINER-TOK does not shadow, and the miscompile it admitted ------
\ A body with no declared signature reaches the primitive row, and what the walk
\ infers for it is recorded under its name for every later caller. This is the
\ case that certified a stack cell the machine never pushes.
: INFER-MK ( -- )       s" : CAX-MK create ;" EV ;
: INFER-BAD ( -- )      s" : CAX-C1 ( -- ptr a ) CAX-MK ;" EV ;
: INFER-GOOD ( -- )     s" : CAX-C2 ( -- ) CAX-MK ;" EV ;

: INFER-CASE ( -- )
   s" an unsigned body calling `create` records what it really leaves" T-LABEL
   [: INFER-MK ;] LOADED TTHROWSQ
   s" CAX-MK" NDICT:SPELL-ARITY {: mkin:n mkout:n :}
   mkin 0 T=  mkout 0 T=

   s" so a caller claiming a cell from it is refused, not certified" T-LABEL
   [: INFER-BAD ;] REJECT-RC TTHROWSQ

   s" and the caller that claims nothing certifies and runs" T-LABEL
   [: INFER-GOOD ;] LOADED TTHROWSQ
   s" CAX-C2 CAX-C2-CELL 8 allot  7 CAX-C2-CELL !  CAX-C2-CELL @" EV-N 7 T= ;

\ ---- the reader the dot named: the native chain compiling a definer body ------
\ MEASURE-HELD runs every stage a held migration runs and keeps none of it, so a
\ measurement is the chain's own verdict on whether it could compile the body.
: MEASURE ( ptr u8 n n n -- ) {: a:ptr u:n din:n dout:n :}
   a u din dout REGS NMIGRATE:MEASURE-HELD ;

: CHAIN-ALONE ( -- )
   s" : CAX-D1 ( -- ) create ;" 0 0 MEASURE ;

: CHAIN-ENUM-HALF ( -- )
   s" : CAX-D2 ( n -- n ) dup create , 1 + ;" 1 1 MEASURE ;

\ `create allot` was lib/string.f BUFFER:'s definer half until that definer was
\ converted to a generated colon accessor (dot habu-the-reader-re-a65e56e5). The
\ shape is kept here because the row it measures is about `create` moving no stack
\ cell, and a body that allots after it is the sharpest form of that question - not
\ because the tree still ships one.
: CHAIN-BUFFER-HALF ( -- )
   s" : CAX-D3 ( n -- ) create allot ;" 1 0 MEASURE ;

: CHAIN-WRONG-OUT ( -- )
   s" : CAX-D4 ( n -- n ) dup create , 1 + ;" 1 2 MEASURE ;

: CHAIN-CASE ( -- )
   s" the chain compiles a body that only calls `create`" T-LABEL
   NELAB:REFUSED-RESET
   [: CHAIN-ALONE ;] LOADED TTHROWSQ
   NELAB:REFUSED-ROW -1 T=

   s" the definer half the tree still ships, and the one it used to" T-LABEL
   [: CHAIN-ENUM-HALF ;] LOADED TTHROWSQ
   [: CHAIN-BUFFER-HALF ;] LOADED TTHROWSQ

   s" the arity refusal those three used to take is still reachable" T-LABEL
   [: CHAIN-WRONG-OUT ;] E-NELAB-ARITY TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   ARITY-CASE
   MADE-CASE
   BODY-CASE
   CHAIN-CASE
   INFER-CASE
   T-REPORT ;

;package

CREATE-AXIOM-TEST:RUN
