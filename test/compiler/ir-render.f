\ ir-render.f - checked deterministic renderer tests.
\
\ Proves the sections 5.6 and 6.6 contract of src/compiler/ir/render.f: a frozen
\ module's diagnostic text says exactly what the module holds, it depends on what
\ the module MEANS and not on the order its tables were interned in, and
\ producing it changes nothing.
\
\ THE GOLDEN. GOLDEN-CASE writes down the whole rendered text of the shared
\ fixture module, line by line, and requires the renderer to produce it byte for
\ byte. That is what makes this a golden rather than a self-consistency check:
\ the test says what the render IS, so any change to a spelling, a field order, a
\ reference letter or a canonical ordinal has to be made here on purpose. Reading
\ it top to bottom is also the readable description of the format.
\
\ THE DETERMINISM FIXTURE. EQUIV-CASE renders the same module built along two
\ topological insertion orders and requires the two texts to be equal. It first
\ measures that the reversal really moved the insertion ordinals, because a
\ fixture that reversed nothing would pass for the wrong reason. The module comes
\ from test/compiler/ir-module-fixture.f, which test/compiler/ir-canon.f uses for
\ the same purpose.
\
\ THE READ-ONLY FIXTURE. READONLY-CASE encodes the module to canonical bytes,
\ renders it, encodes it again, and requires the two frames to be identical, then
\ renders a second time and requires the two texts to be identical. So the
\ renderer is proved not to disturb the module or its canonical table through the
\ encoder, which is an authority that would notice.
\
\ THE REST are the refusals a checked caller can reach: a table that numbers
\ another module, a span one byte too short, a module whose context has torn
\ down, and a name wider than the committed working set - which IR-CANON refuses
\ first, because canonicalization has to succeed before there is anything to
\ render, and its ceilings are the renderer's.

require lib/test.f
require src/compiler/ir/render.f
require src/compiler/ir/encode.f
require test/compiler/ir-module-fixture.f

package IR-RENDER-TEST
private

$8000 constant TEXT-CAP
$8000 constant FRAME-CAP

create TA TEXT-CAP allot              \ the text under test
create TB TEXT-CAP allot              \ a second text to compare it with
create EX TEXT-CAP allot              \ the expected text, built line by line
create FA FRAME-CAP allot             \ the canonical frame before rendering
create FB FRAME-CAP allot             \ and after it

variable EXU

\ ---- the expected text -------------------------------------------------------
: EX-RESET ( -- )
   0 EXU ! ;

: EX-BYTE ( n -- )
   {: b:n :}
   EXU @ TEXT-CAP >= if E-IR-RENDER-ROOM throw then
   b EX EXU @ + c!
   EXU @ 1+ EXU ! ;

: LINE ( ptr u8 n -- )
   {: p:ptr u:n :}
   u 0 ?do
      p i + c@ EX-BYTE
   loop
   $0A EX-BYTE ;

: EX$ ( -- ptr u8 n )
   EX EXU @ ;

\ ---- rendering one build of the fixture --------------------------------------
: TABLE-OF ( IR-CTX:ctx IR-BUILD:module -- IR-CANON:table )
   IR-CANON:CANON ;

: RENDER-INTO ( IR-CTX:ctx n n ptr u8 n -- n )
   {: c:IR-CTX:ctx rev:n sw:n p:ptr cap:n :}
   c rev sw IR-FIXTURE:MODULE-OF {: m:IR-BUILD:module :}
   c m TABLE-OF {: t:IR-CANON:table :}
   m t p cap IR-RENDER:RENDER ;

: BYTES-EQ? ( ptr u8 n ptr u8 n -- bool )
   {: p:ptr pu:n q:ptr qu:n :}
   pu qu <> if false exit then
   pu 0 ?do
      p i + c@ q i + c@ <> if false unloop exit then
   loop
   true ;

\ ---- the golden --------------------------------------------------------------
\ The whole rendered text of the fixture module, stated here on purpose. The
\ digests are SHA-256 over the fixture's two registered source strings.
: EX-HEAD ( -- )
   s\" module dialect s1 \"hir\" schema 1 0" LINE
   s" symbols 11" LINE
 ;

: EX-SYMS ( -- )
   s\" sym s0 \"a-tag\"" LINE
   s\" sym s1 \"hir\"" LINE
   s\" sym s2 \"hir.br1\"" LINE
   s\" sym s3 \"hir.const\"" LINE
   s\" sym s4 \"hir.ret\"" LINE
   s\" sym s5 \"hir.tagged\"" LINE
   s\" sym s6 \"hir.use\"" LINE
   s\" sym s7 \"main\"" LINE
   s\" sym s8 \"render.hir\"" LINE
   s\" sym s9 \"rule.hir\"" LINE
   s\" sym s10 \"z-tag\"" LINE
 ;

: EX-TYPES ( -- )
   s" types 5" LINE
   s" type t0 i8" LINE
   s" type t1 i16" LINE
   s" type t2 i64" LINE
   s" type t3 ptr<generic,i8>" LINE
   s" type t4 ( i64 -- i64 )" LINE
 ;

: EX-ATTRS ( -- )
   s" attrs 6" LINE
   s" attr a0 int(-3)" LINE
   s" attr a1 int(7)" LINE
   s\" attr a2 \"alpha\"" LINE
   s" attr a3 sym s10" LINE
   s" attr a4 type t0" LINE
   s" attr a5 rec(s0=a0 s10=a1)" LINE
 ;

: EX-SRCS ( -- )
   s" sources 2" LINE
   s" source c0 root len 10 depth 0 digest 9ea37bca2131e2bbd3602aff8f57008e8829434812ba6106dd7a6d27481df551" LINE
   s" source c1 root len 17 depth 0 digest 2baf556d2d93f7044ae19beb0523ef15603053c9b6f48748ae9d0631b3720a11" LINE
 ;

: EX-PROGRAM ( -- )
   s" functions 1" LINE
   s" fun f0 name s7 sig t4 defined exported habu attrs a5 a2 blocks 2 span c1 2 5" LINE
   s" blocks 2" LINE
   s" block b0 in f0 args ops 3 span c0 0 4" LINE
   s" block b1 in f0 args v2 ops 2 span c0 0 4" LINE
   s" ops 5" LINE
   s" op o0 code s3 operands results v0 successors attrs span c0 0 4" LINE
   s" op o1 code s5 operands results v1 successors attrs s0=a0 s10=a1 span c0 0 4" LINE
   s" op o2 code s2 operands v0 results successors b1 attrs span c0 0 4" LINE
   s" op o3 code s6 operands v2 results successors attrs span c0 0 4" LINE
   s" op o4 code s4 operands results successors attrs span c0 0 4" LINE
 ;

: EX-VALUES ( -- )
   s" values 3" LINE
   s" value v0 result o0 pos 0 type t2" LINE
   s" value v1 result o1 pos 0 type t2" LINE
   s" value v2 arg b1 index 0 type t2" LINE
 ;

: EX-EDGES ( -- )
   s" edges 2" LINE
   s" edge b0 succs 1 preds" LINE
   s" edge b1 succs 0 preds b0" LINE
 ;

: EX-ALL ( -- )
   EX-RESET
   EX-HEAD
   EX-SYMS
   EX-TYPES
   EX-ATTRS
   EX-SRCS
   EX-PROGRAM
   EX-VALUES
   EX-EDGES ;


\ ---- the golden case ---------------------------------------------------------
: GOLDEN-BODY ( IR-CTX:ctx -- ptr u8 n ptr u8 n )
   {: c:IR-CTX:ctx :}
   EX-ALL
   c 0 0 TA TEXT-CAP RENDER-INTO {: u:n :}
   TA u
   EX$ ;

: GOLDEN-CASE ( -- )
   s" the rendered module is exactly the golden text" T-LABEL
   IR-FIXTURE:BND [: GOLDEN-BODY ;] IR-CTX:WITH-CONTEXT
   T$= ;

\ ---- determinism under two insertion orders ----------------------------------
\ The first two answers measure that reversing the build really moved the
\ insertion ordinals, so the equality below cannot pass for the wrong reason.
: EQUIV-BODY ( IR-CTX:ctx -- n n bool bool )
   {: c:IR-CTX:ctx :}
   c IR-FIXTURE:MK {: ba:IR-BUILD:builder :}
   c ba 0 0 IR-FIXTURE:BUILD
   c ba IR-FIXTURE:I8 IR-ID:TYPE-LOCAL {: i8a:n :}
   c ba IR-FIXTURE:A-TAG IR-ID:SYMBOL-LOCAL {: taga:n :}
   c ba IR-BUILD:FREEZE {: ma:IR-BUILD:module :}
   c IR-FIXTURE:MK {: bb:IR-BUILD:builder :}
   c bb 1 0 IR-FIXTURE:BUILD
   c bb IR-FIXTURE:I8 IR-ID:TYPE-LOCAL {: i8b:n :}
   c bb IR-FIXTURE:A-TAG IR-ID:SYMBOL-LOCAL {: tagb:n :}
   c bb IR-BUILD:FREEZE {: mb:IR-BUILD:module :}
   c ma TABLE-OF {: tabla:IR-CANON:table :}
   c mb TABLE-OF {: tablb:IR-CANON:table :}
   ma tabla TA TEXT-CAP IR-RENDER:RENDER {: ua:n :}
   mb tablb TB TEXT-CAP IR-RENDER:RENDER {: ub:n :}
   i8a i8b -
   taga tagb -
   ua 0 >
   TA ua TB ub BYTES-EQ? ;

: EQUIV-CASE ( -- )
   s" two topological build orders render to the same text" T-LABEL
   IR-FIXTURE:BND [: EQUIV-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T<> 0 T<> ;

\ ---- rendering changes nothing -----------------------------------------------
: FRAME-INTO ( IR-CTX:ctx IR-BUILD:module IR-CANON:table ptr u8 n -- n )
   {: c:IR-CTX:ctx m:IR-BUILD:module t:IR-CANON:table p:ptr cap:n :}
   c m t p cap IR-ENCODE:ENCODE ;

: READONLY-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: m:IR-BUILD:module :}
   c m TABLE-OF {: t:IR-CANON:table :}
   c m t FA FRAME-CAP FRAME-INTO {: framea:n :}
   m t TA TEXT-CAP IR-RENDER:RENDER {: texta:n :}
   c m t FB FRAME-CAP FRAME-INTO {: frameb:n :}
   m t TB TEXT-CAP IR-RENDER:RENDER {: textb:n :}
   FA framea FB frameb BYTES-EQ?
   TA texta TB textb BYTES-EQ? ;

: READONLY-CASE ( -- )
   s" rendering leaves the module and a second render identical" T-LABEL
   IR-FIXTURE:BND [: READONLY-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE ;

\ ---- one row's spelling ------------------------------------------------------
\ What the structural diff names a differing row with.
: ITEM-BODY ( IR-CTX:ctx -- ptr u8 n ptr u8 n )
   {: c:IR-CTX:ctx :}
   c IR-FIXTURE:MK {: b:IR-BUILD:builder :}
   c b 0 0 IR-FIXTURE:BUILD
   c b IR-FIXTURE:PTR8 {: ty:IR-ID:ir-type-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m ty TA TEXT-CAP IR-RENDER:TYPE-TEXT {: u:n :}
   TA u
   s" ptr<generic,i8>" ;

: ITEM-CASE ( -- )
   s" one type row spells its structure for the diff to name it" T-LABEL
   IR-FIXTURE:BND [: ITEM-BODY ;] IR-CTX:WITH-CONTEXT
   T$= ;

\ ---- refusals ----------------------------------------------------------------
\ A canonical table that numbers another module. Two modules of one context have
\ different module keys, so the pairing check refuses before a byte is written.
: PAIR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: ma:IR-BUILD:module :}
   c 0 0 IR-FIXTURE:MODULE-OF {: mb:IR-BUILD:module :}
   c mb TABLE-OF {: tablb:IR-CANON:table :}
   ma tablb TA TEXT-CAP IR-RENDER:RENDER drop ;

: PAIR-RUN ( -- )
   IR-FIXTURE:BND [: PAIR-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A span one byte shorter than the text needs.
: ROOM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 TA TEXT-CAP RENDER-INTO {: u:n :}
   c 0 0 TB u 1- RENDER-INTO drop ;

: ROOM-RUN ( -- )
   IR-FIXTURE:BND [: ROOM-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A module whose own context has torn down: its tables are unmapped, so it is no
\ longer a frozen module anything may read.
: INNER-MODULE ( IR-CTX:ctx -- IR-BUILD:module )
   0 0 IR-FIXTURE:MODULE-OF ;

: STALE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: live:IR-BUILD:module :}
   c live TABLE-OF {: t:IR-CANON:table :}
   IR-FIXTURE:BND [: INNER-MODULE ;] IR-CTX:WITH-CONTEXT {: dead:IR-BUILD:module :}
   dead t TA TEXT-CAP IR-RENDER:RENDER drop ;

: STALE-RUN ( -- )
   IR-FIXTURE:BND [: STALE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A name wider than the committed working set. The renderer and the
\ canonicalizer commit to the same ceilings and canonicalization has to succeed
\ before there is anything to render, so this is IR-CANON's refusal on the
\ renderer's own path.
create BIG-NAME 300 allot

: BIG-NAME-FILL ( -- )
   300 0 ?do
      $61 BIG-NAME i + c!
   loop ;

: CAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-FIXTURE:MK {: b:IR-BUILD:builder :}
   c b 0 0 IR-FIXTURE:BUILD
   BIG-NAME-FILL
   c b BIG-NAME 300 IR-BUILD:INTERN-SYMBOL drop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   c m TABLE-OF drop ;

: CAP-RUN ( -- )
   IR-FIXTURE:BND [: CAP-BODY ;] IR-CTX:WITH-CONTEXT ;

: PAIR-CASE ( -- )
   s" a canonical table that numbers another module rejects" T-LABEL
   [: PAIR-RUN ;] E-IR-CANON-OWNER TTHROWSQ ;

: ROOM-CASE ( -- )
   s" a span one byte short of the text rejects" T-LABEL
   [: ROOM-RUN ;] E-IR-RENDER-ROOM TTHROWSQ ;

: STALE-CASE ( -- )
   s" a module whose context has torn down rejects" T-LABEL
   [: STALE-RUN ;] E-IR-RENDER-STALE TTHROWSQ ;

: CAP-CASE ( -- )
   s" a name wider than the committed working set rejects" T-LABEL
   [: CAP-RUN ;] E-IR-CANON-CAP TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   IR-FIXTURE:RESET
   IR-FIXTURE:BND [: drop GOLDEN-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop EQUIV-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop READONLY-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop ITEM-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop PAIR-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop ROOM-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop STALE-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop CAP-CASE ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

IR-RENDER-TEST:RUN
