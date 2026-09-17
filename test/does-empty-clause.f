\ does-empty-clause.f - a does> clause that compiles nothing leaves the words
\ its definer creates exactly as `create` left them.
\
\ WHAT THIS PROVES. `does>` runs its clause AFTER the created word has pushed
\ its data address, so a clause with no body does nothing to that address: such
\ a definer is asking for a type, not for a behaviour. Both compilers now say
\ so - the created word keeps the RET `create` emitted and the DKIND:ADDR stamp
\ a mention folds through (src/habu/habu2.f DOES-REC:ELIDE-EMPTY, and
\ src/compiler/native/elaborate.f STAGE-DOES-ENTRY for the tier the engine's own
\ build runs at) - so reading one of its words costs what reading a bare
\ `create`d cell costs and not a call, a branch and a frame more. The pointer
\ definers of src/core/pointer-storage.f are why: their clause bodies were
\ 5.9 percent of a tier-0 profile's samples before this.
\
\ WHAT IT DOES NOT LET PASS. The elision is keyed on the clause compiling NO
\ instruction, not on what it declares or on what its body looks like. A clause
\ whose body is the IDENTITY on the address it is handed - `0 ptr-field`, the
\ very thing these definers used to spell - is still patched and still loses the
\ stamp, so nothing here can be mistaken for a rule about pointer types. The
\ declared effect is published either way, which the refusal case asserts.
\
\ EVERY CASE RUNS AT BOTH TIERS, under its own names, because the two compilers
\ reach this by different routes: tier 0 writes over the `adr x10, D` its own
\ opener emitted, tier 1 stages a zero clause entry into the `does-patch` call.
\ A tier that stopped eliding would fail here and not in a benchmark.
\
\ Run: bin/hb --load test/does-empty-clause.f

require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/xref.f
require src/compiler/native/dict.f
require src/compiler/native/compiler.f

package DEC-TEST
private

\ `evaluate` is the metaprogramming boundary the checker does not model, and the
\ only way a test can watch a definition be refused. `set-tier` is refused inside
\ a plain checked body for the same reason tools/tier-census.f wraps it.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;
TRUSTED: SELECT-TIER ( n -- ) set-tier ;

$D65F03C0 constant W-RET
$FC000000 constant OPC-MASK
$14000000 constant OPC-B

\ The last instruction word of a name's compiled body: the RET `create` emitted,
\ or the branch a does> clause put in its place. XREF-N>U8 is the engine's own
\ byte view of a code address, the cast test/does-clause-record.f decodes with.
: W32@ ( n -- n ) {: a:n :}
   a XREF-N>U8 {: p:ptr :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

\ A name this file compiled and cannot find is a broken test, not a red case.
: LAST-INSN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u XREF-FIND-INDEX {: ix:n :}
   ix 0 <= if s" does-empty-clause: subject not in the dictionary" 76 die then
   ix XREF-REC XREF-START  ix XREF-REC XREF-CODE-BYTES +  4 -  W32@ ;

64 constant NAME-CAP
5 constant SUF-LEN                                     \ ";does"
create WANT NAME-CAP allot
variable WANT-U

\ The clause's derived name: its parent's, plus `;does`, as the opener builds it.
: WANT! ( ptr u8 n -- ) {: a:ptr u:n :}
   u SUF-LEN + NAME-CAP > if s" does-empty-clause: name buffer too small" 76 die then
   a WANT u BYTE-COPY
   s" ;does" {: sa:ptr su:n :}
   sa  WANT u +  su BYTE-COPY
   u SUF-LEN + WANT-U ! ;

\ ---- the two answers a created word can give --------------------------------
: ?CREATE-BODY ( ptr u8 n -- ) {: a:ptr u:n :}
   s" an empty clause leaves the definer stamp a mention folds through" T-LABEL
   a u NDICT:SPELL-FIXED  NDICT:FIXED-ADDR T=
   s" ... and leaves the RET `create` emitted" T-LABEL
   a u LAST-INSN  W-RET T= ;

: ?CLAUSE-BODY ( ptr u8 n -- ) {: a:ptr u:n :}
   s" a clause with a body takes the definer stamp away" T-LABEL
   a u NDICT:SPELL-FIXED  NDICT:FIXED-NONE T=
   s" ... and puts a branch where the RET was" T-LABEL
   a u LAST-INSN OPC-MASK and  OPC-B T= ;

\ Eliding the branch does not elide the record. Every does> clause gets one, so
\ that the branch a patched clause plants aims at a record ENTRY an AOT capture
\ can name (test/does-clause-record.f); an elided clause keeps its because
\ removing it would mean unwinding CP and the derived name at `;`.
: ?CLAUSE-RECORD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u WANT!
   s" an elided clause still publishes its record, one slot above its parent" T-LABEL
   WANT WANT-U @ XREF-FIND-INDEX  a u XREF-FIND-INDEX 1+ T= ;

public

\ The subjects are public because a created word only answers NDICT:SPELL-FIXED
\ through the spelling the dictionary carries, and a package's private names
\ carry none.
\ ---- tier 0: the JIT every --load and the REPL run --------------------------
: DEC-EMPTY0 ( -- ) create 0 , does> ( -- ptr ptr a ) ;
: DEC-MARK0  ( -- ) create here ptr-cell-mark 0 , does> ( -- ptr ptr a ) ;
: DEC-BODY0  ( -- ) create 0 , does> ( -- ptr ptr a ) 0 ptr-field ;

DEC-EMPTY0 DEC-CELL0
DEC-MARK0  DEC-MCELL0
DEC-BODY0  DEC-BCELL0
create DEC-BARE0 16 allot

: DEC-ROUND0 ( -- bool )
   DEC-BARE0 DEC-CELL0 !
   DEC-BARE0 DEC-MCELL0 !
   DEC-BARE0 DEC-BCELL0 !
   DEC-CELL0 @ DEC-BARE0 =
   DEC-MCELL0 @ DEC-BARE0 = and
   DEC-BCELL0 @ DEC-BARE0 = and ;

1 SELECT-TIER

\ ---- tier 1: the optimizing compiler the engine's own build runs at ---------
: DEC-EMPTY1 ( -- ) create 0 , does> ( -- ptr ptr a ) ;
: DEC-MARK1  ( -- ) create here ptr-cell-mark 0 , does> ( -- ptr ptr a ) ;
: DEC-BODY1  ( -- ) create 0 , does> ( -- ptr ptr a ) 0 ptr-field ;

DEC-EMPTY1 DEC-CELL1
DEC-MARK1  DEC-MCELL1
DEC-BODY1  DEC-BCELL1
create DEC-BARE1 16 allot

: DEC-ROUND1 ( -- bool )
   DEC-BARE1 DEC-CELL1 !
   DEC-BARE1 DEC-MCELL1 !
   DEC-BARE1 DEC-BCELL1 !
   DEC-CELL1 @ DEC-BARE1 =
   DEC-MCELL1 @ DEC-BARE1 = and
   DEC-BCELL1 @ DEC-BARE1 = and ;

\ A created word carries the effect its clause declared whether or not the
\ clause survived, so `-- ptr ptr a` is still refused where `-- ptr n` is asked.
: DEC-REFUSED? ( -- bool )
   [: s" : DEC-MISUSE ( -- ptr n ) DEC-TEST:DEC-CELL1 ;" EV ;] catch 0<> ;

: RUN ( -- )
   T-RESET

   s" DEC-TEST:DEC-CELL0" ?CREATE-BODY
   s" DEC-TEST:DEC-MCELL0" ?CREATE-BODY
   s" DEC-TEST:DEC-BCELL0" ?CLAUSE-BODY
   s" DEC-TEST:DEC-CELL1" ?CREATE-BODY
   s" DEC-TEST:DEC-MCELL1" ?CREATE-BODY
   s" DEC-TEST:DEC-BCELL1" ?CLAUSE-BODY

   s" DEC-TEST:DEC-EMPTY0" ?CLAUSE-RECORD
   s" DEC-TEST:DEC-EMPTY1" ?CLAUSE-RECORD

   s" a bare created cell answers the same as an elided definer's word" T-LABEL
   s" DEC-TEST:DEC-BARE0" NDICT:SPELL-FIXED  NDICT:FIXED-ADDR T=

   s" tier 0: every created word still stores and reads its pointer" T-LABEL
   DEC-ROUND0 TTRUE
   s" tier 1: every created word still stores and reads its pointer" T-LABEL
   DEC-ROUND1 TTRUE

   s" the declared effect survives the elision" T-LABEL
   DEC-REFUSED? TTRUE

   T-REPORT
   s" does-empty-clause: ok" type cr ;

;package

DEC-TEST:RUN
