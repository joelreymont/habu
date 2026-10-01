\ cast-suite.f - positive behavior contract for the CAST: checked retype declarer
\ (the engine's `cast:` reader keyword plus checker.f CHECKER-DEFCAST). Run BY
\ THE ENGINE over stdin, like test/deftype-suite.f:
\     bin/hb < test/cast-suite.f
\ Registered directly in the native suite.
\
\ CAST: is the converter form that ends per-declaration TRUSTED growth: the
\ checker proves the declared retype legal by its six structural refusals and
\ publishes ( in -- out ), so the retype is CHECKED, not trusted. This suite pins:
\   - a cast retypes n <-> an arity-0 family scalar, both directions
\   - the value passes through UNCHANGED at runtime (identity data flow)
\   - a guarded conversion (a checked word plus a cast) throws out of range and
\     passes in-range values through
\   - a parametric cell family round-trips, and its generic projection
\     ( family<e> -- n ) certifies both generically and at a concrete instance
\   - a checked caller certifies against the published ( in -- out ) row, and the
\     published output is genuinely nominal (demanding a plain n there rejects)
\   - a cast into a layout instance whose argument is a con, or a family its
\     package owns, certifies, and the field reads back the value
\   - a private pointer or quotation mint, bare or as a layout instance's
\     argument, turns an integer address or execution token into a typed one
\     that its package's words read, store and execute
\   - a private pointer view of a layout row is raw: a forged tag stored through
\     it is refused at the next fetch
\ A failure prints F<index> + detail; REPORT exits 1 on any fail.

require test/checker-assert.f
require lib/test/subject.f

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

\ silence expected rejection diagnostics (verdicts are asserted, not printed).
create CSDIAG-BUF 8192 allot
CSDIAG-BUF 8192 DIAG-BUFFER!

9001 constant E-CS-RANGE

\ two arity-0 family scalars and one parametric cell family.
NEWTYPE csrole 0
NEWTYPE csbnd 0
NEWTYPE csix 1

\ 1. empty-body cast: n <-> arity-0 family scalar, both directions.
CAST: >CSROLE ( n -- csrole )
CAST: CSROLE>N ( csrole -- n )

\ 2. a guarded conversion is TWO things now — the retype, and the checked word
\    that refuses an out-of-range value before applying it. The runtime cases
\    below are unchanged, which is the point: the guard still throws and the
\    in-range value still passes through untouched.
CAST: N>CSBND ( n -- csbnd )
package CS-BND
public
: >CSBND ( n -- csbnd ) dup 0 < over 128 >= or if E-CS-RANGE throw then N>CSBND ;
;package
CAST: CSBND>N ( csbnd -- n )

\ 3. parametric cell family: its type argument is phantom, so the generic
\    projection is structurally non-owning.
CAST: >CSIXN ( n -- csix<n> )
CAST: CSIX>N ( csix<e> -- n )

\ --- runtime: the value passes through unchanged (identity data flow). --------
5 >CSROLE CSROLE>N 5 T=
0 >CSROLE CSROLE>N 0 T=

\ --- runtime: the guard passes in-range values and throws out of range. -------
9 CS-BND:>CSBND CSBND>N 9 T=
127 CS-BND:>CSBND CSBND>N 127 T=
variable CS-RC
package CS-OOR
public
: HI ( -- ) 200 CS-BND:>CSBND CSBND>N drop ;
: LO ( -- ) -1 CS-BND:>CSBND CSBND>N drop ;
;package
' CS-OOR:HI catch CS-RC ! CS-RC @ E-CS-RANGE T=
' CS-OOR:LO catch CS-RC ! CS-RC @ E-CS-RANGE T=

\ --- runtime: parametric round-trip through the projection. -------------------
7 >CSIXN CSIX>N 7 T=

\ --- checked callers certify against the published ( in -- out ) rows. --------
\ round trip both directions certifies on n.
s" CC-RT ( n -- n ) >CSROLE CSROLE>N"        CHECK-QUIET-CANDIDATE! -1 T=
\ the caller sees the nominal output row.
s" CC-OUT ( n -- csrole ) >CSROLE"           CHECK-QUIET-CANDIDATE! -1 T=
\ and the published output is genuinely the family: demanding n there rejects.
s" CC-WRONG ( n -- n ) >CSROLE"              CHECK-QUIET-CANDIDATE!  0 T=
\ the projection certifies generically and at a concrete instance.
s" CC-PROJ ( csix<e> -- n ) CSIX>N"          CHECK-QUIET-CANDIDATE! -1 T=
s" CC-PROJ-N ( csix<n> -- n ) CSIX>N"        CHECK-QUIET-CANDIDATE! -1 T=

\ --- a layout instance introduces its arguments. ------------------------------
\ Projecting a field yields a value of the argument family, so a cast into a
\ layout instance needs the argument family's owner, as a cast into the family
\ does. A package's own family certifies inside a global layout, a con argument
\ certifies anywhere, and the field reads the integer back.
STRUCTURE csbox 1 FIELD value a ;STRUCTURE
CAST: >N-BOX ( n -- csbox<n> )
: N-ROUND ( n -- n ) >N-BOX CSBOX:UNMAKE ;
package CS-BOX
public
NEWTYPE own 0
CAST: >OWN-BOX ( n -- csbox<own> )
CAST: OWN>N ( own -- n )
: OWN-ROUND ( n -- n ) >OWN-BOX CSBOX:UNMAKE OWN>N ;
;package
6 N-ROUND 6 T=
5 CS-BOX:OWN-ROUND 5 T=

\ --- the declaration publishes a REAL word, not just a checker row. -----------
\ A cast the checker knows but the dictionary does not would certify every caller
\ in this file and then die at run time on the first call, so findable and
\ callable are asserted apart: the record is in the live wordlist, it has an
\ execution token, and a COMPILED caller reaches it and gets its value back.
package CS-LINK
s" >CSROLE" 0 search-wl 0= 0 T=          \ declared at global scope, so wordlist 0 owns it
s" CSROLE>N" 0 search-wl 0= 0 T=
' >CSROLE 0 <> -1 T=
' CSROLE>N 0 <> -1 T=
: CALL-COMPILED ( n -- n ) >CSROLE CSROLE>N ;
11 CALL-COMPILED 11 T=
;package

\ --- a call to a cast emits ZERO instructions. -------------------------------
\ Not a memory of a measurement: the code pointer is read around three
\ definitions that differ only in how many casts they call, and the three spans
\ must be equal. Only the CAST: definer's identity stamp permits this lowering;
\ the callable body's instruction shape is not a compiler contract.
package CS-BYTES
cp@ constant B0
: NOCAST ( n -- n ) ;
cp@ constant B1
: ONECAST ( n -- n ) >CSROLE CSROLE>N ;
cp@ constant B2
: THREECAST ( n -- n ) >CSROLE CSROLE>N >CSROLE CSROLE>N >CSROLE CSROLE>N ;
cp@ constant B3
B2 B1 -  B1 B0 -  T=
B3 B2 -  B1 B0 -  T=

\ A retype cannot force a literal out of the JIT's virtual stack. Equal spans
\ here also pin constant folding across the two declared casts.
cp@ constant K0
: BASE-LITERAL ( -- n ) 7 2 + ;
cp@ constant K1
: CAST-LITERAL ( -- n ) 7 >CSROLE CSROLE>N 2 + ;
cp@ constant K2
K2 K1 - K1 K0 - T=
CAST-LITERAL 9 T=

\ Core role casts crossed the native build's dictionary capture. Their kind
\ must survive exactly as a newly declared cast's kind does.
cp@ constant R0
: CORE-ROLE ( n -- n ) >IDX IDX>N ;
cp@ constant R1
R1 R0 - B1 B0 - T=
37 CORE-ROLE 37 T=

\ An ordinary empty body still denotes a call, despite having the same scalar
\ effect and machine-code shape as a cast's first-class body.
: ORDINARY ( n -- n ) ;
cp@ constant O0
: CALL-ORDINARY ( n -- n ) ORDINARY ORDINARY ;
cp@ constant O1
O1 O0 - B1 B0 - > -1 T=
23 CALL-ORDINARY 23 T=
;package

package CS-EXECUTION
public
: FIRST-CLASS ( n -- n ) ['] >CSROLE execute ['] CSROLE>N execute ;
: QUOTED ( n -- n ) [: >CSROLE CSROLE>N ;] execute ;
: LOCAL ( n -- n ) {: >CSROLE:n :} >CSROLE 1+ ;   \ the local, in its declared spelling
: CONVERTER ( n -- csrole ) 1+ >CSROLE ;
: CALL-CONVERTER ( n -- n ) CONVERTER CSROLE>N ;
;package
41 CS-EXECUTION:FIRST-CLASS 41 T=
42 CS-EXECUTION:QUOTED 42 T=
40 CS-EXECUTION:LOCAL 41 T=
40 CS-EXECUTION:CALL-CONVERTER 41 T=

package CS-SHADOW
public
: >CSROLE ( n -- n ) 2 + ;
: CALL ( n -- n ) >csrole ;
;package
40 CS-SHADOW:CALL 42 T=
40 >CSROLE CSROLE>N 40 T=

\ The pending name still binds the used package's cast. Once published, the
\ new package member is an ordinary callable with its own behavior.
package CS-PRIOR-BASE
public
CAST: PRIOR-CAST ( n -- n )
;package
package CS-PRIOR
public
using CS-PRIOR-BASE
: PRIOR-CAST ( n -- n ) prior-cast 1+ ;
;using
: CALL ( n -- n ) PRIOR-CAST ;
;package
40 CS-PRIOR:PRIOR-CAST 41 T=
40 CS-PRIOR:CALL 41 T=

\ --- pointer and quotation casts are private class mints. ---------------------
\ A pointer or quotation destination asserts that an integer is an address or an
\ execution token, which nothing checks, so it is declared only in a package's
\ private section. The integer comes from a typed source: an address is the
\ distance `NULL-PTR BYTE-VIEW -`, an execution token is `search-wl`'s answer.
: CS-TWICE ( n -- n ) 2 * ;            \ global, so wordlist 0 finds it

\ A projection out of a quotation mints nothing, so it is declared at top level.
CAST: NN>N ( [ n -- n ] -- n )

package CS-MINT
16 BUFFER: BYTE-ROW
8 BUFFER: XT-CELL
variable HIT
CAST: >BYTES ( n -- ptr u8 )
CAST: >NN ( n -- [ n -- n ] )
CAST: >XT-SLOT ( ptr n -- ptr [ -- ] )
CAST: >BYTE-BOX ( n -- csbox<ptr u8> )
CAST: >NN-BOX ( n -- csbox<[ n -- n ]> )
\ Checked callers certify against the published rows while the package is open,
\ and the output is genuinely `ptr u8`: demanding a cell pointer there rejects.
s" CC-BYTE ( n -- u8 ) >BYTES c@"         CHECK-QUIET-CANDIDATE! -1 T=
s" CC-CELL ( n -- ptr n ) >BYTES"         CHECK-QUIET-CANDIDATE!  0 T=
s" CC-NN ( n n -- n ) >NN execute"        CHECK-QUIET-CANDIDATE! -1 T=
: ADDR ( ptr u8 -- n ) NULL-PTR BYTE-VIEW - ;
public
\ A byte stored through the buffer reads back through its integer address, and
\ one stored through the cast pointer lands in the buffer.
: BYTE-ROUND ( u8 -- u8 ) BYTE-ROW 5 + c!  BYTE-ROW ADDR 5 + >BYTES c@ ;
: BYTE-STORE ( u8 -- u8 ) BYTE-ROW ADDR 7 + >BYTES c!  BYTE-ROW 7 + c@ ;
\ A looked-up execution token, cast to its effect, runs as that word; the
\ projection and the mint are both identities.
: RUN-FOUND ( n -- n ) s" CS-TWICE" 0 search-wl >NN execute ;
: XT-SAME? ( -- bool ) ['] CS-TWICE NN>N s" CS-TWICE" 0 search-wl = ;
: XT-ROUND ( n -- n ) [: 3 + ;] NN>N >NN execute ;
\ A raw cell viewed as a quotation slot takes an xt, and the xt read back runs.
: SLOT-RUN ( -- n )
   0 HIT !
   [: 7 HIT ! ;] XT-CELL CELL-VIEW >XT-SLOT xt!
   XT-CELL CELL-VIEW >XT-SLOT @ execute
   HIT @ ;
\ A layout instance whose argument is a pointer or a quotation is the same mint:
\ its field projection hands back the address or the execution token.
: BOX-BYTE ( u8 -- u8 )
   BYTE-ROW 3 + c!  BYTE-ROW ADDR 3 + >BYTE-BOX CSBOX:UNMAKE c@ ;
: BOX-RUN ( n -- n ) s" CS-TWICE" 0 search-wl >NN-BOX CSBOX:UNMAKE execute ;
;package
200 CS-MINT:BYTE-ROUND 200 T=
201 CS-MINT:BYTE-STORE 201 T=
21 CS-MINT:RUN-FOUND 42 T=
CS-MINT:XT-SAME? -1 T=
5 CS-MINT:XT-ROUND 8 T=
CS-MINT:SLOT-RUN 7 T=
202 CS-MINT:BOX-BYTE 202 T=
11 CS-MINT:BOX-RUN 22 T=

\ --- a pointer view of a layout row is raw; the layout guards its tags. -------
\ A private `( ptr family -- ptr n )` stores a cell the family never held, and
\ the next fetch of that row refuses it. The family is two cells wide: a pointer
\ to it is still one. The refusal ends the process, so the rows run in a forked
\ child of this image, where the words below are defined.
package CS-LAYOUT
ENUM cs-color red green ;ENUM
PRODUCT cs-pixel 0
   FIELD color cs-color
   FIELD count n
;PRODUCT
1 LAYOUT-BUFFER ROW cs-pixel
CAST: RAW ( ptr cs-pixel -- ptr n )
public
: FORGE ( -- ) 2 0 ROW RAW ! ;
: READ ( -- ) 0 ROW @ drop ;
;package

package CS-CHILD
public
$400 constant CAP
10000 constant CHILD-MS
create OUT CAP allot
create ERR CAP allot
variable ERR-U
: RUN ( ptr u8 n -- n )   \ source -> child exit status (-1 = signal or timeout)
   OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS SUBJECT:RUN
   MATCH outcome
     exited OF ENDOF
     signaled OF drop -1 ENDOF
     timeout OF -1 ENDOF
   ;MATCH
   {: rc:n :}
   LEN>N ERR-U !
   LEN>N drop
   rc ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
;package
s" CS-LAYOUT:READ" CS-CHILD:RUN 0 T=
s" CS-LAYOUT:FORGE CS-LAYOUT:READ" CS-CHILD:RUN ENGINE-ERROR:BAD-TAG T=
CS-CHILD:ERR$ s" hb: bad layout tag" CONTAINS? -1 T=

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" cast-suite: failures" 1 die ;
REPORT
