split-str
\ prims.f - the engine primitive specification, one machine-independent table.
\ Each row names one primitive and states its checker effect once: a build
\ states the table, as the kernel object carries it, onto checker.f's `PE-*`
\ constructors (src/habu/link-kernel.f STATE-PRIMS), each backend
\ registers a body under the row's name (src/habu/primitive-registry.f
\ SPEC-CHECK refuses a body with no row, COMPLETE a row with no body), and a
\ REF names its checked reference. Loads ahead of src/core/checker.f, over
\ engine primitives only.

package PRIM-SPEC

public

\ K-ELAB carries no effect: a primitive whose checker elaborates its effect has a
\ body and an explicit row, so the completeness gate passes by a row, not silence.
0 constant K-PRIM            \ a global row
1 constant K-PKG-PRIVATE     \ a package row closed by ECLOSE-PRIVATE: only the
                             \ owning package's checked bodies resolve it
2 constant K-ELAB            \ the checker elaborates this primitive's effect

\ One code per checker row constructor.
 1 constant A-IN
 2 constant A-OUT
 3 constant A-VAR-A
 4 constant A-VAR-B
 5 constant A-VAR-C
 6 constant A-VAR-D
 7 constant A-VAR-E
 8 constant A-NUM
 9 constant A-BOOL
10 constant A-REAL
11 constant A-U8
12 constant A-PTR
13 constant A-RAW
14 constant A-QUOT
15 constant A-QUOT-END
16 constant A-FINALLY
17 constant A-DBASE          \ DBASE-kind the pending pointee: the DATA region's base, the address OF nothing
18 constant A-OFF            \ mark the pending pointee as an INTEGER-offset step: a null riding it becomes a DATA-base pointer
19 constant A-QUOT-IN
20 constant A-QUOT-OUT
21 constant A-C2-INVOKE
22 constant A-BASE           \ the pending pointee binds only a base address's own pointee: a distance row's base operand
23 constant A-NULL           \ NULL-kind the pending pointee: the null, a pointer to no element, permissive inside a pointee
\ ADDR-kind the pending pointee: any type, any family, for a row that reads,
\ writes and sizes nothing through it.
24 constant A-ADDR
\ A quotation input whose input and output rows are independent: [ R -- S ].
25 constant A-QUOT-ANY
\ Package C2-MEM's stow-layout, taken whole.
26 constant A-STOW-LAYOUT
\ `str`: a string as one value, its base address and byte length.
27 constant A-STR
\ STORED-kind the pending pointee: any stored type, a closed record or family
\ included, as a declared `^a` takes it.
28 constant A-STORED
\ CELL-kind the pending var: a one-cell value the primitive reads.
29 constant A-CELL

private

\ Rows hold offsets into NAMES and CODES, not addresses, so the table can be
\ walked by index from any consumer.
$180 constant ROW-CAP
10 constant ROW-CELLS
$2000 constant NAME-CAP
$1000 constant CODE-CAP

0 constant F-KIND
1 constant F-NAME-OFF
2 constant F-NAME-LEN
3 constant F-PKG-OFF
4 constant F-PKG-LEN
5 constant F-CODE-OFF
6 constant F-CODE-LEN
7 constant F-FLAGS
8 constant F-REF-OFF
9 constant F-REF-LEN

1 constant FL-TRUSTED-ONLY
2 constant FL-FIXED-ABI
4 constant FL-IMMEDIATE

create ROWS  ROW-CAP ROW-CELLS * cells allot
create NAMES NAME-CAP allot
create CODES CODE-CAP allot
variable ROW-N
variable NAME-U
variable CODE-U
variable BI

variable CUR-KIND
variable CUR-NAME-OFF   variable CUR-NAME-LEN
variable CUR-PKG-OFF    variable CUR-PKG-LEN
variable CUR-CODE-OFF
variable CUR-REF-OFF    variable CUR-REF-LEN

\ SLOT addresses a row the caller has already bounded. Only ROW-WRITE needs
\ SLOT: it fills the row at ROW-N before ROW-N counts it.
: SLOT ( n n -- ^n ) {: row:n field:n :}
   row ROW-CELLS * field + cells ROWS + ;

: ROW-FIELD ( n n -- ^n ) {: row:n field:n :}
   row 0 < row ROW-N @ >= or IF s" prims: row index out of range" EXIT-STATUS:INTERNAL die THEN
   row field SLOT ;

: NAME-SPAN, ( str -- ) {: a:^ u:n :}
   u 0 < IF s" prims: negative name length" EXIT-STATUS:INTERNAL die THEN
   NAME-U @ u + NAME-CAP > IF s" prims: name buffer full" EXIT-STATUS:INTERNAL die THEN
   0 BI !
   BEGIN BI @ u < WHILE
      BI @ a + c@   NAMES NAME-U @ + BI @ + c!
      BI @ 1 + BI !
   REPEAT
   NAME-U @ u + NAME-U ! ;

: CODE, ( n -- ) {: code:n :}
   CODE-U @ CODE-CAP >= IF s" prims: atom buffer full" EXIT-STATUS:INTERNAL die THEN
   code CODES CODE-U @ + c!
   CODE-U @ 1 + CODE-U ! ;

\ Spelled like the checker's row constructors, and private to this package so
\ both spellings can exist: the checker's build terms, these append the code
\ that names one.
: PE-IN          A-IN CODE, ;
: PE-OUT         A-OUT CODE, ;
: PE-A           A-VAR-A CODE, ;
: PE-B           A-VAR-B CODE, ;
: PE-C           A-VAR-C CODE, ;
: PE-D           A-VAR-D CODE, ;
: PE-N           A-NUM CODE, ;
: PE-F           A-BOOL CODE, ;
: PE-R           A-REAL CODE, ;
: PE-U8          A-U8 CODE, ;
: PE-STR         A-STR CODE, ;
: PE-PTR-A       A-VAR-A CODE, A-PTR CODE, ;
: PE-PTR-B       A-VAR-B CODE, A-PTR CODE, ;
: PE-PTR-C       A-VAR-C CODE, A-PTR CODE, ;
: PE-PTR-D       A-VAR-D CODE, A-PTR CODE, ;
: PE-PTR-E       A-VAR-E CODE, A-PTR CODE, ;
: PE-PTR-N       A-NUM CODE, A-PTR CODE, ;
: PE-PTR-U8      A-U8 CODE, A-PTR CODE, ;
: PE-PTR-PTR-B   A-VAR-B CODE, A-PTR CODE, A-PTR CODE, ;
: PE-PTR-A-RAW   A-VAR-A CODE, A-RAW CODE, A-PTR CODE, ;
: PE-PTR-A-DBASE A-VAR-A CODE, A-DBASE CODE, A-PTR CODE, ;
: PE-PTR-A-NULL  A-VAR-A CODE, A-NULL CODE, A-PTR CODE, ;
: PE-PTR-A-ADDR  A-VAR-A CODE, A-ADDR CODE, A-PTR CODE, ;
: PE-PTR-A-OFF   A-VAR-A CODE, A-OFF CODE, A-PTR CODE, ;
: PE-PTR-B-BASE  A-VAR-B CODE, A-BASE CODE, A-PTR CODE, ;
: PE-PTR-A-STORED A-VAR-A CODE, A-STORED CODE, A-PTR CODE, ;
: PE-A-CELL      A-VAR-A CODE, A-CELL CODE, ;
\ PE-Q opens a quotation the primitive runs before it returns; PE-QANY is an xt it keeps.
: PE-Q           A-QUOT CODE, ;
: PE-QIN         A-QUOT-IN CODE, ;
: PE-QOUT        A-QUOT-OUT CODE, ;
: ;PE-Q          A-QUOT-END CODE, ;
: PE-FINALLY     A-FINALLY CODE, ;
: PE-C2-INVOKE   A-C2-INVOKE CODE, ;
: PE-STOW-LAYOUT A-STOW-LAYOUT CODE, ;
: PE-QANY        A-QUOT-ANY CODE, ;

: ROW-OPEN ( n -- ) {: kind:n :}
   kind CUR-KIND !
   0 CUR-PKG-OFF !  0 CUR-PKG-LEN !
   0 CUR-REF-OFF !  0 CUR-REF-LEN !
   CODE-U @ CUR-CODE-OFF ! ;

: ROW-NAME ( -- )
   NAME-U @ CUR-NAME-OFF !
   parse-name dup CUR-NAME-LEN ! NAME-SPAN, ;

: ROW-WRITE ( -- )
   ROW-N @ ROW-CAP >= IF s" prims: specification table full" EXIT-STATUS:INTERNAL die THEN
   CUR-KIND @      ROW-N @ F-KIND SLOT !
   CUR-NAME-OFF @  ROW-N @ F-NAME-OFF SLOT !
   CUR-NAME-LEN @  ROW-N @ F-NAME-LEN SLOT !
   CUR-PKG-OFF @   ROW-N @ F-PKG-OFF SLOT !
   CUR-PKG-LEN @   ROW-N @ F-PKG-LEN SLOT !
   CUR-CODE-OFF @  ROW-N @ F-CODE-OFF SLOT !
   CODE-U @ CUR-CODE-OFF @ - ROW-N @ F-CODE-LEN SLOT !
   0               ROW-N @ F-FLAGS SLOT !
   CUR-REF-OFF @   ROW-N @ F-REF-OFF SLOT !
   CUR-REF-LEN @   ROW-N @ F-REF-LEN SLOT !
   ROW-N @ 1 + ROW-N ! ;

\ A row body holds only atoms, `PE-...` or `;PE-...`, which parse nothing, and
\ `REF name` clauses, so the first closer outside a REF clause ends the row.
: EPRIM: ( -- )
   K-PRIM ROW-OPEN ROW-NAME ;

: EPRIM; ( -- )
   ROW-WRITE ;

: EPPRIM: ( -- )
   K-PKG-PRIVATE ROW-OPEN
   NAME-U @ CUR-PKG-OFF !
   parse-name dup CUR-PKG-LEN ! NAME-SPAN,
   ROW-NAME ;

: ECLOSE-PRIVATE ( -- )
   ROW-WRITE ;

\ REF names the reference implementation in checked Habu that runs beside each
\ backend's body over the same cases. A reference never shadows a
\ primitive spelling, so the clause spells `PRIM-REF:ADD`, not `PRIM-REF:+`. A
\ row carries one only where the semantics can be written WITHOUT the primitive
\ it answers for and where the gate exercises it; the overloads of a name state
\ it on the row whose instantiation the gate runs.
: REF ( -- )
   NAME-U @ CUR-REF-OFF !
   parse-name dup CUR-REF-LEN ! NAME-SPAN, ;

\ The just-written row's checker row is a trust boundary: a CHECKED caller is
\ refused, while a TRUSTED: body may use it.
: ETRUSTED-ONLY! ( -- )
   ROW-N @ 0 <= IF s" prims: trusted-only before any row" EXIT-STATUS:INTERNAL die THEN
   ROW-N @ 1 - F-FLAGS ROW-FIELD dup @ FL-TRUSTED-ONLY or swap ! ;

\ The just-written row's primitive is immediate.
: EIMMEDIATE! ( -- )
   ROW-N @ 0 <= IF s" prims: immediate before any row" EXIT-STATUS:INTERNAL die THEN
   ROW-N @ 1 - F-FLAGS ROW-FIELD dup @ FL-IMMEDIATE or swap ! ;

\ The machine body uses the row's widths and glue without instantiating them.
: EFIXED-ABI! ( -- )
   ROW-N @ 0 <= IF s" prims: fixed ABI before any row" EXIT-STATUS:INTERNAL die THEN
   ROW-N @ 1 - F-FLAGS ROW-FIELD dup @ FL-FIXED-ABI or swap ! ;

: ELAB: ( -- )
   K-ELAB ROW-OPEN ROW-NAME ROW-WRITE ;

public

: COUNT ( -- n )
   ROW-N @ ;

: KIND@ ( n -- n )
   F-KIND ROW-FIELD @ ;

: NAME$ ( n -- str ) {: row:n :}
   NAMES row F-NAME-OFF ROW-FIELD @ +  row F-NAME-LEN ROW-FIELD @ ;

: PKG$ ( n -- str ) {: row:n :}
   NAMES row F-PKG-OFF ROW-FIELD @ +  row F-PKG-LEN ROW-FIELD @ ;

: REF$ ( n -- str ) {: row:n :}
   NAMES row F-REF-OFF ROW-FIELD @ +  row F-REF-LEN ROW-FIELD @ ;

: TRUSTED-ONLY? ( n -- bool )
   F-FLAGS ROW-FIELD @ FL-TRUSTED-ONLY and 0 <> ;

: FIXED-ABI? ( n -- bool )
   F-FLAGS ROW-FIELD @ FL-FIXED-ABI and 0 <> ;

: IMMEDIATE? ( n -- bool )
   F-FLAGS ROW-FIELD @ FL-IMMEDIATE and 0 <> ;

: CODE-LEN@ ( n -- n )
   F-CODE-LEN ROW-FIELD @ ;

: CODE@ ( n n -- n ) {: row:n idx:n :}
   idx 0 < idx row F-CODE-LEN ROW-FIELD @ >= or
      IF s" prims: atom index out of range" EXIT-STATUS:INTERNAL die THEN
   CODES row F-CODE-OFF ROW-FIELD @ + idx + c@ ;

\ Exact spelling: a body registered under a different case is a different row
\ and must say so. The index lives on the stack: a lookup leaves no state in
\ the image.
: FIND ( str -- n ) {: a:^ u:n :}
   0
   BEGIN dup ROW-N @ < WHILE
      dup NAME$ a u CORE-STR= IF EXIT THEN
      1 +
   REPEAT
   drop -1 ;

private

\ THE ROWS ARE THE CHECKER'S AXIOMS: a primitive has no Habu body to infer an
\ effect from. A row with a REF is held to its reference; a row with none
\ (every syscall, control, defining-word, image, FFI and atomic row) is held
\ only by the engine rebuilding itself through these rows and by the programs
\ the gate runs on it.
EPRIM: finally PE-FINALLY EPRIM;
\ The C2 runtime's scoped invocation is package C2-MEM's.
EPPRIM: C2-MEM c2-invoke PE-C2-INVOKE ECLOSE-PRIVATE
\ The C2 runtime passes the stow-layout, then the frame's bytes; this narrow
\ machine transfer takes the layout whole.
EPPRIM: C2-MEM c2-init-stow
   PE-PTR-U8 PE-IN PE-N PE-IN PE-A PE-IN PE-B PE-IN
   PE-STOW-LAYOUT PE-IN PE-PTR-U8 PE-IN
   PE-PTR-U8 PE-OUT PE-N PE-OUT PE-B PE-OUT ECLOSE-PRIVATE
EPPRIM: C2-MEM c2-records-stow
   PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PE-A PE-IN PE-B PE-IN
   PE-STOW-LAYOUT PE-IN PE-PTR-U8 PE-IN
   PE-PTR-U8 PE-OUT PE-N PE-OUT PE-B PE-OUT ECLOSE-PRIVATE

EPPRIM: SOURCE-ROOT source-file-run PE-Q ;PE-Q PE-IN ECLOSE-PRIVATE

EPRIM: dup   PE-A PE-IN  PE-A PE-OUT PE-A PE-OUT REF PRIM-REF:S-DUP EPRIM;
EPRIM: drop  PE-A PE-IN REF PRIM-REF:S-DROP EPRIM;
EPRIM: swap  PE-A PE-IN PE-B PE-IN  PE-B PE-OUT PE-A PE-OUT REF PRIM-REF:S-SWAP EPRIM;
EPRIM: over  PE-A PE-IN PE-B PE-IN  PE-A PE-OUT PE-B PE-OUT PE-A PE-OUT
            REF PRIM-REF:S-OVER EPRIM;
EPRIM: nip   PE-A PE-IN PE-B PE-IN  PE-B PE-OUT REF PRIM-REF:S-NIP EPRIM;
EPRIM: tuck  PE-A PE-IN PE-B PE-IN  PE-B PE-OUT PE-A PE-OUT PE-B PE-OUT
            REF PRIM-REF:S-TUCK EPRIM;
EPRIM: rot   PE-A PE-IN PE-B PE-IN PE-C PE-IN  PE-B PE-OUT PE-C PE-OUT PE-A PE-OUT
            REF PRIM-REF:S-ROT EPRIM;
EPRIM: -rot  PE-A PE-IN PE-B PE-IN PE-C PE-IN  PE-C PE-OUT PE-A PE-OUT PE-B PE-OUT
            REF PRIM-REF:S-UNROT EPRIM;
EPRIM: 2dup  PE-A PE-IN PE-B PE-IN  PE-A PE-OUT PE-B PE-OUT PE-A PE-OUT PE-B PE-OUT
            REF PRIM-REF:S-2DUP EPRIM;
EPRIM: 2drop PE-A PE-IN PE-B PE-IN REF PRIM-REF:S-2DROP EPRIM;
EPRIM: 2swap PE-A PE-IN PE-B PE-IN PE-C PE-IN PE-D PE-IN
            PE-C PE-OUT PE-D PE-OUT PE-A PE-OUT PE-B PE-OUT REF PRIM-REF:S-2SWAP EPRIM;
EPRIM: 2over PE-A PE-IN PE-B PE-IN PE-C PE-IN PE-D PE-IN
            PE-A PE-OUT PE-B PE-OUT PE-C PE-OUT PE-D PE-OUT PE-A PE-OUT PE-B PE-OUT
            REF PRIM-REF:S-2OVER EPRIM;

EPRIM: +      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:ADD EPRIM;
\ The PE-PTR-B-BASE rows precede `^a ^a`, so `X NULL-PTR -` matches the
\ null's pointee alone and publishes no null kind.
EPRIM: +      PE-PTR-A-OFF PE-IN PE-N PE-IN  PE-PTR-A PE-OUT EPRIM;
EPRIM: +      PE-N PE-IN PE-PTR-A-OFF PE-IN  PE-PTR-A PE-OUT EPRIM;
EPRIM: -      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:SUB EPRIM;
EPRIM: -      PE-PTR-A-OFF PE-IN PE-N PE-IN  PE-PTR-A PE-OUT EPRIM;
EPRIM: -      PE-PTR-A-ADDR PE-IN PE-PTR-B-BASE PE-IN  PE-N PE-OUT EPRIM;
EPRIM: -      PE-PTR-B-BASE PE-IN PE-PTR-A-ADDR PE-IN  PE-N PE-OUT EPRIM;
EPRIM: -      PE-PTR-A-ADDR PE-IN PE-PTR-A-ADDR PE-IN  PE-N PE-OUT EPRIM;
EPRIM: *      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:MUL EPRIM;
EPRIM: and    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:BAND EPRIM;
EPRIM: and    PE-F PE-IN PE-F PE-IN  PE-F PE-OUT REF PRIM-REF:BAND-F EPRIM;
EPRIM: or     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:BOR EPRIM;
EPRIM: or     PE-F PE-IN PE-F PE-IN  PE-F PE-OUT REF PRIM-REF:BOR-F EPRIM;
EPRIM: xor    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:BXOR EPRIM;
EPRIM: xor    PE-F PE-IN PE-F PE-IN  PE-F PE-OUT REF PRIM-REF:BXOR-F EPRIM;
EPRIM: 1+     PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:INC EPRIM;
EPRIM: 1+     PE-PTR-A-OFF PE-IN  PE-PTR-A PE-OUT EPRIM;
EPRIM: 1-     PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:DEC EPRIM;
EPRIM: 1-     PE-PTR-A-OFF PE-IN  PE-PTR-A PE-OUT EPRIM;
EPRIM: negate PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:NEG EPRIM;
EPRIM: invert PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:BNOT EPRIM;
EPRIM: 0=     PE-A-CELL PE-IN  PE-F PE-OUT EPRIM;
EPRIM: 0<     PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:ZERO-NEG? EPRIM;
EPRIM: =      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:EQ EPRIM;
EPRIM: =      PE-PTR-A-ADDR PE-IN PE-PTR-A-ADDR PE-IN  PE-F PE-OUT EPRIM;
EPRIM: <      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:LT EPRIM;
EPRIM: <      PE-PTR-A-ADDR PE-IN PE-PTR-A-ADDR PE-IN  PE-F PE-OUT EPRIM;
EPRIM: >      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:GT EPRIM;
EPRIM: >      PE-PTR-A-ADDR PE-IN PE-PTR-A-ADDR PE-IN  PE-F PE-OUT EPRIM;
EPRIM: <>     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:NEQ EPRIM;
EPRIM: <>     PE-PTR-A-ADDR PE-IN PE-PTR-A-ADDR PE-IN  PE-F PE-OUT EPRIM;
EPRIM: <=     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:LE EPRIM;
EPRIM: <=     PE-PTR-A-ADDR PE-IN PE-PTR-A-ADDR PE-IN  PE-F PE-OUT EPRIM;
EPRIM: >=     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:GE EPRIM;
EPRIM: >=     PE-PTR-A-ADDR PE-IN PE-PTR-A-ADDR PE-IN  PE-F PE-OUT EPRIM;
EPRIM: /      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:DIV EPRIM;
EPRIM: mod    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:REM EPRIM;
EPRIM: /mod   PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PE-N PE-OUT REF PRIM-REF:DIVREM EPRIM;
EPRIM: abs    PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:ABSOLUTE EPRIM;
EPRIM: min    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:MINIMUM EPRIM;
EPRIM: max    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:MAXIMUM EPRIM;
EPRIM: lshift PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:SHL EPRIM;
EPRIM: rshift PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:SHR EPRIM;
EPRIM: cells  PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:CELL-BYTES EPRIM;
EPRIM: cell+  PE-PTR-A-OFF PE-IN  PE-PTR-A PE-OUT EPRIM;
EPRIM: cell+  PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:CELL-STEP EPRIM;
EPRIM: chars  PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:CHAR-BYTES EPRIM;
EPRIM: char+  PE-PTR-A-OFF PE-IN  PE-PTR-A PE-OUT EPRIM;
EPRIM: char+  PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:CHAR-STEP EPRIM;

EPRIM: @          PE-PTR-A PE-IN  PE-A PE-OUT EPRIM;
EPRIM: !          PE-A PE-IN PE-PTR-A PE-IN EPRIM;
\ `xt!` is `!` plus the declaration that the cell it writes holds a code-region
\ address, for a persisted cell whose address the caller works out at run time
\ from a table base and a row index.
\ What the row still cannot say is that the pointee MUST be an execution token:
\ there is no quotation-kinded type variable (no TVK kind admits only
\ quotations), so `xt!` into a plain integer cell type-checks today and would
\ have the loader shift an ordinary integer.
\ A plain `!` of a quotation into such a cell is already refused.
EPRIM: xt!        PE-A PE-IN PE-PTR-A PE-IN EPRIM; EFIXED-ABI!
EPRIM: ptr-cell-mark PE-PTR-A PE-IN EPRIM;
EPRIM: scratch-cell-mark PE-PTR-A PE-IN EPRIM;
\ ptr-cell! is xt!'s store for a cell that holds a DATA pointer: it writes the
\ value and declares the cell as ptr-cell-mark declares it.
EPRIM: ptr-cell!  PE-A PE-IN PE-PTR-A PE-IN EPRIM; EFIXED-ABI!
EPRIM: ptr-field  PE-PTR-A PE-IN PE-N PE-IN  PE-PTR-PTR-B PE-OUT EPRIM;
\ Explicit memory views preserve address bits while changing the access type.
EPRIM: byte-view  PE-PTR-A-STORED PE-IN PE-PTR-U8 PE-OUT EPRIM;
EPRIM: cell-view  PE-PTR-U8 PE-IN PE-PTR-N PE-OUT EPRIM;
\ The only crossing between a string's two cells and `str`, the one value they
\ make: base below length either way.
EPRIM: >str       PE-PTR-U8 PE-IN PE-N PE-IN PE-STR PE-OUT EPRIM;
EPRIM: str>       PE-STR PE-IN PE-PTR-U8 PE-OUT PE-N PE-OUT EPRIM;
\ A str's base and its byte count.
EPPRIM: STR data PE-STR PE-IN PE-PTR-U8 PE-OUT ECLOSE-PRIVATE
EPPRIM: STR len PE-STR PE-IN PE-N PE-OUT ECLOSE-PRIVATE
\ The address as a number, for a distance or a hash; the way back is a
\ package's private `CAST:`.
EPRIM: PTR>N      PE-PTR-A-ADDR PE-IN PE-N PE-OUT EPRIM;
EPRIM: +!         PE-N PE-IN PE-PTR-N PE-IN REF PRIM-REF:ADD-TO EPRIM;
EPRIM: c@         PE-PTR-U8 PE-IN  PE-U8 PE-OUT EPRIM;
EPRIM: c!         PE-U8 PE-IN PE-PTR-U8 PE-IN EPRIM;
EPRIM: atomic@    PE-PTR-A PE-IN  PE-A PE-OUT EPRIM;
EPRIM: atomic!    PE-A PE-IN PE-PTR-A PE-IN EPRIM;
EPRIM: atomic-add PE-N PE-IN PE-PTR-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: atomic-cas PE-A PE-IN PE-A PE-IN PE-PTR-A PE-IN  PE-A PE-OUT EPRIM;
EPRIM: fence      EPRIM;
\ `evaluate-closed` runs its text on a data stack of its own and rejects a text
\ that leaves or takes cells with a record and 70, so whatever the text does
\ its net effect is ( -- ) and the row states the string alone. `evaluate`,
\ whose effect is the text's, stays elaborated below and unsafe in a body.
EPRIM: evaluate-closed PE-STR PE-IN EPRIM;
EPPRIM: SOURCE-ROOT evaluate-range
   PE-STR PE-IN PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPRIM: count      PE-PTR-U8 PE-IN  PE-STR PE-OUT REF PRIM-REF:COUNTED$ EPRIM;

EPRIM: .            PE-N PE-IN EPRIM;
EPRIM: .s           EPRIM;
EPRIM: depth        PE-N PE-OUT EPRIM;
EPRIM: here         PE-PTR-A-RAW PE-OUT EPRIM;
EPPRIM: CHECKER-RESOLVE bare-tok-imm? PE-STR PE-IN PE-N PE-OUT ECLOSE-PRIVATE
EPRIM: scope-kind?  PE-N PE-IN PE-N PE-OUT EPRIM;
EPRIM: allot        PE-N PE-IN EPRIM;
EPRIM: align        EPRIM;
EPRIM: ,            PE-N PE-IN EPRIM;
EPRIM: c,           PE-N PE-IN EPRIM;
EPRIM: type         PE-STR PE-IN EPRIM;
EPRIM: throw        PE-N PE-IN EPRIM;
EPPRIM: TASK halt-throw PE-N PE-IN ECLOSE-PRIVATE
EPRIM: die          PE-STR PE-IN PE-N PE-IN EPRIM;
EPPRIM: INTERPRETER die-closed PE-STR PE-IN PE-N PE-IN ECLOSE-PRIVATE

EPRIM: open     PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: read     PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: ioctl    PE-N PE-IN PE-N PE-IN PE-PTR-A PE-IN  PE-N PE-OUT EPRIM;
EPRIM: map-anon PE-N PE-IN  PE-PTR-A PE-OUT PE-N PE-OUT EPRIM;
EPRIM: mmap     PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: open-rd  PE-PTR-U8 PE-IN  PE-N PE-OUT EPRIM;
EPRIM: access   PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: unlink   PE-PTR-U8 PE-IN  PE-N PE-OUT EPRIM;
EPRIM: rename   PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN  PE-N PE-OUT EPRIM;
EPRIM: chmod    PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: symlink  PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN  PE-N PE-OUT EPRIM;
EPRIM: readlink PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: realpath PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: mkdir    PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: rmdir    PE-PTR-U8 PE-IN  PE-N PE-OUT EPRIM;
EPRIM: stat64   PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN  PE-N PE-OUT EPRIM;
EPRIM: lstat64  PE-PTR-U8 PE-IN PE-PTR-U8 PE-IN  PE-N PE-OUT EPRIM;
EPRIM: getdirentries64
   PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: pipe     PE-N PE-OUT PE-N PE-OUT PE-N PE-OUT EPRIM;
EPRIM: dup2     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: fcntl    PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: poll     PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;   \ ( fds nfds ms -- nready|0|-errno ) the one unrestartable wait: callers restart on -EINTR
EPRIM: kill     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: setpgid  PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;

EPRIM: spawn-io  PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: spawn-argv-io
   PE-PTR-U8 PE-IN PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: spawn-argv-env-io
   PE-PTR-U8 PE-IN PE-PTR-A PE-IN PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN
   PE-N PE-OUT EPRIM;
EPRIM: spawn-argv-env-cwd-io
   PE-PTR-U8 PE-IN PE-PTR-A PE-IN PE-PTR-A PE-IN PE-PTR-U8 PE-IN
   PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: spawn-fds  PE-PTR-A PE-IN  PE-N PE-OUT EPRIM;   \ ( request -- pid|-errno )
EPRIM: fork          PE-N PE-OUT EPRIM;
EPRIM: wait-status   PE-N PE-IN  PE-N PE-OUT EPRIM;
\ Rewriting a write-protected dictionary record cell is package XREF's to do.
EPPRIM: XREF patch32 PE-N PE-IN PE-PTR-N PE-IN ECLOSE-PRIVATE
\ Re-indexing a record under its wordlist cell after a write is package XREF's to do.
EPPRIM: XREF record-rekey PE-N PE-IN ECLOSE-PRIVATE
\ Planting and clearing a breakpoint instruction is package DEBUG's.
EPPRIM: DEBUG patch32 PE-N PE-IN PE-PTR-U8 PE-IN ECLOSE-PRIVATE
\ The four relocation-record primitives below, per target.
\ `code-publish ( src dst len -- )` copies an emission into the code arena,
\ `callmap-set ( addr -- )` and `addrmap-set ( addr -- )` record a call or an
\ address literal at a region address, and `reloc-maps-clear ( start len -- )`
\ forgets the sites of a reclaimed span. The record is RELOC's band.
\ - ARM64, two bitmaps of one bit per four-byte region word: `callmap-set` sets
\   the call map's bit of the BL at `addr` and `addrmap-set` the address map's
\   bit of a carrier's first MOVZ, and a set bit stays set; `code-publish`
\   clears the call map over [dst, dst+len) and `reloc-maps-clear` both maps
\   over its span.
\ - x86-64, rows over the same bytes: `callmap-set` and `addrmap-set` add a
\   SITE-CALL or SITE-ADDR row for the instruction whose first byte (the `call`
\   opcode, the REX byte of `mov r64, imm64`) is at `addr`, at any byte of the
\   region. The writer finds the row's place by offset and moves the rows above
\   it up by one, so the rows stay strictly ascending: a site above the last
\   row is an append, the common case of code published at the code pointer,
\   and a site below it costs a move of the rows above it. No site is refused
\   for being below the last row, so a caller records its sites in any order
\   and merges nothing. The same row again changes nothing; the other kind at a
\   recorded offset, or a row past SITE-CAP, exits EXIT-STATUS:SITE. `code-publish`
\   removes the rows of both kinds in [dst, slot), the span and its int3 fill,
\   and `reloc-maps-clear` those of its span; sites are recorded after
\   `code-publish`, so an x86-64 publication drops the stale rows in the bytes
\   it writes where ARM64 keeps the address bits.
\ CP is always a code slot, the only address the native driver places code at:
\ a four-byte instruction word on ARM64 and a 16-byte slot on x86-64. Every CP
\ writer keeps it one:
\ `cp!` sets a CP an earlier writer made, the boot
\ starts it at the page-aligned DBASE+DICT-SIZE, `code-publish` moves it to
\ the first slot at or past dst+len, x86-64 filling the gap with int3,
\ `does-record` pads the name it writes at CP to a slot with zeros,
\ and `namespace-record`, `alias-record` and `def-open` move it past a long
\ name to the next slot, x86-64 padding with zeros.
\ An `addr` outside the region, or on ARM64 not a whole instruction, exits
\ EXIT-STATUS:SEAL-VIOLATION.
EPPRIM: NPUB code-publish PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: NPUB callmap-set PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: NPUB addrmap-set PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: NPUB xref-retarget PE-N PE-IN PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ Setting DNAME-REP on one live CAST: record is the checker's.
EPPRIM: CHECK rep-mark PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: NPUB wide-mark ECLOSE-PRIVATE
EPPRIM: CHECK wide-mark ECLOSE-PRIVATE
EPPRIM: CODE-RECLAIM reloc-maps-clear PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ The native defining-word runtime patch is package NDICT's.
EPPRIM: NDICT does-patch PE-N PE-IN PE-STR PE-IN ECLOSE-PRIVATE
\ `does-record ( entry len -- )` records the pending definition's `;does`
\ clause, writes its name at CP, zero-padded to a code slot, and sets CLAUSE-CELL.
EPPRIM: NPUB does-record PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPRIM: write         PE-N PE-IN PE-STR PE-IN  PE-N PE-OUT EPRIM;
EPRIM: close         PE-N PE-IN EPRIM;
EPRIM: close-rc      PE-N PE-IN  PE-N PE-OUT EPRIM;   \ close returning host status (0 ok, <0 fail)
EPRIM: epoch-seconds PE-N PE-OUT EPRIM;
EPRIM: mono-ns       PE-N PE-OUT EPRIM;
EPRIM: prof-on       PE-N PE-IN EPRIM;
EPRIM: prof-report   EPRIM;
EPRIM: prof-off      EPRIM;
EPRIM: prof-reset    EPRIM;
EPRIM: prof-child-reset EPRIM;                   \ in a forked child alone: frees the profiler the fork copied
EPRIM: prof-rate     PE-N PE-IN EPRIM;
EPRIM: prof-json     EPRIM;
EPRIM: prof-row      PE-N PE-IN EPRIM;                \ the report row for one dictionary record
EPRIM: prof-pc>rec   PE-N PE-IN  PE-N PE-OUT EPRIM;   \ the armed index's record for a pc, -1 when none owns it

EPRIM: rbase          PE-N PE-OUT EPRIM;
EPRIM: cp@            PE-N PE-OUT EPRIM;
EPPRIM: CHECKER-OVERLAY cp! PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: CODE-RECLAIM cp! PE-N PE-IN ECLOSE-PRIVATE
EPRIM: dbase@         PE-N PE-OUT EPRIM;
EPRIM: check@         PE-N PE-OUT EPRIM;
\ The compiler's prospective trusted-tick order and the owner whose installed
\ query established it. Order 0 is checking first; 1 is trusted gate first.
EPRIM: tick-order@    PE-N PE-OUT PE-PTR-U8 PE-OUT EPRIM;
EPPRIM: CHECK set-check PE-A PE-IN ECLOSE-PRIVATE
EFIXED-ABI!
EPPRIM: CHECK set-preflight PE-A PE-IN ECLOSE-PRIVATE
EFIXED-ABI!
EPRIM: ndict@         PE-N PE-OUT EPRIM;
EPPRIM: CHECKER-OVERLAY ndict! PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: GENERATED-DECL-DICTIONARY ndict! PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: LINKER ndict! PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: NPUB ndict-append PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: CHECK def-occ-select PE-PTR-N PE-IN PE-N PE-OUT PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: DEF-OCC def-occ-select PE-PTR-N PE-IN PE-N PE-OUT PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: DEF-OCC def-occ-resolve PE-N PE-IN PE-N PE-IN PE-PTR-N PE-OUT ECLOSE-PRIVATE
\ What `package`, `export`, `:`, `create`, `does>`, `;`, `immediate`, `cast:`
\ and `defer` change is sealed state after the seal: CUR, WIDN, DEF-WL, TSIG,
\ TCSIG, PKG-*, the body text, COMP-LATCH-CELL and the records behind the
\ PROT window.
\ These rows are how an interpreter written in Habu writes it. A refusal exits
\ and never throws: 79 on a task's thread (the four dictionary rows), 84 for a
\ protected wid after the seal (`alias-record`, `def-open`) and 83 for every
\ other refusal. A caller checks first and prints the engine's own text. While a
\ checker overlay is open, `namespace-private`, `alias-record` and `def-open`
\ throw E-OVERLAY-OPEN instead.
\
\ The three record writers store a name of at least one byte: up to DNAME-INL
\ bytes inline, a longer one at CP rounded up to a code slot, 4 bytes on ARM64
\ and 16 on x86-64, as `:` stores one. Each refuses an empty name, a live (folded name, wid) pair,
\ NDICT at DICT-CAP, a name copy that would reach the code ceiling (REGION -
\ $4000 above DBASE, compared unsigned) and a pending definition, whose record
\ occupies slot NDICT.
\
\ namespace-record ( ^u8 n bool -- n ): publish a namespace row at NDICT and
\ answer its index: [0] a fresh wid, [8] a second fresh wid when the flag is
\ set and else 0, [40] WL:NAMESPACE. It refuses a colon at an edge of the
\ name and a second colon: a name with one is a part's path, `A:B`.
EPPRIM: CHECKER-OVERLAY namespace-record PE-STR PE-IN PE-F PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: INTERPRETER namespace-record PE-STR PE-IN PE-F PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
\ namespace-private ( n -- ): give namespace row n, whose [8] is 0, a fresh
\ private wid. It refuses an index at or above NDICT, unsigned, a row that is
\ not a namespace row and one that already has a private wid.
EPPRIM: INTERPRETER namespace-private PE-N PE-IN ECLOSE-PRIVATE
\ alias-record ( ^u8 n n n -- ) name, source index, wid: a record for the source's code, as
\ `export` makes. It refuses a negative wid and a source at or above NDICT, unsigned.
EPPRIM: INTERPRETER alias-record PE-STR PE-IN PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ package-scope! ( n n -- ) namespace index, parent wid: PKG-PUB and PKG-PRI
\ from the row's [0] and [8], PKG-REC the row's address and PKG-PARENT the
\ wid. A row whose name holds a colon is a part, which first moves the four
\ open cells into ENCLOSING; any other row clears ENCLOSING, and
\ `-1 0` clears all eight. It refuses any other index at or above NDICT,
\ unsigned, a row that is not a namespace row, one without a private wid, and
\ a part when no package or a part is open.
\ It has no task guard of its own: the package keywords refuse one, and a throw
\ puts the scope back through this row.
EPPRIM: CHECKER-OVERLAY package-scope! PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: INTERPRETER package-scope! PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ def-open ( ^u8 n n n -- ) name, wid, kind: write record NDICT unpublished,
\ [0] CP after the name, [8] 0, the kind (0, DKIND:VAL, DKIND:ADDR with
\ DNAME-ALLOC, and DNAME-CELL as well for a variable, or DKIND:CAST) in [16] and
\ the wid in [40]; PEND-CELL is that record; TSIG, TCSIG, DOESB, CLAUSE and
\ TRUSTED clear; COMP-LATCH-CELL takes the compiler latch; LASTC-CELL
\ waits for the body (def-close). It refuses a kind with a bit
\ outside DKIND:MASK, DNAME-CELL and DNAME-ALLOC, a negative wid, and CP at or
\ past the code ceiling.
EPPRIM: INTERPRETER def-open PE-STR PE-IN PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ body-capture ( ^u8 n -- ): append the bytes and one space to the body text
\ through the capture routine `:` uses, which grows the text block when they do
\ not fit and throws E-BODY-TEXT when it cannot. It refuses BODYLEN past the
\ text capacity and a BODYLEN + u + 1 that wraps, with BODYLEN and u both
\ unsigned.
EPPRIM: INTERPRETER body-capture PE-STR PE-IN ECLOSE-PRIVATE
\ literal ( n -- ): append n to the definition `define-word` opened, which the
\ checker reads as that number; it refuses when no definition is open.
EPRIM: literal PE-N PE-IN EPRIM;
\ compile, ( [ R -- S ] -- ): append a call of the word whose entry the
\ quotation is to the definition define-word opened; it refuses when none is
\ open.
EPRIM: compile, PE-QANY EPRIM;
\ ; ( -- ): end the definition define-word opened, as `;` ends a colon
\ definition; it refuses when none is open.
EPRIM: ; EPRIM;
EIMMEDIATE!
\ trust-sig! ( ^u8 n -- ): the pending definition's signature span into
\ TSIG-A and TSIG-U. It refuses when no definition is pending.
EPPRIM: INTERPRETER trust-sig! PE-STR PE-IN ECLOSE-PRIVATE
\ created-sig! ( ^u8 n -- ): the inside of the pending definition's `does>`
\ signature, the effect of the words it creates, into TCSIG-A and TCSIG-U: its
\ copy in the body text. It refuses when no definition is
\ pending.
EPPRIM: INTERPRETER created-sig! PE-STR PE-IN ECLOSE-PRIVATE
\ def-close ( -- ): end the pending definition the native compiler compiled:
\ the state def-open set clears:
\ COMP-LATCH-CELL, TSIG, TCSIG, DOESB, CLAUSE, TRUSTED and PEND-CELL. Before
\ PEND-CELL clears, LASTC-CELL takes the record for a nonzero kind other than
\ DKIND:CAST, the record `does-patch` reads. It refuses when
\ no definition is pending and when the pending one is not a native compiler's
\ (COMP-LATCH-CELL zero), as ndict-append does.
EPPRIM: INTERPRETER def-close ECLOSE-PRIVATE
\ def-abort ( -- ): close an abandoned definition's writable code region
\ and compile-only state after a caught source error. The
\ catch's handler, return and loop stacks remain live for the caller.
EPPRIM: INTERPRETER def-abort ECLOSE-PRIVATE
\ stack-clear ( -- ): the REPL's caught-line boundary discards the user's
\ arbitrary stack residue after a refusal. It leaves handler, return and loop
\ state intact so the catching interpreter can continue.
EPPRIM: INTERPRETER stack-clear ECLOSE-PRIVATE
\ imm-mark ( -- ): set DNAME-IMM on the newest record, as the engine's
\ `immediate` sets it. Like that keyword it checks
\ nothing and refuses nothing.
EPPRIM: INTERPRETER imm-mark ECLOSE-PRIVATE
\ Only CHECKER-OVERLAY types these six: elsewhere a checked call is E-UNDEFINED
\ and a TRUSTED: one binds at tier 0 only. Refusals exit 79 on a task, else 83.
\
\ replay-open ( -- ): save NDICT, CP, WIDN, CURRENT, the open package's cells,
\ the using band and a pending definition in the engine's REPLAY-SCOPE band;
\ inside the overlay no definition pends. It refuses an open overlay and a
\ pending definition while a task is live.
\ replay-record ( ^u8 n n -- ) name, wid: publish a codeless record whose
\ entry traps, exit 76. It refuses no overlay, a negative wid and what the
\ three record writers above refuse.
\ record-wid! ( n n -- ) wid, index: retire a record (-2) or give it back its
\ wid. It refuses no overlay, an index at or above NDICT, unsigned, a live
\ package row and any negative wid but -2 and -3. A seeded primitive's record
\ retires as the live `undefine` retires it.
\ replay-private ( n bool -- ) namespace index, flag: give a row with no
\ private wid a fresh one (true), as `package` does for a row a qualified
\ definition made, or take back the one given (false). It refuses no overlay,
\ an index at or above NDICT, unsigned, and a row not a live package row; true
\ a row with a private wid, false one whose private wid is 0 or older than the
\ overlay.
\ replay-close ( -- ): zero the records from NDICT up to the highest count
\ reached, put the saved state back, a saved pending definition included, and
\ zero the band. The owner has already retired or dropped the ones below.
\ It refuses no overlay, a definition pending, a parked definition while a
\ task is live, NDICT below the saved count or above the highest count
\ replay-record and namespace-record reached, and CP or WIDN below the saved.
\ replay-widn! ( n -- ): put WIDN back to a mark the owner read inside the
\ overlay, once its rollback dropped what it made since. It refuses no overlay
\ and a mark below the saved WIDN or above WIDN, unsigned.
EPPRIM: CHECKER-OVERLAY replay-open ECLOSE-PRIVATE
EPPRIM: CHECKER-OVERLAY replay-close ECLOSE-PRIVATE
EPPRIM: CHECKER-OVERLAY replay-widn! PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: CHECKER-OVERLAY replay-record PE-STR PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: CHECKER-OVERLAY record-wid! PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: CHECKER-OVERLAY replay-private PE-N PE-IN PE-F PE-IN ECLOSE-PRIVATE
EPRIM: DRAIN-PRETRUST EPRIM;
\ data-base is a BASE ADDRESS: the start of the running task's DATA region, not
\ the address of any declared element. Its pointee is TVK-DBASE, so `data-base
\ OFF + @` still answers a number, an xt or a role - what those cells hold - and
\ still subtracts, compares and byte-views like any pointer, while a nominal
\ identity or an address read through it is refused at EVERY pointee depth: a
\ DATA cell is not the address of an address either.
\ A cell of the region that really holds an address
\ is reached with `ptr-field`, the declared door.
EPPRIM: BOOT data-base PE-PTR-A-DBASE PE-OUT ECLOSE-PRIVATE
\ NULL-PTR is the language's null: a pointer of any pointee, and the address OF
\ nothing. Its value is zero, so the body pushes zero and reads no memory.
\ TVK-NULL keeps every honest use: storing a null into a declared pointer cell,
\ comparing one, subtracting one, testing it with 0= - each binds the pointee
\ INSIDE a `^`, where the kind is permissive, and each means this one literal
\ address. What it refuses is a value read THROUGH it: `NULL-PTR + @` taken as
\ a nominal identity or as an address certified and forged one.
EPRIM: NULL-PTR       PE-PTR-A-NULL PE-OUT EPRIM;
\ The design seal's two writers: admit a
\ package's public wordlist by name, then seal the dictionary as it stands. Both
\ refuse once sealed, so a sealed design cannot widen its own vocabulary.
EPRIM: policy-admit   PE-STR PE-IN EPRIM;
EPRIM: policy-seal    EPRIM;
EPRIM: wordlist       PE-N PE-OUT EPRIM;
EPRIM: get-current    PE-N PE-OUT EPRIM;
EPPRIM: CHECKER-RESOLVE bare-search-wl PE-STR PE-IN PE-N PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: CHECKER-RESOLVE xref-search-wl PE-STR PE-IN PE-N PE-IN  PE-PTR-N PE-OUT ECLOSE-PRIVATE
EPPRIM: CHECKER-RESOLVE bare-scope-find PE-STR PE-IN
   PE-PTR-N PE-OUT PE-PTR-N PE-OUT PE-PTR-N PE-OUT PE-N PE-OUT ECLOSE-PRIVATE
EPRIM: parse-name     PE-STR PE-OUT EPRIM;
\ num-parse ( ^u8 n -- n bool bool ) : the engine's own number reader, over
\ bytes the caller already holds - the routine the interpret and compile
\ dispatches call for every literal token. It
\ answers the value, whether that value is a double's bits, and whether the
\ bytes were a number at all. A checked stage that has to know what cell a
\ literal spelling stands for asks this rather than reading the spelling back
\ with a second decoder.
EPRIM: num-parse      PE-STR PE-IN  PE-N PE-OUT PE-F PE-OUT PE-F PE-OUT EPRIM;
\ record-facts! ( n n -- ) offset, index: stores where a record's facts text
\ lies.
EPPRIM: CHECK record-facts! PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ sha256-blocks ( ^n ^u8 n -- n ) state cells, bytes, block count: how
\ many blocks the CPU's SHA-256 instructions compressed.
EPRIM: sha256-blocks PE-PTR-N PE-IN PE-STR PE-IN  PE-N PE-OUT EPRIM;
EPRIM: ffi-call       PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!
EPPRIM: TASK task-entry PE-N PE-OUT ECLOSE-PRIVATE
EPRIM: dlopen-entry PE-N PE-OUT EPRIM;
EPRIM: dlsym-entry PE-N PE-OUT EPRIM;
\ callback-entry answers the shared raw C thunk a compiler-owned stub branches to.
EPPRIM: FFI callback-entry PE-N PE-OUT ECLOSE-PRIVATE

EPPRIM: FFI ffi-call-n PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
\ ffi-call-k ( x1 .. xk fn -- n ): a C call of fn with k integer arguments
\ taken from the stack, x1 the first.
EPPRIM: FFI ffi-call-0 PE-N PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-1 PE-N PE-IN PE-N PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-2 PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-3 PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT
                       ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-4 PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN
                       PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-5 PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN
                       PE-N PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-6 PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN
                       PE-N PE-IN PE-N PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-7 PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN
                       PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-8 PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN
                       PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-abi PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                        PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-abi-r PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                          PE-R PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-struct PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                           PE-N PE-IN  PE-N PE-OUT PE-N PE-OUT ECLOSE-PRIVATE

EPRIM: f+      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT EPRIM;
EPRIM: f-      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT EPRIM;
EPRIM: f*      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT EPRIM;
EPRIM: f/      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT EPRIM;
EPRIM: fnegate PE-R PE-IN  PE-R PE-OUT EPRIM;
EPRIM: fabs    PE-R PE-IN  PE-R PE-OUT EPRIM;
EPRIM: fsqrt   PE-R PE-IN  PE-R PE-OUT EPRIM;
EPRIM: f<      PE-R PE-IN PE-R PE-IN  PE-F PE-OUT EPRIM;
EPRIM: f>      PE-R PE-IN PE-R PE-IN  PE-F PE-OUT EPRIM;
EPRIM: f=      PE-R PE-IN PE-R PE-IN  PE-F PE-OUT EPRIM;
EPRIM: f<=     PE-R PE-IN PE-R PE-IN  PE-F PE-OUT EPRIM;
EPRIM: f>=     PE-R PE-IN PE-R PE-IN  PE-F PE-OUT EPRIM;
EPRIM: f0<     PE-R PE-IN  PE-F PE-OUT EPRIM;
EPRIM: f0=     PE-R PE-IN  PE-F PE-OUT EPRIM;
EPRIM: s>f     PE-N PE-IN  PE-R PE-OUT EPRIM;
EPRIM: f>s     PE-R PE-IN  PE-N PE-OUT EPRIM;
EPRIM: f.      PE-R PE-IN EPRIM;
EPRIM: emit   PE-N PE-IN EPRIM;
EPRIM: cr     EPRIM;
EPRIM: space  EPRIM;
EPRIM: u.     PE-N PE-IN EPRIM;

EPRIM: create   EPRIM;
EPRIM: getpid   PE-N PE-OUT EPRIM;
EPRIM: proc-watch-open PE-N PE-IN PE-N PE-OUT EPRIM;   \ ( pid -- fd|-errno ) process-lifetime watch
EPRIM: kill-errno PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;   \ ( pid sig -- 0|-errno ) signal with errno detail
EPRIM: execve   PE-PTR-U8 PE-IN PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-N PE-OUT EPRIM;   \ ( pathz argv envp -- -errno ) child-side exec; only returns on failure
EPRIM: munmap   PE-PTR-A PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;   \ ( addr len -- 0|-1 ) release a mapping

\ execute-floor ( xt -- flag ): execute the xt, then answer whether it left the
\ data stack below its base. When it did, the stack is reset to the base before
\ the true flag is pushed, so no cell is written to the stack's low guard page.
\ `depth` and `catch` both push, so this is the one word an interpreter loop
\ written in Habu can use to observe a token that underflowed. The row states
\ no effect for the xt, so the xt travels as a number: outside package
\ INTERPRETER, that loop, a checked caller is refused.
EPPRIM: INTERPRETER execute-floor PE-N PE-IN  PE-F PE-OUT ECLOSE-PRIVATE

\ Primitives whose effect the checker elaborates.
\ These have engine bodies and deliberately no row: their effect depends on the
\ call site. A row here
\ states that, so the completeness gate accepts the body without an effect.
ELAB: execute
ELAB: catch
ELAB: evaluate
ELAB: 2>r
ELAB: 2r>
ELAB: 2r@

;package
