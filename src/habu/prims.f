\ prims.f - the engine primitive specification, one machine-independent table.
\ Each row names one primitive and states its checker effect once: a build
\ states the table, as the kernel object carries it, onto checker.f's `PE-*`
\ constructors (src/arch/arm64/link-kernel.f STATE-PRIMS), each backend
\ registers a body under the row's name (src/habu/primitive-registry.f
\ SPEC-CHECK refuses a body with no row, COMPLETE a row with no body), and a
\ REF names its checked reference. Loads ahead of src/core/checker.f, over
\ engine primitives only.

package PRIM-SPEC

public

\ K-ELAB and K-UNROWED carry no effect: they are the two ways a primitive can
\ have a body and no table row, named here so the completeness gate passes by an
\ explicit row rather than by silence.
0 constant K-PRIM            \ a global row
1 constant K-PKG-PRIVATE     \ a package row closed by ECLOSE-PRIVATE: only the
                             \ owning package's checked bodies resolve it
2 constant K-ELAB            \ the checker elaborates this primitive's effect
3 constant K-UNROWED         \ no checker row anywhere; no checked caller exists

\ What a primitive touches besides the data and return stacks, stated by a
\ row's REACH clause.
0 constant R-NONE            \ unclassed: output, files, execution, engine state
1 constant R-STACK           \ the stacks and arithmetic over its operands only;
                             \ a throw unwinds them to its catch
2 constant R-MEMORY          \ memory at an address its operands supply
3 constant R-DATA            \ reads or moves the DATA pointer
4 constant R-SOURCE          \ reads the input after its token
5 constant R-DICT            \ records and wordlist state an object carries
7 constant R-RUN             \ runs its xt or text operand; the effect is the
                             \ operand's, accounted by the operand's own edges
                             \ and the nested runs it makes
8 constant R-EXIT            \ ends the process; no result follows its run

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

private

$4C constant SPEC-RC

\ Rows hold offsets into NAMES and CODES, not addresses, so the table can be
\ walked by index from any consumer.
$180 constant ROW-CAP
12 constant ROW-CELLS
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
10 constant F-MIN-IN
11 constant F-REACH

1 constant FL-TRUSTED-ONLY
2 constant FL-FIXED-ABI
16 constant FL-PREFIX-PROVIDED

create ROWS  ROW-CAP ROW-CELLS * cells allot
create NAMES NAME-CAP allot
create CODES CODE-CAP allot
variable ROW-N
variable NAME-U
variable CODE-U
variable BI
variable IN-N
variable IN-DEPTH

variable CUR-KIND
variable CUR-NAME-OFF   variable CUR-NAME-LEN
variable CUR-PKG-OFF    variable CUR-PKG-LEN
variable CUR-CODE-OFF
variable CUR-REF-OFF    variable CUR-REF-LEN

\ SLOT addresses a row the caller has already bounded. Only ROW-WRITE needs
\ SLOT: it fills the row at ROW-N before ROW-N counts it.
: SLOT ( n n -- ptr n ) {: row:n field:n :}
   row ROW-CELLS * field + cells ROWS + ;

: ROW-FIELD ( n n -- ptr n ) {: row:n field:n :}
   row 0 < row ROW-N @ >= or IF s" prims: row index out of range" SPEC-RC die THEN
   row field SLOT ;

: NAME-SPAN, ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < IF s" prims: negative name length" SPEC-RC die THEN
   NAME-U @ u + NAME-CAP > IF s" prims: name buffer full" SPEC-RC die THEN
   0 BI !
   BEGIN BI @ u < WHILE
      BI @ a + c@   NAMES NAME-U @ + BI @ + c!
      BI @ 1 + BI !
   REPEAT
   NAME-U @ u + NAME-U ! ;

: CODE, ( n -- ) {: code:n :}
   CODE-U @ CODE-CAP >= IF s" prims: atom buffer full" SPEC-RC die THEN
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
: PE-Q           A-QUOT CODE, ;
: PE-QIN         A-QUOT-IN CODE, ;
: PE-QOUT        A-QUOT-OUT CODE, ;
: ;PE-Q          A-QUOT-END CODE, ;
: PE-FINALLY     A-FINALLY CODE, ;
: PE-C2-INVOKE   A-C2-INVOKE CODE, ;

: ROW-OPEN ( n -- ) {: kind:n :}
   kind CUR-KIND !
   0 CUR-PKG-OFF !  0 CUR-PKG-LEN !
   0 CUR-REF-OFF !  0 CUR-REF-LEN !
   CODE-U @ CUR-CODE-OFF ! ;

: ROW-NAME ( -- )
   NAME-U @ CUR-NAME-OFF !
   parse-name dup CUR-NAME-LEN ! NAME-SPAN, ;

\ An input between A-QUOT and A-QUOT-END belongs to the quotation's own
\ effect, not to the row's.
: IN-ATOM ( n -- ) {: code:n :}
   code A-QUOT = IF IN-DEPTH @ 1 + IN-DEPTH ! EXIT THEN
   code A-QUOT-END = IF IN-DEPTH @ 1 - IN-DEPTH ! EXIT THEN
   code A-IN = IN-DEPTH @ 0 = and IF IN-N @ 1 + IN-N ! THEN ;

: IN-COUNT ( -- n )
   0 IN-N !  0 IN-DEPTH !
   CUR-CODE-OFF @ BI !
   BEGIN BI @ CODE-U @ < WHILE
      CODES BI @ + c@ IN-ATOM
      BI @ 1 + BI !
   REPEAT
   IN-N @ ;

: ROW-WRITE ( -- )
   ROW-N @ ROW-CAP >= IF s" prims: specification table full" SPEC-RC die THEN
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
   IN-COUNT        ROW-N @ F-MIN-IN SLOT !
   R-NONE          ROW-N @ F-REACH SLOT !
   ROW-N @ 1 + ROW-N ! ;

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
   ROW-N @ 0 <= IF s" prims: trusted-only before any row" SPEC-RC die THEN
   ROW-N @ 1 - F-FLAGS ROW-FIELD dup @ FL-TRUSTED-ONLY or swap ! ;

\ The machine body uses the row's widths and glue without instantiating them.
: EFIXED-ABI! ( -- )
   ROW-N @ 0 <= IF s" prims: fixed ABI before any row" SPEC-RC die THEN
   ROW-N @ 1 - F-FLAGS ROW-FIELD dup @ FL-FIXED-ABI or swap ! ;

\ The just-written row is one an x86-64 captured runtime provides. Its record
\ stays in every build; in a seeded x86-64 build its body jumps through the
\ row's dispatch cell, which the captured runtime fills, and a build with no
\ captured runtime keeps the backend body. The x86-64 backend registers the
\ cell with the body, and COMPLETE-PROVIDED refuses a marked row registered
\ without one, or a seeded build whose captured runtime leaves the cell empty.
\ An ARM64 record is the assembly body and registers no cell; the body itself
\ jumps through a filled cell.
: EPREFIX-PROVIDED! ( -- )
   ROW-N @ 0 <= IF s" prims: prefix-provided before any row" SPEC-RC die THEN
   ROW-N @ 1 - F-FLAGS ROW-FIELD dup @ FL-PREFIX-PROVIDED or swap ! ;

\ The just-written row's minimum input depth, for a primitive whose body reads
\ the stack although its atoms state no input: the elaborated `execute`,
\ `catch`, `evaluate`, `?dup` and `2>r`, and `finally`. ENGINE-PRIMS:DNAME
\ bakes every row's depth into the primitive's seed record, where the interpret
\ band refuses a shallower stack before the body runs. A row whose atoms state
\ its inputs is refused, so a depth has one source.
: EMIN-IN! ( n -- ) {: depth:n :}
   ROW-N @ 0 <= IF s" prims: min-in before any row" SPEC-RC die THEN
   depth 0 <= IF s" prims: min-in must be positive" SPEC-RC die THEN
   ROW-N @ 1 - F-MIN-IN ROW-FIELD @ 0 <>
      IF s" prims: min-in on a row whose atoms state its inputs" SPEC-RC die THEN
   depth ROW-N @ 1 - F-MIN-IN ROW-FIELD ! ;

: REACH-CODE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" stack" CORE-STR= IF R-STACK EXIT THEN
   a u s" memory" CORE-STR= IF R-MEMORY EXIT THEN
   a u s" data" CORE-STR= IF R-DATA EXIT THEN
   a u s" source" CORE-STR= IF R-SOURCE EXIT THEN
   a u s" dict" CORE-STR= IF R-DICT EXIT THEN
   a u s" run" CORE-STR= IF R-RUN EXIT THEN
   a u s" exit" CORE-STR= IF R-EXIT EXIT THEN
   s" prims: unknown reach" SPEC-RC die ;

: REACH ( -- )
   ROW-N @ 0 <= IF s" prims: reach before any row" SPEC-RC die THEN
   ROW-N @ 1 - F-REACH ROW-FIELD @ R-NONE <>
      IF s" prims: second reach on one row" SPEC-RC die THEN
   parse-name REACH-CODE ROW-N @ 1 - F-REACH ROW-FIELD ! ;

: ELAB: ( -- )
   K-ELAB ROW-OPEN ROW-NAME ROW-WRITE ;

: UNROWED: ( -- )
   K-UNROWED ROW-OPEN ROW-NAME ROW-WRITE ;

public

: COUNT ( -- n )
   ROW-N @ ;

: KIND@ ( n -- n )
   F-KIND ROW-FIELD @ ;

: NAME$ ( n -- ptr u8 n ) {: row:n :}
   NAMES row F-NAME-OFF ROW-FIELD @ +  row F-NAME-LEN ROW-FIELD @ ;

: PKG$ ( n -- ptr u8 n ) {: row:n :}
   NAMES row F-PKG-OFF ROW-FIELD @ +  row F-PKG-LEN ROW-FIELD @ ;

: REF$ ( n -- ptr u8 n ) {: row:n :}
   NAMES row F-REF-OFF ROW-FIELD @ +  row F-REF-LEN ROW-FIELD @ ;

: TRUSTED-ONLY? ( n -- bool )
   F-FLAGS ROW-FIELD @ FL-TRUSTED-ONLY and 0 <> ;

: FIXED-ABI? ( n -- bool )
   F-FLAGS ROW-FIELD @ FL-FIXED-ABI and 0 <> ;

: PREFIX-PROVIDED? ( n -- bool )
   F-FLAGS ROW-FIELD @ FL-PREFIX-PROVIDED and 0 <> ;

: MIN-IN ( n -- n )
   F-MIN-IN ROW-FIELD @ ;

: REACH@ ( n -- n )
   F-REACH ROW-FIELD @ ;

: CODE-LEN@ ( n -- n )
   F-CODE-LEN ROW-FIELD @ ;

: CODE@ ( n n -- n ) {: row:n idx:n :}
   idx 0 < idx row F-CODE-LEN ROW-FIELD @ >= or
      IF s" prims: atom index out of range" SPEC-RC die THEN
   CODES row F-CODE-OFF ROW-FIELD @ + idx + c@ ;

\ Exact spelling: a body registered under a different case is a different row
\ and must say so. The index lives on the stack: a lookup leaves no state in
\ the image.
: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
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
EPRIM: finally PE-FINALLY EPRIM; REACH run
2 EMIN-IN!                           \ the body xt and the cleanup xt
EPRIM: c2-invoke PE-C2-INVOKE EPRIM;
3 EMIN-IN!                           \ body, cleanup, and result-code finisher
ETRUSTED-ONLY!                       \ the C2 runtime owns the exceptional row
\ The trusted runtime passes the frame pointer. The authenticated call site's
\ private width descriptors are consumed by this narrow machine transfer.
EPRIM: c2-init-stow
   PE-PTR-U8 PE-IN PE-N PE-IN PE-A PE-IN PE-B PE-IN
   PE-C PE-IN PE-PTR-N PE-IN
   PE-PTR-U8 PE-OUT PE-N PE-OUT PE-B PE-OUT EPRIM;
ETRUSTED-ONLY!
EPRIM: c2-records-stow
   PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PE-A PE-IN PE-B PE-IN
   PE-C PE-IN PE-PTR-N PE-IN
   PE-PTR-U8 PE-OUT PE-N PE-OUT PE-B PE-OUT EPRIM;
ETRUSTED-ONLY!

EPRIM: source-file-run PE-Q ;PE-Q PE-IN EPRIM;
ETRUSTED-ONLY!                       \ runs a file the loader opened as its own source unit
EPPRIM: SOURCE-ROOT source-file-run PE-Q ;PE-Q PE-IN ECLOSE-PRIVATE

EPRIM: dup   PE-A PE-IN  PE-A PE-OUT PE-A PE-OUT REF PRIM-REF:S-DUP EPRIM; REACH stack
EPRIM: drop  PE-A PE-IN REF PRIM-REF:S-DROP EPRIM; REACH stack
EPRIM: swap  PE-A PE-IN PE-B PE-IN  PE-B PE-OUT PE-A PE-OUT REF PRIM-REF:S-SWAP EPRIM; REACH stack
EPRIM: over  PE-A PE-IN PE-B PE-IN  PE-A PE-OUT PE-B PE-OUT PE-A PE-OUT
            REF PRIM-REF:S-OVER EPRIM; REACH stack
EPRIM: nip   PE-A PE-IN PE-B PE-IN  PE-B PE-OUT REF PRIM-REF:S-NIP EPRIM; REACH stack
EPRIM: tuck  PE-A PE-IN PE-B PE-IN  PE-B PE-OUT PE-A PE-OUT PE-B PE-OUT
            REF PRIM-REF:S-TUCK EPRIM; REACH stack
EPRIM: rot   PE-A PE-IN PE-B PE-IN PE-C PE-IN  PE-B PE-OUT PE-C PE-OUT PE-A PE-OUT
            REF PRIM-REF:S-ROT EPRIM; REACH stack
EPRIM: -rot  PE-A PE-IN PE-B PE-IN PE-C PE-IN  PE-C PE-OUT PE-A PE-OUT PE-B PE-OUT
            REF PRIM-REF:S-UNROT EPRIM; REACH stack
EPRIM: 2dup  PE-A PE-IN PE-B PE-IN  PE-A PE-OUT PE-B PE-OUT PE-A PE-OUT PE-B PE-OUT
            REF PRIM-REF:S-2DUP EPRIM; REACH stack
EPRIM: 2drop PE-A PE-IN PE-B PE-IN REF PRIM-REF:S-2DROP EPRIM; REACH stack
EPRIM: 2swap PE-A PE-IN PE-B PE-IN PE-C PE-IN PE-D PE-IN
            PE-C PE-OUT PE-D PE-OUT PE-A PE-OUT PE-B PE-OUT REF PRIM-REF:S-2SWAP EPRIM; REACH stack
EPRIM: 2over PE-A PE-IN PE-B PE-IN PE-C PE-IN PE-D PE-IN
            PE-A PE-OUT PE-B PE-OUT PE-C PE-OUT PE-D PE-OUT PE-A PE-OUT PE-B PE-OUT
            REF PRIM-REF:S-2OVER EPRIM; REACH stack

EPRIM: +      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:ADD EPRIM; REACH stack
\ The PE-PTR-B-BASE rows precede `ptr a ptr a`, so `X NULL-PTR -` matches the
\ null's pointee alone and publishes no null kind.
EPRIM: +      PE-PTR-A-OFF PE-IN PE-N PE-IN  PE-PTR-A PE-OUT EPRIM; REACH stack
EPRIM: +      PE-N PE-IN PE-PTR-A-OFF PE-IN  PE-PTR-A PE-OUT EPRIM; REACH stack
EPRIM: -      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:SUB EPRIM; REACH stack
EPRIM: -      PE-PTR-A-OFF PE-IN PE-N PE-IN  PE-PTR-A PE-OUT EPRIM; REACH stack
EPRIM: -      PE-PTR-A-ADDR PE-IN PE-PTR-B-BASE PE-IN  PE-N PE-OUT EPRIM; REACH stack
EPRIM: -      PE-PTR-B-BASE PE-IN PE-PTR-A-ADDR PE-IN  PE-N PE-OUT EPRIM; REACH stack
EPRIM: -      PE-PTR-A-ADDR PE-IN PE-PTR-A-ADDR PE-IN  PE-N PE-OUT EPRIM; REACH stack
EPRIM: *      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:MUL EPRIM; REACH stack
EPRIM: and    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:BAND EPRIM; REACH stack
EPRIM: and    PE-F PE-IN PE-F PE-IN  PE-F PE-OUT REF PRIM-REF:BAND-F EPRIM; REACH stack
EPRIM: or     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:BOR EPRIM; REACH stack
EPRIM: or     PE-F PE-IN PE-F PE-IN  PE-F PE-OUT REF PRIM-REF:BOR-F EPRIM; REACH stack
EPRIM: xor    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:BXOR EPRIM; REACH stack
EPRIM: xor    PE-F PE-IN PE-F PE-IN  PE-F PE-OUT REF PRIM-REF:BXOR-F EPRIM; REACH stack
EPRIM: 1+     PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:INC EPRIM; REACH stack
EPRIM: 1+     PE-PTR-A-OFF PE-IN  PE-PTR-A PE-OUT EPRIM; REACH stack
EPRIM: 1-     PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:DEC EPRIM; REACH stack
EPRIM: 1-     PE-PTR-A-OFF PE-IN  PE-PTR-A PE-OUT EPRIM; REACH stack
EPRIM: negate PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:NEG EPRIM; REACH stack
EPRIM: invert PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:BNOT EPRIM; REACH stack
EPRIM: 0=     PE-A PE-IN  PE-F PE-OUT EPRIM; REACH stack
EPRIM: 0<     PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:ZERO-NEG? EPRIM; REACH stack
EPRIM: =      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:EQ EPRIM; REACH stack
EPRIM: =      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT EPRIM; REACH stack
EPRIM: <      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:LT EPRIM; REACH stack
EPRIM: <      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT EPRIM; REACH stack
EPRIM: >      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:GT EPRIM; REACH stack
EPRIM: >      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT EPRIM; REACH stack
EPRIM: <>     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:NEQ EPRIM; REACH stack
EPRIM: <>     PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT EPRIM; REACH stack
EPRIM: <=     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:LE EPRIM; REACH stack
EPRIM: <=     PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT EPRIM; REACH stack
EPRIM: >=     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:GE EPRIM; REACH stack
EPRIM: >=     PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT EPRIM; REACH stack
EPRIM: /      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:DIV EPRIM; REACH stack
EPRIM: mod    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:REM EPRIM; REACH stack
EPRIM: /mod   PE-N PE-IN PE-N PE-IN  PE-N PE-OUT PE-N PE-OUT REF PRIM-REF:DIVREM EPRIM; REACH stack
EPRIM: abs    PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:ABSOLUTE EPRIM; REACH stack
EPRIM: min    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:MINIMUM EPRIM; REACH stack
EPRIM: max    PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:MAXIMUM EPRIM; REACH stack
EPRIM: lshift PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:SHL EPRIM; REACH stack
EPRIM: rshift PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:SHR EPRIM; REACH stack
EPRIM: cells  PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:CELL-BYTES EPRIM; REACH stack
EPRIM: cell+  PE-PTR-A-OFF PE-IN  PE-PTR-A PE-OUT EPRIM; REACH stack
EPRIM: cell+  PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:CELL-STEP EPRIM; REACH stack
EPRIM: chars  PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:CHAR-BYTES EPRIM; REACH stack
EPRIM: char+  PE-PTR-A-OFF PE-IN  PE-PTR-A PE-OUT EPRIM; REACH stack
EPRIM: char+  PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:CHAR-STEP EPRIM; REACH stack

EPRIM: @          PE-PTR-A PE-IN  PE-A PE-OUT EPRIM; REACH memory
EPRIM: !          PE-A PE-IN PE-PTR-A PE-IN EPRIM; REACH memory
\ `xt!` is `!` plus the declaration that the cell it writes holds a code-region
\ address, for a persisted cell whose address the caller works out at run time
\ from a table base and a row index.
\ What the row still cannot say is that the pointee MUST be an execution token:
\ there is no quotation-kinded type variable (no TVK kind admits only
\ quotations), so `xt!` into a plain integer cell type-checks today and would
\ have the loader shift an ordinary integer.
\ A plain `!` of a quotation into such a cell is already refused.
EPRIM: xt!        PE-A PE-IN PE-PTR-A PE-IN EPRIM; REACH memory EFIXED-ABI!
EPRIM: ptr-cell-mark PE-PTR-A PE-IN EPRIM; REACH dict
EPRIM: scratch-cell-mark PE-PTR-A PE-IN EPRIM; REACH dict
\ ptr-cell! is xt!'s store for a cell that holds a DATA pointer: it writes the
\ value and declares the cell as ptr-cell-mark declares it.
EPRIM: ptr-cell!  PE-A PE-IN PE-PTR-A PE-IN EPRIM; REACH memory EFIXED-ABI!
EPRIM: snapshot-format PE-N PE-OUT EPRIM;
EPRIM: ptr-field  PE-PTR-A PE-IN PE-N PE-IN  PE-PTR-PTR-B PE-OUT EPRIM; REACH stack
\ Explicit memory views preserve address bits while changing the access type.
EPRIM: byte-view  PE-PTR-A PE-IN PE-PTR-U8 PE-OUT EPRIM; REACH stack
EPRIM: cell-view  PE-PTR-U8 PE-IN PE-PTR-N PE-OUT EPRIM; REACH stack
\ The address as a number, for a distance or a hash; the way back is a
\ package's private `CAST:`.
EPRIM: PTR>N      PE-PTR-A-ADDR PE-IN PE-N PE-OUT EPRIM; REACH stack
EPRIM: +!         PE-N PE-IN PE-PTR-N PE-IN REF PRIM-REF:ADD-TO EPRIM; REACH memory
EPRIM: c@         PE-PTR-U8 PE-IN  PE-U8 PE-OUT EPRIM; REACH memory
EPRIM: c!         PE-U8 PE-IN PE-PTR-U8 PE-IN EPRIM; REACH memory
EPRIM: atomic@    PE-PTR-A PE-IN  PE-A PE-OUT EPRIM; REACH memory
EPRIM: atomic!    PE-A PE-IN PE-PTR-A PE-IN EPRIM; REACH memory
EPRIM: atomic-add PE-N PE-IN PE-PTR-N PE-IN  PE-N PE-OUT EPRIM; REACH memory
EPRIM: atomic-cas PE-A PE-IN PE-A PE-IN PE-PTR-A PE-IN  PE-A PE-OUT EPRIM; REACH memory
EPRIM: fence      EPRIM; REACH memory
EPRIM: run-in-stack PE-Q ;PE-Q PE-IN PE-PTR-U8 PE-IN PE-N PE-IN EPRIM;
\ `evaluate-closed` runs its text on a data stack of its own and
\ throws E-EVAL-RESIDUE when the text leaves cells, so whatever the text does
\ its net effect is ( -- ) and the row states the string alone. `evaluate`,
\ whose effect is the text's, stays elaborated below and unsafe in a body.
EPRIM: evaluate-closed PE-PTR-U8 PE-IN PE-N PE-IN EPRIM; REACH run
EPRIM: evaluate-range PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
EPREFIX-PROVIDED!
EPPRIM: SOURCE-ROOT evaluate-range
   PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPRIM: count      PE-PTR-U8 PE-IN  PE-PTR-U8 PE-OUT PE-N PE-OUT REF PRIM-REF:COUNTED$ EPRIM; REACH memory

EPRIM: .            PE-N PE-IN EPRIM;
EPRIM: .s           EPRIM;
EPRIM: depth        PE-N PE-OUT EPRIM; REACH stack
EPRIM: here         PE-PTR-A-RAW PE-OUT EPRIM; REACH data
EPRIM: tok-imm?     PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-OUT EPRIM;
EPRIM: scope-kind?  PE-N PE-IN PE-N PE-OUT EPRIM;
EPRIM: allot        PE-N PE-IN EPRIM; REACH data
EPRIM: align        EPRIM; REACH data
EPRIM: ,            PE-N PE-IN EPRIM; REACH data
EPRIM: c,           PE-N PE-IN EPRIM; REACH data
EPRIM: type         PE-PTR-U8 PE-IN PE-N PE-IN EPRIM;
EPRIM: throw        PE-N PE-IN EPRIM; REACH stack
EPRIM: die          PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN EPRIM; REACH exit

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
EPRIM: patch32       PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ code injection: only a TRUSTED: boundary may emit machine code
\ Rewriting a write-protected dictionary record cell is package XREF's to do.
EPPRIM: XREF patch32 PE-N PE-IN PE-PTR-N PE-IN ECLOSE-PRIVATE
\ Planting and clearing a breakpoint instruction is package DEBUG's.
EPPRIM: DEBUG patch32 PE-N PE-IN PE-PTR-U8 PE-IN ECLOSE-PRIVATE
\ The four relocation-record primitives below, per target.
\ `code-publish ( src dst len -- )` copies an emission into the code arena,
\ `callmap-set ( addr -- )` and `addrmap-set ( addr -- )` record a call or an
\ address literal at a region address, and `reloc-maps-clear ( start len -- )`
\ forgets the sites of a reclaimed span. The record is SNAP-RELOC's band.
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
\   recorded offset, or a row past SITE-CAP, exits SITE-RC. `code-publish`
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
\ ENGINE-ERROR:SEAL-VIOLATION.
\
\ THE PUBLISHER OWNS ITS PRIMITIVES. The rows closed ECLOSE-PRIVATE for NPUB
\ resolve only inside that package, so the publisher's own words are checked
\ code, and a checked caller anywhere else is refused by name. code-publish,
\ xref-retarget and does-record (below) are that row alone: outside NPUB the
\ name is undefined to a checked caller. A global trusted-only row beside an
\ owner's row refuses a checked caller even a `trust` row would admit.
EPPRIM: NPUB code-publish PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPRIM: callmap-set   PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ relocation metadata for code the publisher just wrote
EPPRIM: NPUB callmap-set PE-N PE-IN ECLOSE-PRIVATE
EPRIM: addrmap-set   PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ the same, for an address chain the publisher just wrote
EPPRIM: NPUB addrmap-set PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: NPUB xref-retarget PE-N PE-IN PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ Record writers: trusted-only rows, so no `trust` row admits a checked caller.
EPRIM: int-mark      PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ sets DNAME-INT on one live record
EPRIM: rep-mark      PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ sets DNAME-REP on one live CAST: record
EPRIM: min-in-mark   PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ records a certified minimum input arity on one live record
EPPRIM: NPUB min-in-mark PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: INTERPRETER min-in-mark PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: NPUB wide-mark ECLOSE-PRIVATE
EPRIM: reloc-maps-clear PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ clears metadata over reclaimed code
EPPRIM: CODE-RECLAIM reloc-maps-clear PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPRIM: does-patch PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN EPRIM; REACH dict
ETRUSTED-ONLY!                       \ native defining-word runtime patch
\ `does-record ( entry len -- )` records the pending definition's `;does`
\ clause, writes its name at CP, zero-padded to a code slot, and sets CLAUSE-CELL.
EPPRIM: NPUB does-record PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPRIM: write         PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
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
EPRIM: cp!            PE-N PE-IN EPRIM;
EPRIM: dbase@         PE-N PE-OUT EPRIM;
EPRIM: check@         PE-N PE-OUT EPRIM;
\ The compiler's prospective trusted-tick order and the owner whose installed
\ query established it. Order 0 is checking first; 1 is trusted gate first.
EPRIM: tick-order@    PE-N PE-OUT PE-PTR-U8 PE-OUT EPRIM;
EPRIM: set-check     PE-A PE-IN EPRIM;
ETRUSTED-ONLY!
EFIXED-ABI!
EPRIM: set-preflight PE-A PE-IN EPRIM;
ETRUSTED-ONLY!
EFIXED-ABI!
EPRIM: set-top-check PE-A PE-IN EPRIM;
ETRUSTED-ONLY!
EFIXED-ABI!
\ The top-level tracker's own install: its row takes a hook of the shape the
\ tracker is called with, where the global row above takes any one cell, as
\ set-check and set-preflight do.
EPPRIM: CHECK set-top-check
   PE-Q PE-PTR-U8 PE-QIN PE-N PE-QIN PE-N PE-QIN PE-N PE-QIN ;PE-Q PE-IN ECLOSE-PRIVATE
EPRIM: ndict@         PE-N PE-OUT EPRIM;
EPRIM: ndict!         PE-N PE-IN EPRIM;
EPRIM: ndict-append   PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ native pending-record publication
EPPRIM: NPUB ndict-append PE-N PE-IN ECLOSE-PRIVATE
EPRIM: def-occ-select PE-PTR-N PE-IN PE-N PE-OUT PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!                       \ exact resolved dictionary record
EPPRIM: DEF-OCC def-occ-select PE-PTR-N PE-IN PE-N PE-OUT PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: CHECKER-REG def-occ-select PE-PTR-N PE-IN PE-N PE-OUT PE-N PE-OUT ECLOSE-PRIVATE
EPRIM: def-occ-resolve PE-N PE-IN PE-N PE-IN PE-PTR-N PE-OUT EPRIM;
ETRUSTED-ONLY!                       \ slot and nonzero process-local occurrence
EPPRIM: DEF-OCC def-occ-resolve PE-N PE-IN PE-N PE-IN PE-PTR-N PE-OUT ECLOSE-PRIVATE
\ What `package`, `export`, `:`, `create`, `does>`, `;`, `immediate`, `cast:`
\ and `defer` change is sealed state after the seal: CUR, WIDN, DEF-WL, TSIG,
\ TCSIG, PKG-*, the body text, COMP-LATCH-CELL and the records behind the
\ PROT window.
\ These fifteen rows are how an interpreter written in Habu writes it. A
\ refusal exits and never throws: 79 on a task's
\ thread (the five dictionary rows), 84 for a protected wid after
\ the seal (`alias-record`, `def-open`) and 83 for every other refusal. A
\ caller checks first and prints the engine's own text. Two refusals are
\ compile dies instead, which `evaluate` delivers as throws: while a checker
\ overlay is open `namespace-private`, `alias-record`
\ and `def-open` name the token and die
\ ENGINE-ERROR:OVERLAY-OPEN, and `def-create`
\ dies 76 as `allot` does when DP would pass the data region. x86-64 refuses
\ `def-create` and `def-defer`: its Habu loop has NCOMP
\ compile a created or deferred word's body instead.
\
\ The three record writers store a name of at least one byte: up to DNAME-INL
\ bytes inline, a longer one at CP rounded up to a code slot, 4 bytes on ARM64
\ and 16 on x86-64, as `:` stores one. Each refuses an empty name, a live (folded name, wid) pair,
\ NDICT at DICT-CAP, a name copy that would reach the code ceiling (REGION -
\ $4000 above DBASE, compared unsigned) and a pending definition, whose record
\ occupies slot NDICT.
\
\ namespace-record ( ptr u8 n bool -- n ): publish a namespace row at NDICT and
\ answer its index: [0] a fresh wid, [8] a second fresh wid when the flag is
\ set and else 0, [40] WL:NAMESPACE. It refuses a colon at an edge of the
\ name and a second colon: a name with one is a part's path, `A:B`.
EPRIM: namespace-record PE-PTR-U8 PE-IN PE-N PE-IN PE-F PE-IN  PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!
\ The owner-private rows of these writers: inside package CHECKER-OVERLAY or
\ INTERPRETER a row wins, so that owner's
\ checked code calls the writer, and each record carries DNAME-INT|DNAME-OWNED.
EPPRIM: CHECKER-OVERLAY namespace-record PE-PTR-U8 PE-IN PE-N PE-IN PE-F PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: INTERPRETER namespace-record PE-PTR-U8 PE-IN PE-N PE-IN PE-F PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
\ namespace-private ( n -- ): give namespace row n, whose [8] is 0, a fresh
\ private wid. It refuses an index at or above NDICT, unsigned, a row that is
\ not a namespace row and one that already has a private wid.
EPRIM: namespace-private PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
EPPRIM: INTERPRETER namespace-private PE-N PE-IN ECLOSE-PRIVATE
\ alias-record ( ptr u8 n n n -- ) name, source index, wid: a record for the source's code, as
\ `export` makes. It refuses a negative wid and a source at or above NDICT, unsigned.
EPRIM: alias-record PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
EPPRIM: INTERPRETER alias-record PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ package-scope! ( n n -- ) namespace index, parent wid: PKG-PUB and PKG-PRI
\ from the row's [0] and [8], PKG-REC the row's address and PKG-PARENT the
\ wid. A row whose name holds a colon is a part, which first moves the four
\ open cells into ENCLOSING; any other row clears ENCLOSING, and
\ `-1 0` clears all eight. It refuses any other index at or above NDICT,
\ unsigned, a row that is not a namespace row, one without a private wid, and
\ a part when no package or a part is open.
\ It has no task guard of its own: the package keywords refuse one, and a throw
\ puts the scope back through this row.
EPRIM: package-scope! PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
EPPRIM: CHECKER-OVERLAY package-scope! PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: INTERPRETER package-scope! PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ def-open ( ptr u8 n n n -- ) name, wid, kind: write record NDICT unpublished,
\ [0] CP after the name, [8] 0, the kind (0, DKIND:VAL, DKIND:ADDR with
\ DNAME-ALLOC, and DNAME-CELL as well for a variable, or DKIND:CAST) in [16] and
\ the wid in [40]; PEND-CELL is that record; TSIG, TCSIG, DOESB, CLAUSE and
\ TRUSTED clear; COMP-LATCH-CELL takes the compiler latch; LASTC-CELL
\ waits for the body (def-close, def-create). It refuses a kind with a bit
\ outside DKIND:MASK, DNAME-CELL and DNAME-ALLOC, a negative wid, and CP at or
\ past the code ceiling.
EPRIM: def-open PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
EPPRIM: INTERPRETER def-open PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ body-capture ( ptr u8 n -- ): append the bytes and one space to the body text
\ through the capture routine `:` uses, which grows the text block when they do
\ not fit (rc 71 when it cannot). It refuses BODYLEN past the text capacity and
\ a BODYLEN + u + 1 that wraps, with BODYLEN and u both unsigned.
EPRIM: body-capture PE-PTR-U8 PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
EPPRIM: INTERPRETER body-capture PE-PTR-U8 PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ body-append ( ptr u8 n -- ): append the bytes to the definition `define-word`
\ or `:` opened; it refuses when no definition is open.
EPRIM: body-append PE-PTR-U8 PE-IN PE-N PE-IN EPRIM;
EPREFIX-PROVIDED!
\ literal ( n -- ): append n to the definition `define-word` opened, which the
\ checker reads as that number; it refuses when no definition is open.
EPRIM: literal PE-N PE-IN EPRIM;
EPREFIX-PROVIDED!
\ define-word ( name$ sig$ -- ): open a definition in the open package and
\ section as `: NAME ( … )` does, NAME one token and SIG the signature as source
\ writes it.
EPRIM: define-word PE-PTR-U8 PE-IN PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN EPRIM;
EPREFIX-PROVIDED!
\ postponed ( ptr u8 n -- ): what `postpone W` compiles a call of; only the
\ checker binds it.
EPRIM: postponed PE-PTR-U8 PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
EPREFIX-PROVIDED!
EPPRIM: INTERPRETER postponed PE-PTR-U8 PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ trust-sig! ( ptr u8 n -- ): the pending definition's signature span into
\ TSIG-A and TSIG-U. It refuses when no definition is pending.
EPRIM: trust-sig! PE-PTR-U8 PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
\ created-sig! ( ptr u8 n -- ): the inside of the pending definition's `does>`
\ signature, the effect of the words it creates, into TCSIG-A and TCSIG-U: its
\ copy in the body text. It refuses when no definition is
\ pending.
EPRIM: created-sig! PE-PTR-U8 PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
EPPRIM: INTERPRETER created-sig! PE-PTR-U8 PE-IN PE-N PE-IN ECLOSE-PRIVATE
\ def-close ( -- ): end the pending definition the native compiler compiled:
\ the state def-open set clears:
\ COMP-LATCH-CELL, TSIG, TCSIG, DOESB, CLAUSE, TRUSTED and PEND-CELL. Before
\ PEND-CELL clears, LASTC-CELL takes the record for a nonzero kind other than
\ DKIND:CAST, the record `does-patch` reads. It refuses when
\ no definition is pending and when the pending one is not a native compiler's
\ (COMP-LATCH-CELL zero), as ndict-append does.
EPRIM: def-close EPRIM;
ETRUSTED-ONLY!
EPPRIM: INTERPRETER def-close ECLOSE-PRIVATE
\ def-abort ( -- ): close an abandoned definition's writable code region
\ and compile-only state after a caught source error. The
\ catch's handler, return and loop stacks remain live for the caller.
EPRIM: def-abort EPRIM;
ETRUSTED-ONLY!
EPPRIM: INTERPRETER def-abort ECLOSE-PRIVATE
\ stack-clear ( -- ): the REPL's caught-line boundary discards the user's
\ arbitrary stack residue after a refusal. It leaves handler, return and loop
\ state intact so the catching interpreter can continue.
EPRIM: stack-clear EPRIM;
ETRUSTED-ONLY!
EPPRIM: INTERPRETER stack-clear ECLOSE-PRIVATE
\ def-create ( -- ): end the pending DKIND:ADDR definition: DP rounds up to a
\ cell, the
\ code at CP pushes that address, an address-map site, and returns, [8] takes
\ the code's length, LASTC the record, the record is counted and indexed and
\ the state def-open set clears, PEND-CELL with it.
\ It refuses no pending definition, a pending one of another kind and CP at
\ or past the code ceiling.
EPRIM: def-create EPRIM;
ETRUSTED-ONLY!
EPPRIM: INTERPRETER def-create ECLOSE-PRIVATE
\ imm-mark ( -- ): set DNAME-IMM on the newest record, as the engine's
\ `immediate` sets it. Like that keyword it checks
\ nothing and refuses nothing.
EPRIM: imm-mark EPRIM;
ETRUSTED-ONLY!
EPPRIM: INTERPRETER imm-mark ECLOSE-PRIVATE
\ def-cast ( -- ): end the pending DKIND:CAST definition with the empty checked
\ body. Refuses none pending, another kind, and CP at or past the code ceiling.
EPRIM: def-cast EPRIM;
ETRUSTED-ONLY!
EPPRIM: INTERPRETER def-cast ECLOSE-PRIVATE
\ def-defer ( n -- ): end the pending kind-0 definition as a call through
\ dispatch cell n. Refuses none pending, a nonzero kind, and CP at the ceiling.
EPRIM: def-defer PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
\ Only CHECKER-OVERLAY types these six: elsewhere a checked call is E-UNDEFINED
\ and a TRUSTED: one binds at tier 0 only. Refusals exit 79 on a task, else 83.
\
\ replay-open ( -- ): save NDICT, CP, WIDN, CURRENT, the open package's cells
\ and the using band in the engine's REPLAY-SCOPE band. It
\ refuses an open overlay and a pending definition.
\ replay-record ( ptr u8 n n -- ) name, wid: publish a codeless record whose
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
\ replay-close ( -- ): zero the records published since replay-open, put the
\ saved state back and zero the band. It refuses no overlay, a pending
\ definition, NDICT below the saved count or above the highest count
\ replay-record and namespace-record reached, and CP or WIDN below the saved.
\ replay-widn! ( n -- ): put WIDN back to a mark the owner read inside the
\ overlay, once its rollback dropped what it made since. It refuses no overlay
\ and a mark below the saved WIDN or above WIDN, unsigned.
EPPRIM: CHECKER-OVERLAY replay-open ECLOSE-PRIVATE
EPPRIM: CHECKER-OVERLAY replay-close ECLOSE-PRIVATE
EPPRIM: CHECKER-OVERLAY replay-widn! PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: CHECKER-OVERLAY replay-record PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: CHECKER-OVERLAY record-wid! PE-N PE-IN PE-N PE-IN ECLOSE-PRIVATE
EPPRIM: CHECKER-OVERLAY replay-private PE-N PE-IN PE-F PE-IN ECLOSE-PRIVATE
EPRIM: SEAL-CAPTURE   EPRIM;
EPRIM: seal-captured? PE-F PE-OUT EPRIM;
EPRIM: DRAIN-PRETRUST EPRIM;
\ data-base is a BASE ADDRESS: the start of the running task's DATA region, not
\ the address of any declared element. Its pointee is TVK-DBASE, so `data-base
\ OFF + @` still answers a number, an xt or a role - what those cells hold - and
\ still subtracts, compares and byte-views like any pointer, while a nominal
\ identity or an address read through it is refused at EVERY pointee depth: a
\ DATA cell is not the address of an address either.
\ A cell of the region that really holds an address
\ is reached with `ptr-field`, the declared door.
EPRIM: data-base      PE-PTR-A-DBASE PE-OUT EPRIM;
\ NULL-PTR is the language's null: a pointer of any pointee, and the address OF
\ nothing. Its value is zero, so the body pushes zero and reads no memory.
\ TVK-NULL keeps every honest use: storing a null into a declared pointer cell,
\ comparing one, subtracting one, testing it with 0= - each binds the pointee
\ INSIDE a `ptr`, where the kind is permissive, and each means this one literal
\ address. What it refuses is a value read THROUGH it: `NULL-PTR + @` taken as
\ a nominal identity or as an address certified and forged one.
EPRIM: NULL-PTR       PE-PTR-A-NULL PE-OUT EPRIM; REACH stack
\ A null result means the running kernel has no origin record. Source reaches
\ the three optional cells through this byte pointer and the running engine's
\ NATIVE-BODY-ABI indices; only the engine's own emitters bake their offsets.
EPRIM: native-origin-cell PE-PTR-U8 PE-OUT EPRIM;
ETRUSTED-ONLY!
EPRIM: prot-wid-add   PE-N PE-IN EPRIM; REACH dict
EPRIM: prot-wid-room  PE-N PE-OUT EPRIM;
\ The design seal's two writers: admit a
\ package's public wordlist by name, then seal the dictionary as it stands. Both
\ refuse once sealed, so a sealed design cannot widen its own vocabulary.
EPRIM: policy-admit   PE-PTR-U8 PE-IN PE-N PE-IN EPRIM;
EPRIM: policy-seal    EPRIM;
EPRIM: wordlist       PE-N PE-OUT EPRIM; REACH dict
EPRIM: get-current    PE-N PE-OUT EPRIM; REACH dict
EPRIM: set-current    PE-N PE-IN EPRIM; REACH dict
EPRIM: search-wl      PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: xref-search-wl PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-PTR-N PE-OUT EPRIM;
ETRUSTED-ONLY!
\ A row per owner, not one: each owner is a separate package, and NDICT
\ keeps the raw record private.
EPPRIM: CHECKER-RESOLVE xref-search-wl PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-PTR-N PE-OUT ECLOSE-PRIVATE
EPPRIM: NDICT xref-search-wl PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-PTR-N PE-OUT ECLOSE-PRIVATE
EPPRIM: INTERPRETER xref-search-wl PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-PTR-N PE-OUT ECLOSE-PRIVATE
EPPRIM: GENERATED-DECL-NAME-PREFLIGHT xref-search-wl PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-PTR-N PE-OUT ECLOSE-PRIVATE
EPPRIM: XREF xref-search-wl PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-PTR-N PE-OUT ECLOSE-PRIVATE
EPRIM: scope-find     PE-PTR-U8 PE-IN PE-N PE-IN
                      PE-PTR-N PE-OUT PE-PTR-N PE-OUT PE-PTR-N PE-OUT PE-N PE-OUT EPRIM;
EPRIM: parse-name     PE-PTR-U8 PE-OUT PE-N PE-OUT EPRIM; REACH source
\ num-parse ( ptr u8 n -- n bool bool ) : the engine's own number reader, over
\ bytes the caller already holds - the routine the interpret and compile
\ dispatches call for every literal token. It
\ answers the value, whether that value is a double's bits, and whether the
\ bytes were a number at all. A checked stage that has to know what cell a
\ literal spelling stands for asks this rather than reading the spelling back
\ with a second decoder.
EPRIM: num-parse      PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PE-F PE-OUT PE-F PE-OUT EPRIM; REACH memory
EPRIM: wide-mark EPRIM;
\ record-facts! ( n n -- ) offset, index: stores where a record's facts text
\ lies.
EPRIM: record-facts! PE-N PE-IN PE-N PE-IN EPRIM;
\ sha256-blocks ( ptr n ptr u8 n -- n ) state cells, bytes, block count: how
\ many blocks the CPU's SHA-256 instructions compressed.
EPRIM: sha256-blocks PE-PTR-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM; REACH memory
EPRIM: ffi-call       PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!
EPRIM: ffi-call-n     PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!
EPRIM: task-entry PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!                       \ C ABI entry address, never a Habu quotation
EPPRIM: TASK task-entry PE-N PE-OUT ECLOSE-PRIVATE
EPRIM: dlopen-entry PE-N PE-OUT EPRIM;
EPRIM: dlsym-entry PE-N PE-OUT EPRIM;
\ callback-entry -1 answers the shared raw C thunk for a compiler-owned stub.
\ Nonnegative IDs answer the boot stubs.
EPRIM: callback-entry PE-N PE-IN PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!                       \ C ABI entry address, never a Habu quotation
EPPRIM: FFI callback-entry PE-N PE-IN PE-N PE-OUT ECLOSE-PRIVATE
EPRIM: ffi-call-abi    PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                      PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!
EPRIM: ffi-call-abi-r  PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                      PE-R PE-OUT EPRIM;
ETRUSTED-ONLY!
EPRIM: ffi-call-struct PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                      PE-N PE-IN  PE-N PE-OUT PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!

\ FFI's checked words call these by its private rows; the global trusted-only
\ rows above refuse any other checked caller, even past a `trust` row.
EPPRIM: FFI ffi-call-n PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-abi PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                        PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-abi-r PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                          PE-R PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-struct PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                           PE-N PE-IN  PE-N PE-OUT PE-N PE-OUT ECLOSE-PRIVATE

EPRIM: f+      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT EPRIM; REACH stack
EPRIM: f-      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT EPRIM; REACH stack
EPRIM: f*      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT EPRIM; REACH stack
EPRIM: f/      PE-R PE-IN PE-R PE-IN  PE-R PE-OUT EPRIM; REACH stack
EPRIM: fnegate PE-R PE-IN  PE-R PE-OUT EPRIM; REACH stack
EPRIM: fabs    PE-R PE-IN  PE-R PE-OUT EPRIM; REACH stack
EPRIM: fsqrt   PE-R PE-IN  PE-R PE-OUT EPRIM; REACH stack
EPRIM: f<      PE-R PE-IN PE-R PE-IN  PE-F PE-OUT EPRIM; REACH stack
EPRIM: f>      PE-R PE-IN PE-R PE-IN  PE-F PE-OUT EPRIM; REACH stack
EPRIM: f=      PE-R PE-IN PE-R PE-IN  PE-F PE-OUT EPRIM; REACH stack
EPRIM: f0<     PE-R PE-IN  PE-F PE-OUT EPRIM; REACH stack
EPRIM: f0=     PE-R PE-IN  PE-F PE-OUT EPRIM; REACH stack
EPRIM: s>f     PE-N PE-IN  PE-R PE-OUT EPRIM; REACH stack
EPRIM: f>s     PE-R PE-IN  PE-N PE-OUT EPRIM; REACH stack
EPRIM: f.      PE-R PE-IN EPRIM;
EPRIM: emit   PE-N PE-IN EPRIM;
EPRIM: cr     EPRIM;
EPRIM: space  EPRIM;
EPRIM: u.     PE-N PE-IN EPRIM;

EPRIM: create   EPRIM; REACH dict
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
EPRIM: execute-floor PE-N PE-IN  PE-F PE-OUT EPRIM;
ETRUSTED-ONLY!
EPPRIM: INTERPRETER execute-floor PE-N PE-IN  PE-F PE-OUT ECLOSE-PRIVATE

\ Primitives whose effect the checker elaborates.
\ These have engine bodies and deliberately no row: their effect depends on the
\ call site. A row here
\ states that, so the completeness gate accepts the body without an effect.
ELAB: execute REACH run
1 EMIN-IN!
ELAB: catch REACH run
1 EMIN-IN!
ELAB: evaluate
2 EMIN-IN!
EPREFIX-PROVIDED!
ELAB: ?dup REACH stack
1 EMIN-IN!
ELAB: 2>r REACH stack
2 EMIN-IN!
ELAB: 2r> REACH stack
ELAB: 2r@ REACH stack

\ Bodies with no declaration anywhere, so no checked program can name one: a
\ checked caller of `run-rc` is E-UNDEFINED. Only unchecked source
\ or the kernel harness reaches them. They are
\ rows so the gap is stated instead of silent.
UNROWED: run-rc
UNROWED: top-check@

;package
