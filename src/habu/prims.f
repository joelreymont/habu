\ prims.f — the engine primitive specification, in one machine-independent table.
\
\ An engine primitive used to be two things in two files that nothing tied
\ together: an ARM64 body registered in src/habu/habu1.f (`s" dup" ['] BDUP
\ FPRIM-L`) and a `PRIM:` row in src/core/checker.f stating its effect. A second
\ backend would have forked that list, and the drift was already there —
\ `run-rc` and `top-check@` have engine bodies and no checker row at all, so no
\ checked program can call either and nothing said so.
\
\ This file is the single specification. Each row names one primitive, states
\ its checker effect once, and carries the two attachment points a backend and a
\ reference implementation bind to BY NAME:
\
\   EPRIM: dup   PE-A PE-IN  PE-A PE-OUT PE-A PE-OUT  REF PRIM-REF:S-DUP  EPRIM;
\   EPRIM: +     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT   REF PRIM-REF:ADD    EPRIM;
\
\ - src/core/checker.f replays the table onto its own `PE-*` row constructors at
\   the point its table used to hold these rows, so the effects exist here and
\   nowhere else.
\ - each backend registers a body under the row's name; src/habu/habu1.f refuses
\   a body whose name has no row, and src/habu/habu2.f's completeness gate
\   refuses a row that no body answers.
\ - `REF` names a reference implementation in checked Habu (src/habu/prim-ref.f,
\   package PRIM-REF), which test/prim-parity.f runs beside each backend's body
\   over the same cases. A reference never shadows a primitive spelling, so the
\   clause spells `PRIM-REF:ADD`, not `PRIM-REF:+`. A row carries one only where
\   the semantics can be written WITHOUT the primitive it answers for and where
\   the gate exercises it; the overloads of a name state it on the row whose
\   instantiation the gate runs.
\
\ WHY IT IS DATA AND WHY IT LOADS BEFORE THE CHECKER. A row must be registered
\ before checker.f's `PTABLE-END`: that word sets `USIGS-USER-OFF`, and a row
\ appended past it is a USER effect record for its symbol, which collapses an
\ overload set (`+` has three rows) to whichever record is newest. checker.f is
\ one file, so a table loaded after it cannot land inside its row region — hence
\ this file loads BEFORE checker.f and holds the rows as an atom-code stream that
\ checker.f replays. The atom spellings below are the checker's own `PE-*` names,
\ package-private here, so a row reads exactly as it did when it lived there.
\
\ Everything in this file is data construction over engine primitives only: it
\ loads ahead of src/core/checker.f, so nothing from the checker, the include
\ machinery or the checked library is in scope.

package PRIM-SPEC

public

\ ---- row kinds ---------------------------------------------------------------
\ K-ELAB and K-UNROWED carry no effect: they are the two ways a primitive can
\ have a body and no table row, named here so the completeness gate passes by an
\ explicit row rather than by silence.
0 constant K-PRIM            \ a global row, closed by EPRIM;
1 constant K-PKG-PRIVATE     \ a package row closed by ECLOSE-PRIVATE: only the
                             \ owning package's checked bodies resolve it
2 constant K-ELAB            \ the checker elaborates this primitive's effect
3 constant K-UNROWED         \ no checker row anywhere; no checked caller exists

\ ---- atom codes --------------------------------------------------------------
\ One code per checker row constructor. A composite atom is a short sequence:
\ PE-PTR-A is A-VAR-A A-PTR, PE-PTR-A-RAW is A-VAR-A A-RAW A-PTR.
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

private

$4C constant SPEC-RC

\ ---- storage -----------------------------------------------------------------
\ Rows are fixed-width records; names, package names and reference names are
\ byte spans in NAMES; atoms are bytes in CODES. Offsets, not addresses, so the
\ table can be walked by index from any consumer.
$180 constant ROW-CAP        \ 384 rows; 222 are used today
10 constant ROW-CELLS
$1000 constant NAME-CAP
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

create ROWS  ROW-CAP ROW-CELLS * cells allot
create NAMES NAME-CAP allot
create CODES CODE-CAP allot
variable ROW-N
variable NAME-U
variable CODE-U
variable BI
variable FI

\ The row under construction, between an opener and its closer.
variable CUR-KIND
variable CUR-NAME-OFF   variable CUR-NAME-LEN
variable CUR-PKG-OFF    variable CUR-PKG-LEN
variable CUR-CODE-OFF
variable CUR-REF-OFF    variable CUR-REF-LEN

\ SLOT addresses a row the caller has already bounded; ROW-FIELD is what everything
\ else uses and refuses an index the table does not hold, so a reader cannot walk
\ off the end of ROWS. Only ROW-WRITE needs SLOT: it fills the row at ROW-N
\ before ROW-N counts it.
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

\ ---- the row atoms -----------------------------------------------------------
\ Spelled like src/core/checker.f's row constructors, and private to this
\ package so both spellings can exist: the checker's build terms, these append
\ the code that names one.
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
: PE-PTR-A-OFF   A-VAR-A CODE, A-OFF CODE, A-PTR CODE, ;
: PE-Q           A-QUOT CODE, ;
: ;PE-Q          A-QUOT-END CODE, ;
: PE-FINALLY     A-FINALLY CODE, ;

\ ---- openers and closers -----------------------------------------------------
: ROW-OPEN ( n -- ) {: kind:n :}
   kind CUR-KIND !
   0 CUR-PKG-OFF !  0 CUR-PKG-LEN !
   0 CUR-REF-OFF !  0 CUR-REF-LEN !
   CODE-U @ CUR-CODE-OFF ! ;

: ROW-NAME ( -- )
   NAME-U @ CUR-NAME-OFF !
   parse-name dup CUR-NAME-LEN ! NAME-SPAN, ;

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

\ REF names the reference implementation in checked Habu that a parity gate runs
\ beside each backend's body for this row. It is read back with REF$.
: REF ( -- )
   NAME-U @ CUR-REF-OFF !
   parse-name dup CUR-REF-LEN ! NAME-SPAN, ;

\ The just-written row's checker row is a trust boundary: a CHECKED caller is
\ refused, while a TRUSTED: body may use it (checker.f PRIM-TRUSTED-ONLY!).
: ETRUSTED-ONLY! ( -- )
   ROW-N @ 0 <= IF s" prims: trusted-only before any row" SPEC-RC die THEN
   ROW-N @ 1 - F-FLAGS ROW-FIELD dup @ FL-TRUSTED-ONLY or swap ! ;

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

: CODE-LEN@ ( n -- n )
   F-CODE-LEN ROW-FIELD @ ;

: CODE@ ( n n -- n ) {: row:n idx:n :}
   idx 0 < idx row F-CODE-LEN ROW-FIELD @ >= or
      IF s" prims: atom index out of range" SPEC-RC die THEN
   CODES row F-CODE-OFF ROW-FIELD @ + idx + c@ ;

\ The row index for a name, -1 when the table has none. Exact spelling: a body
\ registered under a different case is a different row and must say so.
: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 FI !
   BEGIN FI @ ROW-N @ < WHILE
      FI @ NAME$ a u CORE-STR= IF FI @ EXIT THEN
      FI @ 1 + FI !
   REPEAT
   -1 ;

private

\ ---- the table ---------------------------------------------------------------
EPRIM: finally PE-FINALLY EPRIM;

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
\ THE INTEGER-OFFSET ROWS carry PE-PTR-A-OFF on the pointee they step (the same
\ `a` the result keeps, so the pointee is still preserved). The mark is inert for
\ every pointee but the null: a null that rides one of these rows comes out a
\ DATA-base pointer, because the address it answers is the caller's arithmetic
\ and no longer the one literal address the null's permissive pointee arm is for
\ (dot habu-fence-an-offset-7372e836). The pointer-MINUS-pointer row below is a
\ distance, not a step, and is deliberately left unmarked: `BYTE-VIEW ...
\ NULL-PTR BYTE-VIEW -` is the sanctioned address-to-integer idiom.
EPRIM: +      PE-PTR-A-OFF PE-IN PE-N PE-IN  PE-PTR-A PE-OUT EPRIM;
EPRIM: +      PE-N PE-IN PE-PTR-A-OFF PE-IN  PE-PTR-A PE-OUT EPRIM;
EPRIM: -      PE-N PE-IN PE-N PE-IN  PE-N PE-OUT REF PRIM-REF:SUB EPRIM;
EPRIM: -      PE-PTR-A-OFF PE-IN PE-N PE-IN  PE-PTR-A PE-OUT EPRIM;
EPRIM: -      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-N PE-OUT EPRIM;
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
EPRIM: 0=     PE-A PE-IN  PE-F PE-OUT EPRIM;
EPRIM: 0<     PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:ZERO-NEG? EPRIM;
EPRIM: =      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:EQ EPRIM;
EPRIM: =      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT EPRIM;
EPRIM: <      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:LT EPRIM;
EPRIM: <      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT EPRIM;
EPRIM: >      PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:GT EPRIM;
EPRIM: >      PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT EPRIM;
EPRIM: <>     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:NEQ EPRIM;
EPRIM: <>     PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT EPRIM;
EPRIM: <=     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:LE EPRIM;
EPRIM: <=     PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT EPRIM;
EPRIM: >=     PE-N PE-IN PE-N PE-IN  PE-F PE-OUT REF PRIM-REF:GE EPRIM;
EPRIM: >=     PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-F PE-OUT EPRIM;
\ The three dividing rows. Their effects say nothing about the zero divisor
\ because a throw is not an output: each body refuses it with ARITH-ABI:E-DIV-ZERO
\ and MIN-N -1 wraps, the two contracts docs/forth.md states and
\ test/prim-parity.f pins on both the body and the reference.
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
\ `xt!` is `!` plus the declaration that the cell it writes holds a JIT-region
\ address, for a persisted cell whose address the caller works out at run time
\ from a table base and a row index (dot habu-declare-persisted-cb-b150b5d5).
\ Its row is `!`'s row, so the value and the pointee are the same type and a
\ quotation-typed cell is written with a quotation of exactly its own effect.
\ What the row still cannot say is that the pointee MUST be an execution token:
\ there is no quotation-kinded type variable, only TVK-ANY and TVK-RAW, so `xt!`
\ into a plain integer cell type-checks today and would have the loader shift an
\ ordinary integer. That missing kind, and the rule that would make a plain `!`
\ of a quotation into a persisted cell a reject, are dotted as
\ habu-add-a-quotation-1610f30c.
EPRIM: xt!        PE-A PE-IN PE-PTR-A PE-IN EPRIM;
EPRIM: ptr-cell-mark PE-PTR-A PE-IN EPRIM;
EPRIM: addr-cells-abi PE-N PE-OUT EPRIM;
EPRIM: ptr-field  PE-PTR-A PE-IN PE-N PE-IN  PE-PTR-PTR-B PE-OUT EPRIM;
\ Explicit memory views preserve address bits while changing the access type.
EPRIM: byte-view  PE-PTR-A PE-IN PE-PTR-U8 PE-OUT EPRIM;
EPRIM: cell-view  PE-PTR-U8 PE-IN PE-PTR-N PE-OUT EPRIM;
EPRIM: +!         PE-N PE-IN PE-PTR-N PE-IN REF PRIM-REF:ADD-TO EPRIM;
EPRIM: c@         PE-PTR-U8 PE-IN  PE-U8 PE-OUT EPRIM;
EPRIM: c!         PE-U8 PE-IN PE-PTR-U8 PE-IN EPRIM;
EPRIM: atomic@    PE-PTR-A PE-IN  PE-A PE-OUT EPRIM;
EPRIM: atomic!    PE-A PE-IN PE-PTR-A PE-IN EPRIM;
EPRIM: atomic-add PE-N PE-IN PE-PTR-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: atomic-cas PE-A PE-IN PE-A PE-IN PE-PTR-A PE-IN  PE-A PE-OUT EPRIM;
EPRIM: fence      EPRIM;
EPRIM: run-in-stack PE-Q ;PE-Q PE-IN PE-PTR-U8 PE-IN PE-N PE-IN EPRIM;
EPRIM: count      PE-PTR-U8 PE-IN  PE-PTR-U8 PE-OUT PE-N PE-OUT REF PRIM-REF:COUNTED$ EPRIM;

EPRIM: .            PE-N PE-IN EPRIM;
EPRIM: .s           EPRIM;
EPRIM: depth        PE-N PE-OUT EPRIM;
EPRIM: here         PE-PTR-A-RAW PE-OUT EPRIM;
EPRIM: tok-imm?     PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-OUT EPRIM;
EPRIM: allot        PE-N PE-IN EPRIM;
EPRIM: align        EPRIM;
EPRIM: ,            PE-N PE-IN EPRIM;
EPRIM: c,           PE-N PE-IN EPRIM;
EPRIM: type         PE-PTR-U8 PE-IN PE-N PE-IN EPRIM;
EPRIM: throw        PE-N PE-IN EPRIM;
EPRIM: die          PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN EPRIM;

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
EPRIM: fork          PE-N PE-OUT EPRIM;
EPRIM: wait-rc       PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: wait-status   PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: patch32       PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ code injection: only a TRUSTED: boundary may emit machine code (F3)
EPRIM: code-publish  PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ the bulk publication window is code injection too
EPRIM: callmap-set   PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ relocation metadata for code the publisher just wrote
EPRIM: addrmap-set   PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ the same, for an address chain the publisher just wrote
EPRIM: xref-retarget PE-N PE-IN PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ points a live dictionary record at new code
\ The seal's own two record writers. A row here is what lets the OPTIMIZING
\ compiler build the call window from src/core/internal-mark.f's TRUSTED:
\ wrappers, the same reason CHECKER-VERIFY-PKG-START below carries one: that
\ compiler reads a callee's cell widths out of this table for every name a body
\ writes, and a primitive is the one kind of name no scan can ever supply. The
\ JIT tier never asked, which is why the absence survived until a product engine
\ hosted a build at tier 1 and `TRUSTED: MARK-INTERNAL ( n -- ) int-mark ;` came
\ back E-HIR-UNMODELED. TRUSTED-only keeps the boundary exactly where it was:
\ a CHECKED caller is refused here, and the emitted records carry DNAME-INT
\ (habu1.f PRIM-GLOBAL-INT-WID) so neither name is executable or tickable.
EPRIM: int-mark      PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ sets DNAME-INT on one live record
EPRIM: min-in-mark   PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ records a certified minimum input arity on one
EPRIM: reloc-maps-clear PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ clears metadata over reclaimed code
EPRIM: does-patch PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ native defining-word runtime patch
EPRIM: does-record PE-N PE-IN PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ native `;does` companion publication
EPRIM: snap-rebase PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN EPRIM;
EPRIM: write         PE-N PE-IN PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: close         PE-N PE-IN EPRIM;
EPRIM: close-rc      PE-N PE-IN  PE-N PE-OUT EPRIM;   \ close returning host status (0 ok, <0 fail)
EPRIM: epoch-seconds PE-N PE-OUT EPRIM;
EPRIM: mono-ns       PE-N PE-OUT EPRIM;
EPRIM: prof-on       PE-N PE-IN EPRIM;
EPRIM: prof-report   EPRIM;
EPRIM: prof-off      EPRIM;
EPRIM: prof-reset    EPRIM;
EPRIM: prof-rate     PE-N PE-IN EPRIM;
EPRIM: prof-json     EPRIM;
EPRIM: prof-row      PE-N PE-IN EPRIM;                \ the report row for one dictionary record
EPRIM: prof-pc>rec   PE-N PE-IN  PE-N PE-OUT EPRIM;   \ the armed index's record for a pc, -1 when none owns it

EPRIM: rbase          PE-N PE-OUT EPRIM;
EPRIM: cp@            PE-N PE-OUT EPRIM;
EPRIM: cp!            PE-N PE-IN EPRIM;
EPRIM: dbase@         PE-N PE-OUT EPRIM;
EPRIM: check@         PE-N PE-OUT EPRIM;
\ Reading the selected compiler tier decides nothing and mutates nothing, so it
\ is an ordinary reader like check@ beside it.
EPRIM: tier@          PE-N PE-OUT EPRIM;
EPRIM: code-origin    PE-N PE-IN PE-N PE-IN PE-N PE-OUT EPRIM;
EPRIM: executable-build-enter EPRIM;
ETRUSTED-ONLY!
EPRIM: executable-build-leave EPRIM;
ETRUSTED-ONLY!
\ Compiler hook installation is an explicit engine boundary.
EPRIM: set-check     PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
EPRIM: set-preflight PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
EPRIM: set-top-check PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
\ Choosing the compiler is the same class of boundary as installing the hook and
\ takes the same restriction. Its callers are build drivers, which already reach
\ it from top level or from a TRUSTED: word, exactly as they reach set-check.
EPRIM: set-tier      PE-N PE-IN EPRIM;
ETRUSTED-ONLY!
EPRIM: ndict@         PE-N PE-OUT EPRIM;
EPRIM: ndict!         PE-N PE-IN EPRIM;
EPRIM: seed-ndict!    PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ explicit trusted reset boundary
EPRIM: ndict-append   PE-N PE-IN EPRIM;
ETRUSTED-ONLY!                       \ native pending-record publication
EPRIM: SEAL-CAPTURE   EPRIM;
EPRIM: seal-captured? PE-F PE-OUT EPRIM;
EPRIM: SEAL-FRIEND    EPRIM;
EPRIM: DRAIN-PRETRUST EPRIM;   \ dot habu-engine-pre-trust-77410827: drains the pending pre-trust defer table
\ data-base is a BASE ADDRESS: the start of the running task's DATA region, not
\ the address of any declared element. Its pointee is TVK-DBASE, so `data-base
\ OFF + @` still answers a number, an xt or a role - what those cells hold - and
\ still subtracts, compares and byte-views like any pointer, while a nominal
\ identity or an address read through it is refused at EVERY pointee depth: a
\ DATA cell is not the address of an address either (dot
\ habu-fence-a-base-c6c1d71d). A cell of the region that really holds an address
\ is reached with `ptr-field`, the declared door, exactly as before.
EPRIM: data-base      PE-PTR-A-DBASE PE-OUT EPRIM;
EPRIM: prot-wid-add   PE-N PE-IN EPRIM;
EPRIM: prot-wid-room  PE-N PE-OUT EPRIM;
EPRIM: wordlist       PE-N PE-OUT EPRIM;
EPRIM: get-current    PE-N PE-OUT EPRIM;
EPRIM: set-current    PE-N PE-IN EPRIM;
EPRIM: search-wl      PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
EPRIM: xref-search-wl PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN  PE-PTR-N PE-OUT EPRIM;
ETRUSTED-ONLY!                       \ NDICT's private indexed-record boundary
EPRIM: parse-name     PE-PTR-U8 PE-OUT PE-N PE-OUT EPRIM;
\ num-parse ( ptr u8 n -- n bool bool ) : the engine's own number reader, over
\ bytes the caller already holds - the routine the interpret and compile
\ dispatches call for every literal token (habu1.f BNUMPARSE, at LNUM). It
\ answers the value, whether that value is a double's bits, and whether the
\ bytes were a number at all. A checked stage that has to know what cell a
\ literal spelling stands for asks this rather than reading the spelling back
\ with a second decoder (src/compiler/native/feed.f).
EPRIM: num-parse      PE-PTR-U8 PE-IN PE-N PE-IN  PE-N PE-OUT PE-F PE-OUT PE-F PE-OUT EPRIM;
EPRIM: wide-mark EPRIM;
EPRIM: ffi-call       PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!
EPRIM: ffi-call-n     PE-PTR-A PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!
EPRIM: ffi-call-bounded PE-PTR-A PE-IN PE-PTR-B PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!
EPRIM: task-entry PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!                       \ C ABI entry address, never a Habu quotation
EPRIM: ffi-call-abi-bounded PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN
                           PE-PTR-D PE-IN PE-PTR-E PE-IN PE-N PE-IN PE-N PE-IN
                           PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!
EPRIM: ffi-call-abi-r-bounded PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN
                             PE-PTR-D PE-IN PE-PTR-E PE-IN PE-N PE-IN PE-N PE-IN
                             PE-R PE-OUT EPRIM;
ETRUSTED-ONLY!
EPRIM: ffi-call-abi   PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                     PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;
ETRUSTED-ONLY!
EPRIM: ffi-call-abi-r PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN PE-N PE-IN PE-N PE-IN
                     PE-N PE-IN PE-N PE-IN  PE-R PE-OUT EPRIM;
ETRUSTED-ONLY!

\ ---- package FFI's owner-private capability rows ----------------------------
\ Each primitive here keeps the global ETRUSTED-ONLY! row above — the outside
\ boundary (E-CAP-TRUSTED) and the record's callability past the seal — and gains
\ an owner-private row, so a CHECKED body compiled inside package FFI resolves it
\ and every other scope misses the symbol. This is what retires the TRUSTED:
\ bodies in lib/ffi-abi.f and lib/net/udp4.f: FFI's own words become ordinary
\ checked Habu, and no consumer can reach a raw foreign call at all.
EPPRIM: FFI ffi-call-bounded PE-PTR-A PE-IN PE-PTR-B PE-IN PE-N PE-IN PE-N PE-IN  PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-abi-bounded PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN
                                PE-PTR-D PE-IN PE-PTR-E PE-IN PE-N PE-IN PE-N PE-IN
                                PE-N PE-OUT ECLOSE-PRIVATE
EPPRIM: FFI ffi-call-abi-r-bounded PE-PTR-A PE-IN PE-PTR-B PE-IN PE-PTR-C PE-IN
                                  PE-PTR-D PE-IN PE-PTR-E PE-IN PE-N PE-IN PE-N PE-IN
                                  PE-R PE-OUT ECLOSE-PRIVATE

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
EPRIM: getpid   PE-N PE-OUT EPRIM;   \ ( -- pid ) process-identity syscall
EPRIM: proc-watch-open PE-N PE-IN PE-N PE-OUT EPRIM;   \ ( pid -- fd|-1 ) process-lifetime watch
EPRIM: kill-errno PE-N PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;   \ ( pid sig -- 0|-errno ) signal with errno detail
EPRIM: execve   PE-PTR-U8 PE-IN PE-PTR-A PE-IN PE-PTR-A PE-IN  PE-N PE-OUT EPRIM;   \ ( pathz argv envp -- -errno ) child-side exec; only returns on failure
EPRIM: munmap   PE-PTR-A PE-IN PE-N PE-IN  PE-N PE-OUT EPRIM;   \ ( addr len -- 0|-1 ) release a mapping; consumed by MEM:RELEASE-BYTES

\ ---- primitives whose effect the checker elaborates --------------------------
\ These have engine bodies and deliberately no row: their effect depends on the
\ call site, so src/core/checker.f computes it in the elaborator instead of
\ reading one from the table (RSEXEC, RSCATCH, RS2->R, RS2R>, RS2R@ in CF-TOK?,
\ `evaluate`'s dynamic rule, and `?dup`'s branch-shaped result). A row here
\ states that, so the completeness gate accepts the body without an effect.
ELAB: execute
ELAB: catch
ELAB: evaluate
ELAB: ?dup
ELAB: 2>r
ELAB: 2r>
ELAB: 2r@

\ ---- primitives with no checker row at all -----------------------------------
\ Bodies with no declaration anywhere, so no checked program can name one: a
\ checked caller of `run-rc` is E-UNDEFINED, measured. Only unchecked source
\ reaches them (test/engine-suite.f spawns through `run-rc`;
\ test/top-row-hook-test.f reads `top-check@`). They are rows so the gap is
\ stated instead of silent; giving them effects, or removing the bodies, is a
\ decision of its own and is taken separately.
UNROWED: run-rc
UNROWED: top-check@

;package
