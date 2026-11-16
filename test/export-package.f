\ export-package.f - interpret-level `EXPORT` in fresh child engines. Inside an
\ open package's public section it publishes an existing word under its own
\ tail; outside a package it is rejected.

\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f
\   src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f
\   test/export-package.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/ending.f
require src/lib/engine-candidate.f

2048 constant XPK-CAP
70 constant XPK-UNDEF-RC              \ undefined-word child exit status
70 constant XPK-REJECT-RC             \ a rejection's exit status (CHECKER-REJECT-RC)

variable XPK-ROOT-U
variable XPK-CHILD-U
variable XPK-IN-U
variable XPK-OUT-U
variable XPK-ERR-U
variable XPK-EXITED                 \ bool: child completed by exit
variable XPK-RC

create XPK-ROOT-BUF PATH-CAP allot
create XPK-CHILD-BUF PATH-CAP allot
create XPK-IN XPK-CAP allot
create XPK-OUT XPK-CAP allot
create XPK-ERR XPK-CAP allot
create XPK-EMPTY 1 allot

: XPK-COPY! ( ^u8 n ^u8 ^n -- ) {: src:^ u:n dst:^ lenp:^ :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: XPK-ROOT ( -- ^u8 n )   XPK-ROOT-BUF XPK-ROOT-U @ ;
: XPK-CHILD ( -- ^u8 n )  XPK-CHILD-BUF XPK-CHILD-U @ ;
: XPK-IN$ ( -- ^u8 n )    XPK-IN XPK-IN-U @ ;

: XPK-LF ( -- )   10 SB-APPEND-C ;

: XPK-LINE ( ^u8 n -- )   SB-APPEND XPK-LF ;

\ --- forge programs -----------------------------------------------------------
\ Child exits: a primitive source, a trusted primitive and a top-level EXPORT
\ 70 with their records; a sealed package's private word 70.

: XPK-SRC-PKG ( -- )                          \ shared source package XA with XA:W
   s" package XA" XPK-LINE
   s" public" XPK-LINE
   s" : W ( n -- n ) 2 * ;" XPK-LINE
   s" ;package" XPK-LINE ;

: XPK-OK-FORGE$ ( -- ^u8 n )               \ dual-name execution + checked callers
   SB-RESET
   XPK-SRC-PKG
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT XA:W" XPK-LINE
   s" ;package" XPK-LINE
   s" : U1 ( n -- n ) XB:W ;" XPK-LINE
   s" : U2 ( n -- n ) XA:W ;" XPK-LINE
   s" 7 XA:W . cr" XPK-LINE
   s" 7 XB:W . cr" XPK-LINE
   s" 5 U1 . cr" XPK-LINE
   s" 5 U2 . cr" XPK-LINE
   SB$ ;

: XPK-TOP-FORGE$ ( -- ^u8 n )              \ top-level EXPORT rejects
   SB-RESET
   s" : SQ ( n -- n ) dup * ;" XPK-LINE
   s" EXPORT SQ" XPK-LINE
   SB$ ;

: XPK-UNDEF-FORGE$ ( -- ^u8 n )            \ undefined source rejects
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT NOSUCH-EXPORT-SRC" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-QDUP-FORGE$ ( -- ^u8 n )             \ top-level ?dup over an enum value
   SB-RESET
   s" SUMTYPE xpklight VARIANT red ;VARIANT VARIANT green ;VARIANT ;SUMTYPE" XPK-LINE
   s" XPKLIGHT:RED ?dup" XPK-LINE
   SB$ ;

: XPK-NONAME-FORGE$ ( -- ^u8 n )           \ EXPORT with no name rejects
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT" XPK-LINE
   SB$ ;

: XPK-DUP-FORGE$ ( -- ^u8 n )              \ second export of the same tail rejects
   SB-RESET
   XPK-SRC-PKG
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT XA:W" XPK-LINE
   s" EXPORT XA:W" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-SELF-FORGE$ ( -- ^u8 n )             \ self-export in the same section rejects
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" : SW ( -- n ) 1 ;" XPK-LINE
   s" EXPORT SW" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-PRIM-FORGE$ ( -- ^u8 n )             \ primitive source rejects (E-EXPORT-PRIM)
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT dup" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-UNSAFE-FORGE$ ( -- ^u8 n )           \ trusted primitive rejects (E-EXPORT-UNSAFE)
   SB-RESET
   s" package TICK-ALIAS" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT patch32" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-PRIV-FORGE$ ( -- ^u8 n )             \ an unsealed package's private word
   SB-RESET
   s" package XA" XPK-LINE
   s" : PW ( -- n ) 73 ;" XPK-LINE
   s" ;package" XPK-LINE
   s" package XB" XPK-LINE
   s" EXPORT XA:PW" XPK-LINE
   s" public" XPK-LINE
   s" : TAKE ( -- n ) PW ;" XPK-LINE
   s" ;package" XPK-LINE
   s" XB:TAKE . cr" XPK-LINE
   SB$ ;

\ CHECK is an engine package, sealed before program source.
: XPK-SEALED-FORGE$ ( -- ^u8 n )           \ a sealed package's private word
   SB-RESET
   s" package XB" XPK-LINE
   s" EXPORT CHECK:PRIM-ENSURE" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-DATA-FORGE$ ( -- ^u8 n )             \ a data record is never internal
   SB-RESET
   s" 42 constant K package P public export K ;package P:K ." XPK-LINE
   SB$ ;

\ Re-exporting a generated constructor (closed but callable) under a second
\ public name is allowed, a checked caller through the alias compiles, and the
\ alias inherits DNAME-WIDE.
: XPK-CTOR-FORGE$ ( -- ^u8 n )
   SB-RESET
   s" SUMTYPE xpw" XPK-LINE
   s"   VARIANT ok n ;VARIANT" XPK-LINE
   s" ;SUMTYPE" XPK-LINE
   s" package XC" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT xpw:ok" XPK-LINE
   s" ;package" XPK-LINE
   s" : MK ( n -- xpw ) XC:ok ;" XPK-LINE
   S\" s\" ctor-alias-ok\" type cr" XPK-LINE
   SB$ ;

: XPK-CTOR-WIDE-SRC-FORGE$ ( -- ^u8 n )    \ interpret gate on the SOURCE ctor
   SB-RESET
   s" SUMTYPE xpw" XPK-LINE
   s"   VARIANT ok n ;VARIANT" XPK-LINE
   s" ;SUMTYPE" XPK-LINE
   s" 1 xpw:ok" XPK-LINE
   S\" s\" ran-wide\" type cr" XPK-LINE
   s" dup" XPK-LINE
   SB$ ;

: XPK-CTOR-WIDE-ALIAS-FORGE$ ( -- ^u8 n )  \ interpret gate on the ALIAS
   SB-RESET
   s" SUMTYPE xpw" XPK-LINE
   s"   VARIANT ok n ;VARIANT" XPK-LINE
   s" ;SUMTYPE" XPK-LINE
   s" package XC" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT xpw:ok" XPK-LINE
   s" ;package" XPK-LINE
   s" 1 XC:ok" XPK-LINE
   S\" s\" ran-wide\" type cr" XPK-LINE
   s" dup" XPK-LINE
   SB$ ;

\ --- child spawn + outcome capture --------------------------------------------
\ Children run ENGINE-CANDIDATE:PATH$ over piped stdin or under --load.

: XPK-STORE! ( len len PROCESS:ending ^u8 n -- )
   {: outu:len erru:len oc src:^ srcu:n :}
   outu LEN>N XPK-OUT-U !  erru LEN>N XPK-ERR-U !
   oc MATCH PROCESS:ending
     exited OF XPK-RC ! 0 0= XPK-EXITED ! ENDOF
     signaled OF XPK-RC ! 0 0= 0= XPK-EXITED ! ENDOF
     timeout OF
        src srcu XPK-OUT XPK-OUT-U @ XPK-ERR XPK-ERR-U @ T-TIMED-OUT
     ENDOF
   ;MATCH ;

: XPK-IN! ( ^u8 n -- ) {: a:^ u:n :}
   u XPK-CAP > if E-FS-CAPACITY throw then
   a XPK-IN u BYTE-COPY
   u XPK-IN-U ! ;

: XPK-RUN-LOAD ( ^u8 n -- )
   {: src:^ srcu:n :}
   XPK-CHILD src srcu FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   XPK-CHILD >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  XPK-EMPTY 0 >LEN  XPK-OUT XPK-CAP >LEN
   XPK-ERR XPK-CAP >LEN  PROCESS:WAIT-FOREVER
   RUN-ARGV-STDIN-CAPTURE-OUTCOME
   src srcu XPK-STORE! ;

: XPK-RUN-STDIN ( ^u8 n -- )
   XPK-IN!
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN  XPK-IN$ >LEN  XPK-OUT XPK-CAP >LEN
   XPK-ERR XPK-CAP >LEN  PROCESS:WAIT-FOREVER
   RUN-ARGV-STDIN-CAPTURE-OUTCOME
   XPK-IN$ XPK-STORE! ;

: XPK-ASSERT-RC ( n -- ) {: rc:n :}
   XPK-EXITED @ TTRUE
   XPK-RC @ rc T= ;

: XPK-OUT? ( ^u8 n -- ) {: a:^ u:n :}
   XPK-OUT XPK-OUT-U @ a u CONTAINS? TTRUE ;

: XPK-ERR? ( ^u8 n -- ) {: a:^ u:n :}
   XPK-ERR XPK-ERR-U @ a u CONTAINS? TTRUE ;

: XPK-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-export-pkg" FS:MKTEMP-DIR {: a:^ u:n :}
   a u XPK-ROOT-BUF XPK-ROOT-U XPK-COPY!
   XPK-ROOT FS:CLEANUP-TREE+
   XPK-ROOT s" forge.f" XPK-CHILD-BUF FS:JOIN-PATH XPK-CHILD-U ! ;

: XPK-CLEANUP ( -- )
   FS:CLEANUP-RUN
   XPK-ROOT FS:EXISTS? TFALSE ;

\ --- cases ---------------------------------------------------------------------

: XPK-POSITIVES ( -- )
   s" dual-name execution + checked callers" T-LABEL
   XPK-OK-FORGE$ XPK-RUN-STDIN 0 XPK-ASSERT-RC s" 14" XPK-OUT? s" 10" XPK-OUT?
   s" generated ctor re-export allowed; checked alias caller compiles" T-LABEL
   XPK-CTOR-FORGE$ XPK-RUN-STDIN 0 XPK-ASSERT-RC s" ctor-alias-ok" XPK-OUT?
   s" data record exports (--load)" T-LABEL
   XPK-DATA-FORGE$ XPK-RUN-LOAD 0 XPK-ASSERT-RC s" 42" XPK-OUT?
   s" an unsealed package's private word exports (--load)" T-LABEL
   XPK-PRIV-FORGE$ XPK-RUN-LOAD 0 XPK-ASSERT-RC s" 73" XPK-OUT? ;

: XPK-NEGATIVES ( -- )
   s" undefined source rejects (stdin)" T-LABEL
   XPK-UNDEF-FORGE$ XPK-RUN-STDIN XPK-UNDEF-RC XPK-ASSERT-RC
   s" NOSUCH-EXPORT-SRC" XPK-ERR?
   s" undefined source rejects (--load)" T-LABEL
   XPK-UNDEF-FORGE$ XPK-RUN-LOAD XPK-UNDEF-RC XPK-ASSERT-RC
   s" missing name rejects" T-LABEL
   XPK-NONAME-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" duplicate tail rejects with labeled diagnosis" T-LABEL
   XPK-DUP-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-DUPLICATE-DEFINITION]" XPK-ERR?
   s" self-export rejects as duplicate" T-LABEL
   XPK-SELF-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" primitive source rejects (E-EXPORT-PRIM)" T-LABEL
   XPK-PRIM-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-EXPORT-PRIM]" XPK-ERR?  s" `dup`" XPK-ERR?
   s" exporting a trusted primitive rejects (E-EXPORT-UNSAFE)" T-LABEL
   XPK-UNSAFE-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-EXPORT-UNSAFE]" XPK-ERR?  s" `patch32`" XPK-ERR?
   s" a sealed package's private word rejects (--load)" T-LABEL
   XPK-SEALED-FORGE$ XPK-RUN-LOAD XPK-UNDEF-RC XPK-ASSERT-RC
   s" error[E-EXPORT-UNDEFINED]" XPK-ERR?  s" `CHECK:PRIM-ENSURE`" XPK-ERR?
   s" top-level EXPORT rejects (E-EXPORT-NO-PACKAGE)" T-LABEL
   XPK-TOP-FORGE$ XPK-RUN-STDIN XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-EXPORT-NO-PACKAGE]" XPK-ERR?
   s" top-level ?dup over an enum value is undefined" T-LABEL
   XPK-QDUP-FORGE$ XPK-RUN-STDIN XPK-UNDEF-RC XPK-ASSERT-RC
   s" error[E-UNDEFINED]: cannot find word `?dup`" XPK-ERR? ;

\ The source ctor and its alias must behave alike at top level: both run and
\ leave their two-cell value, and a primitive over it (dup) is rejected alike
\ (E-LAYOUT-VALUE, same rc), proving the alias carries the constructor's effect; equality is
\ the contract.
variable XPK-WIDE-EXITED   variable XPK-WIDE-RC

: XPK-WIDE-PARITY ( -- )
   s" interpret wide gate: source ctor" T-LABEL
   XPK-CTOR-WIDE-SRC-FORGE$ XPK-RUN-STDIN
   s" ran-wide" XPK-OUT?
   s" error[E-LAYOUT-VALUE]" XPK-ERR?  s" `dup`" XPK-ERR?
   XPK-EXITED @ XPK-WIDE-EXITED !  XPK-RC @ XPK-WIDE-RC !
   XPK-RC @ 0 T<>
   s" interpret wide gate: alias behaves identically" T-LABEL
   XPK-CTOR-WIDE-ALIAS-FORGE$ XPK-RUN-STDIN
   s" ran-wide" XPK-OUT?
   s" error[E-LAYOUT-VALUE]" XPK-ERR?  s" `dup`" XPK-ERR?
   XPK-EXITED @ TTRUE  XPK-WIDE-EXITED @ TTRUE
   XPK-RC @ XPK-WIDE-RC @ T= ;

: XPK-PT-LINES ( -- )
   s" package PT" XPK-LINE
   s" public" XPK-LINE
   s" STRUCTURE point FIELD x n FIELD y n ;STRUCTURE" XPK-LINE
   s" : AT ( n n -- point ) PT-POINT:MAKE ;" XPK-LINE
   s" : SMASH ( point -- point ) 1 throw ;" XPK-LINE
   s" ;package" XPK-LINE
   s" 2 3 PT:AT" XPK-LINE ;

\ A point, then the primitive a u on its own line, run; the record names a u.
: XPK-PT-XPORT ( ^u8 n -- ) {: a:^ u:n :}
   a u T-LABEL
   SB-RESET XPK-PT-LINES  a u XPK-LINE  SB$ XPK-RUN-STDIN
   XPK-REJECT-RC XPK-ASSERT-RC  s" error[E-LAYOUT-VALUE]: `" XPK-ERR?
   SB-RESET s" `" SB-APPEND  a u SB-APPEND
   s" ` at top level would take part of a value held in more than one cell" SB-APPEND
   SB$ XPK-ERR? ;

\ `constant` over a point is rejected at the keyword, before it takes a cell.
: XPK-PT-CONSTANT ( -- )
   s" top-level constant over a point" T-LABEL
   SB-RESET XPK-PT-LINES  s" constant K" XPK-LINE  SB$ XPK-RUN-STDIN
   XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-LAYOUT-VALUE]: `constant` would take one cell of `pt:point`, a value held in more than one cell"
   XPK-ERR?
   s"  --> <input>:8:1" XPK-ERR?
   s" 8 | constant K" XPK-ERR?
   s"   | ^^^^^^^^ takes one cell" XPK-ERR?
   s"   = help: define a word that leaves the whole value, `: K ( -- pt:point ) ... ;`, in place of the constant"
   XPK-ERR? ;

\ `constant` on an empty stack is rejected at the keyword, before it takes a cell.
: XPK-EMPTY-CONSTANT ( -- )
   s" top-level constant on an empty stack" T-LABEL
   SB-RESET s" constant K" XPK-LINE  SB$ XPK-RUN-STDIN
   XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-INPUT-UNDERFLOW]: `constant` takes the value `K` names, and the stack is empty"
   XPK-ERR?
   s"  --> <input>:1:1" XPK-ERR?
   s" 1 | constant K" XPK-ERR?
   s"   | ^^^^^^^^ takes one cell" XPK-ERR?
   s"   = help: push the value before `constant`, as `0 constant K`" XPK-ERR? ;

\ A `constant` that took the cell above a point leaves the point on top: the
\ next `constant` is rejected too.
: XPK-PT-CONSTANTS ( -- )
   s" top-level constant after a constant over a point" T-LABEL
   SB-RESET XPK-PT-LINES
   s" 7 constant SEVEN constant K" XPK-LINE  SB$ XPK-RUN-STDIN
   XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-LAYOUT-VALUE]: `constant` would take one cell of `pt:point`, a value held in more than one cell"
   XPK-ERR?
   s"  --> <input>:8:18" XPK-ERR?
   s" 8 | 7 constant SEVEN constant K" XPK-ERR?
   s"   |                  ^^^^^^^^ takes one cell" XPK-ERR?
   s"   = help: define a word that leaves the whole value, `: K ( -- pt:point ) ... ;`, in place of the constant"
   XPK-ERR? ;

\ At top level each transport primitive over a point, and `constant` over a
\ point or on an empty stack, is rejected before it runs, naming itself.
: XPK-WIDE-TAKE ( -- )
   s" dup" XPK-PT-XPORT  s" swap" XPK-PT-XPORT  s" over" XPK-PT-XPORT
   s" nip" XPK-PT-XPORT  s" tuck" XPK-PT-XPORT  s" rot" XPK-PT-XPORT
   s" -rot" XPK-PT-XPORT  s" 2dup" XPK-PT-XPORT  s" 2drop" XPK-PT-XPORT
   s" 2swap" XPK-PT-XPORT  s" 2over" XPK-PT-XPORT  s" 2>r" XPK-PT-XPORT
   XPK-PT-CONSTANT XPK-EMPTY-CONSTANT XPK-PT-CONSTANTS ;

: XPK-PT-DROP ( ^u8 n ^u8 n -- ) {: w:^ v:n a:^ u:n :}
   w v T-LABEL
   SB-RESET XPK-PT-LINES  a u XPK-LINE  SB$ XPK-RUN-STDIN
   0 XPK-ASSERT-RC  s" 100" XPK-OUT? ;

: XPK-LT-DROP ( -- )
   s" top-level drop of a linear token" T-LABEL
   SB-RESET
   s" package LT" XPK-LINE
   s" public" XPK-LINE
   s" DEFLINEAR LT:tok" XPK-LINE
   s" private" XPK-LINE
   s" LINEAR: MINT ( n -- LT:tok )" XPK-LINE
   s" public" XPK-LINE
   s" : NEW ( n -- LT:tok ) MINT ;" XPK-LINE
   s" ;package" XPK-LINE
   s" 5 LT:NEW drop" XPK-LINE
   SB$ XPK-RUN-STDIN
   XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-LINEAR-DROP]: a linear value is discarded here" XPK-ERR?
   s" ^^^^ linear value dropped" XPK-ERR? ;

\ An item of open width has no cell count to drop.
: XPK-OPEN-DROP ( -- )
   s" top-level drop of a value of open width" T-LABEL
   SB-RESET
   s" SUMTYPE myopt<a> VARIANT none ;VARIANT VARIANT some a ;VARIANT ;SUMTYPE" XPK-LINE
   s" : XPK-OPEN ( -- myopt<a> ) MYOPT:NONE ;" XPK-LINE
   s" XPK-OPEN drop" XPK-LINE
   SB$ XPK-RUN-STDIN
   XPK-REJECT-RC XPK-ASSERT-RC
   s" error[E-LAYOUT-VALUE]: `drop` cannot drop `myopt<a>`, a value whose cell count is not known here"
   XPK-ERR?
   s"  --> <input>:3:10" XPK-ERR?
   s" 3 | XPK-OPEN drop" XPK-ERR?
   s"   |          ^^^^ cell count not known" XPK-ERR?
   s"   = help: leave the value from a word whose effect gives `myopt<a>` concrete type arguments, as `( -- myopt<n> )`"
   XPK-ERR? ;

\ `drop` over a point, and over one a top-level `catch` left stale, empties the
\ row, so `depth` runs; a linear token's `drop` is rejected as in a body, and
\ one of open width is rejected.
: XPK-WIDE-DROP ( -- )
   s" top-level drop of a point" s" drop depth 100 + . cr" XPK-PT-DROP
   s" top-level drop of a stale point"
   s" ' PT:SMASH catch drop drop depth 100 + . cr" XPK-PT-DROP
   XPK-LT-DROP  XPK-OPEN-DROP ;

\ A user's global `drop`, defined after `undefine drop`, runs once, as any word.
: XPK-USER-DROP ( -- )
   s" a user's global drop runs once at top level" T-LABEL
   SB-RESET
   s" undefine drop : drop ( n n -- ) + . cr ;" XPK-LINE
   s" 1 2 drop depth 100 + . cr" XPK-LINE
   SB$ XPK-RUN-STDIN
   0 XPK-ASSERT-RC  s" 3" XPK-OUT?  s" 100" XPK-OUT? ;

\ A word whose effect carries the point keeps it whole at top level: run by
\ `evaluate`, by a tick and `execute`, and as a `does>` child, then taken by
\ one word, so the load ends clean.
: XPK-WIDE-KEEP ( -- )
   s" top-level evaluate, execute and a does> child keep a point whole" T-LABEL
   SB-RESET XPK-PT-LINES
   s" : XPK-SAME ( PT:point -- PT:point ) ;" XPK-LINE
   s" : XPK-SUM ( PT:point -- n ) PT-POINT:UNMAKE + ;" XPK-LINE
   s" : XPK-KEEPS ( -- ) create does> ( PT:point -- PT:point ) drop ;" XPK-LINE
   s" XPK-KEEPS XPK-KEPT" XPK-LINE
   S\" s\" XPK-SAME\" evaluate" XPK-LINE
   s" ' XPK-SAME execute" XPK-LINE
   s" XPK-KEPT" XPK-LINE
   s" XPK-SUM 100 + . cr" XPK-LINE
   SB$ XPK-RUN-STDIN
   0 XPK-ASSERT-RC  s" 105" XPK-OUT? ;

T-RESET
XPK-PREPARE
XPK-POSITIVES
XPK-NEGATIVES
XPK-WIDE-PARITY
XPK-WIDE-TAKE
XPK-WIDE-DROP
XPK-USER-DROP
XPK-WIDE-KEEP
XPK-CLEANUP
T-REPORT
s" export-package: ok" type cr
