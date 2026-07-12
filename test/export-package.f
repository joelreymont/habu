\ export-package.f - EXPORT keyword engine-contract regressions (dot
\ habu-compiler-pkg-re-688212c1).
\
\ Proves the interpret-level `EXPORT` keyword's fail-closed walls and its two
\ documented roles in fresh child engines:
\ - inside an open package it publishes an existing word under its own tail
\   (dual-name execution, checked callers, generated-ctor sources allowed,
\   DNAME-WIDE parity between source and alias at the interpret gate);
\ - at top level it is the hb-build --repl export directive surface and
\   consumes the name as a no-op, keeping directive-carrying programs (for
\   example lib/prelude.f) directly loadable.
\ Rejects pinned by child exit status: undefined source (rc 70, token named),
\ sealed-system source prefix (rc 84, E-SEAL-PACKAGE), missing name (rc 74),
\ duplicate tail / self-export (rc 78, labeled "duplicate definition:"),
\ primitive source (uncaught E-EXPORT-PRIM 7115 -> rc 67, code named), and a
\ private word behind a closed package (qualified lookup is public-only,
\ rc 70).
\
\ Each program runs in a fresh child engine (HABU_UNDER_TEST when the gate
\ sets it, else bin/hb) over piped stdin; the representative undefined case
\ also runs --load for entry-path parity, exactly like test/seal-package.f.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\   test/export-package.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

2048 constant XPK-CAP
10000 constant XPK-TIMEOUT-MS
70 constant XPK-UNDEF-RC              \ undefined-word child exit status
74 constant XPK-NONAME-RC             \ missing EXPORT name ($4A)
78 constant XPK-DUP-RC                \ duplicate definition ($4E)
84 constant XPK-SEAL-RC               \ E-SEAL-PACKAGE
67 constant XPK-THROW-RC              \ engine uncaught-throw boundary exit

variable XPK-ROOT-U
variable XPK-CHILD-U
variable XPK-IN-U
variable XPK-OUT-U
variable XPK-ERR-U
variable XPK-EXITED                 \ bool: child completed by exit
variable XPK-RC

create XPK-ROOT-BUF FS-PATH-CAP allot
create XPK-CHILD-BUF FS-PATH-CAP allot
create XPK-IN XPK-CAP allot
create XPK-OUT XPK-CAP allot
create XPK-ERR XPK-CAP allot
create XPK-EMPTY 1 allot

: XPK-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: XPK-ROOT ( -- ptr u8 n )   XPK-ROOT-BUF XPK-ROOT-U @ ;
: XPK-CHILD ( -- ptr u8 n )  XPK-CHILD-BUF XPK-CHILD-U @ ;
: XPK-IN$ ( -- ptr u8 n )    XPK-IN XPK-IN-U @ ;

: XPK-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: XPK-LF ( -- )   10 SB-APPEND-C ;

: XPK-LINE ( ptr u8 n -- )   SB-APPEND XPK-LF ;

\ --- forge programs -----------------------------------------------------------

: XPK-SRC-PKG ( -- )                          \ shared source package XA with XA:W
   s" package XA" XPK-LINE
   s" public" XPK-LINE
   s" : W ( n -- n ) 2 * ;" XPK-LINE
   s" ;package" XPK-LINE ;

: XPK-OK-FORGE$ ( -- ptr u8 n )               \ dual-name execution + checked callers
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

: XPK-DIRECTIVE-FORGE$ ( -- ptr u8 n )        \ top-level directive is a no-op
   SB-RESET
   s" : SQ ( n -- n ) dup * ;" XPK-LINE
   s" EXPORT SQ" XPK-LINE
   s" EXPORT NEVER-DEFINED-ANYWHERE" XPK-LINE
   s" 6 SQ . cr" XPK-LINE
   SB$ ;

: XPK-UNDEF-FORGE$ ( -- ptr u8 n )            \ undefined source rejects
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT NOSUCH-EXPORT-SRC" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-SEALED-FORGE$ ( -- ptr u8 n )           \ sealed system-package source rejects
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT tfam:anything" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-NONAME-FORGE$ ( -- ptr u8 n )           \ EXPORT with no name rejects
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT" XPK-LINE
   SB$ ;

: XPK-DUP-FORGE$ ( -- ptr u8 n )              \ second export of the same tail rejects
   SB-RESET
   XPK-SRC-PKG
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT XA:W" XPK-LINE
   s" EXPORT XA:W" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-SELF-FORGE$ ( -- ptr u8 n )             \ self-export in the same section rejects
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" : SW ( -- n ) 1 ;" XPK-LINE
   s" EXPORT SW" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-PRIM-FORGE$ ( -- ptr u8 n )             \ primitive source rejects (E-EXPORT-PRIM)
   SB-RESET
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT dup" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

: XPK-PRIV-FORGE$ ( -- ptr u8 n )             \ private word behind a closed package
   SB-RESET
   s" package XA" XPK-LINE
   s" : PW ( -- n ) 1 ;" XPK-LINE
   s" public" XPK-LINE
   s" ;package" XPK-LINE
   s" package XB" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT XA:PW" XPK-LINE
   s" ;package" XPK-LINE
   SB$ ;

\ Generated constructor sources are closed-but-callable: re-exporting one under
\ a second public name is ALLOWED, a checked caller through the alias compiles,
\ and the alias inherits DNAME-WIDE (the interpret gate rejects an alias call
\ exactly like the source, proving the wide bit copied).
: XPK-CTOR-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" SUMTYPE xpw 0" XPK-LINE
   s"   VARIANT ok n ;VARIANT" XPK-LINE
   s" ;SUMTYPE" XPK-LINE
   s" package XC" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT xpw:ok" XPK-LINE
   s" ;package" XPK-LINE
   s" : MK ( n -- xpw ) XC:ok ;" XPK-LINE
   S\" s\" ctor-alias-ok\" type cr" XPK-LINE
   SB$ ;

: XPK-CTOR-WIDE-SRC-FORGE$ ( -- ptr u8 n )    \ interpret gate on the SOURCE ctor
   SB-RESET
   s" SUMTYPE xpw 0" XPK-LINE
   s"   VARIANT ok n ;VARIANT" XPK-LINE
   s" ;SUMTYPE" XPK-LINE
   S\" s\" pre-wide\" type cr" XPK-LINE
   s" 1 xpw:ok" XPK-LINE
   SB$ ;

: XPK-CTOR-WIDE-ALIAS-FORGE$ ( -- ptr u8 n )  \ interpret gate on the ALIAS
   SB-RESET
   s" SUMTYPE xpw 0" XPK-LINE
   s"   VARIANT ok n ;VARIANT" XPK-LINE
   s" ;SUMTYPE" XPK-LINE
   s" package XC" XPK-LINE
   s" public" XPK-LINE
   s" EXPORT xpw:ok" XPK-LINE
   s" ;package" XPK-LINE
   S\" s\" pre-wide\" type cr" XPK-LINE
   s" 1 XC:ok" XPK-LINE
   SB$ ;

\ --- child spawn + outcome capture --------------------------------------------

: XPK-STORE! ( len len outcome -- )
   MATCH outcome
     exited OF XPK-RC ! 0 0= XPK-EXITED ! ENDOF
     signaled OF XPK-RC ! 0 0= 0= XPK-EXITED ! ENDOF
     timeout OF 0 XPK-RC ! 0 0= 0= XPK-EXITED ! ENDOF
   ;MATCH
   LEN>N XPK-ERR-U !  LEN>N XPK-OUT-U ! ;

: XPK-IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u XPK-CAP > if E-FS-CAPACITY throw then
   a XPK-IN u BYTE-COPY
   u XPK-IN-U ! ;

: XPK-RUN-LOAD ( ptr u8 n -- )
   XPK-CHILD 2swap WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   XPK-CHILD >LEN PROC-ARGV+
   XPK-HB$ >LEN  XPK-EMPTY 0 >LEN  XPK-OUT XPK-CAP >LEN
   XPK-ERR XPK-CAP >LEN  XPK-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   XPK-STORE! ;

: XPK-RUN-STDIN ( ptr u8 n -- )
   XPK-IN!
   PROC-ARGV-RESET
   XPK-HB$ >LEN  XPK-IN$ >LEN  XPK-OUT XPK-CAP >LEN
   XPK-ERR XPK-CAP >LEN  XPK-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   XPK-STORE! ;

: XPK-ASSERT-RC ( n -- ) {: rc:n :}
   XPK-EXITED @ TTRUE
   XPK-RC @ rc T= ;

: XPK-OUT? ( ptr u8 n -- ) {: a:ptr u:n :}
   XPK-OUT XPK-OUT-U @ a u CONTAINS? TTRUE ;

: XPK-ERR? ( ptr u8 n -- ) {: a:ptr u:n :}
   XPK-ERR XPK-ERR-U @ a u CONTAINS? TTRUE ;

: XPK-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-export-pkg" TMPDIR-MKDIR {: a:ptr u:n :}
   a u XPK-ROOT-BUF XPK-ROOT-U XPK-COPY!
   XPK-ROOT CLEANUP-TREE+
   XPK-ROOT s" forge.f" XPK-CHILD-BUF JOIN-PATH XPK-CHILD-U ! ;

: XPK-CLEANUP ( -- )
   CLEANUP-RUN
   XPK-ROOT EXISTS? TFALSE ;

\ --- cases ---------------------------------------------------------------------

: XPK-POSITIVES ( -- )
   s" dual-name execution + checked callers" T-LABEL
   XPK-OK-FORGE$ XPK-RUN-STDIN 0 XPK-ASSERT-RC s" 14" XPK-OUT? s" 10" XPK-OUT?
   s" top-level EXPORT is the directive no-op" T-LABEL
   XPK-DIRECTIVE-FORGE$ XPK-RUN-STDIN 0 XPK-ASSERT-RC s" 36" XPK-OUT?
   s" generated ctor re-export allowed; checked alias caller compiles" T-LABEL
   XPK-CTOR-FORGE$ XPK-RUN-STDIN 0 XPK-ASSERT-RC s" ctor-alias-ok" XPK-OUT? ;

: XPK-NEGATIVES ( -- )
   s" undefined source rejects (stdin)" T-LABEL
   XPK-UNDEF-FORGE$ XPK-RUN-STDIN XPK-UNDEF-RC XPK-ASSERT-RC
   s" NOSUCH-EXPORT-SRC" XPK-ERR?
   s" undefined source rejects (--load)" T-LABEL
   XPK-UNDEF-FORGE$ XPK-RUN-LOAD XPK-UNDEF-RC XPK-ASSERT-RC
   s" sealed system-package source rejects" T-LABEL
   XPK-SEALED-FORGE$ XPK-RUN-STDIN XPK-SEAL-RC XPK-ASSERT-RC
   s" missing name rejects" T-LABEL
   XPK-NONAME-FORGE$ XPK-RUN-STDIN XPK-NONAME-RC XPK-ASSERT-RC
   s" duplicate tail rejects with labeled diagnosis" T-LABEL
   XPK-DUP-FORGE$ XPK-RUN-STDIN XPK-DUP-RC XPK-ASSERT-RC
   s" duplicate definition: " XPK-ERR?
   s" self-export rejects as duplicate" T-LABEL
   XPK-SELF-FORGE$ XPK-RUN-STDIN XPK-DUP-RC XPK-ASSERT-RC
   s" primitive source rejects (E-EXPORT-PRIM)" T-LABEL
   XPK-PRIM-FORGE$ XPK-RUN-STDIN XPK-THROW-RC XPK-ASSERT-RC
   s" 7115" XPK-ERR?
   s" private word behind a closed package rejects" T-LABEL
   XPK-PRIV-FORGE$ XPK-RUN-STDIN XPK-UNDEF-RC XPK-ASSERT-RC ;

\ The wide-bit parity pair: the SOURCE ctor and its ALIAS must fail the
\ interpret-level wide gate identically (same kind + rc), proving the alias
\ record copied DNAME-WIDE. The exact rc is whatever the engine's wide gate
\ exits with; equality between the two runs is the contract.
variable XPK-WIDE-EXITED   variable XPK-WIDE-RC

: XPK-WIDE-PARITY ( -- )
   s" interpret wide gate: source ctor" T-LABEL
   XPK-CTOR-WIDE-SRC-FORGE$ XPK-RUN-STDIN
   s" pre-wide" XPK-OUT?
   XPK-EXITED @ XPK-WIDE-EXITED !  XPK-RC @ XPK-WIDE-RC !
   XPK-RC @ 0 T<>
   s" interpret wide gate: alias behaves identically" T-LABEL
   XPK-CTOR-WIDE-ALIAS-FORGE$ XPK-RUN-STDIN
   s" pre-wide" XPK-OUT?
   XPK-EXITED @ TTRUE  XPK-WIDE-EXITED @ TTRUE
   XPK-RC @ XPK-WIDE-RC @ T= ;

T-RESET
XPK-PREPARE
XPK-POSITIVES
XPK-NEGATIVES
XPK-WIDE-PARITY
XPK-CLEANUP
T-REPORT
s" export-package: ok" type cr
