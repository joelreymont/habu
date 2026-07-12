\ maki/cad-ref-test.f - checked tests for named value references in MODEL: capture.
\
\ A body may NAME a value and reference it as a later op's operand, so an op reads an
\ EARLIER intermediate or a declared input instead of the next positional input (true
\ residual / skip connections, fan-out DAGs): a signature input name binds to that
\ input; ">V NAME" names the current value; a bare NAME token pushes that value as the
\ next op's parameter operand; "NAME^T" pushes its TRANSPOSE (an ordinary movement
\ node feeding the op - the DSL A@B^T capability), with numeric Q@K^T semantics
\ proven on the host executor. Fail-closed probes drive the (package-visible) capture
\ engine directly, mirroring cad-test.f / cad-bind-test.f.

require lib/test.f
require lib/string.f
require lib/float.f
require maki/report.f
require maki/cad.f
require maki/executor.f

package MAKI

variable CR-VA  variable CR-VU
: CR-SAVE ( ptr u8 n -- )  CR-VU ! CR-VA ! ;
: CR-IN ( ptr u8 n -- )  CR-VA @ CR-VU @ 2swap CONTAINS? TTRUE ;

\ ---- fail-closed probes (drive the package-visible translator primitives directly) ----------
\ v2 translates the body at MODEL: time; the reference/name errors below fire in the same
\ translator words that MODEL: drives, so driving them directly is faithful (and catchable,
\ unlike the MODEL: driver whose own throws cross an `evaluate` boundary).
: CR-TRY-NOVALUE   ( -- )  CAP-BEGIN s" H1" NT-BIND-CUR ;              \ ">V" with no running value
: CR-TRY-DUPNAME   ( -- )  CAP-BEGIN s" DUP" NT-BIND drop  s" DUP" NT-BIND drop ;  \ duplicate name
: CR-TRY-OPSHADOW  ( -- )  CAP-BEGIN s" GELU" NT-BIND drop ;          \ a name shadows an op token
: CR-TRY-REF-UNARY ( -- )                                            \ a ref a unary op cannot accept
   CAP-BEGIN 0 0 CAP-PEND-PUSH  0 CAP-EMIT-PARAMS ;                   \ 1 pending ref, unary op takes 0
: CR-TRY-REF-DANGLE ( -- )                                            \ a ref left unconsumed at ";"
   CAP-BEGIN 0 0 CAP-PEND-PUSH  CAP-FINISH ;
: CR-TRY-UNBOUND   ( -- )  s" H9" OP-KIND drop ;                      \ unbound reference = unknown token
: CR-TRY-TR-MARK   ( -- )                                            \ malformed marker: "K^X"
   CAP-BEGIN s" K" NT-BIND drop  s" K^X" CAP-TOKEN ;
: CR-TRY-TR-EMPTY  ( -- )                                            \ malformed marker: bare "K^"
   CAP-BEGIN s" K" NT-BIND drop  s" K^" CAP-TOKEN ;
: CR-TRY-TR-LOWER  ( -- )                                            \ malformed marker: "K^t" (case-exact)
   CAP-BEGIN s" K" NT-BIND drop  s" K^t" CAP-TOKEN ;
: CR-TRY-TR-UNBOUND ( -- )  CAP-BEGIN s" Z^T" CAP-TOKEN ;             \ transposed ref to an unbound name
: CR-TRY-TR-NAME   ( -- )  CAP-BEGIN s" K^T" NT-BIND drop ;           \ '^' is reserved in value names
: CR-TRY-REF-BADSHAPE ( -- )                                         \ a residual param whose shape != the data operand
   TENSOR:TV-RESET  4 8 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC  2 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC  MAKI-OPKIND:RESIDUAL-ADD EW-SHAPE-CHECK ;

T-RESET

\ ---- every named-value fail-closed path stays closed -------------------------
' CR-TRY-NOVALUE    E-CAD-NOVALUE TTHROWS
' CR-TRY-DUPNAME    E-CAD-NAME    TTHROWS
' CR-TRY-OPSHADOW   E-CAD-NAME    TTHROWS
' CR-TRY-REF-UNARY  E-CAD-REF     TTHROWS
' CR-TRY-REF-DANGLE E-CAD-REF     TTHROWS
' CR-TRY-UNBOUND    E-CAD-OP      TTHROWS
' CR-TRY-REF-BADSHAPE E-CAD-PARAM-SHAPE TTHROWS
' CR-TRY-TR-MARK    E-CAD-SYNTAX  TTHROWS
' CR-TRY-TR-EMPTY   E-CAD-SYNTAX  TTHROWS
' CR-TRY-TR-LOWER   E-CAD-SYNTAX  TTHROWS
' CR-TRY-TR-UNBOUND E-CAD-OP      TTHROWS
' CR-TRY-TR-NAME    E-CAD-NAME    TTHROWS

\ ---- residual to a declared INPUT (true skip): "x RESIDUAL-ADD" reads input 0 --
\ the residual's second operand is x (i0), NOT the next positional declared input.
MODEL: RRES ( x:2x4 w1:4x4 b1:1x4 -- y ) LINEAR GELU x RESIDUAL-ADD ;
MODEL-K 3 T=
MIR-RENDER CR-SAVE
s" ir.inputs: 3"            CR-IN
s" node.0.op: linear"       CR-IN
s" node.0.in: i0 i1 i2"     CR-IN
s" node.1.op: gelu"         CR-IN
s" node.2.op: residual-add" CR-IN
s" node.2.in: n1 i0"        CR-IN

\ ---- named INTERMEDIATE fan-out: ">V H1 ... H1 RESIDUAL-ADD" reads node 0 twice --
\ H1 names the first LINEAR's output (node 0); GELU consumes it AND the residual
\ consumes it -> a real fan-out (node 0 feeds two consumers).
MODEL: RFAN ( x:2x4 w1:4x4 b1:1x4 -- y ) LINEAR >V H1 GELU H1 RESIDUAL-ADD ;
MODEL-K 3 T=
MIR-RENDER CR-SAVE
s" node.1.op: gelu"         CR-IN
s" node.1.in: n0"           CR-IN
s" node.2.op: residual-add" CR-IN
s" node.2.in: n1 n0"        CR-IN

\ ---- a reference leaves the positional cursor untouched (no input consumed) -----
\ RIN keeps all 3 inputs; RESIDUAL-ADD re-reads x rather than consuming a 4th input.
MODEL: RIN ( x:2x4 w1:4x4 b1:1x4 -- y ) LINEAR x RESIDUAL-ADD ;
MODEL-K 2 T=
MIR-RENDER CR-SAVE
s" ir.inputs: 3"     CR-IN
s" node.1.in: n0 i0" CR-IN

\ ---- a named reference also supplies a movement op's second operand (CONCAT) ----
\ "x CONCAT" appends the ORIGINAL input x under the current value instead of the next
\ declared input; the model still declares just the two inputs.
MODEL: RCAT ( x:2x4 w1:4x4 b1:1x4 -- y ) LINEAR x CONCAT ;
MIR-RENDER CR-SAVE
s" node.1.op: concat" CR-IN
s" node.1.in: n0 i0"  CR-IN

\ ---- "k^T MATMUL" (A@B^T): the transposed reference inserts a transpose node ----
\ K stays in its natural 4x3 orientation; the translator feeds the matmul a staged
\ transpose node (3x4) of input 1 - no caller pre-transposition, no new op kind.
MODEL: RTRI ( q:4x3 k:4x3 -- y ) k^T MATMUL ;
MODEL-K 2 T=
MIR-RENDER CR-SAVE
s" node.0.op: transpose"   CR-IN
s" node.0.in: i1"          CR-IN
s" node.0.shape: 3x4"      CR-IN
s" node.0.verdict: staged" CR-IN
s" node.1.op: matmul"      CR-IN
s" node.1.in: i0 n0"       CR-IN
s" node.1.shape: 4x4"      CR-IN

\ ---- a ">V" intermediate is transposable too: "H^T" reads node 0 transposed ----
MODEL: RTRV ( x:2x3 w:3x3 -- y ) MATMUL >V H H^T MATMUL ;
MODEL-K 3 T=
MIR-RENDER CR-SAVE
s" node.1.op: transpose" CR-IN
s" node.1.in: n0"        CR-IN
s" node.1.shape: 3x2"    CR-IN
s" node.2.op: matmul"    CR-IN
s" node.2.in: n0 n1"     CR-IN
s" node.2.shape: 2x2"    CR-IN

\ ---- ^T numeric semantics on the host executor: y = Q @ K^T, hand-computed ----
\ q = [[1 2 3][4 5 6]], k = [[7 8 9][10 11 12]] -> y = [[50 68][122 167]] (exact
\ small-integer contractions, so f>s comparison is exact).
create RQ 6 cells allot   create RK 6 cells allot
: RTR-FILL ( -- )
   6 0 ?do  i 1+ s>f  RQ i T-SET  loop
   6 0 ?do  i 7 + s>f  RK i T-SET  loop ;
MODEL: RTRX ( q:2x3 k:2x3 -- y ) k^T MATMUL ;
RTR-FILL
EX-RESET  RQ 0 MIR-SLOT-ID EX-BIND  RK 1 MIR-SLOT-ID EX-BIND  EX-RUN
1 MIR-NODE-ID EX-OUT@ 0 T-GET f>s  50 T=
1 MIR-NODE-ID EX-OUT@ 1 T-GET f>s  68 T=
1 MIR-NODE-ID EX-OUT@ 2 T-GET f>s 122 T=
1 MIR-NODE-ID EX-OUT@ 3 T-GET f>s 167 T=

T-REPORT

end-package
