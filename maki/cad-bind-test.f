\ maki/cad-bind-test.f - checked tests for OPTIMIZE-time shape binding (BIND-SHAPES).
\ Binding fills an unbound (0) MODEL: extent, re-propagates node shapes over the IR, and
\ invalidates the fusion plan so FUSE/MEMORY/TILE re-plan. Fail-closed probes drive the
\ (package-visible) bind engine directly, mirroring cad-test.f's capture-engine probes.

require lib/test.f
require lib/string.f
require maki/report.f
require maki/cad.f

package MAKI

variable CB-VA  variable CB-VU
: CB-SAVE ( ptr u8 n -- )  CB-VU ! CB-VA ! ;
: CB-IN ( ptr u8 n -- )  CB-VA @ CB-VU @ 2swap CONTAINS? TTRUE ;

\ ---- fail-closed probes (drive the private bind engine, like cad-test.f) ----
: TRY-BIND-NOMODEL  ( -- )  MODEL-CLEAR  BS-RESET  2 2 BS-PUSH  BS-BIND ;
: TRY-BIND-COUNT    ( -- )  BS-RESET  2 3 BS-PUSH  2 3 BS-PUSH  BS-BIND ;
: TRY-BIND-CONFLICT ( -- )  BS-RESET  4 3 BS-PUSH  BS-BIND ;
: TRY-BIND-ZERO     ( -- )  BS-RESET  0 3 BS-PUSH ;

T-RESET

\ ---- binding a NOMODEL / empty IR fails closed (count 1 vs 0 slots) ----------
' TRY-BIND-NOMODEL E-CAD-BIND-COUNT TTHROWS

\ ---- FFN with an unbound batch row: extents render "?" until bound -----------
MODEL: RBF ( x:0x3 w1:3x4 b1:1x4 w2:4x5 b2:1x5 -- y ) LINEAR GELU LINEAR ;
MODEL-K 3 T=
LOWER dup REPORT:SHAPE$ s" ?x5" T$= drop
MIR-RENDER CB-SAVE
s" input.0.shape: ?x3" CB-IN
s" node.0.shape: ?x4" CB-IN
s" node.2.shape: ?x5" CB-IN

\ ---- BIND-SHAPES fills the batch row and re-propagates the whole cone --------
BIND-SHAPES x:4x3 w1:3x4 b1:1x4 w2:4x5 b2:1x5 ;
LOWER dup REPORT:SHAPE$ s" 4x5" T$= drop
MIR-RENDER CB-SAVE
s" input.0.shape: 4x3"  CB-IN
s" node.0.shape: 4x4"   CB-IN
s" node.1.shape: 4x4"   CB-IN
s" node.2.shape: 4x5"   CB-IN

\ ---- FUSE re-plans against the now-bound shapes: 2 regions, bytes known ------
FUSE
dup REPORT:REGIONS@       2 T=
dup REPORT:OPS-AFTER@     2 T=
dup REPORT:SPLIT-COUNT    1 T=
dup 0 REPORT:SPLIT@ s" matmul-boundary at node 2" T$=
dup REPORT:BYTES-KNOWN? TTRUE
drop

\ ---- pure-elementwise batch bind: data shape propagates through both ops -----
MODEL: RBE ( x:0x4 -- y ) GELU RELU ;
LOWER dup REPORT:SHAPE$ s" ?x4" T$= drop
BIND-SHAPES 3x4 ;
LOWER dup REPORT:SHAPE$ s" 3x4" T$= drop
MIR-RENDER CB-SAVE
s" node.0.shape: 3x4" CB-IN
s" node.1.shape: 3x4" CB-IN

\ ---- restating an already-bound model is idempotent (matching specs pass) ----
MODEL: RBB ( x:2x3 -- y ) RELU ;
BIND-SHAPES 2x3 ;
LOWER dup REPORT:SHAPE$ s" 2x3" T$= drop

\ ---- movement re-propagation: transpose extents follow the bound input -------
MODEL: RBT ( x:0x4 -- y ) TRANSPOSE ;
BIND-SHAPES 8x4 ;
LOWER dup REPORT:SHAPE$ s" 4x8" T$= drop      \ transpose of 8x4 -> 4x8

\ ---- fail-closed: wrong count, a bound-extent conflict, a zero spec dim ------
MODEL: RBC ( x:2x3 -- y ) RELU ;
' TRY-BIND-COUNT    E-CAD-BIND-COUNT    TTHROWS
' TRY-BIND-CONFLICT E-CAD-BIND-CONFLICT TTHROWS
' TRY-BIND-ZERO     E-CAD-BIND-SHAPE    TTHROWS

\ ---- reprop enforces param-shape legality once the deferred (0) extent binds -----
\ RBM's residual param r:2x4 is legal only if the data batch row binds to 2; binding
\ it to 3 makes the residual data 3x4 vs the 2x4 param -> reprop throws E-CAD-PARAM-SHAPE.
MODEL: RBM ( x:0x4 r:2x4 -- y ) RELU RESIDUAL-ADD ;
: TRY-BIND-EWSHAPE ( -- )  BS-RESET  3 4 BS-PUSH  2 4 BS-PUSH  BS-BIND ;
' TRY-BIND-EWSHAPE E-CAD-PARAM-SHAPE TTHROWS

T-REPORT

end-package
