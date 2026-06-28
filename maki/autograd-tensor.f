\ maki/autograd-tensor.f - tensor-level autograd: lift the scalar VJPs to whole
\ arrays (the batched VJP layer). The forward writes an output buffer; the
\ backward maps the per-element scalar VJP over the cotangent buffer(s). This is
\ the "lift scalar VJPs to whole-tensor ops" layer - numerically gradcheckable on
\ the CPU host now (real Habu floats), the same maps that lower onto the checked
\ PTX kernels later. Needs maki/array.f (T-GET/T-SET/T-FILL) + maki/autograd.f
\ (the scalar *-F / *-BWD rules). maki -> habu only.

require maki/array.f
require maki/autograd.f

package MAKI
public

\ --- forward (apply the scalar op elementwise into the output tensor) ---
: TT-ADD-F  ( ptr a ptr a ptr a n -- ) {: xb:ptr yb:ptr zb:ptr len:n :}
   len 0 ?do  xb i T-GET  yb i T-GET  ADD-F  zb i T-SET  loop ;
: TT-MUL-F  ( ptr a ptr a ptr a n -- ) {: xb:ptr yb:ptr zb:ptr len:n :}
   len 0 ?do  xb i T-GET  yb i T-GET  MUL-F  zb i T-SET  loop ;
: TT-SUB-F  ( ptr a ptr a ptr a n -- ) {: xb:ptr yb:ptr zb:ptr len:n :}
   len 0 ?do  xb i T-GET  yb i T-GET  SUB-F  zb i T-SET  loop ;
: TT-RELU-F ( ptr a ptr a n -- )       {: xb:ptr zb:ptr len:n :}
   len 0 ?do  xb i T-GET  RELU-F   zb i T-SET  loop ;
: TT-SQUARE-F ( ptr a ptr a n -- )     {: xb:ptr zb:ptr len:n :}
   len 0 ?do  xb i T-GET  SQUARE-F  zb i T-SET  loop ;
: TT-SUM-F  ( ptr a n -- r )  T-SUM ;       \ reduce the whole tensor to a scalar

\ --- backward (map the scalar VJP; write the input-cotangent tensor(s)) ---
\ ADD: dX=dZ, dY=dZ
: TT-ADD-BWD ( ptr a ptr a ptr a n -- ) {: dzb:ptr dxb:ptr dyb:ptr len:n :}
   len 0 ?do
      dzb i T-GET  ADD-BWD              \ ( dx dy )
      dyb i T-SET  dxb i T-SET
   loop ;
\ MUL: dX=dZ*Y, dY=dZ*X  (saved primals X, Y)
: TT-MUL-BWD ( ptr a ptr a ptr a ptr a ptr a n -- )
   {: dzb:ptr xb:ptr yb:ptr dxb:ptr dyb:ptr len:n :}
   len 0 ?do
      dzb i T-GET  xb i T-GET  yb i T-GET  MUL-BWD   \ ( dx dy )
      dyb i T-SET  dxb i T-SET
   loop ;
\ SUB: dX=dZ, dY=-dZ
: TT-SUB-BWD ( ptr a ptr a ptr a n -- ) {: dzb:ptr dxb:ptr dyb:ptr len:n :}
   len 0 ?do
      dzb i T-GET  SUB-BWD              \ ( dx dy )
      dyb i T-SET  dxb i T-SET
   loop ;
\ RELU: dX = x>0 ? dZ : 0  (saved primal X)
: TT-RELU-BWD ( ptr a ptr a ptr a n -- ) {: dzb:ptr xb:ptr dxb:ptr len:n :}
   len 0 ?do
      dzb i T-GET  xb i T-GET  RELU-BWD  dxb i T-SET
   loop ;
\ SQUARE: dX = dZ*2X  (saved primal X)
: TT-SQUARE-BWD ( ptr a ptr a ptr a n -- ) {: dzb:ptr xb:ptr dxb:ptr len:n :}
   len 0 ?do
      dzb i T-GET  xb i T-GET  SQUARE-BWD  dxb i T-SET
   loop ;
\ SUM: the scalar output cotangent broadcasts to every input element (dX[i]=dZ)
: TT-SUM-BWD ( r ptr a n -- ) {: dz:r dxb:ptr len:n :}
   dz dxb len T-FILL ;

end-package
