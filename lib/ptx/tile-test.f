\ ptx-tile-test.f - checked SAXPY kernel over the M4 tile vocabulary.
\
\ The KERNEL: definition IS the positive proof: it certifies against its declared
\ parametric effect (a reject would emit a checker diagnostic and fail the load).
\ The element type f32, extent token extent-n, and block-256 instantiate the
\ polymorphic operation signatures in lib/ptx/tile.f; the mask token threads from
\ GRID-CTX through LOAD / SCALE / +. to STORE by unification.

T-RESET

256 %BLOCK

KERNEL: SAXPY ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n>  uniform<f32> -- )  GRID: ceil-n-256
   {: x y a :}
   x GRID-CTX {: g :}
   x g LOAD  a SCALE
   y g LOAD  +.
   y g STORE ;

KERNEL: RELU-SPAN ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   dup GRID-CTX
   2dup LOAD RELU
   rot rot STORE ;

\ Clean load past this point is the positive proof: KERNEL: verified SAXPY's body
\ against its declared parametric effect. A reject emits a diagnostic + fails load.

T-REPORT
