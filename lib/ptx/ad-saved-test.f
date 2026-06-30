\ ad-saved-test.f - the auto-derived adjoints TYPE-CHECK as checked KERNEL: bodies once
\ the saved-value vocabulary (lib/ptx/ad-saved.f) is typed. Each KERNEL: certifying IS the
\ proof; a reject would emit a diagnostic and fail the load. Load after lib/ptx/tile.f,
\ lib/ptx/collective.f, and lib/ptx/ad-saved.f.

require lib/ptx/test-prelude.f

T-RESET

256 %BLOCK

\ EXP. backward (AD-REVERSE of `... EXP. ...` -> `... SAVED-Y *. ...`): dx = dz * y
KERNEL: TEST-EXP-BWD ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: dz dx :}  dz GRID-CTX {: g :}
   dz g LOAD  SAVED-Y *.  dx g STORE ;

\ NEG on a UNIFORM (the B-/B/ adjoint shape -Sum(dz)): dz -> BLOCK-SUM -> NEG -> BROADCAST
KERNEL: TEST-NEG-UNIFORM ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: dz dx :}  dz GRID-CTX {: g :}
   dz g LOAD  BLOCK-SUM NEG BROADCAST  dx g STORE ;

\ NEG on a TILE (forward NEG is self-adjoint): proves NEG ( a -- a ) is polymorphic
KERNEL: TEST-NEG-TILE ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: dz dx :}  dz GRID-CTX {: g :}
   dz g LOAD  NEG  dx g STORE ;

\ Clean load past this point is the positive proof: the typed saved-value vocabulary makes
\ the auto-derived backward fragments certify as checked kernels (SAVED-Y, polymorphic NEG).

T-REPORT
