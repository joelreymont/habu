\ relu-v4-cg.f - emit a checked v4 RELU kernel to PTX.

256 %BLOCK

KERNEL: RELUK-V4 ( span<space-global,f32,e> span<space-global,f32,e> uniform<f32> -- ) GRID: ceil-n-1024
   {: x y a :} \ ( x y a -- ) typed-local-lint: allow-bare-local - PTX param roles.
   x GRID-CTX-V4 {: g :} \ ( -- ) typed-local-lint: allow-bare-local - fresh mask role.
   x g LOAD-V4 RELU-V4 \ ( -- )
   y g STORE-V4 ; \ ( -- )

: EMIT-RELUK-V4 ( -- )
   CG-RESET CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS \ ( -- )
   1 SPAN-REG 2 SPAN-REG 1 UNIFORM-REG RELUK-V4 \ ( -- )
   CG-RET CG-CLOSE ; \ ( -- )

EMIT-RELUK-V4
