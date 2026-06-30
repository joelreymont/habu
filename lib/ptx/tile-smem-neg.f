\ tile-smem-neg.f - NEGATIVE fixture: a kernel that reads a global span through the
\ shared-memory load. SLOAD requires span<space-shared,t,e>, but `s` is
\ span<space-global,...>; the concrete address-space symbols never unify, so the checker
\ MUST reject this at load with a located 'sload' diagnostic (proven by
\ lib/ptx/tile-smem-neg-test.f). This file is expected to FAIL to load; it is never part
\ of a positive suite. Load after lib/ptx/tile.f and lib/ptx/tile-smem.f.

256 %BLOCK

KERNEL: BAD-SPACE ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: s :} \ typed-local-lint: allow-bare-local - generic PTX param types contain commas.
   s COOP-CTX {: g :} \ typed-local-lint: allow-bare-local - fresh coopctx mask is inferred.
   s g SLOAD                       \ s is space-global; SLOAD wants space-shared -> REJECT
   drop ;
