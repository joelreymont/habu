\ tile-smem-neg-test.f - committed negative regression for the shared-memory address space.
\
\ Spawns bin/hb to load lib/ptx/tile-smem-neg.f (a kernel that reads a global span through
\ SLOAD, which requires a shared span) and asserts the checker REJECTED it: non-zero exit
\ AND a diagnostic located at 'sload'. This pins the space-shared / space-global
\ never-unify rule as a reproducible regression. Load after the process libs (lib/fs.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f) and lib/test.f.

create TSN-OUT $2000 allot
create TSN-ERR $4000 allot

: TSN-RUN ( -- len len rc )
   PROC-ARGV-RESET
   s" --load"              >LEN PROC-ARGV+
   s" lib/errors.f"        >LEN PROC-ARGV+   s" lib/string.f"           >LEN PROC-ARGV+
   s" lib/float.f"         >LEN PROC-ARGV+   s" lib/fmt.f"              >LEN PROC-ARGV+
   s" lib/test.f"          >LEN PROC-ARGV+   s" src/arch/ptx/emit.f"    >LEN PROC-ARGV+
   s" lib/ptx/cg.f"        >LEN PROC-ARGV+   s" lib/ptx/header.f"       >LEN PROC-ARGV+
   s" lib/ptx/tile.f"      >LEN PROC-ARGV+   s" lib/ptx/tile-smem.f"    >LEN PROC-ARGV+
   s" lib/ptx/tile-smem-neg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  TSN-OUT $2000 >LEN  TSN-ERR $4000 >LEN  30000 >MS  RUN-ARGV-CAPTURE ;

: TSN-MAIN ( -- )
   T-RESET
   TSN-RUN {: outu erru rc :}
   rc RC>N 0 T<>                                  \ checker REJECTED -> non-zero exit
   TSN-ERR erru LEN>N s" sload" CONTAINS? TTRUE   \ diagnostic located at the shared load
   s" NEG: global span read through SLOAD rejected (space-shared != space-global)" type cr
   T-REPORT ;

TSN-MAIN
