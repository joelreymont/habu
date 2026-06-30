\ gemm-checked-neg-test.f - committed negative regression for the checked inline K-loop.
\
\ Spawns bin/hb to load lib/ptx/gemm-checked-neg.f (a GEMM body whose inline `?do` K-loop
\ is not stack-neutral) and asserts the checker REJECTED it: non-zero exit AND a diagnostic
\ located at 'loop'. This pins "the inline counted loop is genuinely checked - its body must
\ be stack-neutral (the accumulator is loop-invariant)" as a reproducible regression. Load
\ after the process libs (lib/fs.f, lib/process.f, lib/process-argv.f, lib/process-env.f)
\ and lib/test.

require lib/ptx/process-test-prelude.f

create GCN-OUT $2000 allot
create GCN-ERR $4000 allot

: GCN-RUN ( -- len len rc )
   PROC-ARGV-RESET
   s" --load"              >LEN PROC-ARGV+
   s" lib/errors.f"        >LEN PROC-ARGV+   s" lib/string.f"           >LEN PROC-ARGV+
   s" lib/float.f"         >LEN PROC-ARGV+   s" lib/fmt.f"              >LEN PROC-ARGV+
   s" lib/test.f"          >LEN PROC-ARGV+   s" src/arch/ptx/emit.f"    >LEN PROC-ARGV+
   s" lib/ptx/cg.f"        >LEN PROC-ARGV+   s" lib/ptx/header.f"       >LEN PROC-ARGV+
   s" lib/ptx/tile.f"      >LEN PROC-ARGV+   s" lib/ptx/tile-smem.f"    >LEN PROC-ARGV+
   s" lib/ptx/tile-acc.f"  >LEN PROC-ARGV+   s" lib/ptx/gemm-checked-neg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  GCN-OUT $2000 >LEN  GCN-ERR $4000 >LEN  30000 >MS  RUN-ARGV-CAPTURE ;

: GCN-MAIN ( -- )
   T-RESET
   GCN-RUN {: outu erru rc :}
   rc RC>N 0 T<>                                 \ checker REJECTED -> non-zero exit
   GCN-ERR erru LEN>N s" loop" CONTAINS? TTRUE   \ diagnostic located at the loop
   s" NEG: non-stack-neutral inline K-loop rejected (accumulator not loop-invariant)" type cr
   T-REPORT ;

GCN-MAIN
