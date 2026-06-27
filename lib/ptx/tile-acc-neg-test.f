\ tile-acc-neg-test.f - committed negative regression for the accumulator completion gate.
\
\ Spawns bin/hb to load lib/ptx/tile-acc-neg.f (a kernel that stores a raw acc<>) and
\ asserts the checker REJECTED it: non-zero exit AND a diagnostic located at 'store'. This
\ pins the "an unfinalized accumulator cannot be stored to global" soundness rule (acc<>
\ never unifies with tile<>) as a reproducible regression. Load after lib/memory.f, the
\ process libs (lib/fs.f, lib/process.f, lib/process-argv.f, lib/process-env.f), and
\ lib/test.f.

create TAN-OUT $2000 allot
create TAN-ERR $4000 allot

: TAN-RUN ( -- len len rc )
   PROC-ARGV-RESET
   s" --load"              >LEN PROC-ARGV+
   s" lib/errors.f"        >LEN PROC-ARGV+   s" lib/string.f"           >LEN PROC-ARGV+
   s" lib/float.f"         >LEN PROC-ARGV+   s" lib/fmt.f"              >LEN PROC-ARGV+
   s" lib/test.f"          >LEN PROC-ARGV+   s" src/arch/ptx/emit.f"    >LEN PROC-ARGV+
   s" lib/ptx/cg.f"        >LEN PROC-ARGV+   s" lib/ptx/header.f"       >LEN PROC-ARGV+
   s" lib/ptx/tile.f"      >LEN PROC-ARGV+   s" lib/ptx/tile-acc.f"     >LEN PROC-ARGV+
   s" lib/ptx/tile-acc-neg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  TAN-OUT $2000 >LEN  TAN-ERR $4000 >LEN  30000 >MS  RUN-ARGV-CAPTURE ;

: TAN-MAIN ( -- )
   T-RESET
   TAN-RUN {: outu erru rc :}
   rc RC>N 0 T<>                                  \ checker REJECTED -> non-zero exit
   TAN-ERR erru LEN>N s" store" CONTAINS? TTRUE   \ diagnostic located at the store
   s" NEG: raw (unfinalized) accumulator store rejected (acc<> != tile<>)" type cr
   T-REPORT ;

TAN-MAIN
