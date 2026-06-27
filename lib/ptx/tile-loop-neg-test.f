\ tile-loop-neg-test.f - committed negative regression for TILE-LOOP.
\
\ Spawns bin/hb to load lib/ptx/tile-loop-neg.f (a kernel whose loop body does not
\ preserve the accumulator) and asserts the checker REJECTED it: non-zero exit AND a
\ diagnostic located at 'tile-loop'. This pins the typed-counted-loop soundness as a
\ reproducible regression - the checker must reject an accumulator-violating body before
\ runtime. Load after lib/memory.f, the process libs (lib/fs.f, lib/process.f,
\ lib/process-argv.f, lib/process-env.f), and lib/test.f.

create TLN-OUT $2000 allot
create TLN-ERR $4000 allot

: TLN-RUN ( -- len len rc )
   PROC-ARGV-RESET
   s" --load"              >LEN PROC-ARGV+
   s" lib/errors.f"        >LEN PROC-ARGV+   s" lib/string.f"           >LEN PROC-ARGV+
   s" lib/float.f"         >LEN PROC-ARGV+   s" lib/fmt.f"              >LEN PROC-ARGV+
   s" lib/test.f"          >LEN PROC-ARGV+   s" src/arch/ptx/emit.f"    >LEN PROC-ARGV+
   s" lib/ptx/cg.f"        >LEN PROC-ARGV+   s" lib/ptx/header.f"       >LEN PROC-ARGV+
   s" lib/ptx/tile.f"      >LEN PROC-ARGV+   s" lib/ptx/tile-loop.f"    >LEN PROC-ARGV+
   s" lib/ptx/tile-loop-neg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  TLN-OUT $2000 >LEN  TLN-ERR $4000 >LEN  30000 >MS  RUN-ARGV-CAPTURE ;

: TLN-MAIN ( -- )
   T-RESET
   TLN-RUN {: outu erru rc :}
   rc RC>N 0 T<>                                      \ checker REJECTED -> non-zero exit
   TLN-ERR erru LEN>N s" tile-loop" CONTAINS? TTRUE   \ diagnostic located at the combinator
   s" NEG: accumulator-violating TILE-LOOP body rejected (located at tile-loop)" type cr
   T-REPORT ;

TLN-MAIN
