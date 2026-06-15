\ shadow-lint.f — toolchain colon definitions must not shadow engine PRIM names.
\ Self-hosted shadow lint. The engine dict is later-wins and the checker
\ records later sigs over PTAB, so a toolchain word named like a prim silently
\ replaces it for every program the toolchain-loaded engine compiles.
\ Run:  cat tools/lint/lib.f tools/lint/shadow-lint.f | bin/hb
\ (lib.f already did `0 set-check`.)

create FB 131072 allot                       \ one file at a time

\ ---- prim-name store: copied out of habu1.f so FB can be reused per file ----
create PNAMES 8192 allot   variable PEND
$200 constant PMAX
create POFF PMAX cells allot   create PLEN PMAX cells allot   variable PN#
: ADD-PRIM  {: a u :}
   a  PNAMES PEND @ +  u BMOVE
   PNAMES PEND @ +  POFF PN# @ cells + !   u PLEN PN# @ cells + !
   PEND @ u + PEND !   PN# @ 1+ PN# ! ;
: PRIM?  {: a u :}  ( -- f )                  \ case-insensitive membership
   0 begin dup PN# @ < while
      dup cells POFF + @  over cells PLEN + @  a u STR=CI IF drop -1 exit THEN  1+
   repeat  drop  0 ;

\ token "NAME"" (trailing quote from s" NAME") -> NAME
: TRIMQ  {: a u :}  ( -- a u' )  a  u 0 > a u 1- + c@ 34 = and IF u 1- ELSE u THEN ;

\ ---- extract prim names: s" NAME" ['] X FPRIM[-L] -> NAME (habu1.f tokenized) ----
variable SI
: SCAN-PRIMS  ( -- )
   0 PN# !  0 PEND !  3 SI !
   begin SI @ TN# @ < while
      SI @ TOK s" FPRIM" PREFIX?  SI @ 2 - TOK s" [']" STR= and IF
         SI @ 3 - TOK TRIMQ ADD-PRIM THEN
      SI @ 1+ SI !
   repeat ;

\ ---- lint one toolchain file: a BOL ':' def whose name is a prim is a SHADOW ----
variable BAD  variable LI
: LINT-FILE  {: pa pu :}
   pa pu FB 131072 READ-FILE  TOKENIZE
   0 LI !
   begin LI @ TN# @ 1- < while
      LI @ TOK s" :" STR=  LI @ TOK0? and IF
         LI @ 1+ TOK  2dup PRIM? IF
            s" SHADOW " type pa pu type s" : `: " type type s" ` hides a prim" type cr
            BAD @ 1+ BAD !
         ELSE 2drop THEN
      THEN
      LI @ 1+ LI !
   repeat ;

\ prims live in habu1.f; lint every snap-toolchain file against them.
: SHADOW-LINT
   0 BAD !
   s" src/habu/habu1.f" FB 131072 READ-FILE  TOKENIZE  SCAN-PRIMS
   s" src/core/util.f"      LINT-FILE   s" src/core/checker.f"   LINT-FILE
   s" src/core/render.f"    LINT-FILE   s" src/core/sha256.f"    LINT-FILE
   s" src/arch/arm64/asm.f" LINT-FILE   s" src/arch/arm64/icode.f" LINT-FILE
   s" src/arch/arm64/mnem.f" LINT-FILE  s" src/os/macos/sys.f"   LINT-FILE
   s" src/os/macos/env.f"   LINT-FILE   s" src/habu/treeshake.f" LINT-FILE
   s" src/habu/rt.f"        LINT-FILE   s" src/habu/crash.f"     LINT-FILE
   s" src/os/macos/macho.f" LINT-FILE   s" src/os/macos/sign2.f" LINT-FILE
   s" src/habu/habu1.f"     LINT-FILE   s" src/habu/prof.f"      LINT-FILE
   s" src/habu/regalloc.f"  LINT-FILE   s" src/habu/jit.f"       LINT-FILE
   s" src/habu/habu2.f"     LINT-FILE   s" src/habu/snap.f"      LINT-FILE
   BAD @ 0 > IF  s" shadow-lint: " type BAD @ . s"  collision(s)" type cr  1 die
   ELSE  s" shadow-lint: clean (" type PN# @ . s"  prims checked)" type cr  THEN ;
SHADOW-LINT
