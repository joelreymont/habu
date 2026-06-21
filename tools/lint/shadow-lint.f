\ shadow-lint.f — toolchain definitions must not shadow engine PRIM names.
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
: ADD-PRIM  ( ptr u8 n -- ) {: a:ptr u :}
   a  PNAMES PEND @ +  u BMOVE
   PNAMES PEND @ +  POFF PN# @ cells + !   u PLEN PN# @ cells + !
   PEND @ u + PEND !   PN# @ 1+ PN# ! ;
: PRIM?  ( ptr u8 n -- bool ) {: a:ptr u :}   \ case-insensitive membership
   0 begin dup PN# @ < while
      dup cells POFF + @  over cells PLEN + @  a u STR=CI IF drop LINT-TRUE exit THEN  1+
   repeat  drop  LINT-FALSE ;

\ token "NAME"" (trailing quote from s" NAME") -> NAME
: TRIMQ  ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u 0 <= IF a u exit THEN
   a u 1- + c@ 34 = IF a u 1- exit THEN
   a u ;

\ ---- extract prim names: s" NAME" ['] X FPRIM[-L] -> NAME (habu1.f tokenized) ----
variable SI
: SCAN-PRIMS  ( -- )
   0 PN# !  0 PEND !  3 SI !
   begin SI @ TN# @ < while
      SI @ TOK s" FPRIM" PREFIX?  SI @ 2 - TOK s" [']" STR= and IF
         SI @ 3 - TOK TRIMQ ADD-PRIM THEN
      SI @ 1+ SI !
   repeat ;

\ ---- lint one toolchain file: any defining word whose name is a prim is a SHADOW ----
variable BAD  variable LI
: DEF-NAME-OFFSET ( n -- n )
   dup TOK s" :" STR= IF drop 1 exit THEN
   dup TOK s" constant" STR=CI IF drop 1 exit THEN
   dup TOK s" variable" STR=CI IF drop 1 exit THEN
   dup TOK s" create" STR=CI IF drop 1 exit THEN
   drop 0 ;

: LINT-DEFINITION ( ptr u8 n n -- ) {: pa:ptr pu k :}
   k DEF-NAME-OFFSET dup 0= IF drop exit THEN
   k + dup TN# @ >= IF drop exit THEN
   TOK 2dup PRIM? IF
      s" SHADOW " type pa pu type s" : `" type type s" ` hides a prim" type cr
      BAD @ 1+ BAD !
   ELSE 2drop THEN ;

: LINT-FILE  ( ptr u8 n -- ) {: pa:ptr pu :}
   pa pu FB 131072 READ-FILE  TOKENIZE
   0 LI !
   begin LI @ TN# @ < while
      pa pu LI @ LINT-DEFINITION
      LI @ 1+ LI !
   repeat ;

\ prims live in habu1.f; lint every snap-toolchain file against them.
: SHADOW-LINT
   0 BAD !
   s" src/habu/habu1.f" FB 131072 READ-FILE  TOKENIZE  SCAN-PRIMS
   s" tools/lint/lib.f"  LINT-FILE   s" tools/lint/shadow-lint.f" LINT-FILE
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
