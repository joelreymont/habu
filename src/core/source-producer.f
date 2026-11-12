\ src/core/source-producer.f — the engine roles an image keeps: its kernel, checker and compiler.
\ Loaded after include.f and the target layout in the engine prefix.

package SOURCE-PRODUCER
private

create CHECKER-DIGEST 32 allot
create COMPILER-DIGEST 32 allot
variable CHECKER-KNOWN
variable COMPILER-KNOWN
TYPED-VARIABLE DEFAULT-COMPILER [ ptr u8 n -- ]

CAST: KERNEL-XT ( n -- [ -- ptr u8 n bool ] )
CAST: COMPILER>N ( [ ptr u8 n -- ] -- n )

\ A source definition on an older host must not impersonate its absent kernel
\ metadata primitive. The text extent is the running engine's, as its boot
\ published it: a build window for another OS loads that OS's header offsets, so
\ no layout constant may enter this test.
: ENGINE-TEXT? ( n -- bool )
   {: xt:n :}
   data-base RBASE-CELL + @ {: base:n :}
   data-base TEXT-SIZE-CELL + @ {: size:n :}
   xt base >= xt base - size < and ;

: KERNEL$ ( -- ptr u8 n bool )
   s" kernel-producer$" WL:PRIMITIVES search-wl {: xt:n :}
   xt 0= if NULL$ 0 0= 0= exit then
   xt ENGINE-TEXT? 0= if NULL$ 0 0= 0= exit then
   xt KERNEL-XT execute ;

\ Sets a target's roles, through the private wordlist.
: INSTALL-ROLES ( ptr u8 ptr u8 [ ptr u8 n -- ] -- )
   {: checker:ptr compiler:ptr entry :}
   checker CHECKER-DIGEST 32 BYTE-COPY
   compiler COMPILER-DIGEST 32 BYTE-COPY
   entry DEFAULT-COMPILER !
   0 0= CHECKER-KNOWN !
   0 0= COMPILER-KNOWN ! ;

public

0 constant KERNEL
1 constant CHECKER
2 constant COMPILER

\ A provider registered after the image roles were installed can change native
\ output while leaving the compiler dispatch cell untouched. The captured
\ compiler role no longer describes that emitting process.
: COMPILER-CHANGED ( -- ) 0 0= 0= COMPILER-KNOWN ! ;

\ The two captured digests describe the implementation loaded to produce the
\ image. The kernel digest comes from immutable metadata in the running host;
\ a host built before that primitive was added has no kernel fact to report.
: ROLE$ ( n -- ptr u8 n bool )
   dup KERNEL = if drop KERNEL$ exit then
   dup CHECKER = if drop
      CHECKER-KNOWN @ 0= if NULL$ 0 0= 0= exit then
      CHECKER-DIGEST 32 0 0= exit
   then
   COMPILER = if
      COMPILER-KNOWN @ 0= if NULL$ 0 0= 0= exit then
      COMPILER-DIGEST 32 0 0= exit
   then
   NULL$ 0 0= 0= ;


\ A prior source load may replace the tier-1 compiler dispatch. The captured
\ digest describes the active compiler only while the default entry still owns
\ that dispatch; a wrapper or replacement has no known emitting role.
: ACTIVE-COMPILER$ ( -- ptr u8 n bool )
   COMPILER-KNOWN @ 0= if NULL$ 0 0= 0= exit then
   data-base NCOMP-DISPATCH:XT-CELL + CELL-VIEW @
      DEFAULT-COMPILER @ COMPILER>N <> if NULL$ 0 0= 0= exit then
   COMPILER-DIGEST 32 0 0= ;

;package
