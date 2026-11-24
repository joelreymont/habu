\ linux-flags.f -- the BSD-shaped open and mmap flag bits the engine takes and
\ the Linux bits each becomes. Every pair is one BSD bit and one Linux bit, the
\ same on aarch64 and x86-64; a bit not kept and not paired is dropped.

package LINUX-FLAGS
public

\ The access mode, O_RDONLY, O_WRONLY or O_RDWR, matches in BSD and Linux.
3 constant OPEN-KEPT

\ Runs q on each open flag's BSD bit and Linux bit.
: OPEN-PAIRS ( [ n n -- ] -- )
   {: q :}
   $4 $800 q execute                             \ O_NONBLOCK
   $8 $400 q execute                             \ O_APPEND
   $200 $40 q execute                            \ O_CREAT
   $400 $200 q execute                           \ O_TRUNC
   $20000 $100 q execute ;                       \ O_NOCTTY

\ MAP_SHARED $1, MAP_PRIVATE $2 and MAP_FIXED $10 match in BSD and Linux.
$13 constant MMAP-KEPT

\ Runs q on each mmap flag's BSD bit and Linux bit.
: MMAP-PAIRS ( [ n n -- ] -- )
   {: q :}
   $1000 $20 q execute                           \ MAP_ANON
   $40 $4000 q execute ;                         \ MAP_NORESERVE

;package
