split-str
\ aio-fill.f - AIO held at its ceiling, package AIO-FILL. FILL submits polls that
\ never fire until AIO refuses one, and answers that refusal's code; UNFILL
\ cancels and awaits each and closes its pipe. CEILING is the code AIO refuses
\ with on this target. RAISE-FDS lifts the soft descriptor limit to what a test
\ holds open, the fill's pipes included.
\
\ STORAGE CLASS. PROCESS-WIDE: one fill at a time, its tickets and pipes in
\ static buffers.

require src/lib/error.f
require src/lib/aio.f
require src/lib/ffi-abi.f
require src/lib/test/assert.f

package AIO-FILL

private

-9950 constant E-FILL-PIPE        \ a fill pipe the OS refused, outside every lib block

public

\ Darwin's poll bound (OPEN_MAX), past Linux's record ceiling: the most polls that fill AIO
$2800 constant FILL-CAP

private

variable FILL-N                   \ the polls in flight
variable FILL-CODE
FILL-CAP TYPED-BUFFER FILL-TICKET AIO:ticket SCRATCH-ONLY
FILL-CAP TYPED-BUFFER FILL-R n    \ each on its own pipe: Darwin bounds descriptors, and polls on
FILL-CAP TYPED-BUFFER FILL-W n    \ duplicates of one pipe make each AIO turn take 2-3 s

PROCESS-SYMBOLS
FUNCTION: GET-LIMIT getrlimit ( n ^u8 -- i32 ) ;FUNCTION
FUNCTION: SET-LIMIT setrlimit ( n ^u8 -- i32 ) ;FUNCTION

create FD-LIMIT 16 allot

: NOFILE ( -- n ) HB-TARGET-MACOS? if 8 else 7 then ;

: FILL-ONE ( -- )
   FILL-N @ {: idx:n :}
   idx FILL-R @ >FD AIO:READABLE -1 >MS AIO:POLL idx FILL-TICKET !
   1 FILL-N +! ;

: OUTCOME-GONE ( AIO:outcome -- )
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      refused OF drop ENDOF
   ;MATCH ;

public

\ launchd gives a macOS login shell a soft limit of 256, under a fill's pipes.
\ Linux's RLIM_INFINITY reads as -1 here, and is left as it is.
: RAISE-FDS ( n -- ) {: want:n :}
   NOFILE FD-LIMIT GET-LIMIT 0 T=
   FD-LIMIT @ {: soft:n :}
   soft 0< soft want >= or if exit then
   want FD-LIMIT !
   NOFILE FD-LIMIT SET-LIMIT 0 T= ;

: FILL ( -- n )
   0 FILL-CODE !
   begin
      FILL-CODE @ 0= FILL-N @ FILL-CAP < and
   while
      pipe {: r:n w:n rc:n :}
      rc 0 <> if E-FILL-PIPE throw then
      r FILL-N @ FILL-R !
      w FILL-N @ FILL-W !
      [: FILL-ONE ;] catch FILL-CODE !
      FILL-CODE @ 0 <> if r close-rc drop w close-rc drop then
   repeat
   FILL-CODE @ ;

: UNFILL ( -- )
   FILL-N @ 0 ?do
      i FILL-TICKET @ {: t:AIO:ticket :}
      t AIO:CANCEL
      t AIO:AWAIT OUTCOME-GONE
      i FILL-R @ close-rc drop
      i FILL-W @ close-rc drop
   loop
   0 FILL-N ! ;

: CEILING ( -- n )
   HB-TARGET-MACOS? if E-AIO-POLL-BOUND else E-AIO-FULL then ;

;package
