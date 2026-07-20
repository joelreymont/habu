\ layout-defer.f - DEFER-LAYOUT-BUFFER (derive-from-model deferred-offset column).
\
\ Proves the stage-1-validated mechanism end to end: the deferred accessor body
\ (reading rebindable offset/count cells) type-checks IDENTICALLY to the immediate
\ LAYOUT-BUFFER accessor under the armed LAYOUT-INTRO window; an unbound access
\ dies NAMED; NAME-BIND sizes the storage from the counted need with grow-to-
\ largest reuse; a bind past the sanity ceiling dies NAMED before any mutation
\ (transactional), leaving the prior bind intact.

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;

: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" expected " type want . s" got " type got . cr
   then ;

\ ---- deferred column over an arity-0 nominal scalar family (width 1) ---------
TYPEFAMILY dtk 0

\ The declaration itself is the decisive assertion: if the checker rejected the
\ deferred accessor body, this line throws and the load fails.
DEFER-LAYOUT-BUFFER DTK-AT dtk

TRUSTED: N>DTK ( n -- dtk ) ;
TRUSTED: DTK>N ( dtk -- n ) ;

: DTK-GET ( n -- n )  DTK-AT @ DTK>N ;
: DTK-PUT ( n n -- ) {: v:n i:n :}  v N>DTK i DTK-AT ! ;

variable DTK-I
: DTK-IDX ( -- )  DTK-I @ DTK-AT drop ;
: DTK-IDX-RC ( n -- n )  DTK-I !  [: DTK-IDX ;] catch ;
: DTK-BIND ( -- )  DTK-I @ DTK-AT-BIND ;
: DTK-BIND-RC ( n -- n )  DTK-I !  [: DTK-BIND ;] catch ;

\ (A) unbound: count-cell 0 -> dies NAMED, never a silent zero-offset read
0 DTK-IDX-RC E-LAYOUT-UNBOUND T=

\ (B) bind to 3 (derived count); zero image; store + read back
3 DTK-AT-BIND
0 DTK-GET 0 T=                       \ fresh region reads as family id 0
11 0 DTK-PUT  22 1 DTK-PUT  33 2 DTK-PUT
0 DTK-GET 11 T=
1 DTK-GET 22 T=
2 DTK-GET 33 T=

\ (C) bounds: live count is the bound, both ends die NAMED
3 DTK-IDX-RC E-LAYOUT-BOUNDS T=
-1 DTK-IDX-RC E-LAYOUT-BOUNDS T=

\ (D) grow-to-largest: bind to 5 allots a fresh larger region (zero image)
5 DTK-AT-BIND
4 DTK-GET 0 T=                       \ new slot present and zeroed
44 4 DTK-PUT  4 DTK-GET 44 T=

\ (E) shrink reuses the region; the live count shrinks, so index 2 is now oob
2 DTK-AT-BIND
2 DTK-IDX-RC E-LAYOUT-BOUNDS T=
0 DTK-GET 0 T=                       \ index 0 still readable

\ (F) ceiling is transactional: a bind past LDEFER-CELL-MAX dies NAMED with NO
\ mutation, so the prior (count=2) bind survives
$200000 DTK-BIND-RC E-LAYOUT-CEIL T=
1 DTK-IDX-RC 0 T=                    \ index 1 still in bounds (prior bind intact)
2 DTK-IDX-RC E-LAYOUT-BOUNDS T=      \ index 2 still oob (count unchanged at 2)

\ ---- copy-on-grow (NAME-GROW): preserve live cells across a two-phase grow ----
variable DTK-BASE
TRUSTED: DTK-ADDR ( ptr dtk -- n ) ;   \ read the accessor's base pointer as a number
: DTK-GROW ( -- )  DTK-I @ DTK-AT-GROW ;
: DTK-GROW-RC ( n -- n )  DTK-I !  [: DTK-GROW ;] catch ;

\ a second deferred column, left unbound, for the unbound-grow test
DEFER-LAYOUT-BUFFER UGB-AT dtk
: UGB-GROW ( -- )  DTK-I @ UGB-AT-GROW ;
: UGB-GROW-RC ( n -- n )  DTK-I !  [: UGB-GROW ;] catch ;

\ (G) copy-on-grow preserves live cells: bind to 3, write, GROW past capacity;
\ every live cell is byte-identical in the moved region and new cells read 0.
3 DTK-AT-BIND
111 0 DTK-PUT  222 1 DTK-PUT  333 2 DTK-PUT
7 DTK-AT-GROW                        \ 7 > cap 5 -> realloc, copy live 3, zero the tail
0 DTK-GET 111 T=                     \ live cell carried byte-identical
1 DTK-GET 222 T=
2 DTK-GET 333 T=
3 DTK-GET 0 T=                       \ grown-into cells zeroed
6 DTK-GET 0 T=
6 DTK-IDX-RC 0 T=                    \ live count is exactly 7: index 6 in bounds
7 DTK-IDX-RC E-LAYOUT-BOUNDS T=      \ index 7 oob

\ (H) grow-to-at-least: G's grow to 7 doubled the region (cap 5 -> max(10,7)=10),
\ so growing to 8 fits and reuses it in place - the accessor base does not move.
0 DTK-AT DTK-ADDR DTK-BASE !
8 DTK-AT-GROW
0 DTK-AT DTK-ADDR DTK-BASE @ T=      \ base unmoved => capacity grew past the request
0 DTK-GET 111 T=                     \ live cells intact across the in-place grow
7 DTK-GET 0 T=                       \ index 7 now exposed and zeroed

\ (I) growing a never-bound column dies NAMED - the caller must BIND first
0 UGB-GROW-RC E-LAYOUT-UNBOUND T=

\ (J) ceiling is transactional for GROW too: a grow past LDEFER-CELL-MAX dies
\ NAMED with NO mutation, so the prior (live 8) region and its cells survive.
$200000 DTK-GROW-RC E-LAYOUT-CEIL T=
0 DTK-GET 111 T=                     \ live cells intact
7 DTK-IDX-RC 0 T=                    \ index 7 still in bounds (live unchanged at 8)
8 DTK-IDX-RC E-LAYOUT-BOUNDS T=      \ index 8 still oob

\ (K) a within-capacity grow ZEROES the newly exposed slots even when a prior
\ shrink left them dirty: write index 5, shrink live below it, grow back -> 0.
555 5 DTK-PUT
3 DTK-AT-BIND                        \ shrink live to 3 (region reused, index 5 dirty)
8 DTK-AT-GROW                        \ grow back within cap -> exposes [3,8), zeroes them
5 DTK-GET 0 T=                       \ index 5 zeroed, not the stale 555

\ ---- definer rejects the same illegal types LAYOUT-BUFFER does --------------
DEFLINEAR dfl-lin
SUMTYPE dfl-owned 0
   VARIANT hold dfl-lin ;VARIANT
;SUMTYPE

variable DFL-EVAL-A
variable DFL-EVAL-U
: DFL-EVAL-RUN ( -- )  DFL-EVAL-A @ DFL-EVAL-U @ INCLUDE-EVALUATE ;
: DFL-EVAL ( ptr u8 n -- n )  DFL-EVAL-U ! DFL-EVAL-A !  [: DFL-EVAL-RUN ;] catch ;

s" DEFER-LAYOUT-BUFFER DC-OPEN dfl-owned" DFL-EVAL E-LAYOUT-BUFFER T=
s" DEFER-LAYOUT-BUFFER DTK-AT dtk" DFL-EVAL $4E T=      \ redefinition of the accessor
s" DEFER-LAYOUT-BUFFER DC-BAD TFAM:BAD" DFL-EVAL E-LAYOUT-BUFFER T=

\ armed-window state stays sealed (no user handle to the one-shot boundary)
s" LBUF-PEND!" 0 search-wl 0= -1 T=
s" LBUF-PEND-MATCH?" 0 search-wl 0= -1 T=

: DTK-REPORT ( -- )
   #FAIL @ 0= if s" ok" type cr exit then
   #FAIL @ . s" layout-defer failures" 1 die ;
DTK-REPORT
