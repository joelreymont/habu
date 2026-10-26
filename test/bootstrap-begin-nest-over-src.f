\ bootstrap-begin-nest-over-src.f - one `begin` past the seed's 28 frames.

\ The 29th `begin` asks for a frame the band does not have. The seed refuses
\ the definition by name, with its limit and the depth it needed, and exits 75.
\ The bound is the seed's own SNAP-FRAMES (bootstrap/cg/forth.fs); native sizes
\ its frames by the control-flow cap and has no such refusal.
\ Nothing after the refusal runs.

: BN-OVER ( n -- n )
   begin begin begin begin begin begin begin
   begin begin begin begin begin begin begin
   begin begin begin begin begin begin begin
   begin begin begin begin begin begin begin
   begin
   1 +
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until ;
s" BN-OVER compiled" type cr
