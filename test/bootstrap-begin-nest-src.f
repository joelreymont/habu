\ bootstrap-begin-nest-src.f - stage0 BEGIN frames stay off the compile cells.

\ Every `begin` pushes a 24-byte snapshot frame of the virtual stack, and the
\ seed's frame area holds 28 of them, the ceiling native names as
\ JIT-SNAP:FRAMES. While those frames sat at $360..$600 in the DATA header, the
\ 22nd landed on the seed's compile cells LASTC and RSP, the 23rd on EXITH (the
\ EXIT placeholder chain), LVD (the open DO levels) and the first LVH cell (that
\ level's LEAVE chain), and the rest on deeper LVH cells. So each definition
\ below nests to the full 28 with one of those chains open across the nest: an
\ early `exit` (EXITH) and an earlier `leave` (LVD, LVH). On the old band
\ BN-EXIT died of SIGILL and BN-LEAVE's `loop` was refused with rc 70.

variable BN-FAILS

: BN-EXPECT ( n n -- )
   = 0= if BN-FAILS @ 1 + BN-FAILS ! then ;

\ n + 1 through 28 one-turn loops, or n itself through the early exit.
: BN-EXIT ( n -- n )
   dup 0 < if exit then
   begin begin begin begin begin begin begin
   begin begin begin begin begin begin begin
   begin begin begin begin begin begin begin
   begin begin begin begin begin begin begin
   1 +
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until ;

\ Two turns add one each; the third leaves before the nest.
: BN-LEAVE ( -- n )
   0  4 0 do
      i 2 = if leave then
      begin begin begin begin begin begin begin
      begin begin begin begin begin begin begin
      begin begin begin begin begin begin begin
      begin begin begin begin begin begin begin
      1 +
      dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
      dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
      dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
      dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
      dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
      dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
      dup 0 > until  dup 0 > until  dup 0 > until  dup 0 > until
   loop ;

: BN-REPORT ( -- )
   BN-FAILS @ 0= if s" ok" type cr exit then
   BN-FAILS @ . s" bootstrap-begin-nest failures" 1 die ;

5 BN-EXIT 6 BN-EXPECT
-5 BN-EXIT -5 BN-EXPECT
BN-LEAVE 2 BN-EXPECT
BN-REPORT
