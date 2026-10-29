\ bootstrap-begin-nest-src.f - stage0 BEGIN frames stay off the compile cells.

\ Every `begin` pushes a 24-byte snapshot frame of the virtual stack, and the
\ seed's frame area holds 28 (SNAP-FRAMES, bootstrap/cg/forth.fs). A frame area
\ at $360..$600 in the DATA header puts the 22nd frame on the compile cells
\ LASTC and RSP, the 23rd on EXITH (the EXIT placeholder chain), LVD (open DO
\ levels) and the first LVH cell (that level's LEAVE chain). Each definition
\ nests to 28 with one chain open: an early `exit` (EXITH) or an earlier `leave`
\ (LVD, LVH). On that band BN-EXIT dies of SIGILL and BN-LEAVE's `loop` is
\ refused, rc 70.

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
