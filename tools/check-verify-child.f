\ check-verify-child.f - the verifier child CHECK:VERIFY-BYTES runs.
\
\ tools/check-verify-core.f spawns it on bin/hb, in the caller's working
\ directory, the tree root; nothing else runs it:
\
\    ENGINE --load tools/check-verify-child.f -- SUBJECT < BYTES
\
\ BYTES is the subject's text and SUBJECT the canonical absolute path it is
\ checked as. The image is the engine's boot prefix, the verifier and
\ this file, so no tool's word stands in for, or collides with, a word of the
\ subject. Nothing of SUBJECT or its closure runs: the verifier scans source and
\ records what it declares.
\
\ The verifier follows top-level loader acts in one neutral checker scope, with
\ each file's path and positions while it is open. `require` skips held paths;
\ `include` verifies every occurrence.
\
\ stdout is the schema-1 JSON packets, one per line in verification order, each
\ written as the checker makes it, so a child that dies has passed on every
\ packet made before; then one result line:
\
\    check-verify: verified | refused | held
\
\ held is answered before anything is verified: this image holds SUBJECT though
\ the engine does not provide it (the verifier's source and this file), so it
\ cannot be verified here. The parent answers engine-provided itself. stderr is
\ prose: a line for each file whose verification a throw stopped, and whatever
\ else the engine writes there, a `die`'s message among it. A child that ends
\ any other way, the verifier's own `die` included, writes no result line.

require src/habu/verify-source.f

\ Engine words with no checked effect; tools/check-core.f declares them alike.
s" CHECKER-SCOPE-START-NEUTRAL" s" --" TRUST
s" CHECKER-SCOPE-DONE" s" --" TRUST

package CHECK-VERIFY-CHILD

$10000 constant CHUNK                   \ bytes asked of one read
32 constant NUM-CAP
10 constant LF
1 constant OUT-FD
2 constant ERR-FD

DYNAMIC-BUFFER SUBJECT u8               \ the subject's bytes, from stdin
variable SUBJECT-U
variable RD
variable FAILED
variable NUM-I
create NUM NUM-CAP allot
create NL 1 allot


: WRITE ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
   u 0= if exit then
   fd a u write u <> if s" check-verify: write failed" 74 die then ;


: NEWLINE ( n -- ) {: fd:n :}
   LF NL c!
   fd NL 1 WRITE ;


: U$ ( n -- ptr u8 n )
   NUM-CAP NUM-I !
   begin
      dup 10 mod 48 + NUM-I @ 1- dup NUM-I ! NUM + c!
      10 /
   dup 0= until drop
   NUM NUM-I @ + NUM-CAP NUM-I @ - ;


: ERR-N ( n -- ) {: n:n :}
   n 0 < if ERR-FD s" -" WRITE ERR-FD 0 n - U$ WRITE exit then
   ERR-FD n U$ WRITE ;


: READ-SUBJECT ( -- )
   0 SUBJECT-U !
   begin
      SUBJECT-U @ CHUNK + SUBJECT-RESERVE
      0 SUBJECT-U @ SUBJECT CHUNK read RD !
      RD @ 0 < if s" check-verify: cannot read the subject" 74 die then
      RD @ 0 >
   while
      SUBJECT-U @ RD @ + SUBJECT-U !
   repeat ;


\ The definitions after the throw went unverified, and a throw the checker
\ rendered no packet for has nothing else to show it.
: STOPPED ( ptr u8 n n n -- ) {: label:ptr labelu:n rc:n rejects:n :}
   ERR-FD label labelu WRITE
   ERR-FD s" : verification stopped by throw " WRITE
   rc ERR-N
   ERR-FD s"  after " WRITE
   rejects ERR-N
   ERR-FD s"  rejected definitions" WRITE
   ERR-FD NEWLINE ;


: VERIFY-CUR ( -- )
   0 SUBJECT SUBJECT-U @ 0 SCRIPT-ARGV$ VERIFY:SOURCE-COMPOSE-IN-SCOPE ;


\ One multi-error window covers the complete load composition.
: VERIFY-ALL ( -- )
   0 SCRIPT-ARGV$ DIAG-FILE!
   1 1 0 DIAG-ORIGIN!
   MULTI-ERR-BEGIN
   [: VERIFY-CUR ;] catch {: rc:n :}
   MULTI-ERR-END {: rejects:n :}
   rc 0<> rejects 0<> or if -1 FAILED ! then
   rc 0= if exit then
   VERIFY:SOURCE-COMPOSE-STOPPED$ rc rejects STOPPED ;


\ The engine provides the path, or this child loaded it: what `require` skips.
: HELD? ( ptr u8 n -- bool )
   SOURCE-ROOT:RESOLVE nip nip ;


: RESULT ( ptr u8 n -- ) {: a:ptr u:n :}
   OUT-FD s" check-verify: " WRITE
   OUT-FD a u WRITE
   OUT-FD NEWLINE ;


\ Every diagnostic the checker renders from here is a packet on stdout.
: VERIFY-CLOSURE ( -- )
   0 FAILED !
   0 0= DIAG-JSON!
   OUT-FD DIAG-FD!
   CHECKER-SCOPE-START-NEUTRAL
   [: VERIFY-ALL ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   rc 0<> if
      ERR-FD s" check-verify: stopped by throw " WRITE rc ERR-N ERR-FD NEWLINE
      rc throw
   then
   FAILED @ 0<> if s" refused" RESULT exit then
   s" verified" RESULT ;

public

: MAIN ( -- )
   SCRIPT-ARGC 1 <> if s" usage: check-verify-child.f -- SUBJECT" 64 die then
   READ-SUBJECT
   0 SCRIPT-ARGV$ HELD? if s" held" RESULT exit then
   VERIFY-CLOSURE ;

;package

CHECK-VERIFY-CHILD:MAIN
