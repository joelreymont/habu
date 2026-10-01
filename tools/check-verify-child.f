\ check-verify-child.f - the verifier child CHECK:VERIFY-BYTES runs.
\
\ tools/check-verify-core.f spawns it on bin/hb, in the caller's working
\ directory, the tree root; nothing else runs it:
\
\    ENGINE --load tools/check-verify-child.f -- SUBJECT [DEP ...] < BYTES
\
\ BYTES is the subject's text and SUBJECT the canonical absolute path it is
\ checked as. Each DEP is a file of its require closure, canonical and absolute,
\ in dependency order. The image is the engine's boot prefix, the verifier and
\ this file, so no tool's word stands in for, or collides with, a word of the
\ subject. Nothing of SUBJECT or its closure runs: the verifier scans source and
\ records what it declares.
\
\ Every DEP this image does not hold already, then SUBJECT, is verified with all
\ errors in one neutral checker scope, each under its own path and with
\ positions in its own bytes, so a file sees what every earlier one declared. A
\ DEP the image holds is skipped, as `require` skips it.
\
\ stdout is the schema-1 JSON packets, one per line in verification order, then
\ one result line:
\
\    check-verify: verified | refused | held
\
\ held is answered before anything is verified: this image holds SUBJECT though
\ the engine does not provide it (the verifier's source and this file), so it
\ cannot be verified here. The parent answers engine-provided itself. stderr is
\ prose: whatever else the checker renders, and a line for each file whose
\ verification a throw stopped. A child that ends any other way, the verifier's
\ own `die` included, writes no result line.

require src/habu/verify-source.f

\ Engine words with no checked effect; tools/check-core.f declares them alike.
s" CHECKER-SCOPE-START-NEUTRAL" s" --" TRUST
s" CHECKER-SCOPE-DONE" s" --" TRUST

package CHECK-VERIFY-CHILD

$10000 constant CHUNK                   \ bytes asked of one read
$100000 constant DIAG-CAP               \ what the checker renders for one file
32 constant NUM-CAP
10 constant LF
$7B constant LBRACE
1 constant OUT-FD
2 constant ERR-FD

DYNAMIC-BUFFER SUBJECT u8               \ the subject's bytes, from stdin
DYNAMIC-BUFFER DEP u8                   \ the dependency being verified, from its file
DYNAMIC-BUFFER DIAG u8                  \ the checker renders into this
variable SUBJECT-U
variable DEP-U
variable FD
variable RD
variable LINE-AT
variable FAILED
variable NUM-I
TYPED-VARIABLE CUR-A ptr u8             \ the bytes VERIFY-CUR verifies
variable CUR-U
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


: READ-DEP ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu PATH0 open-rd FD !
   FD @ 0 < if
      ERR-FD pa pu WRITE ERR-FD NEWLINE
      s" check-verify: cannot open a dependency" 74 die
   then
   0 DEP-U !
   begin
      DEP-U @ CHUNK + DEP-RESERVE
      FD @ DEP-U @ DEP CHUNK read RD !
      RD @ 0 < if s" check-verify: cannot read a dependency" 74 die then
      RD @ 0 >
   while
      DEP-U @ RD @ + DEP-U !
   repeat
   FD @ close ;


\ A JSON object line is a packet for stdout; any other rendered line is prose.
: EMIT-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   a c@ LBRACE = if OUT-FD a u WRITE OUT-FD NEWLINE exit then
   ERR-FD a u WRITE ERR-FD NEWLINE ;


: EMIT-DIAG ( -- )
   DIAG-BUFFER$ {: a:ptr u:n :}
   0 LINE-AT !
   u 0 ?do
      a i + c@ LF = if
         a LINE-AT @ + i LINE-AT @ - EMIT-LINE
         i 1+ LINE-AT !
      then
   loop
   a LINE-AT @ + u LINE-AT @ - EMIT-LINE ;


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
   CUR-A @ CUR-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;


\ Verify the bytes as LABEL with all errors, positions counted from their first
\ byte, and pass on what the checker rendered.
: VERIFY-AS ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n label:ptr labelu:n :}
   a CUR-A !
   u CUR-U !
   label labelu DIAG-FILE!
   0 0= DIAG-JSON!
   1 1 0 DIAG-ORIGIN!
   0 DIAG DIAG-CAP DIAG-BUFFER!
   MULTI-ERR-BEGIN
   [: VERIFY-CUR ;] catch {: rc:n :}
   MULTI-ERR-END {: rejects:n :}
   EMIT-DIAG
   DIAG-BUFFER-OFF
   rc 0<> rejects 0<> or if -1 FAILED ! then
   rc 0= if exit then
   label labelu rc rejects STOPPED ;


\ The engine provides the path, or this child loaded it: what `require` skips.
: HELD? ( ptr u8 n -- bool )
   SOURCE-ROOT:RESOLVE nip nip ;


: VERIFY-DEP ( n -- ) {: i:n :}
   i SCRIPT-ARGV$ {: pa:ptr pu:n :}
   pa pu HELD? if exit then
   pa pu READ-DEP
   0 DEP DEP-U @ pa pu VERIFY-AS ;


: VERIFY-ALL ( -- )
   SCRIPT-ARGC 1 ?do i VERIFY-DEP loop
   0 SUBJECT SUBJECT-U @ 0 SCRIPT-ARGV$ VERIFY-AS ;


: RESULT ( ptr u8 n -- ) {: a:ptr u:n :}
   OUT-FD s" check-verify: " WRITE
   OUT-FD a u WRITE
   OUT-FD NEWLINE ;


: VERIFY-CLOSURE ( -- )
   0 FAILED !
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
   SCRIPT-ARGC 1 < if s" usage: check-verify-child.f -- SUBJECT [DEP ...]" 64 die then
   DIAG-CAP DIAG-RESERVE
   READ-SUBJECT
   0 SCRIPT-ARGV$ HELD? if s" held" RESULT exit then
   VERIFY-CLOSURE ;

;package

CHECK-VERIFY-CHILD:MAIN
