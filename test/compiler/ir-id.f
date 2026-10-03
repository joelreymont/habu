\ ir-id.f - checked shared compiler IR identity and authority tests.

require lib/test.f
require lib/string.f
require lib/process-argv.f
require lib/test/outcome.f
require lib/test/subject.f
require test/checker-assert.f

require src/compiler/ir/id.f

package IR-ID-TEST
private

$FFFFFFFF constant LOCAL-MAX
$1000 constant SUBJECT-CAP

\ The exit status the concurrency child uses for every overlap failure, named
\ OVERLAP-RC there too (test/compiler/ir-id-concurrency.f). The two live in
\ different processes, so the value has to be written twice; keep them equal.
$4C constant OVERLAP-RC

\ Every child case below decides its verdict from the child's own exit status,
\ and an exit status does not change when the host gets busy. The millisecond
\ budget handed to the capture is therefore a deadlock guard and nothing else:
\ it exists so a child that never exits cannot hang the gate forever, and it
\ must never be reachable by a child that is merely slow.
\
\ Measured 2026-07-30 on a 12-core machine. The spawned concurrency child costs
\ 0.62 to 1.10 s with the machine idle and 2.34 to 3.00 s while eight gate pool
\ slots are busy, and it is the most expensive child here: it starts a fresh
\ engine and loads lib/task.f and the identity module from scratch. The subject
\ children start from a fork of this process instead, so even the cases that
\ reload the identity module inside the fork cost a fraction of that: the whole
\ file, every child included, runs in 2.8 s idle and 9 to 14 s under those same
\ eight busy slots. WORST-CHILD-MS records that busiest measurement and
\ HANG-MARGIN keeps the guard an order of magnitude above it, so host load
\ cannot reach the guard. The product also stays well inside the registry's
\ outer timeout, so a real
\ deadlock is still reported here by name, with its case, instead of arriving as
\ an anonymous killed phase.
3000 constant WORST-CHILD-MS
10 constant HANG-MARGIN
WORST-CHILD-MS HANG-MARGIN * constant HANG-MS

create SUBJECT-OUT SUBJECT-CAP allot
create SUBJECT-ERR SUBJECT-CAP allot

: SCALAR-CASES ( -- )
   0 IR-ID:COUNT IR-ID:COUNT-N 0 T=
   $7FFFFFFFFFFFFFFF IR-ID:COUNT IR-ID:COUNT-N
      $7FFFFFFFFFFFFFFF T=
   0 IR-ID:POOL-OFF IR-ID:POOL-OFF-N 0 T=
   $7FFFFFFFFFFFFFFF IR-ID:POOL-OFF IR-ID:POOL-OFF-N
      $7FFFFFFFFFFFFFFF T=
   [: -1 IR-ID:COUNT drop ;] E-IR-SCALAR-RANGE TTHROWSQ
   [: -1 IR-ID:POOL-OFF drop ;] E-IR-SCALAR-RANGE TTHROWSQ ;

: SOURCE-CASE ( -- )
   IR-ID:NEW-MODULE {: key:IR-ID:ir-module-key owner:IR-ID:ir-module-id :}
   key 7 IR-ID:PACK-SOURCE {: id:IR-ID:ir-source-id :}
   id IR-ID:SOURCE-LOCAL 7 T=
   id IR-ID:SOURCE-OWNER owner IR-ID:MODULE-SAME? TTRUE
   key 8 IR-ID:COUNT id IR-ID:SOURCE-CHECK
      IR-ID:SOURCE-LOCAL 7 T= ;

: FUN-CASE ( -- )
   IR-ID:NEW-MODULE {: key:IR-ID:ir-module-key owner:IR-ID:ir-module-id :}
   key 8 IR-ID:PACK-FUN {: id:IR-ID:ir-fun-id :}
   id IR-ID:FUN-LOCAL 8 T=
   id IR-ID:FUN-OWNER owner IR-ID:MODULE-SAME? TTRUE
   key 9 IR-ID:COUNT id IR-ID:FUN-CHECK IR-ID:FUN-LOCAL 8 T= ;

: BLOCK-CASE ( -- )
   IR-ID:NEW-MODULE {: key:IR-ID:ir-module-key owner:IR-ID:ir-module-id :}
   key 9 IR-ID:PACK-BLOCK {: id:IR-ID:ir-block-id :}
   id IR-ID:BLOCK-LOCAL 9 T=
   id IR-ID:BLOCK-OWNER owner IR-ID:MODULE-SAME? TTRUE
   key 10 IR-ID:COUNT id IR-ID:BLOCK-CHECK IR-ID:BLOCK-LOCAL 9 T= ;

: OP-CASE ( -- )
   IR-ID:NEW-MODULE {: key:IR-ID:ir-module-key owner:IR-ID:ir-module-id :}
   key 10 IR-ID:PACK-OP {: id:IR-ID:ir-op-id :}
   id IR-ID:OP-LOCAL 10 T=
   id IR-ID:OP-OWNER owner IR-ID:MODULE-SAME? TTRUE
   key 11 IR-ID:COUNT id IR-ID:OP-CHECK IR-ID:OP-LOCAL 10 T= ;

: VALUE-CASE ( -- )
   IR-ID:NEW-MODULE {: key:IR-ID:ir-module-key owner:IR-ID:ir-module-id :}
   key 11 IR-ID:PACK-VALUE {: id:IR-ID:ir-value-id :}
   id IR-ID:VALUE-LOCAL 11 T=
   id IR-ID:VALUE-OWNER owner IR-ID:MODULE-SAME? TTRUE
   key 12 IR-ID:COUNT id IR-ID:VALUE-CHECK IR-ID:VALUE-LOCAL 11 T= ;

: TYPE-CASE ( -- )
   IR-ID:NEW-MODULE {: key:IR-ID:ir-module-key owner:IR-ID:ir-module-id :}
   key 12 IR-ID:PACK-TYPE {: id:IR-ID:ir-type-id :}
   id IR-ID:TYPE-LOCAL 12 T=
   id IR-ID:TYPE-OWNER owner IR-ID:MODULE-SAME? TTRUE
   key 13 IR-ID:COUNT id IR-ID:TYPE-CHECK IR-ID:TYPE-LOCAL 12 T= ;

: ATTR-CASE ( -- )
   IR-ID:NEW-MODULE {: key:IR-ID:ir-module-key owner:IR-ID:ir-module-id :}
   key 13 IR-ID:PACK-ATTR {: id:IR-ID:ir-attr-id :}
   id IR-ID:ATTR-LOCAL 13 T=
   id IR-ID:ATTR-OWNER owner IR-ID:MODULE-SAME? TTRUE
   key 14 IR-ID:COUNT id IR-ID:ATTR-CHECK IR-ID:ATTR-LOCAL 13 T= ;

: SYMBOL-CASE ( -- )
   IR-ID:NEW-MODULE {: key:IR-ID:ir-module-key owner:IR-ID:ir-module-id :}
   key 14 IR-ID:PACK-SYMBOL {: id:IR-ID:ir-symbol-id :}
   id IR-ID:SYMBOL-LOCAL 14 T=
   id IR-ID:SYMBOL-OWNER owner IR-ID:MODULE-SAME? TTRUE
   key 15 IR-ID:COUNT id IR-ID:SYMBOL-CHECK IR-ID:SYMBOL-LOCAL 14 T= ;

: SPAN-CASE ( -- )
   IR-ID:NEW-MODULE {: key:IR-ID:ir-module-key owner:IR-ID:ir-module-id :}
   key 15 IR-ID:PACK-SPAN {: id:IR-ID:ir-span-id :}
   id IR-ID:SPAN-LOCAL 15 T=
   id IR-ID:SPAN-OWNER owner IR-ID:MODULE-SAME? TTRUE
   key 16 IR-ID:COUNT id IR-ID:SPAN-CHECK IR-ID:SPAN-LOCAL 15 T= ;

: BAD-LOCAL-NEG ( -- )
   IR-ID:NEW-MODULE drop -1 IR-ID:PACK-SOURCE drop ;

: BAD-LOCAL-HIGH ( -- )
   IR-ID:NEW-MODULE drop LOCAL-MAX 1+ IR-ID:PACK-SOURCE drop ;

: BAD-BOUND ( -- )
   IR-ID:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   key 7 IR-ID:COUNT key 7 IR-ID:PACK-SOURCE IR-ID:SOURCE-CHECK drop ;

: BAD-OWNER ( -- )
   IR-ID:NEW-MODULE drop {: key-a:IR-ID:ir-module-key :}
   IR-ID:NEW-MODULE drop {: key-b:IR-ID:ir-module-key :}
   key-b 8 IR-ID:COUNT key-a 7 IR-ID:PACK-SOURCE IR-ID:SOURCE-CHECK drop ;

: RANGE-CASES ( -- )
   IR-ID:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   key LOCAL-MAX IR-ID:PACK-SOURCE IR-ID:SOURCE-LOCAL LOCAL-MAX T=
   [: BAD-LOCAL-NEG ;] E-IR-INDEX-RANGE TTHROWSQ
   [: BAD-LOCAL-HIGH ;] E-IR-INDEX-RANGE TTHROWSQ
   [: BAD-BOUND ;] E-IR-INDEX-BOUND TTHROWSQ
   [: BAD-OWNER ;] E-IR-OWNER TTHROWSQ ;

: WRONG-FAMILY-CASES ( -- )
   s" IR-BAD-SRC ( IR-ID:ir-module-key n -- IR-ID:ir-fun-id ) IR-ID:PACK-SOURCE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IR-BAD-FN ( IR-ID:ir-module-key n -- IR-ID:ir-source-id ) IR-ID:PACK-FUN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IR-BAD-OWNER ( IR-ID:ir-source-id -- IR-ID:ir-count ) IR-ID:SOURCE-OWNER"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IR-BAD-LOCAL ( IR-ID:ir-fun-id -- n ) IR-ID:SOURCE-LOCAL"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IR-BAD-CHECK ( IR-ID:ir-module-key IR-ID:ir-count IR-ID:ir-fun-id -- IR-ID:ir-fun-id ) IR-ID:SOURCE-CHECK"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IR-KEY-FORGE ( n -- IR-ID:ir-module-key )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IR-KEY-ERASE ( IR-ID:ir-module-key -- n )"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ Runs the source in a subject child under the deadlock guard and asserts its
\ exit code, leaving the stdout and stderr lengths.
: SUBJECT-EXITS ( ptr u8 n n -- n n ) {: src:ptr srcu:n want:n :}
   src srcu SUBJECT-OUT SUBJECT-CAP >LEN
   SUBJECT-ERR SUBJECT-CAP >LEN
   HANG-MS >MS SUBJECT:RUN {: outu:len erru:len oc :}
   src srcu SUBJECT-OUT outu LEN>N SUBJECT-ERR erru LEN>N oc want T-OUTCOME-EXITED=
   outu LEN>N erru LEN>N ;

: CONCURRENT-SOURCE$ ( n -- ptr u8 n ) {: mode:n :}
   SB-RESET
   s" require test/compiler/ir-id-concurrency.f" SB-APPEND
   10 SB-APPEND-C
   mode 2 = if
      s" IR-ID-CONCURRENCY:CLEANUP-REUSE" SB-APPEND
      SB$ exit
   then
   mode
   case
      0 of s" 0" endof
      1 of s" 1" endof
      E-TBL-BOUNDS throw
   endcase
   SB-APPEND
   s"  IR-ID-CONCURRENCY:RUN" SB-APPEND
   SB$ ;

\ Runs the concurrency child in this mode on a fresh engine under the deadlock
\ guard and asserts its exit code, leaving the stdout and stderr lengths.
: CONCURRENT-EXITS ( n n -- n n ) {: mode:n want:n :}
   mode CONCURRENT-SOURCE$ {: src:ptr srcu:n :}
   PROC-ARGV-RESET
   0 ARGV$ >LEN src srcu >LEN
   SUBJECT-OUT SUBJECT-CAP >LEN
   SUBJECT-ERR SUBJECT-CAP >LEN
   HANG-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   src srcu SUBJECT-OUT outu LEN>N SUBJECT-ERR erru LEN>N oc want T-OUTCOME-EXITED=
   outu LEN>N erru LEN>N ;

: CONCURRENT-GREEN ( -- )
   s" concurrent allocator barrier" T-LABEL
   1 0 CONCURRENT-EXITS {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: CONCURRENT-MUTATION ( -- )
   s" barrier-removal mutation fails overlap witness" T-LABEL
   0 OVERLAP-RC CONCURRENT-EXITS {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: CONCURRENT-ACTIVATE-CLEANUP ( -- )
   s" activation cleanup permits same-process task reuse" T-LABEL
   2 0 CONCURRENT-EXITS {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: CONCURRENT-UNIQUE ( -- )
   CONCURRENT-GREEN
   CONCURRENT-MUTATION
   CONCURRENT-ACTIVATE-CLEANUP ;

: SEAL-CASE ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n needle:ptr needleu:n :}
   src srcu ENGINE-ERROR:SEAL-PACKAGE SUBJECT-EXITS {: outu:n erru:n :}
   outu 0 T=
   SUBJECT-ERR erru needle needleu CONTAINS? TTRUE ;

: CONTEXT-SEAL-CASE ( ptr u8 n -- )
   ENGINE-ERROR:SEAL-VIOLATION SUBJECT-EXITS 2drop ;

: OWNER-CAST-REJECT ( ptr u8 n -- )
   UNCAUGHT-RC SUBJECT-EXITS {: outu:n erru:n :}
   outu 0 T=
   SUBJECT-ERR erru s" uncaught throw code 7135" CONTAINS? TTRUE ;

\ The forged package must be a name no engine package owns: `package` refuses a
\ sealed name (ENGINE-ERROR:SEAL-PACKAGE) before the cast is reached, which is a
\ different refusal than the ownership one this case is about.
: OWNER-CAST-CASE ( -- )
   S\" package FOREIGN\npublic\nCAST: ANY ( n -- IR-ID:ir-module-key )\n;package"
   OWNER-CAST-REJECT ;

: OWNER-CAST-SPOOF ( -- )
   S\" s\" IR-ID\" CHECKER-PACKAGE\nCAST: FAKE ( n -- IR-ID:ir-module-key )"
   OWNER-CAST-REJECT ;

: OWNER-MIRROR-SPOOF ( -- )
   S\" 105 CHECKER-PACKAGE-NAME c!\n114 CHECKER-PACKAGE-NAME 1 + c!\n45 CHECKER-PACKAGE-NAME 2 + c!\n105 CHECKER-PACKAGE-NAME 3 + c!\n100 CHECKER-PACKAGE-NAME 4 + c!\n5 CHECKER-PACKAGE-U !\nCHECKER-PACKAGE-PRIVATE CHECKER-PACKAGE-MODE !\nCAST: FAKE ( n -- IR-ID:ir-module-key )"
   OWNER-CAST-REJECT ;

: RELOAD-STABLE$ ( -- ptr u8 n )
   SB-RESET
   s" require src/compiler/ir/id.f" SB-APPEND 10 SB-APPEND-C
   s" IR-ID:NEW-MODULE nip" SB-APPEND 10 SB-APPEND-C
   s" INCLUDE-RESET-SCRATCH" SB-APPEND 10 SB-APPEND-C
   s" require src/compiler/ir/id.f" SB-APPEND 10 SB-APPEND-C
   s" : DISTINCT ( IR-ID:ir-module-id -- ) IR-ID:NEW-MODULE nip"
      SB-APPEND
   s"  IR-ID:MODULE-SAME? if 76 throw then ;" SB-APPEND 10 SB-APPEND-C
   s" DISTINCT" SB-APPEND
   SB$ ;

\ Does package PKG's private wordlist still hold NAME? A missing package record
\ answers true, so the case fails instead of passing on nothing.
: PRIVATE-FOUND? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n p:ptr pu:n :}
   p pu XREF-NAMESPACE-WL XREF-FIND-WL {: r:ptr :}
   r XREF-FOUND? 0= if 0 0= exit then
   a u r XREF-PKG-PRIVATE XREF-FIND-WL XREF-FOUND? ;

: AUTHORITY-CASES ( -- )
   s" package context hook is not addressable" T-LABEL
   s" PKG-LIVE-XT" XREF-FIND XREF-FOUND? TFALSE
   s" boot package provider is not addressable" T-LABEL
   s" CHECKER-PKG-LIVE-DEFAULT" XREF-FIND XREF-FOUND? TFALSE
   s" using slot provider hook is not addressable" T-LABEL
   s" USE-SLOT-XT" XREF-FIND XREF-FOUND? TFALSE
   s" boot using slot provider is not addressable" T-LABEL
   s" USE-SLOT-BOOT" XREF-FIND XREF-FOUND? TFALSE
   s" package context selector is not addressable" T-LABEL
   s" CHECKER-PKG-CONTEXT" XREF-FIND XREF-FOUND? TFALSE
   s" package authority reader is not addressable" T-LABEL
   s" CHECKER-RESOLVE:AUTHORITY" XREF-FIND XREF-FOUND? TFALSE
   s" package resync entry is not addressable in its package" T-LABEL
   s" SCOPE" s" CHECKER-RESYNC" PRIVATE-FOUND? TFALSE
   s" verifier package scope cell is not addressable" T-LABEL
   s" CHECKER-VERIFY-PKG-DEPTH" XREF-FIND XREF-FOUND? TFALSE
   s" verifier package snapshot name is not addressable" T-LABEL
   s" VPKG-NAME" XREF-FIND XREF-FOUND? TFALSE
   s" verifier package snapshot length is not addressable" T-LABEL
   s" VPKG-U" XREF-FIND XREF-FOUND? TFALSE
   s" verifier package snapshot mode is not addressable" T-LABEL
   s" VPKG-MODE" XREF-FIND XREF-FOUND? TFALSE
   s" verifier package snapshot save is not addressable" T-LABEL
   s" VPKG-SAVE" XREF-FIND XREF-FOUND? TFALSE
   s" verifier package snapshot restore is not addressable" T-LABEL
   s" VPKG-RESTORE" XREF-FIND XREF-FOUND? TFALSE
   s" family package hook is not addressable" T-LABEL
   s" TFAM-PKG-XT" XREF-FIND XREF-FOUND? TFALSE
   s" family package wrapper is not addressable" T-LABEL
   s" TFAM-PKG$*" XREF-FIND XREF-FOUND? TFALSE
   s" sealed private package cannot reopen" T-LABEL
   S\" package IR-ID\nprivate\n: FORGE ( n -- IR-ID:ir-module-key ) MINT-KEY ;\n;package"
      s" IR-ID" SEAL-CASE
   s" sealed qualified tail cannot define" T-LABEL
   s" : IR-ID:FORGE ( -- ) ;" s" IR-ID:FORGE" SEAL-CASE
   s" sealed private wordlist cannot mutate" T-LABEL
   S\" s\" IR-ID\" XREF-NAMESPACE-WL XREF-FIND-WL XREF-PKG-PRIVATE set-current\n: FORGE ( -- ) ;"
      s" FORGE" SEAL-CASE
   s" sealed source cannot include twice" T-LABEL
   S\" s\" src/compiler/ir/id.f\" included" s" IR-ID" SEAL-CASE
   s" nominal cast output requires owner package" T-LABEL
   OWNER-CAST-CASE
   s" checker package mirror cannot authorize cast" T-LABEL
   OWNER-CAST-SPOOF
   s" direct checker mirror mutation cannot authorize cast" T-LABEL
   OWNER-MIRROR-SPOOF
   s" stale namespace record cannot be installed" T-LABEL
   s" dbase@ data-base $90 + !" CONTEXT-SEAL-CASE
   s" public WID cannot diverge from its namespace" T-LABEL
   s" 1 data-base $78 + !" CONTEXT-SEAL-CASE
   s" private WID cannot diverge from its namespace" T-LABEL
   s" 1 data-base $80 + !" CONTEXT-SEAL-CASE
   s" current WID cannot diverge from its package" T-LABEL
   s" 1 data-base $28 + !" CONTEXT-SEAL-CASE
   s" module serial survives require replay" T-LABEL
   RELOAD-STABLE$ 0 SUBJECT-EXITS {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

public

: RUN ( -- )
   T-RESET
   SCALAR-CASES
   SOURCE-CASE
   FUN-CASE
   BLOCK-CASE
   OP-CASE
   VALUE-CASE
   TYPE-CASE
   ATTR-CASE
   SYMBOL-CASE
   SPAN-CASE
   RANGE-CASES
   WRONG-FAMILY-CASES
   CONCURRENT-UNIQUE
   AUTHORITY-CASES
   T-REPORT ;

;package

package IR-ID-AUDIT
private

26 constant RAW#
13 constant FAMILY#

: KIND$ ( n -- ptr u8 n )
   case
      0 of s" SOURCE" endof
      1 of s" FUN" endof
      2 of s" BLOCK" endof
      3 of s" OP" endof
      4 of s" VALUE" endof
      5 of s" TYPE" endof
      6 of s" ATTR" endof
      7 of s" SYMBOL" endof
      8 of s" SPAN" endof
      E-TBL-BOUNDS throw
   endcase ;

: FAMILY$ ( n -- ptr u8 n )
   case
      0 of s" ir-module-key" endof
      1 of s" ir-module-id" endof
      2 of s" ir-source-id" endof
      3 of s" ir-fun-id" endof
      4 of s" ir-block-id" endof
      5 of s" ir-op-id" endof
      6 of s" ir-value-id" endof
      7 of s" ir-type-id" endof
      8 of s" ir-attr-id" endof
      9 of s" ir-symbol-id" endof
      10 of s" ir-span-id" endof
      11 of s" ir-pool-offset" endof
      12 of s" ir-count" endof
      E-TBL-BOUNDS throw
   endcase ;

: KIND-RAW$ ( n n -- ptr u8 n ) {: kind:n form:n :}
   SB-RESET
   form 0= if s" MINT-" SB-APPEND kind KIND$ SB-APPEND else
      kind KIND$ SB-APPEND s" >N" SB-APPEND
   then
   SB$ ;

: RAW$ ( n -- ptr u8 n ) {: k:n :}
   k 8 < if
      k
      case
         0 of s" MINT-KEY" endof
         1 of s" KEY>N" endof
         2 of s" MINT-MODULE" endof
         3 of s" MODULE>N" endof
         4 of s" MINT-COUNT" endof
         5 of s" COUNT>N" endof
         6 of s" MINT-POOL-OFF" endof
         7 of s" POOL-OFF>N" endof
         E-TBL-BOUNDS throw
      endcase
      exit
   then
   k 8 - {: raw:n :}
   raw 2 / raw 2 mod KIND-RAW$ ;

: AUTH-NS ( -- ptr n )
   s" IR-ID" XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? TTRUE ;

: RAW-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   AUTH-NS {: ns:ptr :}
   a u ns XREF-PKG-PUBLIC XREF-FIND-WL XREF-FOUND? TFALSE
   a u ns XREF-PKG-PRIVATE XREF-FIND-WL XREF-FOUND? TTRUE ;


\ Read the public metadata surface; checker lookup helpers are private, so the
\ family's package name comes through a trusted wrapper of the sealed accessor.
TRUSTED: FAMILY-PKG$ ( n -- ptr u8 n ) TFAM:TFAM-PKG$ ;

: FAMILY-ID ( n -- n bool ) {: idx:n :}
   TFAM:TFAM-N@ 0 ?do
      i FAMILY-PKG$ s" IR-ID" STR= if
         i TFAM-NAME$ idx FAMILY$ STR= if i true unloop exit then
      then
   loop
   0 false ;

: FAMILY-SURFACE ( -- )
   FAMILY# 0 ?do
      i FAMILY-ID TTRUE
      dup TFAM:TFAM-ARITY@ 0 T=
      TFAM:TFAM-PUBLIC? TTRUE
   loop ;


: DICTIONARY-OWNERSHIP ( -- )
   RAW# 0 ?do i RAW$ RAW-ROW loop
   s" SERIAL-NEXT" AUTH-NS XREF-PKG-PUBLIC XREF-FIND-WL XREF-FOUND? TFALSE
   s" IR-RAW" XREF-NAMESPACE-WL XREF-FIND-WL XREF-FOUND? TFALSE ;

public

: RUN ( -- )
   T-RESET
   FAMILY-SURFACE
   DICTIONARY-OWNERSHIP
   T-REPORT ;

;package

IR-ID-AUDIT:RUN
IR-ID-TEST:RUN
