\ ir-id.f - checked shared compiler IR identity and authority tests.

require lib/test.f
require lib/string.f
require lib/process-argv.f
require lib/test/outcome.f
require lib/test/subject.f
require test/checker-assert.f

package IR-ID-AUDIT
private

variable ND0
variable ND1
variable TF0
variable TF1
variable RAW-HITS
variable PUBLIC-HITS

public

: BEFORE ( -- )
   ndict@ ND0 !
   TFAM-N@ TF0 ! ;

: AFTER ( -- )
   ndict@ ND1 !
   TFAM-N@ TF1 ! ;

;package

IR-ID-AUDIT:BEFORE
require src/compiler/ir/id.f
IR-ID-AUDIT:AFTER

package IR-ID-TEST
private

$FFFFFFFF constant LOCAL-MAX
$1000 constant SUBJECT-CAP
70 constant INTERNAL-RC

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
\ cannot reach the guard. The product also stays well inside the gate's own
\ per-suite guard (STDLIB-GATE:SUITE-TIMEOUT-MS, 120 s nominal and more when the
\ box is loaded), so a real
\ deadlock is still reported here by name, with its case, instead of arriving as
\ an anonymous killed phase.
3000 constant WORST-CHILD-MS
10 constant HANG-MARGIN
WORST-CHILD-MS HANG-MARGIN * constant HANG-MS

create SUBJECT-OUT SUBJECT-CAP allot
create SUBJECT-ERR SUBJECT-CAP allot

\ One assert per call, exactly like the shared T-OUTCOME-EXITED= it replaces,
\ but every way of not exiting gets its own name. A guard that expired and a
\ child that exited with the wrong status are different findings and must never
\ print the same line: the shared assertion reports both as `expected 0 got 1`,
\ which is how a busy host and a broken allocator became indistinguishable here.
\ Naming the guard needs the budget, and only this caller knows it; giving
\ lib/test/outcome.f its own per-variant diagnostics needs that file packaged
\ first, which is tracked by dot habu-name-the-outcome-a80c2197.
: CHILD-HUNG ( -- )
   T-NEXT
   s" child never exited: deadlock guard expired" T-ASSERT-DETAIL
   s" guard ms: " type HANG-MS .
   T-LABEL-CLEAR ;

: CHILD-SIGNALED ( n -- ) {: sig:n :}
   T-NEXT
   s" child died on a signal without an exit status" T-ASSERT-DETAIL
   s" signal: " type sig .
   T-LABEL-CLEAR ;

: CHILD-EXITED= ( outcome n -- ) {: want:n :}
   MATCH outcome
     exited OF want T= ENDOF
     signaled OF CHILD-SIGNALED ENDOF
     timeout OF CHILD-HUNG ENDOF
   ;MATCH ;

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

: SUBJECT-RUN ( ptr u8 n -- len len outcome )
   SUBJECT-OUT SUBJECT-CAP >LEN
   SUBJECT-ERR SUBJECT-CAP >LEN
   HANG-MS >MS SUBJECT:RUN ;

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

: CONCURRENT-RUN ( n -- len len outcome ) {: mode:n :}
   mode CONCURRENT-SOURCE$ {: src:ptr srcu:n :}
   PROC-ARGV-RESET
   0 ARGV$ >LEN src srcu >LEN
   SUBJECT-OUT SUBJECT-CAP >LEN
   SUBJECT-ERR SUBJECT-CAP >LEN
   HANG-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME ;

: CONCURRENT-GREEN ( -- )
   s" concurrent allocator barrier" T-LABEL
   1 CONCURRENT-RUN 0 CHILD-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   erru 0 T= ;

: CONCURRENT-MUTATION ( -- )
   s" barrier-removal mutation fails overlap witness" T-LABEL
   0 CONCURRENT-RUN OVERLAP-RC CHILD-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   erru 0 T= ;

: CONCURRENT-ACTIVATE-CLEANUP ( -- )
   s" activation cleanup permits same-process task reuse" T-LABEL
   2 CONCURRENT-RUN 0 CHILD-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   erru 0 T= ;

: CONCURRENT-UNIQUE ( -- )
   CONCURRENT-GREEN
   CONCURRENT-MUTATION
   CONCURRENT-ACTIVATE-CLEANUP ;

: SEAL-CASE ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n needle:ptr needleu:n :}
   src srcu SUBJECT-RUN ENGINE-ERROR:SEAL-PACKAGE CHILD-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   SUBJECT-ERR erru needle needleu CONTAINS? TTRUE ;

: CONTEXT-SEAL-CASE ( ptr u8 n -- )
   SUBJECT-RUN ENGINE-ERROR:SEAL-VIOLATION CHILD-EXITED=
   LEN>N drop
   LEN>N drop ;

: INTERNAL-AUTH-CASE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SUBJECT-RUN INTERNAL-RC CHILD-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   SUBJECT-ERR erru s" hb: internal engine word: " CONTAINS? TTRUE
   SUBJECT-ERR erru a u CONTAINS? TTRUE ;

: OWNER-CAST-REJECT ( ptr u8 n -- )
   SUBJECT-RUN UNCAUGHT-RC CHILD-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   SUBJECT-ERR erru s" uncaught throw code 7135" CONTAINS? TTRUE ;

: OWNER-CAST-CASE ( -- )
   S\" package HIR\npublic\nCAST: ANY ( n -- IR-ID:ir-module-key )\n;package"
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

: AUTHORITY-CASES ( -- )
   s" package context hook is not addressable" T-LABEL
   s" PKG-LIVE-XT" XREF-FIND XREF-FOUND? TFALSE
   s" boot package provider is not addressable" T-LABEL
   s" CHECKER-PKG-LIVE-DEFAULT" XREF-FIND XREF-FOUND? TFALSE
   s" package context selector is not addressable" T-LABEL
   s" CHECKER-PKG-CONTEXT" XREF-FIND XREF-FOUND? TFALSE
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
   s" verifier mirror start is engine-internal" T-LABEL
   s" CHECKER-VERIFY-PKG-START" INTERNAL-AUTH-CASE
   s" verifier mirror close is engine-internal" T-LABEL
   s" CHECKER-VERIFY-PKG-DONE" INTERNAL-AUTH-CASE
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
   S\" s\" IR-ID\" XREF-NAMESPACE-WL XREF-FIND-WL XREF-LEN set-current\n: FORGE ( -- ) ;"
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
   RELOAD-STABLE$ SUBJECT-RUN 0 CHILD-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
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
42 constant PUBLIC#
13 constant FAMILY#
9 constant KIND#

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

: KIND-API$ ( n n -- ptr u8 n ) {: kind:n form:n :}
   SB-RESET
   form
   case
      0 of s" PACK-" SB-APPEND kind KIND$ SB-APPEND endof
      1 of kind KIND$ SB-APPEND s" -OWNER" SB-APPEND endof
      2 of kind KIND$ SB-APPEND s" -LOCAL" SB-APPEND endof
      3 of kind KIND$ SB-APPEND s" -CHECK" SB-APPEND endof
      E-TBL-BOUNDS throw
   endcase
   SB$ ;

: PUBLIC$ ( n -- ptr u8 n ) {: k:n :}
   k 6 < if
      k
      case
         0 of s" NEW-MODULE" endof
         1 of s" MODULE-SAME?" endof
         2 of s" COUNT" endof
         3 of s" COUNT-N" endof
         4 of s" POOL-OFF" endof
         5 of s" POOL-OFF-N" endof
         E-TBL-BOUNDS throw
      endcase
      exit
   then
   k 6 - {: api:n :}
   api 4 / api 4 mod KIND-API$ ;

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

: AUTH-NS ( -- ptr a )
   s" IR-ID" XREF-NAMESPACE-WL XREF-FIND-WL
   dup XREF-FOUND? TTRUE ;

: RAW-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   AUTH-NS {: ns:ptr :}
   ns XREF-START {: pub:n :}
   ns XREF-LEN {: pri:n :}
   a u pub XREF-FIND-WL XREF-FOUND? TFALSE
   a u pri XREF-FIND-WL-INDEX {: idx:n :}
   idx ND0 @ >= TTRUE
   idx ND1 @ < TTRUE
   0 RAW-HITS !
   ND0 @ begin dup ND1 @ < while
      dup XREF-REC dup a u XREF-MATCH? if
         XREF-WORDLIST pri T=
         1 RAW-HITS +!
      else
         drop
      then
      1+
   repeat drop
   RAW-HITS @ 1 T= ;

: PUBLIC-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   AUTH-NS {: ns:ptr :}
   ns XREF-START {: pub:n :}
   a u pub XREF-FIND-WL-INDEX {: idx:n :}
   idx ND0 @ >= TTRUE
   idx ND1 @ < TTRUE
   0 PUBLIC-HITS !
   ND0 @ begin dup ND1 @ < while
      dup XREF-REC dup a u XREF-MATCH? if
         XREF-WORDLIST pub T=
         1 PUBLIC-HITS +!
      else
         drop
      then
      1+
   repeat drop
   PUBLIC-HITS @ 1 T= ;

: PUBLIC-SURFACE ( -- )
   AUTH-NS XREF-START {: pub:n :}
   0 PUBLIC-HITS !
   ND0 @ begin dup ND1 @ < while
      dup XREF-REC XREF-WORDLIST pub = if 1 PUBLIC-HITS +! then
      1+
   repeat drop
   PUBLIC-HITS @ PUBLIC# T=
   PUBLIC# 0 ?do i PUBLIC$ PUBLIC-ROW loop
   s" SERIAL-NEXT" pub XREF-FIND-WL XREF-FOUND? TFALSE ;

: FAMILY-SURFACE ( -- )
   TF1 @ TF0 @ - FAMILY# T=
   FAMILY# 0 ?do
      TF0 @ i + dup TFAM-NAME$ i FAMILY$ T-STR= TTRUE
      TFAM-ARITY@ 0 T=
   loop ;

: DICTIONARY-OWNERSHIP ( -- )
   KIND# 2 * 8 + RAW# T=
   RAW# 0 ?do i RAW$ RAW-ROW loop
   s" IR-RAW" XREF-NAMESPACE-WL XREF-FIND-WL XREF-FOUND? TFALSE ;

public

: RUN ( -- )
   T-RESET
   FAMILY-SURFACE
   PUBLIC-SURFACE
   DICTIONARY-OWNERSHIP
   T-REPORT ;

;package

IR-ID-AUDIT:RUN
IR-ID-TEST:RUN
