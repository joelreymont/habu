\ owner-wid-emitter.f - test-image-only cold owner-registry proof.

package OWNER-WID-COLD-TEST

\ Standalone guard. This file is bundle-injected by BUILD-EXT:OWNER-WID-BUILD;
\ a require would bypass the build pin, so the guard is inline with the same
\ message and rc as test/owner-wid-guard.f. The emitter vocabulary (LIT64,)
\ only exists in the image-build stage, never in an installed engine.
78 constant OWE-GUARD-RC

: OWE-BUILD-STAGE? ( -- bool )
   s" LIT64," 0 search-wl 0 <> ;

: OWE-GUARD ( -- )
   OWE-BUILD-STAGE? if exit then
   s" owner-wid suites run inside test/run.f's forge harness" OWE-GUARD-RC die ;

OWE-GUARD

0 0= constant YES
0 0= 0= constant NO
$C8DFFCA6 constant COUNT-LDAR
$F90000AE constant ROW-STR
$C89FFCA6 constant COUNT-STLR
$400 constant PREFIX-CAP
$4C constant PREFIX-RC
create PREFIX-BUF PREFIX-CAP allot
create PREFIX-STAT 144 allot
variable PREFIX-U
variable PREFIX-FD
0 PREFIX-U !
-1 PREFIX-FD !

: PREFIX-FAIL ( -- )
   PREFIX-FD @ dup 0 >= if close else drop then
   -1 PREFIX-FD !
   s" owner-WID fixture source read failed" PREFIX-RC die ;

: PREFIX-SIZE ( -- n )
   s" test/owner-wid-source.f" PATH0 PREFIX-STAT stat64 0 < if PREFIX-FAIL then
   PREFIX-STAT 96 + @ dup 0 < if drop PREFIX-FAIL then
   dup PREFIX-CAP > if drop PREFIX-FAIL then ;

: PACKAGE-NAME$ ( -- ptr u8 n )
   s" OWNER-WID-COLD-TEST" ;

: ADD ( n n bool label -- ) {: pub:n pri:n want:bool fail:label :}
   9 pub LIT64,  9 G-PUSH
   9 pri LIT64,  9 G-PUSH
   OWNER-WID-EMIT:ADD-LABEL@ BL,
   9 G-POP  9 0 CMPI,
   want if C-EQ fail BCOND, else C-NE fail BCOND, then ;

: COUNT= ( n label -- ) {: want:n fail:label :}
   6 DATA OWNER-WID-N-CELL LDR,
   7 want MOVZ,
   6 7 CMP,  C-NE fail BCOND, ;

: ROW= ( n n label -- ) {: idx:n want:n fail:label :}
   5 OWNER-WID-OFF idx OWNER-WID-ROW * + MOVZ,
   5 DATA 5 ADD,  6 5 0 LDR,
   7 want LIT64,
   6 7 CMP,  C-NE fail BCOND, ;

: COUNT@, ( -- )
   5 OWNER-WID-N-CELL MOVZ,  5 DATA 5 ADD,
   COUNT-LDAR EMITW ;

: COUNT!, ( -- )
   5 OWNER-WID-N-CELL MOVZ,  5 DATA 5 ADD,
   COUNT-STLR EMITW ;

: RECORD-MATCH ( label label label -- )
   {: name:label hit:label miss:label :}
   LBL LBL {: bytes:label inline:label :}
   14 5 40 LDR,  15 0 MOVN,  14 15 CMP,  C-NE miss BCOND,
   14 5 16 LDR,
   15 14 DNAME-EXT ANDI,
   14 14 12 LSLI,  14 14 12 LSRI,
   3 PACKAGE-NAME$ nip MOVZ,  14 3 CMP,  C-NE miss BCOND,
   16 5 24 ADDI,
   15 inline CBZ,
   16 5 24 LDR,
   inline LBL,
   7 0 MOVZ,
   bytes LBL,
   7 3 CMP,  C-GE hit BCOND,
   14 16 7 ADD,  14 14 0 LDRB,
   15 name ADR,  15 15 7 ADD,  15 15 0 LDRB,
   14 15 CMP,  C-NE miss BCOND,
   7 7 1 ADDI,  bytes B, ;

: WIDN-ADVANCE ( -- )
   LBL LBL LBL {: pub:label max:label done:label :}
   9 10 CMP,  C-HI pub BCOND,
   14 10 1 ADDI,  max B,
   pub LBL,  14 9 1 ADDI,
   max LBL,
   15 DATA WIDN-CELL LDR,
   15 14 CMP,  C-GE done BCOND,
   14 DATA WIDN-CELL STR,
   done LBL, ;

: REGISTER-PAIR ( label -- ) {: fail:label :}
   LBL {: done:label :}
   OWNER-WID-EMIT:PAIR-LABEL@ BL,
   13 done CBNZ,
   9 G-PUSH  10 G-PUSH
   OWNER-WID-EMIT:ADD-LABEL@ BL,
   9 G-POP  9 fail CBZ,
   done LBL,
   WIDN-ADVANCE ;

: EMIT-PREFIX ( -- )
   LBL LBL LBL LBL LBL LBL LBL LBL LBL
   {: loop:label miss:label hit:label found:label fail:label done:label
      data-done:label msg:label name:label :}
   SP SP 16 SUBI,  30 SP 0 STR,
   5 DBASE 0 ADDI,  6 NDICT 0 ADDI,  17 0 MOVZ,
   loop LBL,
      6 found CBZ,
      name hit miss RECORD-MATCH
      hit LBL,
         17 fail CBNZ,
         17 5 0 ADDI,
      miss LBL,
         5 5 DREC ADDI,  6 6 1 SUBI,  loop B,
   found LBL,
   17 done CBZ,
   9 17 0 LDR,  10 17 8 LDR,
   14 9 32 LSRI,  14 fail CBNZ,
   14 10 32 LSRI,  14 fail CBNZ,
   9 fail CBZ,  10 fail CBZ,
   9 10 CMP,  C-EQ fail BCOND,
   14 OWNER-WID-LIMIT LIT64,
   9 14 CMP,  C-HI fail BCOND,
   10 14 CMP,  C-HI fail BCOND,
   fail REGISTER-PAIR
   done LBL,
      30 SP 0 LDR,  SP SP 16 ADDI,
      data-done B,
   fail LBL,
      0 2 MOVZ,  1 msg ADR,  2 40 MOVZ,  NR-WRITE SYS,
      0 70 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  s" hb: owner-WID prefix registration failed" BYTES,
   name LBL,  PACKAGE-NAME$ BYTES,
   data-done LBL, ;

: PREFLIGHT-FALSE ( label -- ) {: fail:label :}
   9 $10FF MOVZ,  10 $20FF MOVZ,  11 255 MOVZ,
   OWNER-WID-EMIT:PREFLIGHT-LABEL@ BL,
   13 0 CMPI,  C-NE fail BCOND, ;

: PROTWID-PRESERVES ( label -- ) {: fail:label :}
   5 111 MOVZ,  6 222 MOVZ,  7 333 MOVZ,  14 444 MOVZ,
   9 $1234 MOVZ,
   LPROTWIDQ LABEL@ BL,
   3 111 MOVZ,  5 3 CMP,  C-NE fail BCOND,
   3 222 MOVZ,  6 3 CMP,  C-NE fail BCOND,
   3 333 MOVZ,  7 3 CMP,  C-NE fail BCOND,
   3 444 MOVZ,  14 3 CMP,  C-NE fail BCOND,
   13 0 CMPI,  C-NE fail BCOND, ;

: FILL ( label -- ) {: fail:label :}
   LBL {: loop:label :}
   17 1 MOVZ,
   loop LBL,
   9 $1000 MOVZ,  9 9 17 ADD,  9 G-PUSH
   9 $2000 MOVZ,  9 9 17 ADD,  9 G-PUSH
   OWNER-WID-EMIT:ADD-LABEL@ BL,
   9 G-POP  9 0 CMPI,  C-EQ fail BCOND,
   17 17 1 ADDI,
   17 255 CMPI,  C-LT loop BCOND, ;

: POISON ( -- )
   LBL {: loop:label :}
   5 OWNER-WID-OFF MOVZ,  5 DATA 5 ADD,
   9 $300000003000 LIT64,
   17 OWNER-WID-MAX MOVZ,
   loop LBL,
   9 5 0 STR,
   5 5 OWNER-WID-ROW ADDI,
   17 17 1 SUBI,
   17 loop CBNZ,
   9 OWNER-WID-MAX MOVZ,  9 DATA OWNER-WID-N-CELL STR, ;

: SAVE-LIVE ( -- )
   LBL {: loop:label :}
   SP SP OWNER-REG-LEN 8 + SUBI,
   COUNT@,
   6 SP 0 STR,
   5 OWNER-WID-OFF MOVZ,  5 DATA 5 ADD,
   7 SP 8 ADDI,
   17 OWNER-WID-MAX MOVZ,
   loop LBL,
   14 5 0 LDR,  14 7 0 STR,
   5 5 OWNER-WID-ROW ADDI,
   7 7 OWNER-WID-ROW ADDI,
   17 17 1 SUBI,
   17 loop CBNZ,
   14 DATA WIDN-CELL LDR,
   14 SP OWNER-REG-LEN STR, ;

: CLEAR-LIVE ( -- )
   LBL {: loop:label :}
   6 0 MOVZ,  COUNT!,
   9 0 MOVZ,
   5 OWNER-WID-OFF MOVZ,  5 DATA 5 ADD,
   7 OWNER-WID-MAX MOVZ,
   loop LBL,
   9 5 0 STR,
   5 5 OWNER-WID-ROW ADDI,
   7 7 1 SUBI,  7 loop CBNZ, ;

: RESTORE-LIVE ( -- )
   LBL {: loop:label :}
   5 OWNER-WID-OFF MOVZ,  5 DATA 5 ADD,
   7 SP 8 ADDI,
   17 OWNER-WID-MAX MOVZ,
   loop LBL,
   14 7 0 LDR,  14 5 0 STR,
   5 5 OWNER-WID-ROW ADDI,
   7 7 OWNER-WID-ROW ADDI,
   17 17 1 SUBI,  17 loop CBNZ,
   14 SP OWNER-REG-LEN LDR,
   14 DATA WIDN-CELL STR,
   6 SP 0 LDR,  COUNT!,
   SP SP OWNER-REG-LEN 8 + ADDI, ;

: AUTHENTIC ( label -- ) {: fail:label :}
   1 fail COUNT=
   5 OWNER-WID-OFF MOVZ,  5 DATA 5 ADD,
   9 5 OWNER-WID-PUB LDRW,  10 5 OWNER-WID-PRI LDRW,
   9 fail CBZ,  10 fail CBZ,
   9 10 CMP,  C-EQ fail BCOND,
   14 OWNER-WID-LIMIT LIT64,
   9 14 CMP,  C-HI fail BCOND,  10 14 CMP,  C-HI fail BCOND,
   14 DATA WIDN-CELL LDR,
   14 9 CMP,  C-LS fail BCOND,
   14 10 CMP,  C-LS fail BCOND, ;

: REENTER ( label -- ) {: fail:label :}
   POISON
   OWNER-WID-EMIT:COLD-LABEL@ BL,
   OWNER-WID-EMIT:OWNER-LABEL@ BL,
   fail AUTHENTIC ;

: W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

: LABEL-A ( label -- ptr u8 )
   LABEL>N cells LBLP + @ CW@ ;

: HAS-W? ( ptr u8 ptr u8 n -- bool ) {: p:ptr e:ptr want:n :}
   p e >= if NO exit then
   p W32@ want = if YES exit then
   p 4 + e want recurse ;

: NEXT-PHASE ( n n -- n ) {: phase:n w:n :}
   w COUNT-LDAR = if phase 0 <> if -1 exit then 1 exit then
   w ROW-STR = if phase 1 <> if -1 exit then 2 exit then
   w COUNT-STLR = if phase 2 <> if -1 exit then 3 exit then
   phase ;

: ORDERED? ( ptr u8 ptr u8 n -- bool ) {: p:ptr e:ptr phase:n :}
   p e >= if phase 3 = exit then
   phase p W32@ NEXT-PHASE {: next:n :}
   p 4 + e next recurse ;

: PROOF-FAIL ( -- )
   s" hb: owner-WID code proof failed" 70 die ;

: PROOF ( -- )
   OWNER-WID-EMIT:PUBLIC-LABEL@ LABEL-A
   OWNER-WID-EMIT:PRIVATE-LABEL@ LABEL-A COUNT-LDAR HAS-W? 0= if PROOF-FAIL then
   OWNER-WID-EMIT:PRIVATE-LABEL@ LABEL-A
   OWNER-WID-EMIT:ANY-LABEL@ LABEL-A COUNT-LDAR HAS-W? 0= if PROOF-FAIL then
   OWNER-WID-EMIT:ANY-LABEL@ LABEL-A
   OWNER-WID-EMIT:PREFLIGHT-LABEL@ LABEL-A COUNT-LDAR HAS-W? 0= if PROOF-FAIL then
   OWNER-WID-EMIT:PREFLIGHT-LABEL@ LABEL-A
   OWNER-WID-EMIT:ADD-LABEL@ LABEL-A COUNT-LDAR HAS-W? 0= if PROOF-FAIL then
   OWNER-WID-EMIT:ADD-LABEL@ LABEL-A
   ASM-CP @ CW@ 0 ORDERED? 0= if PROOF-FAIL then ;

: EMIT ( -- )
   LBL LBL LBL {: fail:label done:label msg:label :}
   SP SP 16 SUBI,  30 SP 0 STR,
   fail REENTER
   fail PROTWID-PRESERVES
   SAVE-LIVE
   CLEAR-LIVE
   $1000 $2000 YES fail ADD
   $1000 $2000 NO fail ADD
   $2000 $1000 NO fail ADD
   $3000 $1000 NO fail ADD
   1 fail COUNT=
   0 $200000001000 fail ROW=
   1 0 fail ROW=
   fail FILL
   255 fail COUNT=
   fail PREFLIGHT-FALSE
   255 0 fail ROW=
   $10FF $20FF YES fail ADD
   256 fail COUNT=
   $1100 $2100 NO fail ADD
   $1000 $2000 NO fail ADD
   $2000 $1000 NO fail ADD
   $3000 $1000 NO fail ADD
   256 fail COUNT=
   0 $200000001000 fail ROW=
   255 $20FF000010FF fail ROW=
   RESTORE-LIVE
   fail AUTHENTIC
   30 SP 0 LDR,  SP SP 16 ADDI,
   done B,
   fail LBL,
   0 2 MOVZ,  1 msg ADR,  2 31 MOVZ,  NR-WRITE SYS,
   0 70 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  s" hb: owner-WID cold proof failed" BYTES,
   done LBL, ;

: PREFIX-BUILD ( -- )
   PREFIX-SIZE {: size:n :}
   s" test/owner-wid-source.f" PATH0 0 0 open PREFIX-FD !
   PREFIX-FD @ 0 < if PREFIX-FAIL then
   PREFIX-FD @ PREFIX-BUF size read size <> if PREFIX-FAIL then
   PREFIX-FD @ close
   -1 PREFIX-FD !
   size PREFIX-U ! ;

: PREFIX-SOURCE ( -- ptr u8 n )
   PREFIX-BUF PREFIX-U @ ;

: INSTALL-SOURCE ( -- )
   [: PREFIX-SOURCE ;] OWNER-WID-EMIT:SOURCE-HOOK!
   [: EMIT-PREFIX ;] OWNER-WID-EMIT:PREFIX-HOOK!
   [: EMIT ;] OWNER-WID-EMIT:COLD-HOOK!
   [: PROOF ;] OWNER-WID-EMIT:PROOF-HOOK! ;

PREFIX-BUILD
INSTALL-SOURCE

;package
