\ owner-wid-emitter.f - test-image-only cold owner-registry proof.

package OWNER-WID-COLD-TEST

\ Standalone guard. This file is bundle-injected by BUILD-EXT:OWNER-WID-STDIN;
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

: END-A ( -- )
   4 OWNER-WID-END MOVZ,  4 DATA 4 ADD, ;

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

: ROWS-ZERO ( label -- ) {: fail:label :}
   LBL {: loop:label :}
   5 OWNER-WID-OFF MOVZ,  5 DATA 5 ADD,
   17 OWNER-WID-MAX MOVZ,
   loop LBL,
   9 5 0 LDR,  9 fail CBNZ,
   5 5 OWNER-WID-ROW ADDI,
   17 17 1 SUBI,
   17 loop CBNZ, ;

: REENTER ( label -- ) {: fail:label :}
   LBL {: ready:label :}
   END-A  15 4 0 LDRW,
   15 ready CBNZ,
   POISON
   15 1 MOVZ,  15 4 0 STRW,
   30 SP 0 LDR,  SP SP 16 ADDI,
   OWNER-WID-EMIT:COLD-LABEL@ B,
   ready LBL,
   0 fail COUNT=
   fail ROWS-ZERO ;

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
   END-A  15 4 0 LDRW,
   16 4 4 LDRW,
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
   END-A  9 4 0 LDRW,  9 15 CMP,  C-NE fail BCOND,
   9 4 4 LDRW,  9 16 CMP,  C-NE fail BCOND,
   30 SP 0 LDR,  SP SP 16 ADDI,
   done B,
   fail LBL,
   0 2 MOVZ,  1 msg ADR,  2 31 MOVZ,  NR-WRITE SYS,
   0 70 MOVZ,  NR-EXIT-GROUP SYS,
   msg LBL,  s" hb: owner-WID cold proof failed" BYTES,
   done LBL, ;

' EMIT OWNER-WID-EMIT:COLD-HOOK!
' PROOF OWNER-WID-EMIT:PROOF-HOOK!

;package
