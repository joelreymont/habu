\ owner-wid-emitter.f - test-image-only cold owner-registry proof.

package OWNER-WID-COLD-TEST

0 0= constant YES
0 0= 0= constant NO

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

: REENTER ( label -- ) {: fail:label :}
   LBL {: ready:label :}
   END-A  15 4 0 LDRW,
   15 ready CBNZ,
   9 $300000003000 LIT64,  9 DATA OWNER-WID-OFF STR,
   9 $400000004000 LIT64,  9 DATA OWNER-WID-END OWNER-WID-ROW - STR,
   9 2 MOVZ,  9 DATA OWNER-WID-N-CELL STR,
   15 1 MOVZ,  15 4 0 STRW,
   30 SP 0 LDR,  SP SP 16 ADDI,
   OWNER-WID-EMIT:COLD-LABEL@ B,
   ready LBL,
   0 fail COUNT=
   0 0 fail ROW=
   255 0 fail ROW= ;

: EMIT ( -- )
   LBL LBL LBL {: fail:label done:label msg:label :}
   SP SP 16 SUBI,  30 SP 0 STR,
   fail REENTER
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

;package
