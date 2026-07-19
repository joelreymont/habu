\ clobber-wrap-fixture.f - negative/positive fixtures for wrapped emitter calls.
\
\ Read as text by clobber-lint-test.f (never loaded or executed), so it is a
\ committed filemap-lint exclusion. PROT-GUARD:CALL is the modeled wrapped call:
\ it moves the caller's (addr,len) pair into x10/x11, then branches to the
\ resident span guard. The guard body reads x10/x11 and touches only x12/x13, so
\ the pair survives (x10=addr, x11=len); the move overwrites x10/x11 when the
\ operands are not already there, and the branch clobbers x30.

variable LWRAP-GUARD
variable LWRAP-CALLER

\ Positive: a value parked in x12 is destroyed by the guard, then read -> flag.
: EMIT-WRAP-LOSES-X12 ( -- )
   12 9 0 ADDI,                   \ x12 holds a caller-preserved value
   10 9 0 ADDI,  11 8 0 ADDI,     \ addr -> x10, len -> x11
   10 11 PROT-GUARD:CALL          \ guard clobbers x12/x13
   4 12 0 LDRB, ;                 \ stale read of x12 -> CLOBBER

\ Negative (BATCAS x14 pattern): the live value is saved in x14, which the guard
\ never touches; x10 is re-read as the guard's returned addr. No finding.
: EMIT-WRAP-SAVES-X14 ( -- )
   14 10 0 ADDI,                  \ save the live value across the guard's x10 clobber
   11 9 0 ADDI,  10 8 0 ADDI,     \ addr -> x11, len -> x10
   11 10 PROT-GUARD:CALL          \ clobbers x10-x13; returns x10=addr, x11=len
   11 10 0 ADDI,                  \ read x10 (guard-returned addr) -> not stale
   10 14 0 ADDI,                  \ read x14 (untouched) -> not stale
   4 10 0 STR, ;

\ Transitive: a BL-able routine wraps the guard, so its clobber set gains x12/x13
\ through the closure; a caller that keeps x12 live across a call to it is flagged.
: EMIT-TWRAP-GUARD ( -- )
   LWRAP-GUARD LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,   \ preserve the return address across the guard's branch
   10 9 0 ADDI,  11 8 0 ADDI,
   10 11 PROT-GUARD:CALL
   30 SP 0 LDR,  SP SP 16 ADDI,
   RET, ;

: EMIT-TWRAP-CALLER-BAD ( -- )
   LWRAP-CALLER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   12 9 0 ADDI,                   \ x12 live across the wrapped-guard call
   LWRAP-GUARD LABEL@ BL,         \ inherits the guard's x12/x13 clobber transitively
   4 12 0 LDRB,                   \ stale read of x12 -> CLOBBER
   30 SP 0 LDR,  SP SP 16 ADDI,
   RET, ;
