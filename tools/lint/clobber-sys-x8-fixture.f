\ clobber-sys-x8-fixture.f - negative fixture for syscall scratch liveness.

variable LBAD
variable LCALL
variable LEXITGROUP
variable LRET
variable LX13-CALLEE
variable LX13-CALLER
variable LX13-CLEAN
variable LP2CWAT
variable LCEMIT
variable LHIDXADD
variable LRETURN-USE
variable LPRESERVE-X13
variable LPRESERVE-X17
variable LHEADER-RELOAD
variable LHEADER-STALE
variable LROWS-RELOAD
variable LROWS-STALE
variable LINDEX-RELOAD
variable LINDEX-STALE

: EMIT-ROWS-RELOAD-OK ( -- )
   LROWS-RELOAD LABEL@ LBL,
   MARK-HEADER
   NR-MMAP SYS,
   MARK-HEADER MARK-ROWS
   RET, ;

: EMIT-ROWS-RELOAD-BAD ( -- )
   LROWS-STALE LABEL@ LBL,
   MARK-HEADER
   NR-MMAP SYS,
   MARK-ROWS
   RET, ;

: EMIT-INDEX-RELOAD-OK ( -- )
   LINDEX-RELOAD LABEL@ LBL,
   8 9 0 ADDI,
   NR-MMAP SYS,
   8 0 0 ADDI,
   bad INDEX-SHAPE
   RET, ;

: EMIT-INDEX-RELOAD-BAD ( -- )
   LINDEX-STALE LABEL@ LBL,
   8 9 0 ADDI,
   NR-MMAP SYS,
   bad INDEX-SHAPE
   RET, ;

: EMIT-HEADER-RELOAD-OK ( -- )
   LHEADER-RELOAD LABEL@ LBL,
   16 9 0 ADDI,
   NR-MMAP SYS,
   MARK-HEADER
   5 16 0 LDR,
   6 4 0 LDR,
   RET, ;

: EMIT-HEADER-RELOAD-BAD ( -- )
   LHEADER-STALE LABEL@ LBL,
   16 9 0 ADDI,
   NR-MMAP SYS,
   5 16 0 LDR,
   RET, ;

: EMIT-SYS-X8-BAD ( -- )
   LBAD LABEL@ LBL,
   8 9 0 ADDI,
   NR-CLOSE SYS,
   4 8 0 LDRB,
   RET, ;

: EMIT-LR-BAD ( -- )
   LCALL LABEL@ LBL,
   LRET LABEL@ BL,
   RET,
   LRET LABEL@ LBL,
   RET, ;

: EMIT-EXIT-GROUP-OK ( -- )
   LEXITGROUP LABEL@ LBL,
   16 9 0 ADDI,
   NR-EXIT-GROUP SYS,
   4 16 0 LDRB,
   RET, ;

: EMIT-X13-CALLEE ( -- )
   LX13-CALLEE LABEL@ LBL,
   13 0 MOVZ,
   RET, ;

: EMIT-X13-CALLER-BAD ( -- )
   LX13-CALLER LABEL@ LBL,
   SP SP 16 SUBI,
   30 SP 0 STR,
   13 9 0 ADDI,
   LX13-CALLEE LABEL@ BL,
   4 13 0 LDRB,
   30 SP 0 LDR,
   SP SP 16 ADDI,
   RET, ;

: EMIT-X13-CALLER-OK ( -- )
   LX13-CLEAN LABEL@ LBL,
   SP SP 16 SUBI,
   30 SP 0 STR,
   13 9 0 ADDI,
   LX13-CALLEE LABEL@ BL,
   13 9 0 ADDI,
   4 13 0 LDRB,
   30 SP 0 LDR,
   SP SP 16 ADDI,
   RET, ;

: EMIT-RETURNS-X10 ( -- )
   LP2CWAT LABEL@ LBL,
   10 1 MOVZ,
   RET, ;

: EMIT-USES-RETURNED-X10 ( -- )
   LRETURN-USE LABEL@ LBL,
   SP SP 16 SUBI,
   30 SP 0 STR,
   LP2CWAT LABEL@ BL,
   4 10 0 LDRB,
   30 SP 0 LDR,
   SP SP 16 ADDI,
   RET, ;

: EMIT-PRESERVES-X13 ( -- )
   LCEMIT LABEL@ LBL,
   SP SP 16 SUBI,
   13 SP 0 STR,
   13 0 MOVZ,
   13 SP 0 LDR,
   SP SP 16 ADDI,
   RET, ;

: EMIT-KEEPS-X13 ( -- )
   LPRESERVE-X13 LABEL@ LBL,
   SP SP 16 SUBI,
   30 SP 0 STR,
   13 9 0 ADDI,
   LCEMIT LABEL@ BL,
   4 13 0 LDRB,
   30 SP 0 LDR,
   SP SP 16 ADDI,
   RET, ;

: EMIT-PRESERVES-X17 ( -- )
   LHIDXADD LABEL@ LBL,
   SP SP 16 SUBI,
   17 SP 0 STR,
   17 0 MOVZ,
   17 SP 0 LDR,
   SP SP 16 ADDI,
   RET, ;

: EMIT-KEEPS-X17 ( -- )
   LPRESERVE-X17 LABEL@ LBL,
   SP SP 16 SUBI,
   30 SP 0 STR,
   17 9 0 ADDI,
   LHIDXADD LABEL@ BL,
   4 17 0 LDRB,
   30 SP 0 LDR,
   SP SP 16 ADDI,
   RET, ;

package CLOBBER-FIX
public
variable LQUAL

: EMIT-QUALIFIED-CALLEE ( -- )
   LQUAL LABEL@ LBL,
   13 0 MOVZ,
   RET, ;
;package

package CLOBBER-FIXTURE
variable LQUAL-CALLER

: EMIT-QUALIFIED-CALLER-BAD ( -- )
   LQUAL-CALLER LABEL@ LBL,
   SP SP 16 SUBI,
   30 SP 0 STR,
   13 9 0 ADDI,
   CLOBBER-FIX:LQUAL LABEL@ BL,
   4 13 0 LDRB,
   30 SP 0 LDR,
   SP SP 16 ADDI,
   RET, ;
;package

package CLOBBER-FIXTURE
variable LSEPARATE-CLOBBER
variable LSEPARATE-READ
variable LFALL-CLOBBER
variable LFALL-READ
variable LJUMP-CLOBBER
variable LJUMP-READ
variable LUNUSED
variable LCOND-CLOBBER
variable LCOND-READ
variable LLOCAL-CLOBBER
variable LLOCAL-SKIP

\ A read in the next independent routine does not use the prior routine's
\ poisoned value. The same read reached by fall-through must still be caught.
: EMIT-SEPARATE-ENTRIES-OK ( -- )
   LSEPARATE-CLOBBER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LX13-CALLEE LABEL@ BL,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET,
   LSEPARATE-READ LABEL@ LBL,
   4 13 0 LDRB,  RET, ;

: EMIT-FALL-THROUGH-BAD ( -- )
   LFALL-CLOBBER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LX13-CALLEE LABEL@ BL,
   LFALL-READ LABEL@ LBL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: EMIT-JUMP-TO-NEXT-BAD ( -- )
   LJUMP-CLOBBER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LX13-CALLEE LABEL@ BL,
   30 SP 0 LDR,
   LJUMP-READ LABEL@ B,
   LUNUSED LABEL@ LBL, RET,
   LJUMP-READ LABEL@ LBL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: EMIT-JUMP-PAST-RETURN-BAD ( -- )
   LCOND-CLOBBER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LX13-CALLEE LABEL@ BL,
   C-EQ LCOND-READ LABEL@ BCOND,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET,
   LCOND-READ LABEL@ LBL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: EMIT-LOCAL-JUMP-PAST-ENTRY-BAD ( -- )
   LBL {: shared:label :}
   LLOCAL-CLOBBER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LX13-CALLEE LABEL@ BL,
   30 SP 0 LDR,
   shared B,
   LLOCAL-SKIP LABEL@ LBL, RET,
   shared LBL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;
;package

package CLOBBER-FIXTURE
variable LUDIV-CALLEE
variable LUDIV-CALLER

\ UDIV writes its first register: the callee's clobber set gains x13.
: EMIT-UDIV-CALLEE ( -- )
   LUDIV-CALLEE LABEL@ LBL,
   13 9 10 UDIV,
   RET, ;

: EMIT-UDIV-CALLER-BAD ( -- )
   LUDIV-CALLER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LUDIV-CALLEE LABEL@ BL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;
;package

\ Each callee below writes x13 through one more instruction class; each caller
\ keeps x13 live across the call and reads it after.
package CLOBBER-FIXTURE
variable LORRI-CALLEE    variable LORRI-CALLER
variable LLDAR-CALLEE    variable LLDAR-CALLER
variable LCSEL-CALLEE    variable LCSEL-CALLER
variable LFMOVDX-CALLEE  variable LFMOVDX-CALLER
variable LLDRPRE-CALLEE  variable LLDRPRE-CALLER
variable LSTRPOST-CALLEE variable LSTRPOST-CALLER
variable LCASAL-CALLEE   variable LCASAL-CALLER

\ ORRI, a two-register immediate, writes its first register.
: EMIT-ORRI-CALLEE ( -- )
   LORRI-CALLEE LABEL@ LBL,
   13 9 1 ORRI,
   RET, ;

: EMIT-ORRI-CALLER-BAD ( -- )
   LORRI-CALLER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LORRI-CALLEE LABEL@ BL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LDAR, the load-acquire, writes its first register.
: EMIT-LDAR-CALLEE ( -- )
   LLDAR-CALLEE LABEL@ LBL,
   13 9 LDAR,
   RET, ;

: EMIT-LDAR-CALLER-BAD ( -- )
   LLDAR-CALLER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LLDAR-CALLEE LABEL@ BL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ CSEL, the conditional select, writes its first register.
: EMIT-CSEL-CALLEE ( -- )
   LCSEL-CALLEE LABEL@ LBL,
   9 10 CMP,  13 9 10 C-LS CSEL,
   RET, ;

: EMIT-CSEL-CALLER-BAD ( -- )
   LCSEL-CALLER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LCSEL-CALLEE LABEL@ BL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ FMOVDX, the FP-to-x move, writes its x register, the first.
: EMIT-FMOVDX-CALLEE ( -- )
   LFMOVDX-CALLEE LABEL@ LBL,
   13 0 FMOVDX,
   RET, ;

: EMIT-FMOVDX-CALLER-BAD ( -- )
   LFMOVDX-CALLER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LFMOVDX-CALLEE LABEL@ BL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LDRPRE, the pre-indexed load, writes its destination as well as its base.
: EMIT-LDRPRE-CALLEE ( -- )
   LLDRPRE-CALLEE LABEL@ LBL,
   13 9 -8 LDRPRE,
   RET, ;

: EMIT-LDRPRE-CALLER-BAD ( -- )
   LLDRPRE-CALLER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LLDRPRE-CALLEE LABEL@ BL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ STRPOST, the post-indexed store, writes its base register, the second.
: EMIT-STRPOST-CALLEE ( -- )
   LSTRPOST-CALLEE LABEL@ LBL,
   9 13 8 STRPOST,
   RET, ;

: EMIT-STRPOST-CALLER-BAD ( -- )
   LSTRPOST-CALLER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LSTRPOST-CALLEE LABEL@ BL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ CASAL, the compare-and-swap, returns the old value in its first register.
: EMIT-CASAL-CALLEE ( -- )
   LCASAL-CALLEE LABEL@ LBL,
   13 10 9 CASAL,
   RET, ;

: EMIT-CASAL-CALLER-BAD ( -- )
   LCASAL-CALLER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LCASAL-CALLEE LABEL@ BL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;
;package

package CLOBBER-FIXTURE
variable LCBSELF-CALLEE  variable LCBSELF-CALLER
variable LFMOVXD-CALLER

\ CB-SELF reads the thread base into its register: the callee writes x13.
: EMIT-CBSELF-CALLEE ( -- )
   LCBSELF-CALLEE LABEL@ LBL,
   13 CB-SELF,
   RET, ;

: EMIT-CBSELF-CALLER-BAD ( -- )
   LCBSELF-CALLER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LCBSELF-CALLEE LABEL@ BL,
   4 13 0 LDRB,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ FMOVXD reads its x register, the second: the read after the call reports.
: EMIT-FMOVXD-CALLER-BAD ( -- )
   LFMOVXD-CALLER LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   13 9 0 ADDI,
   LORRI-CALLEE LABEL@ BL,
   0 13 FMOVXD,
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;
;package
