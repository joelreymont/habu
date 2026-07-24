\ maki/db/transaction-test.f - checked acceptance for the deterministic transaction
\ data model (maki/db/transaction.f, dot habu-v2-txn-journal-d0bc644f). Proves the
\ plan § 23 acceptance, each item by a named test:
\   TT-RT              : canonical round-trip (ENCODE -> DECODE -> re-ENCODE byte-identical)
\   TT-ORDER           : the canonical form is insertion-order independent
\   TT-DUP-WRITE       : duplicate/conflicting writes -> VALIDATE duplicate-write (typed)
\   TT-OMITTED         : a dependency edge not in the read set -> VALIDATE omitted-read (typed)
\   TT-STD-OK          : a well-formed transaction validates ok
\   TT-IDEM-STABLE     : retry identity is stable (same logical txn -> same idempotency key)
\   TT-IDEM-DIFF       : a different write set -> a different idempotency key
\   TT-POLARITY-KEY    : a negative lookup (absent) is recorded distinctly from a present read
\   TT-BASE-KEY        : the base revision is part of the action identity
\   TT-PROPOSE-STABLE  : the commit proposal (proposed rev-id) is deterministic across a retry
\   TT-PROPOSE-DIFF    : a different write set proposes a different revision
\   TT-COUNTS          : every field round-trips at the expected cardinality
\   TT-MALFORMED       : truncated bytes -> DECODE malformed (typed, not a throw)
\   capacity / polarity / no-base : throws only at the capacity/precondition boundaries
\
\ The test reopens package TX (a friend) so MATCH over the public tx-result and the
\ builder API read bare; object identities are minted through ARTIFACT:REGISTER and
\ revisions through REV:COMMIT - never a raw cast - exactly as a real producer would.

require lib/test.f
require test/checker-assert.f
require maki/db/transaction.f
require maki/artifact.f
require maki/rev.f
require maki/db/obligation.f

package TX

create RT-A 4096 allot
create RT-B 4096 allot
variable RT-LB

\ ---- fixtures: interned object identities + base revisions ----------------------
: OBJ-A ( -- CAD-KIND:artifact-id )   s" tx-obj-a" ARTIFACT:REGISTER ;
: OBJ-B ( -- CAD-KIND:artifact-id )   s" tx-obj-b" ARTIFACT:REGISTER ;
: OBJ-C ( -- CAD-KIND:artifact-id )   s" tx-obj-c" ARTIFACT:REGISTER ;
: R0 ( -- CAD-KIND:rev-id )   s" tx-rev-0" REV:COMMIT ;
: R1 ( -- CAD-KIND:rev-id )   s" tx-rev-1" REV:COMMIT ;

\ OB1 builds and interns a canonical proof-obligation, returning its CAD-KIND:obligation-id.
\ INTERN is content-addressed, so every call returns the SAME id (retry-stable): the
\ idempotency key / canonical form stay insertion-order-independent across MK-STD variants.
: OB1 ( -- CAD-KIND:obligation-id )
   OBLIG:NEW
   s" tx-oblig/subject" ARTIFACT:REGISTER OBLIG:SUBJECT
   OBLIG-RELATION:SEMANTIC-EQUIV OBLIG:RELATION
   OBLIG-DOMAIN:EXACT OBLIG:DOMAIN
   OBLIG-INDEPENDENCE:INDEPENDENT OBLIG:POLICY
   OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:VERIFIER-CLASS
   s" tx-oblig/env-sm87" CONFIG:REGISTER OBLIG:ENVIRONMENT
   s" tx-oblig/producer" PRODUCER:REGISTER OBLIG:PRODUCER
   OBLIG:SEAL OBLIG:INTERN ;

: BYTES-EQ? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr an:n b:ptr bn:n :}
   an bn <> if false exit then
   0 begin dup an < while
      dup {: k:n :}
      a k + c@  b k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: VCODE ( tx-result<n> -- n )                   \ 0=ok else taxonomy ordinal
   MATCH tx-result
      ok OF drop 0 ENDOF
      duplicate-write OF 1 ENDOF
      omitted-read OF 2 ENDOF
      malformed OF 3 ENDOF
      bounds OF 4 ENDOF
   ;MATCH ;

\ ---- generated record surface -------------------------------------------------
: TT-TXN-LAYOUT ( -- n )
   37 TX-TXN:MAKE TX-TXN:UNMAKE ;

: TT-IDEM-LAYOUT ( -- bool )
   1 2 3 4 TX-IDEM--KEY:MAKE TX-IDEM--KEY:UNMAKE
   {: w0:n w1:n w2:n w3:n :}
   w0 1 = w1 2 = and w2 3 = and w3 4 = and ;

\ ---- transaction fixtures -----------------------------------------------------
\ MK-STD: a well-formed action - reads OBJ-A (present) and OBJ-B (a NEGATIVE LOOKUP,
\ absent), writes OBJ-C, depends on the read OBJ-A, two capabilities, one budget line,
\ one obligation.
: MK-STD ( -- txn )
   R0 OPEN
   OBJ-A PRESENT READ+
   OBJ-B ABSENT  READ+
   OBJ-C WRITE+
   OBJ-A DEP+
   7 CAP+  3 CAP+
   1 100 BUDGET+
   OB1 OBLIG+
   BUILD ;

\ MK-STD-REORDER: the SAME logical action, every set added in a different order.
: MK-STD-REORDER ( -- txn )
   R0 OPEN
   OBJ-B ABSENT  READ+
   OBJ-A PRESENT READ+
   OBJ-C WRITE+
   OBJ-A DEP+
   3 CAP+  7 CAP+
   1 100 BUDGET+
   OB1 OBLIG+
   BUILD ;

\ MK-ALL-PRESENT: MK-STD with OBJ-B read PRESENT rather than a negative lookup.
: MK-ALL-PRESENT ( -- txn )
   R0 OPEN
   OBJ-A PRESENT READ+
   OBJ-B PRESENT READ+
   OBJ-C WRITE+
   OBJ-A DEP+
   7 CAP+  3 CAP+
   1 100 BUDGET+
   OB1 OBLIG+
   BUILD ;

\ MK-DIFF-WRITE: a different write set (OBJ-A instead of OBJ-C).
: MK-DIFF-WRITE ( -- txn )
   R0 OPEN
   OBJ-A PRESENT READ+
   OBJ-A WRITE+
   OBJ-A DEP+
   BUILD ;

\ MK-DIFF-BASE: the same logical action on a different base revision.
: MK-DIFF-BASE ( -- txn )
   R1 OPEN
   OBJ-A PRESENT READ+
   OBJ-B ABSENT  READ+
   OBJ-C WRITE+
   OBJ-A DEP+
   7 CAP+  3 CAP+
   1 100 BUDGET+
   OB1 OBLIG+
   BUILD ;

\ MK-DUP: two conflicting writes to OBJ-C.
: MK-DUP ( -- txn )
   R0 OPEN
   OBJ-A PRESENT READ+
   OBJ-C WRITE+  OBJ-C WRITE+
   BUILD ;

\ MK-OMITTED: depends on OBJ-C, which is not in the read set.
: MK-OMITTED ( -- txn )
   R0 OPEN
   OBJ-A PRESENT READ+
   OBJ-B WRITE+
   OBJ-C DEP+
   BUILD ;

\ ---- 1. canonical round-trip + order independence -----------------------------
: TT-RT ( -- bool )
   MK-STD RT-A 4096 ENCODE {: la:n :}
   RT-A la DECODE
   MATCH tx-result
      ok OF TXN-OF RT-B 4096 ENCODE RT-LB !
            RT-A la RT-B RT-LB @ BYTES-EQ? ENDOF
      duplicate-write OF false ENDOF
      omitted-read OF false ENDOF
      malformed OF false ENDOF
      bounds OF false ENDOF
   ;MATCH ;

: TT-ORDER ( -- bool )
   MK-STD RT-A 4096 ENCODE {: la:n :}
   MK-STD-REORDER RT-B 4096 ENCODE {: lb:n :}
   RT-A la RT-B lb BYTES-EQ? ;

\ ---- 2. duplicate / conflicting writes reject ---------------------------------
: TT-DUP-WRITE ( -- n )   MK-DUP VALIDATE VCODE ;

\ ---- 3. omitted read dependency rejects validation ----------------------------
: TT-OMITTED ( -- n )   MK-OMITTED VALIDATE VCODE ;

\ ---- a well-formed transaction validates ok -----------------------------------
: TT-STD-OK ( -- n )   MK-STD VALIDATE VCODE ;

\ ---- 4. retry identity is stable ----------------------------------------------
: TT-IDEM-STABLE ( -- bool )
   MK-STD IDEMPOTENCY-KEY  MK-STD-REORDER IDEMPOTENCY-KEY  KEY-EQ? ;
: TT-IDEM-DIFF ( -- bool )
   MK-STD IDEMPOTENCY-KEY  MK-DIFF-WRITE IDEMPOTENCY-KEY  KEY-EQ? 0= ;
: TT-POLARITY-KEY ( -- bool )                   \ absent vs present read -> different key
   MK-STD IDEMPOTENCY-KEY  MK-ALL-PRESENT IDEMPOTENCY-KEY  KEY-EQ? 0= ;
: TT-BASE-KEY ( -- bool )                       \ base revision is part of the action identity
   MK-STD IDEMPOTENCY-KEY  MK-DIFF-BASE IDEMPOTENCY-KEY  KEY-EQ? 0= ;

\ ---- commit proposal: deterministic proposed revision -------------------------
: TT-PROPOSE-STABLE ( -- bool )
   MK-STD PROPOSE  MK-STD-REORDER PROPOSE  REV:EQUAL? ;
: TT-PROPOSE-DIFF ( -- bool )
   MK-STD PROPOSE  MK-DIFF-WRITE PROPOSE  REV:EQUAL? 0= ;

\ ---- every field round-trips at the expected cardinality ----------------------
: TT-COUNTS-READ ( -- n )    MK-STD READ-COUNT ;
: TT-COUNTS-WRITE ( -- n )   MK-STD WRITE-COUNT ;
: TT-COUNTS-DEP ( -- n )     MK-STD DEP-COUNT ;
: TT-COUNTS-CAP ( -- n )     MK-STD CAP-COUNT ;
: TT-COUNTS-BUDGET ( -- n )  MK-STD BUDGET-COUNT ;
: TT-COUNTS-OBLIG ( -- n )   MK-STD OBLIG-COUNT ;
: TT-BASE-OK ( -- bool )     MK-STD BASE-REV R0 REV:EQUAL? ;

\ ---- decode taxonomy: truncated bytes -> malformed ----------------------------
: TT-MALFORMED ( -- n )
   MK-STD RT-A 4096 ENCODE {: la:n :}
   RT-A la 1- DECODE VCODE ;

\ ---- capacity / precondition throws -------------------------------------------
: TT-OVERFLOW ( -- )                            \ SET-CAP+1 reads overflow the read set
   R0 OPEN
   0 begin dup SET-CAP 1+ < while
      OBJ-A PRESENT READ+
      1+
   repeat drop ;
: TT-BAD-POLARITY ( -- )   R0 OPEN OBJ-A 5 READ+ ;
: TT-NO-BASE ( -- )        0 B-BASE-SET ! BUILD drop ;

T-RESET

s" TXC-TXN-MAKE ( n -- TX:txn ) TX-TXN:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" TXC-TXN-UNMAKE ( TX:txn -- n ) TX-TXN:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" TXC-IDEM-MAKE ( n n n n -- TX:idem-key ) TX-IDEM--KEY:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" TXC-IDEM-UNMAKE ( TX:idem-key -- n n n n ) TX-IDEM--KEY:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" TXC-TXN-BOOL ( bool -- TX:txn ) TX-TXN:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" TXC-IDEM-B0 ( bool n n n -- TX:idem-key ) TX-IDEM--KEY:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" TXC-IDEM-B1 ( n bool n n -- TX:idem-key ) TX-IDEM--KEY:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" TXC-IDEM-B2 ( n n bool n -- TX:idem-key ) TX-IDEM--KEY:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" TXC-IDEM-B3 ( n n n bool -- TX:idem-key ) TX-IDEM--KEY:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
TT-TXN-LAYOUT 37 T=
TT-IDEM-LAYOUT TTRUE
TT-RT TTRUE
TT-ORDER TTRUE
TT-DUP-WRITE 1 T=
TT-OMITTED 2 T=
TT-STD-OK 0 T=
TT-IDEM-STABLE TTRUE
TT-IDEM-DIFF TTRUE
TT-POLARITY-KEY TTRUE
TT-BASE-KEY TTRUE
TT-PROPOSE-STABLE TTRUE
TT-PROPOSE-DIFF TTRUE
TT-COUNTS-READ 2 T=
TT-COUNTS-WRITE 1 T=
TT-COUNTS-DEP 1 T=
TT-COUNTS-CAP 2 T=
TT-COUNTS-BUDGET 1 T=
TT-COUNTS-OBLIG 1 T=
TT-BASE-OK TTRUE
TT-MALFORMED 3 T=
' TT-OVERFLOW E-TX-SET-CAP TTHROWS
' TT-BAD-POLARITY E-TX-POLARITY TTHROWS
' TT-NO-BASE E-TX-NO-BASE TTHROWS

T-REPORT

;package
