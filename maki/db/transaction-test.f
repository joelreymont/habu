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
\ The test reopens package TX (a friend) so MATCH over the public result and the
\ builder API read bare; object identities are minted through ARTIFACT:REGISTER and
\ revisions through REV:COMMIT - never a raw cast - exactly as a real producer would.

require lib/test.f
require test/checker-assert.f
require maki/db/transaction.f
require maki/artifact.f
require maki/rev.f
require maki/db/obligation.f
\ The GLOBAL result family, loaded on purpose and used by nothing here: TX's outcome
\ family is now named `result` too, so this require is what makes the identity
\ assertions at the end of this file live. Without it they would pass in an empty
\ room, and the one real hazard of the rename - two families sharing the tail
\ `result`, told apart only by the constructor package - would go unproven. It also
\ puts the bare `result` inside this package's own MATCH sites under a live global
\ homonym, which is exactly the coexistence maki/test.f exercises.
require lib/adt/result.f

using TFAM

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

: VCODE ( result<n> -- n )                   \ 0=ok else taxonomy ordinal
   MATCH result
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
   MATCH result
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

\ ---- the family is TX:result now, and only TX:result -------------------------
\ The rename moved this family's tail off its own package name (`tx-result` inside
\ package TX repeated its owner). Renaming a family also renames its generated
\ constructor package - TX-TX--RESULT became TX-RESULT - so every call site in this
\ package, in maki/db/commit-store.f and in the cross-process child
\ maki/db/keywire-xproc-env-child.f moved with it. The suite above proves the
\ BEHAVIOUR still holds; what it cannot show is that the old spellings are really
\ gone and that the new tail did not quietly merge with the global `result` family
\ that lib/adt/result.f declares. That is what this section pins.
\
\ Its T-RESET comes FIRST and every assertion sits below it: an assertion above a
\ suite's reset is counted against the previous report and passes silently even when
\ it fails.
T-RESET

package TX-TEST
private

\ CHECK-QUIET-CANDIDATE! answers -1 accepted, 0 refused, 1 unresolvable. All three
\ appear below and each means something different, so they get separate words rather
\ than a single "not accepted" helper.
: YES   ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO    ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;
: UNRES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  1 T= ;

\ REFLECT keys a family on its tail AND the constructor package its variants carry.
\ For this rename that pairing is the whole safety argument: TX$ and GLOB$ share the
\ tail `result` and are told apart only by the second half.
: TX$   ( -- ptr u8 n ptr u8 n )   s" result" s" TX-RESULT" ;
: OLD$  ( -- ptr u8 n ptr u8 n )   s" tx-result" s" TX-TX--RESULT" ;
: GLOB$ ( -- ptr u8 n ptr u8 n )   s" result" s" RESULT" ;

\ ---- identity: the old pair is gone, the new pair is unique ------------------
OLD$ REFLECT:FAMS 0 T=              \ nothing answers to the old (tail, ctor package)
TX$ REFLECT:FAMS 1 T=               \ exactly one family is TX's result ...
GLOB$ REFLECT:FAMS 1 T=             \ ... and exactly one is the global result
TX$ REFLECT:ARITY 1 T=              \ they are not the same family: different arity ...
GLOB$ REFLECT:ARITY 2 T=
TX$ 0 REFLECT:ARM-CTOR$ s" TX-RESULT" T$=     \ ... and different constructor packages
TX$ 4 REFLECT:ARM-CTOR$ s" TX-RESULT" T$=

\ ---- the recorded shape is untouched by the rename ---------------------------
TX$ REFLECT:KIND TK-SUM T=
TX$ REFLECT:WIDTH 2 T=
TX$ REFLECT:VIS 1 T=
TX$ REFLECT:VARS 5 T=
TX$ 0 REFLECT:ARM$ s" ok" T$=       \ case order fixes the tags
TX$ 1 REFLECT:ARM$ s" duplicate-write" T$=
TX$ 2 REFLECT:ARM$ s" omitted-read" T$=
TX$ 3 REFLECT:ARM$ s" malformed" T$=
TX$ 4 REFLECT:ARM$ s" bounds" T$=
TX$ 5 REFLECT:ARM$ s" <missing>" T$=

\ ---- the new spellings carry the whole effect table --------------------------
s" N-OK ( n -- TX:result<n> ) TX-RESULT:OK" YES
s" N-DUP ( -- TX:result<n> ) TX-RESULT:DUPLICATE-WRITE" YES
s" N-OMIT ( -- TX:result<n> ) TX-RESULT:OMITTED-READ" YES
s" N-MAL ( -- TX:result<n> ) TX-RESULT:MALFORMED" YES
s" N-BND ( -- TX:result<n> ) TX-RESULT:BOUNDS" YES
s" N-REV ( CAD-KIND:rev-id -- TX:result<CAD-KIND:rev-id> ) TX-RESULT:OK" YES

\ ---- the old spellings carry nothing -----------------------------------------
\ The two halves fail by different mechanisms, and pinning them separately is what
\ makes this proof readable. An unresolvable WORD in the body answers 1; an
\ unresolvable TYPE in the stack effect is reported as a type error and answers 0.
\ So the old constructor alone is unresolvable, the old type alone is refused, and
\ the pair - the literal text every call site used to hold - is refused. What matters
\ is that not one of them is ACCEPTED.
s" O-CTOR ( n -- TX:result<n> ) TX-TX--RESULT:OK" UNRES
s" O-TYPE ( n -- TX:tx-result<n> ) TX-RESULT:OK" NO
s" O-BOTH ( n -- TX:tx-result<n> ) TX-TX--RESULT:OK" NO
s" O-DUP ( -- TX:tx-result<n> ) TX-TX--RESULT:DUPLICATE-WRITE" NO
s" O-BND ( -- TX:tx-result<n> ) TX-TX--RESULT:BOUNDS" NO

\ ---- the two result families do not unify ------------------------------------
\ Sharing a tail is legal; sharing an identity is not. A bare `result` written here
\ resolves the global row, so these two cross the packages in both directions.
s" X-GLOB-CTOR ( n -- TX:result<n> ) RESULT:OK" NO
s" X-TX-CTOR ( n n -- result<n,n> ) TX-RESULT:OK" NO
\ And the consequence callers must live with, pinned as its own fact: outside package
\ TX a BARE `result` is the global arity-2 family, not this one. The old tail was
\ unique repo-wide so a bare token used to reach TX's family; after the rename it does
\ not, which is why maki/db/keywire-xproc-env-child.f spells the effect TX:result.
\ This section runs inside package TX-TEST, which owns no `result` row, so a bare
\ token here resolves exactly as it does in that child.
s" X-BARE-ARITY ( result<n> -- n ) drop 0" NO

;package

T-REPORT

\ ---- what the unified ENUM declaration registered and generated ---------------
\ TX:result moved off the retired legacy sum opener onto the unified ENUM front end in
\ full mode. Nothing above this line changed, and neither consumer changed - not
\ maki/db/commit-store.f and not the cross-process child
\ maki/db/keywire-xproc-env-child.f - which is why this section exists: the two
\ declaration forms are MATCH-identical and width-identical, so the suites above
\ cannot see the difference and therefore cannot see a REGRESSION either.
\
\ This reopens package TX-TEST and reuses the YES / NO / UNRES verdict words and the
\ TX$ identity from the rename section rather than redeclaring them. Its T-RESET comes
\ FIRST and every assertion sits below it: an assertion above a suite's reset is
\ counted against the previous report and passes silently even when it fails.
T-RESET

package TX-TEST
private

\ The twin's identity. TX$ (the production family) comes from the section above.
: TW$ ( -- ptr u8 n ptr u8 n )   s" tr-twin" s" TX--TEST-TR--TWIN" ;

\ ---- fixtures built through the real production builder ------------------------
\ These use the public TX builder surface a genuine producer uses, not the private
\ helpers of the suite above, so the round trip below runs the production path.
: OBJ-P ( -- CAD-KIND:artifact-id )   s" tx-mig-obj-p" ARTIFACT:REGISTER ;
: OBJ-Q ( -- CAD-KIND:artifact-id )   s" tx-mig-obj-q" ARTIFACT:REGISTER ;
: RB0 ( -- CAD-KIND:rev-id )   s" tx-mig-rev-0" REV:COMMIT ;
: RB1 ( -- CAD-KIND:rev-id )   s" tx-mig-rev-1" REV:COMMIT ;

: MK-ON ( CAD-KIND:rev-id -- TX:txn )        \ smallest valid action on a given base
   TX:OPEN
   OBJ-P TX:PRESENT TX:READ+
   OBJ-P TX:WRITE+
   OBJ-P TX:DEP+
   TX:BUILD ;
: MK-DUP-ON ( -- TX:txn )                    \ two conflicting writes to one object
   RB0 TX:OPEN
   OBJ-P TX:PRESENT TX:READ+
   OBJ-P TX:WRITE+
   OBJ-P TX:WRITE+
   OBJ-P TX:DEP+
   TX:BUILD ;
: MK-OMIT-ON ( -- TX:txn )                   \ a dependency on an object never read
   RB0 TX:OPEN
   OBJ-P TX:PRESENT TX:READ+
   OBJ-P TX:WRITE+
   OBJ-Q TX:DEP+
   TX:BUILD ;

\ ---- compiled round trip through all five generated constructors --------------
\ The checker pins below prove the constructors resolve and type-check as candidate
\ text. This word proves they work in COMPILED code: it takes a real TX:result apart
\ and puts it back together arm by arm, so all five generated constructors are
\ compiled, and the ok arm binds its payload through a typed local before handing it
\ back to the ok constructor. The five-arm shape is repeated rather than factored
\ because the suite above cannot lend its own classifier - that one is private to the
\ production TX reopen - and a polymorphic eliminator over the whole bundle is not
\ expressible yet (dot habu-typestate-result-drop-5ae048a7).
\
\ Every effect here spells the family TX:result QUALIFIED: this package owns no
\ `result` row, so a bare token would reach the global arity-2 family instead.
: REBUILD ( TX:result<n> -- TX:result<n> )
   MATCH TX:result
      ok OF {: got:n :} got TX-RESULT:OK ENDOF
      duplicate-write OF TX-RESULT:DUPLICATE-WRITE ENDOF
      omitted-read OF TX-RESULT:OMITTED-READ ENDOF
      malformed OF TX-RESULT:MALFORMED ENDOF
      bounds OF TX-RESULT:BOUNDS ENDOF
   ;MATCH ;

: CODE ( TX:result<n> -- n )                 \ 0 ok, else the reject taxonomy ordinal
   MATCH TX:result
      ok OF drop 0 ENDOF
      duplicate-write OF 1 ENDOF
      omitted-read OF 2 ENDOF
      malformed OF 3 ENDOF
      bounds OF 4 ENDOF
   ;MATCH ;

\ The payload's VALUE, not merely its presence. The recovered slot is fed straight
\ back through the production TX:TXN-OF, exactly as the keywire child does, and the
\ rebuilt handle must still name a transaction sitting on the base revision it was
\ built on. A slot the round trip zeroed or altered would name a different pool entry
\ and the base comparison would disagree.
: OK-BASE? ( CAD-KIND:rev-id TX:result<n> -- bool )
   MATCH TX:result
      ok OF {: got:n :} got TX:TXN-OF TX:BASE-REV REV:EQUAL? ENDOF
      duplicate-write OF drop false ENDOF
      omitted-read OF drop false ENDOF
      malformed OF drop false ENDOF
      bounds OF drop false ENDOF
   ;MATCH ;

: RT-OK# ( -- n )       RB0 MK-ON TX:VALIDATE REBUILD CODE ;
: RT-DUP# ( -- n )      MK-DUP-ON TX:VALIDATE REBUILD CODE ;
: RT-OMIT# ( -- n )     MK-OMIT-ON TX:VALIDATE REBUILD CODE ;
: RT-SAME? ( -- bool )  RB0 RB0 MK-ON TX:VALIDATE REBUILD OK-BASE? ;
: RT-ALT? ( -- bool )   RB1 RB1 MK-ON TX:VALIDATE REBUILD OK-BASE? ;
: RT-CROSS? ( -- bool ) RB1 RB0 MK-ON TX:VALIDATE REBUILD OK-BASE? ;

public

\ tr-twin is TX:result's SHAPE under another name: same arity, the same five cases in
\ the same order, the same named payload field. It exists only so the negatives below
\ can prove transaction-outcome identity is NOMINAL - two identically shaped families
\ never unify, in either direction. It has to be public, because a private family
\ publishes no constructors and the positive control builds through the twin's own ok,
\ so neither negative could pass by being unresolvable instead of ill-typed. Its
\ generated constructor package TX--TEST-TR--TWIN is 17 bytes, well inside the 32-byte
\ readable-spelling cap TF-CTOR-NAME-LIMIT (src/core/type-family.f), so it keeps the
\ readable escaped spelling rather than falling back to the opaque SHA form; the pin on
\ that exact spelling below is what would notice a crossing.
ENUM tr-twin 1
   VARIANT ok FIELD slot a ;VARIANT
   VARIANT duplicate-write ;VARIANT
   VARIANT omitted-read ;VARIANT
   VARIANT malformed ;VARIANT
   VARIANT bounds ;VARIANT
;ENUM

private

\ ---- the named payload field the migration adds -------------------------------
\ This is the one registry row the move changes: the legacy positional payload
\ registered NO field row at all, so `1` here fails on the previous declaration, and
\ `0` is the slot the ok arm binds first. The shape pins in the section above (kind,
\ arity, width, visibility, case order, constructor package) still hold unchanged,
\ which is the other half of the claim - the declaration mode moved and nothing else.
TX$ 0 REFLECT:ARM-FLDS 1 T=
TX$ 0 s" slot" REFLECT:ARM-SLOT 0 T=
TX$ 0 s" slot" REFLECT:ARM-CELLS 1 T=
TX$ 0 s" value" REFLECT:ARM-SLOT -1 T=   \ and it is spelled `slot`, nothing else
TX$ 1 REFLECT:ARM-FLDS 0 T=              \ the reject arms carry no payload at all
TX$ 4 REFLECT:ARM-FLDS 0 T=
TX$ 5 REFLECT:ARM-FLDS -1 T=             \ a case that does not exist answers the sentinel
TX$ REFLECT:KIND TK-SUM T=               \ a payload family is a general sum ...
TX$ REFLECT:KIND TK-ENUM = 0 T=          \ ... never recorded as a payloadless enum

\ ---- generated constructors: unchanged spelling + unchanged effect ------------
s" TC-OK ( n -- TX:result<n> ) TX-RESULT:OK" YES
s" TC-DUP ( -- TX:result<n> ) TX-RESULT:DUPLICATE-WRITE" YES
s" TC-OMIT ( -- TX:result<n> ) TX-RESULT:OMITTED-READ" YES
s" TC-MAL ( -- TX:result<n> ) TX-RESULT:MALFORMED" YES
s" TC-BND ( -- TX:result<n> ) TX-RESULT:BOUNDS" YES
\ The payload really is a parameter: an arity-0 nominal cell family instantiates it as
\ readily as raw n, and a reject arm under that instantiation still takes no payload.
s" TC-OK-REV ( CAD-KIND:rev-id -- TX:result<CAD-KIND:rev-id> ) TX-RESULT:OK" YES
s" TC-BND-REV ( -- TX:result<CAD-KIND:rev-id> ) TX-RESULT:BOUNDS" YES

\ Calibration for the seven YES lines above: these three spellings do not exist, and
\ an unresolvable constructor word answers 1. Were the family renamed again, the real
\ constructors would answer 1 too and every YES would fail - which is what makes -1
\ mean "the checker resolved exactly this name".
s" TC-X-SEP ( n -- TX:result<n> ) TXRESULT:OK" UNRES
s" TC-X-PKG ( n -- TX:result<n> ) TX--RESULT:OK" UNRES
s" TC-X-ARM ( n -- TX:result<n> ) TX-RESULT:OKAY" UNRES

\ ---- forge negatives on the ok payload slot -----------------------------------
\ This family's payload is instantiated at raw n in production, so "a raw cell in the
\ payload slot" is legitimate here and cannot be a negative. The discipline rests on
\ INSTANTIATION IDENTITY instead: the ok constructor must not hand back a result
\ instantiated at a type it did not consume, must not drop the mandatory payload, and
\ must not attach one to a reject arm.
\
\ Every type argument below is one the checker ACCEPTS on its own - n and the CAD-KIND
\ nominals each instantiate this family successfully in the positives above - so none
\ of these mismatches can pass merely by naming a type argument the family would have
\ refused anyway. TF-XNOM is the sharpest: rev-id and artifact-id are both one-cell
\ nominals, so nothing but family identity separates them.
s" TF-BARE ( n -- n ) TX-RESULT:OK" NO
s" TF-NONE ( -- TX:result<n> ) TX-RESULT:OK" NO
s" TF-PAY ( n -- TX:result<n> ) TX-RESULT:BOUNDS" NO
s" TF-PAY-REV ( CAD-KIND:rev-id -- TX:result<CAD-KIND:rev-id> ) TX-RESULT:BOUNDS" NO
s" TF-INST ( n -- TX:result<CAD-KIND:rev-id> ) TX-RESULT:OK" NO
s" TF-NOM ( CAD-KIND:rev-id -- TX:result<n> ) TX-RESULT:OK" NO
s" TF-XNOM ( CAD-KIND:rev-id -- TX:result<CAD-KIND:artifact-id> ) TX-RESULT:OK" NO

\ ---- live registry + non-unification for the shape twin ----------------------
TW$ REFLECT:FAMS 1 T=
TW$ REFLECT:KIND TK-SUM T=
TW$ REFLECT:ARITY 1 T=
TW$ REFLECT:WIDTH 2 T=
TW$ REFLECT:VIS 1 T=
TW$ REFLECT:VARS 5 T=
TW$ 0 REFLECT:ARM-CTOR$ s" TX--TEST-TR--TWIN" T$=   \ readable spelling, not the SHA form
TW$ 0 REFLECT:ARM-FLDS 1 T=
TW$ 0 s" slot" REFLECT:ARM-SLOT 0 T=
s" TT-TW ( n -- tr-twin<n> ) TX--TEST-TR--TWIN:OK" YES
s" TT-TW-X1 ( n -- tr-twin<n> ) TX-RESULT:OK" NO
s" TT-TW-X2 ( n -- TX:result<n> ) TX--TEST-TR--TWIN:OK" NO

\ ---- compiled round trip through every generated constructor -----------------
RT-OK# 0 T=                 \ ok is rebuilt as ok ...
RT-DUP# 1 T=                \ ... duplicate-write as duplicate-write ...
RT-OMIT# 2 T=               \ ... and omitted-read as omitted-read
RT-SAME? TTRUE              \ the recovered slot still names its own transaction
RT-ALT? TTRUE               \ on a second, different base revision too
RT-CROSS? TFALSE            \ and it is not merely answering true for any base

;package

T-REPORT

;using
