\ cad-kinds-test.f - nominal Model CAD kind checker regressions.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require maki/cad-kinds.f
require maki/async-dag.f              \ ADAG:event-id: the runtime async event identity

package CAD-KIND-TEST

create BUF 8192 allot

: YES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: NO ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: DIAG< ( ptr u8 n -- )
   BUF 8192 DIAG-BUFFER!
   0 0= DIAG-JSON!
   CHECK-CANDIDATE! 0 T= ;

: DIAG? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;

: DIAG-END ( -- )
   0 0= 0= DIAG-JSON!
   DIAG-BUFFER-OFF ;

T-RESET

\ Every public declaration resolves by its qualified nominal identity.
s" CK-DESIGN ( CAD-KIND:design-id -- CAD-KIND:design-id )" YES
s" CK-REV ( CAD-KIND:rev-id -- CAD-KIND:rev-id )" YES
s" CK-OBJ ( CAD-KIND:obj-id -- CAD-KIND:obj-id )" YES
s" CK-NODE ( CAD-KIND:node-id -- CAD-KIND:node-id )" YES
s" CK-ANALYSIS ( CAD-KIND:analysis-id -- CAD-KIND:analysis-id )" YES
s" CK-PLAN ( CAD-KIND:plan-id -- CAD-KIND:plan-id )" YES
s" CK-ARTIFACT ( CAD-KIND:artifact-id -- CAD-KIND:artifact-id )" YES
s" CK-EVIDENCE ( CAD-KIND:evidence-id -- CAD-KIND:evidence-id )" YES
s" CK-OBLIGATION ( CAD-KIND:obligation-id -- CAD-KIND:obligation-id )" YES
s" CK-TARGET ( CAD-KIND:target-id -- CAD-KIND:target-id )" YES
s" CK-TOOLCHAIN ( CAD-KIND:toolchain-id -- CAD-KIND:toolchain-id )" YES
s" CK-PASS ( CAD-KIND:pass-id -- CAD-KIND:pass-id )" YES
s" CK-SCHEMA ( CAD-KIND:schema-id -- CAD-KIND:schema-id )" YES
s" CK-DIM ( CAD-KIND:dim -- CAD-KIND:dim )" YES
s" CK-SHAPE ( CAD-KIND:shape -- CAD-KIND:shape )" YES
s" CK-ROWS ( CAD-KIND:rows -- CAD-KIND:rows )" YES
s" CK-COLS ( CAD-KIND:cols -- CAD-KIND:cols )" YES
\ dtype/layout are NOT CAD-KIND rows on this tree: those roles are the maki
\ ENUM families (maki/tensor.f `dtype`, maki/tensor-value.f `layout`), whose
\ swap negatives live in tensor-test/tensor-value-test/model-ir-test (merge
\ policy habu-merge-policy-master-961bb2b7).
s" CK-SPACE ( CAD-KIND:address-space -- CAD-KIND:address-space )" YES
s" CK-STAGE ( CAD-KIND:stage -- CAD-KIND:stage )" YES
s" CK-EFFECT ( CAD-KIND:effect -- CAD-KIND:effect )" YES
s" CK-REGION ( CAD-KIND:region -- CAD-KIND:region )" YES

\ Adjacent roles never collapse to one cell-shaped scalar identity.
s" CK-X01 ( CAD-KIND:design-id -- CAD-KIND:rev-id )" NO
s" CK-X02 ( CAD-KIND:rev-id -- CAD-KIND:obj-id )" NO
s" CK-X03 ( CAD-KIND:obj-id -- CAD-KIND:node-id )" NO
s" CK-X04 ( CAD-KIND:node-id -- CAD-KIND:analysis-id )" NO
s" CK-X05 ( CAD-KIND:analysis-id -- CAD-KIND:plan-id )" NO
s" CK-X06 ( CAD-KIND:plan-id -- CAD-KIND:artifact-id )" NO
s" CK-X07 ( CAD-KIND:artifact-id -- CAD-KIND:evidence-id )" NO
s" CK-X08 ( CAD-KIND:evidence-id -- CAD-KIND:target-id )" NO
s" CK-X09 ( CAD-KIND:target-id -- CAD-KIND:toolchain-id )" NO
s" CK-X10 ( CAD-KIND:toolchain-id -- CAD-KIND:pass-id )" NO
s" CK-X11 ( CAD-KIND:pass-id -- CAD-KIND:schema-id )" NO
s" CK-X12 ( CAD-KIND:schema-id -- CAD-KIND:dim )" NO
s" CK-X13 ( CAD-KIND:dim -- CAD-KIND:shape )" NO
s" CK-X14 ( CAD-KIND:shape -- CAD-KIND:rows )" NO
s" CK-X15 ( CAD-KIND:rows -- CAD-KIND:cols )" NO
s" CK-X16 ( CAD-KIND:cols -- CAD-KIND:address-space )" NO
s" CK-X19 ( CAD-KIND:address-space -- CAD-KIND:stage )" NO
s" CK-X20 ( CAD-KIND:stage -- CAD-KIND:effect )" NO
s" CK-X21 ( CAD-KIND:effect -- CAD-KIND:region )" NO

\ The proof-OBLIGATION identity and the EVIDENCE identity are distinct nominals:
\ evidence discharges an obligation but can never launder into its identity, and
\ neither collapses to the artifact-id it is measured on.
s" CK-XO1 ( CAD-KIND:obligation-id -- CAD-KIND:evidence-id )" NO
s" CK-XO2 ( CAD-KIND:evidence-id -- CAD-KIND:obligation-id )" NO
s" CK-XO3 ( CAD-KIND:artifact-id -- CAD-KIND:obligation-id )" NO

\ The machine-facing action identity is a distinct nominal: an action-id names a
\ PROTOCOL OPERATION (owner package ACTION, maki/db/action.f) and can never launder
\ into a producer-id (a producing component), a schema-id, or an artifact-id.
s" CK-ACTION ( CAD-KIND:action-id -- CAD-KIND:action-id )" YES
s" CK-XAC1 ( CAD-KIND:action-id -- CAD-KIND:producer-id )" NO
s" CK-XAC2 ( CAD-KIND:producer-id -- CAD-KIND:action-id )" NO
s" CK-XAC3 ( CAD-KIND:action-id -- CAD-KIND:schema-id )" NO
s" CK-XAC4 ( CAD-KIND:artifact-id -- CAD-KIND:action-id )" NO

\ Generic typed memory keeps the declared role on store and fetch.
s" CK-PUT ( CAD-KIND:design-id ptr CAD-KIND:design-id -- ) !" YES
s" CK-GET ( ptr CAD-KIND:design-id -- CAD-KIND:design-id ) @" YES
s" CK-BAD-STORE ( CAD-KIND:node-id ptr CAD-KIND:design-id -- ) !" NO
\ Plan R3 verbatim fixture: mixed-role inputs cannot manufacture evidence.
s" CK-BAD-STAGE ( CAD-KIND:artifact-id CAD-KIND:stage -- CAD-KIND:evidence-id )" NO

\ Failure packets expose both qualified roles to a repair agent.
s" CK-BAD-ID ( CAD-KIND:design-id -- CAD-KIND:node-id )" DIAG<
s\" \"expected\"" DIAG?
s\" \"actual\"" DIAG?
s" cad-kind:design-id<> " DIAG?
s" cad-kind:node-id<> " DIAG?
DIAG-END

s" CK-BAD-STORE-DIAG ( CAD-KIND:node-id ptr CAD-KIND:design-id -- ) !" DIAG<
s\" \"expected\"" DIAG?
s\" \"actual\"" DIAG?
s" cad-kind:design-id<> " DIAG?
s" cad-kind:node-id<> " DIAG?
DIAG-END

\ Typed nominal-scalar cells: LAYOUT-BUFFER is the one pointer introduction.
1 LAYOUT-BUFFER CKT-RID CAD-KIND:region

s" CKT-PUT ( CAD-KIND:region -- ) 0 CKT-RID !" YES
s" CKT-GET ( -- CAD-KIND:region ) 0 CKT-RID @" YES
\ ST2 (habu-maki-typed-storage-2279c4b0) flipped: a raw n cannot launder
\ through the typed cell into a nominal role.
s" CKT-ST2 ( n -- CAD-KIND:region ) 0 CKT-RID ! 0 CKT-RID @" NO
\ P1/P2 flipped: a plain variable never certifies as a nominal-scalar pointer.
variable CKT-CELL
s" CKT-P1 ( -- ptr CAD-KIND:region ) CKT-CELL" NO
s" CKT-P2 ( -- ptr CAD-KIND:cols ) CKT-CELL" NO

\ ---- Canonical artifact envelope provenance roles (§ 23.9) ------------------
\ Every new envelope identity resolves by its qualified nominal identity.
s" CK-AKIND ( CAD-KIND:artifact-kind -- CAD-KIND:artifact-kind )" YES
s" CK-PRODUCER ( CAD-KIND:producer-id -- CAD-KIND:producer-id )" YES
s" CK-CONFIG ( CAD-KIND:config-id -- CAD-KIND:config-id )" YES
s" CK-NPOLICY ( CAD-KIND:numeric-policy-id -- CAD-KIND:numeric-policy-id )" YES
s" CK-CAP ( CAD-KIND:capability-id -- CAD-KIND:capability-id )" YES
s" CK-AUDIT ( CAD-KIND:audit-event-id -- CAD-KIND:audit-event-id )" YES

\ Adjacent envelope roles never collapse to one cell-shaped scalar identity.
s" CK-XA1 ( CAD-KIND:artifact-kind -- CAD-KIND:producer-id )" NO
s" CK-XA2 ( CAD-KIND:producer-id -- CAD-KIND:config-id )" NO
s" CK-XA3 ( CAD-KIND:config-id -- CAD-KIND:numeric-policy-id )" NO
s" CK-XA4 ( CAD-KIND:numeric-policy-id -- CAD-KIND:capability-id )" NO
s" CK-XA5 ( CAD-KIND:capability-id -- CAD-KIND:audit-event-id )" NO
\ The artifact CLASS is not the artifact IDENTITY (a kind cannot forge an id).
s" CK-XA6 ( CAD-KIND:artifact-kind -- CAD-KIND:artifact-id )" NO
s" CK-XA7 ( CAD-KIND:artifact-id -- CAD-KIND:artifact-kind )" NO
\ An existing neighbor role does not launder into a new envelope identity.
s" CK-XA8 ( CAD-KIND:schema-id -- CAD-KIND:config-id )" NO

\ Generic typed memory keeps the new role on store and fetch.
s" CK-CAP-PUT ( CAD-KIND:capability-id ptr CAD-KIND:capability-id -- ) !" YES
s" CK-CAP-GET ( ptr CAD-KIND:capability-id -- CAD-KIND:capability-id ) @" YES
s" CK-CAP-BAD ( CAD-KIND:producer-id ptr CAD-KIND:capability-id -- ) !" NO

\ The PERSISTENT audit-event identity is not the EPHEMERAL runtime async event
\ (ADAG:event-id): audit provenance can never unify with a synchronization
\ resource handle, in either direction.
s" CK-EV-POS ( ADAG:event-id -- ADAG:event-id )" YES
s" CK-EV-X1 ( CAD-KIND:audit-event-id -- ADAG:event-id )" NO
s" CK-EV-X2 ( ADAG:event-id -- CAD-KIND:audit-event-id )" NO

\ ---- Envelope kind separation and digest/id separation ---------------------
\ artifact<k> is the canonical envelope keyed by artifact-kind (package ARTIFACT
\ owns the real fielded type; these test-scoped stand-ins prove the checker
\ MECHANISM that enforces the frozen invariants without pinning the encoder's
\ representation). Distinct concrete kinds never unify. The 256-bit content
\ digest is a multi-cell owned value that never unifies with the one-cell
\ artifact-id, so no id-shaped scalar can be read or stored as a digest.
\ artifact<k>: the envelope stand-in, parameterized by kind.
TYPEFAMILY artifact 1
\ weight-kind / kernel-kind: two distinct concrete artifact kinds.
TYPEFAMILY weight-kind 0
TYPEFAMILY kernel-kind 0
\ content-digest: the 256-bit digest as four 64-bit words (deliberately not one cell).
PRODUCT content-digest 0
   FIELD w0 n
   FIELD w1 n
   FIELD w2 n
   FIELD w3 n
;PRODUCT

s" CK-ART-POS ( artifact<weight-kind> -- artifact<weight-kind> )" YES
s" CK-ART-XK ( artifact<weight-kind> -- artifact<kernel-kind> )" NO
s" CK-DIG-POS ( content-digest -- content-digest )" YES
s" CK-DIG-X1 ( CAD-KIND:artifact-id -- content-digest )" NO
s" CK-DIG-X2 ( content-digest -- CAD-KIND:artifact-id )" NO

\ Failure packet exposes both qualified roles to a repair agent.
s" CK-BAD-EVENT ( CAD-KIND:audit-event-id -- CAD-KIND:artifact-id )" DIAG<
s\" \"expected\"" DIAG?
s\" \"actual\"" DIAG?
s" cad-kind:artifact-id<> " DIAG?
s" cad-kind:audit-event-id<> " DIAG?
DIAG-END

T-REPORT

;package
