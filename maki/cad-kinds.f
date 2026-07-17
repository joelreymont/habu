\ cad-kinds.f - nominal Model CAD handle and domain kinds.

package CAD-KIND
public

\ Persistent identity roles.
TYPEFAMILY design-id 0
TYPEFAMILY rev-id 0
TYPEFAMILY obj-id 0
TYPEFAMILY node-id 0
TYPEFAMILY analysis-id 0
TYPEFAMILY plan-id 0
TYPEFAMILY artifact-id 0
TYPEFAMILY evidence-id 0
\ The immutable proof-OBLIGATION identity (MODEL-CAD-V2-PLAN.md § 23.9; dot
\ habu-v2-evidence-applicability-73ac58b9). Deliberately distinct from
\ evidence-id: EVIDENCE discharges an obligation, but neither identity may launder
\ into the other. Its content-addressed owner registry is package OBLIG
\ (maki/db/obligation.f), the transaction codec's obligation wire form.
TYPEFAMILY obligation-id 0
TYPEFAMILY target-id 0
TYPEFAMILY toolchain-id 0
TYPEFAMILY pass-id 0
TYPEFAMILY schema-id 0

\ Canonical artifact envelope provenance roles (MODEL-CAD-V2-PLAN.md § 23.9
\ Canonical typed artifacts). These are the persistent identities the stored
\ envelope binds: `artifact-kind` is the artifact's semantic CLASS and is
\ deliberately distinct from `artifact-id` (its content-addressed IDENTITY, line
\ 13), so a kind can never stand in for an id. `audit-event-id` is the append-only
\ audit record link (§23.9 "append-only audit records"); it is a persistent
\ provenance identity, NOT the ephemeral runtime synchronization event
\ `ADAG:event-id` (maki/async-dag.f) - the two must never unify. The 256-bit
\ content digest is NOT declared here: a one-cell nominal would let an id-shaped
\ scalar launder as a digest; it is an owned multi-cell value type owned by the
\ envelope encoder (package ARTIFACT), kept structurally distinct from every id.
TYPEFAMILY artifact-kind 0
TYPEFAMILY producer-id 0
TYPEFAMILY config-id 0
TYPEFAMILY numeric-policy-id 0
TYPEFAMILY capability-id 0
TYPEFAMILY audit-event-id 0

\ Model and lowering domain roles.
\ dtype and layout are NOT declared here: those roles are owned by the maki
\ ENUM families (`dtype` in maki/tensor.f, `layout` in maki/tensor-value.f),
\ whose variant-closed constructors + MATCH render boundaries are strictly
\ stronger than an opaque nominal (out-of-range unrepresentable, no raw-n
\ conversion surface). See .dots/habu-merge-policy-master-961bb2b7.md.
TYPEFAMILY dim 0
TYPEFAMILY shape 0
TYPEFAMILY rows 0
TYPEFAMILY cols 0
TYPEFAMILY address-space 0
TYPEFAMILY stage 0
TYPEFAMILY effect 0
TYPEFAMILY region 0

;package
