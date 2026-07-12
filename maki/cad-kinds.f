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
TYPEFAMILY target-id 0
TYPEFAMILY toolchain-id 0
TYPEFAMILY pass-id 0
TYPEFAMILY schema-id 0

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
