\ extent-role.f - converters for the core extent-index families ix<e>,
\ extprod<free,inner> and redx<e>, registered by the engine in the global package
\ (src/core/type-family.f). A CAST: whose output is a redx must be declared at
\ global scope: only a family's declaring package may introduce its values
\ (checker.f CAST-OWNER?, 7135 E-CAST-OWNER). Like deftype.f, it has no package.
\ STORAGE CLASS. CALLER-OWNED: casts only; no module state.

\ There is no reverse cast: an n into an index space needs a runtime range guard
\ that a checked body cannot express.
CAST: IX>N ( ix<e> -- n )

\ It cannot launder a free factor into a contraction: checker.f
\ EXT-REDX-BAD-ARG? rejects a declared signature with redx over a free extent or
\ a whole product.
CAST: >RED ( ix<e> -- redx<e> )
