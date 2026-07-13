
## EVIDENCE ATTACHMENT 2026-07-13 (from the TK-CELL capability review; duplicate
dot habu-checker-nominal-value-d6533898 closed into this one)

Live probes on the unified tree: `( n -- CAD-KIND:region ) VAR ! VAR @`
CERTIFIES - a fetch from an untyped cell is a fresh var that binds a declared
family output in value position; the SAME raw variable also certifies as
`ptr CAD-KIND:region` AND `ptr CAD-KIND:cols` before the NOMPTR-BLOCK? guard
(pointee side now closed by 93a3b968; VALUE side is this dot). The TK-CELL
landing migrated all maki scratch cells to typed LAYOUT-BUFFER slots, so the
in-tree consumers are ready; this dot's TVK-RAW design closes the remaining
mint path (raw definers publishing unrestricted polymorphic effects).
