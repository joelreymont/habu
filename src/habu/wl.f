\ wl.f - the wordlist ids the engine gives a meaning. Each core prefix loads it
\ after boot-cells.f and before checker.f, which reads it (native-runtime.f
\ provides it).
package WL
public
\ A live package row's: the row keeps its public and private wids where a word
\ keeps its code span (xref.f XREF:PACKAGE-WL?). BOOT:NAMESPACE-WL-CELL holds it
\ at boot.
-1 constant NAMESPACE
\ A retired record's; the row stays on its old hash chain (habu1.f WLFIND).
-2 constant RETIRED
\ A package row a rollback retired (habu1.f EM-DEF-OCC:LRETIRE); no lookup is
\ keyed on it.
-3 constant RETIRED-NAMESPACE
\ WL:MAX is the largest value a wordlist id may take; it bounds every
\ WID-indexed table and the snapshot's registry validation.
$FFFFFFFE constant MAX
\ The wordlist id of the kernel's built-in words (primitive-registry.f ADD).
3 constant PRIMITIVES
\ Built-in words count as global.
: GLOBAL? ( n -- bool ) dup BOOT:GLOBAL-WL = swap PRIMITIVES = or ;
;package
