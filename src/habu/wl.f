\ wl.f - the wordlist ids the engine gives a meaning. Its startup row loads it
\ after boot-cells.f and before checker.f, which reads it.
package WL
public
\ A live package row's: the row keeps its public and private wids where a word
\ keeps its code span.
-1 constant NAMESPACE
\ A retired record's.
-2 constant RETIRED
\ A package row a rollback retired; no lookup is keyed on it.
-3 constant RETIRED-NAMESPACE
\ The wordlist id of the kernel's built-in words.
3 constant PRIMITIVES
: GLOBAL? ( n -- bool ) dup BOOT:GLOBAL-WL = swap PRIMITIVES = or ;
;package
