\ orphan-ref.f - MUST REPORT. The only source naming this path is dangling.f,
\ which is itself unreached. Reachability is a closure from the ROOTS, so a
\ literal in an unreached file grants nothing; a lint that scanned every source
\ for literals instead of walking the closure would clear this file.
package SCHED-FIXTURE-ORPHAN-REF
: ORPHAN ( -- n ) 4 ;
;package
