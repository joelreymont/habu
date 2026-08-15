\ dangling.f - MUST REPORT. On disk, conforming, and named nowhere a runner can
\ see: root.f mentions this path only inside a `\` comment. It also names its own
\ path in a literal below, so a scanner that credited any mention of a path -
\ including the file's own - would call it reached.
package SCHED-FIXTURE-DANGLING

: SELF ( -- ptr u8 n )
   s" tools/lint/sched-fixture/dangling.f" ;

\ Reach does not flow from an unreached file: this literal names a real neighbour
\ that nothing else names, and orphan-ref.f must still report.
: NEIGHBOUR ( -- ptr u8 n )
   s" tools/lint/sched-fixture/orphan-ref.f" ;

;package
