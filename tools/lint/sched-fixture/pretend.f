\ pretend.f - MUST REPORT. It contains the pragma text three times and is
\ excused by none of them, because a pragma is read at the head of a comment
\ line and nowhere else.
\
\ Prose that mentions schedule-lint: allow-unscheduled mid-sentence is a
\ discussion of the rule, not a use of it.
package SCHED-FIXTURE-PRETEND

: QUOTED ( -- ptr u8 n )
   s" \ schedule-lint: allow-unscheduled - inside a string literal" ;

: PRETEND ( -- n ) 6 ;   \ schedule-lint: allow-unscheduled - trailing, not at a line head

;package
