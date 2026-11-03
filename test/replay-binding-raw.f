\ replay-binding-raw.f - package RB-RAW's public `@`, which the engine holds
\ and CHECKER-UNDEFINE takes from the checker (test/replay-binding.f RAW-CASE).

package RB-RAW

public

: @ ( ptr n n -- n ) {: b:ptr o:n :} b o cells + @ ;
s" @" CHECKER-UNDEFINE

;package
