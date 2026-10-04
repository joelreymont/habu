\ replay-binding-raw.f - package RB-RAW, compiled with the check hook off: the
\ engine holds RB-RAW's `@` and the checker has no record of it. A body that
\ reopens RB-RAW and spells `@` bare calls this word, so test/replay-binding.f
\ requires the live check and a replay of that body to refuse it as a
\ trust-boundary word (E-CAP-TRUSTED), never to type it as the engine's fetch.

variable RB-RAW-CHECK
check@ RB-RAW-CHECK !
0 set-check

package RB-RAW

public

: @ ( ptr n n -- n ) {: b:ptr o:n :} b o cells + @ ;

;package

RB-RAW-CHECK @ set-check
