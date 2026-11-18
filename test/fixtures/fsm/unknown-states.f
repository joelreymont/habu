split-str
\ unknown-states.f - the header's states token on line 11 names no type; DEFINE
\ reads it as a type after reading the whole table, and its rejection names that
\ line, not the `;DEFINE` below it.
require src/lib/fsm.f
package FSM-UNKNOWN
ENUM ph idle busy ;ENUM
ENUM sig go halt ;ENUM
: S ( n -- sig ) drop construct sig go ;
using FSM
DEFINE M ( n )
   no-such-type
   sig
IN idle S go - >busy halt - >idle
FINAL busy -
;DEFINE
;using
;package
