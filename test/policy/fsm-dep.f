\ fsm-dep.f - the types and words of the sealed FSM tables in test/policy;
\ allow-fsm.f admits PFSM. A sealed source cannot declare an ENUM, and a
\ table's `>state` NEXT constructs only a family of its own package, so its
\ NEXTs are pickers.

package PFSM
public
ENUM ph idle ;ENUM
ENUM sig go halt ;ENUM
: S ( n -- sig ) drop construct sig go ;
: ACT ( n -- ) drop ;
: STAY ( n -- ph ) drop construct ph idle ;
;package
