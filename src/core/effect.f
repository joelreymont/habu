\ effect.f - the type and effect values a definer builds a definition from: a
\ type is a schema node, an effect a type whose node is a quotation, its
\ inputs and outputs deepest first. Each word asks the running engine's
\ checker through its cell, never by name: in a build a definer is the tree's
\ copy, whose checker certifies nothing, and a node only the checker that
\ certifies made can reach it. The nodes are transient: they end with the file
\ that built them, or a build's piece.

package EFFECT
public

NEWTYPE type

private

CAST: >TYPE ( n -- type )
CAST: TYPE>N ( type -- n )

\ The engine's cell holds CHECK:TYPE-OP as a raw execution token.
CAST: AS-TYPE-OP ( n -- [ n n n -- n ] )

: OP ( n n n -- type )
   data-base PROVIDED-XT:TYPE-OP-CELL + @ AS-TYPE-OP execute >TYPE ;

public

: N ( -- type ) TYPE-OP-ABI:INT 0 0 OP ;

\ Family FAM applied to parameters no other value holds.
: FAMILY ( n -- type ) {: fam:n :}
   TYPE-OP-ABI:FAMILY fam 0 OP ;

: PTR ( type -- type ) {: t:type :}
   TYPE-OP-ABI:PTR t TYPE>N 0 OP ;

\ The effect with no inputs and no outputs.
: NEW ( -- type ) TYPE-OP-ABI:EFFECT 0 0 OP ;

\ Effect E with T as its new top input.
: IN ( type type -- type ) {: e:type t:type :}
   TYPE-OP-ABI:IN e TYPE>N t TYPE>N OP ;

\ Effect E with T as its new top output.
: OUT ( type type -- type ) {: e:type t:type :}
   TYPE-OP-ABI:OUT e TYPE>N t TYPE>N OP ;

;package
