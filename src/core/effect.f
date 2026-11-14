\ effect.f - the type and effect values a definer builds a definition from: a
\ type is a schema node, an effect a type whose node is a quotation, its
\ inputs and outputs deepest first, and `define-word` opens a definition from a
\ name and an effect. Each word asks the running engine's checker through its
\ cell, never by name: in a build a definer is the tree's copy, whose checker
\ certifies nothing, and a node only the checker that certifies made can reach
\ it. The nodes are transient: they end with the file that built them, or a
\ build's piece.

package EFFECT
public

NEWTYPE type

private

CAST: >TYPE ( n -- type )
CAST: TYPE>N ( type -- n )

\ The engine's cell holds CHECK:TYPE-OP as a raw execution token.
CAST: AS-TYPE-OP ( n -- [ n n n -- n ] )

\ The engine's cell holds INTERPRETER:DEFINE-RUN as a raw execution token.
CAST: AS-OPEN ( n -- [ ptr u8 n n -- ] )

: OP ( n n n -- type )
   data-base PROVIDED-XT:TYPE-OP-CELL + @ AS-TYPE-OP execute >TYPE ;

public

: N ( -- type ) TYPE-OP-ABI:INT 0 0 OP ;

\ The family the spelling names (`PKG:name`, or a global family's bare name),
\ applied to parameters no other value holds; E-UNKNOWN-SIGNATURE-TYPE when it names none.
: FAMILY ( ptr u8 n -- type ) {: a:ptr u:n :}
   TYPE-OP-ABI:NAMED a NULL-PTR - u OP ;

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

\ Opens the definition the string names with effect E, in the running engine.
: OPEN-WORD ( ptr u8 n type -- ) {: e:type :}
   e TYPE>N data-base PROVIDED-XT:DEFINE-CELL + @ AS-OPEN execute ;

;package

\ A definer's opener: the definition the string names, with effect E.
: define-word ( ptr u8 n EFFECT:type -- ) EFFECT:OPEN-WORD ;
