\ effect.f - the type and effect values a definer builds a definition from: a
\ type is a schema node, an effect a type whose node is a quotation, its
\ inputs and outputs deepest first, and `define-word` opens a definition from a
\ name and an effect; KEYWORD, OPERAND, TYPE-ITEM, CALL, LOCAL and LOCAL@
\ append its items. VARIABLE declares a variable of a type.
\ Each word asks the running engine's checker through its cell, never by name:
\ in a build a definer is the tree's copy, whose checker certifies nothing, and
\ a node only the checker that certifies made can reach it. The nodes are
\ transient: they end with the file that built them, or a build's piece.

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

\ The built-in type the spelling names (`n`, `u8`), else the family it names as
\ source where the definer runs would, over fresh parameters; else rejected.
: NAMED ( ptr u8 n -- type ) {: a:ptr u:n :}
   TYPE-OP-ABI:NAMED a NULL-PTR - u OP ;

\ The one type the text spells (`ptr u8`, `nullable<u8>`), read as a declaration
\ where the definer runs reads it; a letter is rejected.
: READ ( ptr u8 n -- type ) {: a:ptr u:n :}
   TYPE-OP-ABI:READ a NULL-PTR - u OP ;

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

private

\ The engine's cell holds the item runner as a raw execution token.
CAST: AS-ITEM ( n -- [ ptr u8 n n n -- ] )

\ The engine's cell holds the typed-variable maker as a raw execution token.
CAST: AS-VARIABLE ( n -- [ ptr u8 n n -- ] )

: ITEM ( ptr u8 n n n -- )
   data-base PROVIDED-XT:ITEM-CELL + @ AS-ITEM execute ;

public

\ Appends the keyword the string spells (`match`, `of`, `begin`).
: KEYWORD ( ptr u8 n -- ) 0 ITEM-ABI:KEYWORD ITEM ;

\ Appends an operand: the variant name an `of` arm or a `construct` takes.
: OPERAND ( ptr u8 n -- ) 0 ITEM-ABI:OPERAND ITEM ;

\ Appends type T, shown in the listing as the string spells it.
: TYPE-ITEM ( ptr u8 n type -- ) TYPE>N ITEM-ABI:TYPE ITEM ;

\ Appends a call of the word the name binds where the definer runs.
: CALL ( ptr u8 n -- ) 0 ITEM-ABI:CALL ITEM ;

\ Appends a local of type T to the open `{:` group and answers its index,
\ which counts the locals the definition declared before it.
: LOCAL ( type -- n ) {: t:type :}
   data-base BODY-ITEMS:LOCALS-CELL + @ {: i:n :}
   s" EFFECT:LOCAL" t TYPE>N ITEM-ABI:LOCAL ITEM
   i ;

\ Appends a read of the local whose index LOCAL answered.
: LOCAL@ ( n -- ) {: i:n :}
   s" EFFECT:LOCAL@" i ITEM-ABI:LOCAL-REF ITEM ;

\ Declares the variable the string names, holding one T, as
\ TYPED-VARIABLE does.
: VARIABLE ( ptr u8 n type -- ) {: t:type :}
   t TYPE>N data-base PROVIDED-XT:VARIABLE-CELL + @ AS-VARIABLE execute ;

;package

\ A definer's opener: the definition the string names, with effect E.
: define-word ( ptr u8 n EFFECT:type -- ) EFFECT:OPEN-WORD ;
