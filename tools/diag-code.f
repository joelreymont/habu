\ diag-code.f - what a checker diagnostic's code names.
\
\ tools/check.f --json-errors writes each diagnostic as one JSON object in the
\ shape its code names (docs/repair-diagnostics.md). tools/gate-json-assert.f
\ checks a record and tools/repair-packet.f builds its packet by the row read
\ here, so a code enters both through one row.

require lib/string.f

package DIAG-CODE
public

\ definition: a refused definition, the shape of every code without a row.
\ declaration and storage: a refused family or storage declaration.
\ source-span: a refusal placed in the source outside any definition.
\ input: an input refused whole. record: a refusal, mostly of a checker record,
\ that names its token but not its place. warning: no refusal, since the
\ definition loaded.
ENUM shape definition declaration storage source-span input record warning ;ENUM

private

\ A code's row: its shape, the repair classes it names, separated by spaces, and
\ the field its record adds to its token. A code that names no class leaves
\ the class to the record's own evidence.
: ROW ( ptr u8 n -- shape ptr u8 n ptr u8 n ) {: c:ptr u:n :}
   c u s" E-BAD-DECLARATION" STR= IF
      construct shape declaration s" fix_family_declaration" s" " EXIT THEN
   c u s" E-BAD-STORAGE" STR= IF
      construct shape storage s" fix_storage_type fix_storage_name fix_storage_count" s" " EXIT THEN
   c u s" E-STATEMENT-THROW" STR= IF
      construct shape source-span s" unknown_rejection" s" " EXIT THEN
   c u s" E-UNTERMINATED-STRING" STR= IF
      construct shape source-span s" close_string" s" " EXIT THEN
   c u s" E-MALFORMED-REGISTRY-ROW" STR= IF
      construct shape source-span s" close_primitive_row" s" " EXIT THEN
   c u s" E-ENGINE-PROVIDED" STR= IF
      construct shape input s" rebuild_engine" s" " EXIT THEN
   c u s" E-TRUST-UNRESOLVED" STR= IF
      construct shape record s" fix_stale_trust_row" s" " EXIT THEN
   c u s" E-PKG-CONTEXT" STR= IF
      construct shape record s" use_storage_definer" s" " EXIT THEN
   c u s" E-BAD-QUALIFIED-RECORD" STR= IF
      construct shape record s" fix_qualified_name" s" " EXIT THEN
   c u s" E-BAD-STORED-SIGNATURE" STR= IF
      construct shape record
      s" fix_signature_type fix_bare_ptr_element fix_signature_arity fix_signature_syntax fix_signature_size"
      s" signature" EXIT THEN
   c u s" E-USING-SHADOW-GLOBAL" STR= IF
      construct shape record s" disambiguate_using_shadow" s" used_package" EXIT THEN
   c u s" E-SHADOWED-ARITY" STR= IF
      construct shape record s" match_shadowed_private_effect" s" package" EXIT THEN
   c u s" W-EFFECT-NOT-RECORDED" STR= IF
      construct shape warning s" " s" " EXIT THEN
   construct shape definition s" " s" " ;

\ Whether CLASS is one of the space-separated NAMES.
: NAMED? ( ptr u8 n ptr u8 n -- bool ) {: names:ptr namesu:n class:ptr classu:n :}
   0 begin
      >r names namesu 32 r> SPLIT-NEXT
   while
      >r class classu STR= IF r> drop STR-TRUE EXIT THEN r>
   repeat
   drop 2drop STR-FALSE ;

public

\ The shape of a record under CODE.
: SHAPE ( ptr u8 n -- shape )
   ROW 2drop 2drop ;

\ Whether a record under CODE may carry CLASS: one of the classes its code
\ names, or any when it names none.
: ADMITS? ( ptr u8 n ptr u8 n -- bool ) {: c:ptr u:n class:ptr classu:n :}
   c u ROW 2drop rot drop {: names:ptr namesu:n :}
   namesu 0= IF STR-TRUE EXIT THEN
   names namesu class classu NAMED? ;

\ The field a refused record under CODE adds to its token, empty when none.
: EVIDENCE ( ptr u8 n -- ptr u8 n )
   ROW 2swap 2drop rot drop ;

\ Whether a record under CODE is a refusal, which a warning is not.
: REFUSAL? ( ptr u8 n -- bool )
   SHAPE MATCH shape
      definition OF STR-TRUE ENDOF
      declaration OF STR-TRUE ENDOF
      storage OF STR-TRUE ENDOF
      source-span OF STR-TRUE ENDOF
      input OF STR-TRUE ENDOF
      record OF STR-TRUE ENDOF
      warning OF STR-FALSE ENDOF
   ;MATCH ;

;package
