\ diag-code.f - what a checker diagnostic's code names.
\
\ tools/check.f --json-errors writes each diagnostic as one JSON object in the
\ shape its code names. tools/gate-json-assert.f checks a record and
\ tools/repair-packet.f builds its packet by the row read here, so a code
\ enters both through one row.
\
\ THE CONTRACT. Records are newline-delimited JSON on stderr with
\ schema_version 1, each a valid line even when the checker rejects the input;
\ the field set per shape is what tools/gate-json-assert-core.f requires
\ (diag-contract), over every fixture test/gate-diagnostics.f emits. verdict
\ is `uncheckable` for E-UNCHECKABLE, `deferred` on a deferral, else
\ `rejected`; a certified definition writes no record. A definition may fail
\ more than one check; its record names one refusal, the one the engine's own
\ load reports, in the load's order. line and column are one-based, column in
\ bytes; byte_start and byte_end are zero-based, end exclusive, in the labeled
\ file. Text with no file bytes behind it (the engine's own load, `evaluate`,
\ checker-generated definitions) reports its definition's origin plus the
\ token's offset in the checked text. The packet's source_excerpt is the
\ record's definition_source: packet builders normalize, the checker emits no
\ aliases.
\ Under --all-errors, and in CHECK:VERIFY-BYTES, every refused definition is
\ reported and the check goes on at the next one. A duplicate name is reported
\ by its record in both; CHECK:VERIFY-BYTES skips the definition and goes on,
\ --all-errors ends at it as the load does. A later definition sees a refused
\ one by its declared signature when that parses; a use of one whose signature
\ does not parse is E-UNDEFINED. Without --all-errors tools/check.f stops at
\ the first refusal.

require src/lib/string.f

package DIAG-CODE
public

\ definition: a refused definition, the shape of every code without a row.
\ declaration and storage: a refused family or storage declaration.
\ source-span: a refusal placed in the source outside any definition.
\ deferral: no refusal, a top-level stretch or a definition placed in the
\ source that the source pre-pass leaves to the run. input: an input refused whole. record: a
\ refusal, mostly of a checker record, that names its token and its place when
\ known. warning: no refusal, since the definition loaded. using-refusal: a
\ bare token with its used packages.
ENUM shape definition declaration storage source-span deferral input record warning using-refusal ;ENUM

private

\ A code's row: its shape, the repair classes it names, separated by spaces, and
\ the field its record adds to its token. A code that names no class leaves
\ the class to the record's own evidence: a storage refusal's is the class its
\ reason takes (tools/gate-json-assert-core.f GJA-STORAGE-CLASS$), which
\ GJA-STORAGE-CLASS holds it to.
: ROW ( ptr u8 n -- shape ptr u8 n ptr u8 n )
   {: c:ptr u:n :}
   c u s" E-BAD-DECLARATION" STR= IF
      construct shape declaration s" fix_family_declaration" s" " EXIT THEN
   c u s" E-BAD-STORAGE" STR= IF
      construct shape storage s" " s" " EXIT THEN
   c u s" E-STATEMENT-THROW" STR= IF
      construct shape source-span s" unknown_rejection" s" " EXIT THEN
   c u s" E-UNTERMINATED-STRING" STR= IF
      construct shape source-span s" close_string" s" " EXIT THEN
   c u s" E-MISSING-SOURCE" STR= IF
      construct shape source-span s" fix_load_path" s" " EXIT THEN
   c u s" E-UNREADABLE-SOURCE" STR= IF
      construct shape source-span s" make_source_readable" s" " EXIT THEN
   c u s" E-LOADER-FORM" STR= IF
      construct shape source-span s" literal_loader_form" s" " EXIT THEN
   c u s" E-MALFORMED-REGISTRY-ROW" STR= IF
      construct shape source-span s" close_primitive_row" s" " EXIT THEN
   c u s" E-UNDEFINED-TOP-LEVEL" STR= IF
      construct shape source-span s" unknown_rejection" s" " EXIT THEN
   c u s" E-BAD-QUALIFIED-TOP-LEVEL" STR= IF
      construct shape source-span s" fix_qualified_name" s" " EXIT THEN
   c u s" W-CHECK-DEFERRED" STR= IF
      construct shape deferral s" rewrite_uncheckable" s" " EXIT THEN
   c u s" E-GENERATES-ROW" STR= IF
      construct shape record
      s" fix_generates_row delete_generates_row fix_signature_syntax fix_signature_type fix_signature_arity fix_bare_ptr_element fix_return_stack"
      s" " EXIT THEN
   c u s" E-PARSES-ROW" STR= IF
      construct shape record s" fix_parses_row fix_parses_syntax" s" " EXIT THEN
   c u s" E-NAMES-ROW" STR= IF
      construct shape record s" fix_names_row fix_names_syntax" s" " EXIT THEN
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
      s" fix_signature_type fix_bare_ptr_element fix_signature_arity fix_signature_syntax fix_signature_size fix_return_stack"
      s" signature" EXIT THEN
   c u s" E-USING-SHADOW-GLOBAL" STR= IF
      construct shape using-refusal s" disambiguate_using_shadow" s" " EXIT THEN
   c u s" E-USING-AMBIGUOUS" STR= IF
      construct shape using-refusal s" disambiguate_using_ambiguous" s" " EXIT THEN
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

\ Whether a record under CODE is a refusal, which a deferral and a warning are
\ not.
: REFUSAL? ( ptr u8 n -- bool )
   SHAPE MATCH shape
      definition OF STR-TRUE ENDOF
      declaration OF STR-TRUE ENDOF
      storage OF STR-TRUE ENDOF
      source-span OF STR-TRUE ENDOF
      deferral OF STR-FALSE ENDOF
      input OF STR-TRUE ENDOF
      record OF STR-TRUE ENDOF
      warning OF STR-FALSE ENDOF
      using-refusal OF STR-TRUE ENDOF
   ;MATCH ;

;package
