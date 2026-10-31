\ diag-code.f - what a checker diagnostic's code names: its record shape, its
\ repair classes and its evidence field. tools/gate-json-assert.f checks a record
\ and tools/repair-packet.f builds its packet by the row read here, so a code
\ enters both through one row.

require src/lib/string.f

package DIAG-CODE
public

\ The record contract: newline-delimited JSON on stderr, schema_version 1, a
\ valid line even when the input is rejected; the field set per shape is what
\ tools/gate-json-assert-core.f requires (diag-contract) over every fixture
\ test/gate-diagnostics.f emits. verdict is `uncheckable` for E-UNCHECKABLE,
\ `deferred` on a deferral, else `rejected`; a certified definition writes no
\ record. A record names one refusal, the first the engine's own load reports.
\ line and column are one-based, column in bytes; byte_start and byte_end are
\ zero-based, end exclusive, in the labeled file. Text with no file bytes behind
\ it (the engine's own load, `evaluate`, checker-generated definitions) reports
\ its definition's origin plus the token's offset in the checked text.
\ source_excerpt is the record's definition_source: packet builders normalize,
\ the checker emits no aliases. --all-errors and CHECK:VERIFY-BYTES report every
\ refused definition and go on; at a duplicate name CHECK:VERIFY-BYTES skips the
\ definition and --all-errors ends as the load does. A later definition sees a
\ refused one by its declared signature when that parses, else its use is
\ E-UNDEFINED. Without --all-errors tools/check.f stops at the first refusal.

\ definition: a refused definition, the shape of every code without a row.
\ declaration and storage: a refused family or storage declaration.
\ source-span: a refusal placed in the source outside any definition.
\ deferral: no refusal, a top-level stretch or a definition placed in the
\ source that the source pre-pass leaves to the run. input: an input refused
\ whole. record: a refusal, mostly of a checker record, that names its token
\ and its place when known. warning: no refusal, since the definition loaded.
\ using-refusal: a bare token with its used packages.
ENUM shape definition declaration storage source-span deferral input record warning using-refusal ;ENUM

private

\ A code's row: its shape, the space-separated repair classes it names, and the
\ field its record adds to its token. A code that names no class leaves the
\ class to the record's evidence: a storage refusal's is its reason's class,
\ held by GJA-STORAGE-CLASS (tools/gate-json-assert-core.f).
: ROW ( ptr u8 n -- shape ptr u8 n ptr u8 n )
   {: c:ptr u:n :}
   c u s" E-BAD-DECLARATION" STR= IF
      construct shape declaration s" fix_family_declaration" s" " EXIT THEN
   c u s" E-BAD-STORAGE" STR= IF
      construct shape storage s" " s" " EXIT THEN
   c u s" E-STATEMENT-THROW" STR= IF
      construct shape source-span s" unknown_rejection" s" " EXIT THEN
   c u s" E-MISSING-SIGNATURE" STR= IF
      construct shape source-span s" add_signature" s" " EXIT THEN
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

: NAMED? ( ptr u8 n ptr u8 n -- bool ) {: names:ptr namesu:n class:ptr classu:n :}
   0 begin
      >r names namesu 32 r> SPLIT-NEXT
   while
      >r class classu STR= IF r> drop STR-TRUE EXIT THEN r>
   repeat
   drop 2drop STR-FALSE ;

public

: SHAPE ( ptr u8 n -- shape )
   ROW 2drop 2drop ;

: ADMITS? ( ptr u8 n ptr u8 n -- bool ) {: c:ptr u:n class:ptr classu:n :}
   c u ROW 2drop rot drop {: names:ptr namesu:n :}
   namesu 0= IF STR-TRUE EXIT THEN
   names namesu class classu NAMED? ;

\ Empty when the code adds no field.
: EVIDENCE ( ptr u8 n -- ptr u8 n )
   ROW 2swap 2drop rot drop ;

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
