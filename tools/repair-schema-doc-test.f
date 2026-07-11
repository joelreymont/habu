\ repair-schema-doc-test.f - checked fixture for repair diagnostic docs.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f tools/lint/text.f
\ tools/lint/token.f tools/lint/lib.f tools/lint/json-writer.f
\ tools/lint/source-lex.f tools/check-all-errors-core.f tools/json.f
\ tools/gate-json-assert-core.f tools/repair-schema-doc-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/check-all-errors-core.f
require tools/json.f
require tools/gate-json-assert-core.f

$40000 constant RSD-DOC-CAP
$10000 constant RSD-EMIT-CAP
8192 constant RSD-BUF-CAP

variable RSD-ROOT-U
variable RSD-SRC-U
variable RSD-DIAG-U
variable RSD-DOC-U
variable RSD-LLM-U
variable RSD-ERR-U
variable RSD-REND-U
variable RSD-CHK-U
variable RSD-DOC-BUF-A
variable RSD-LLM-BUF-A
variable RSD-OUT-A
variable RSD-ERR-A

create RSD-ROOT-BUF FS-PATH-CAP allot
create RSD-SRC-BUF FS-PATH-CAP allot
create RSD-DIAG-BUF FS-PATH-CAP allot
create RSD-REND-BUF RSD-EMIT-CAP allot
create RSD-CHK-BUF RSD-EMIT-CAP allot

: RSD-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: RSD-PTR-U8@ ( ptr a -- ptr u8 )
   RSD-PTR-U8-FIELD @ ;

: RSD-PTR-U8! ( ptr u8 ptr a -- )
   RSD-PTR-U8-FIELD ! ;

: RSD-ALLOC-BUF ( n -- ptr u8 )
   MEM-ALLOC-BYTES drop ;

: RSD-BUF ( ptr a n -- ptr u8 ) {: slot:ptr cap :}
   slot @ 0= if cap RSD-ALLOC-BUF slot RSD-PTR-U8! then
   slot RSD-PTR-U8@ ;

: RSD-DOC-BUF ( -- ptr u8 )
   RSD-DOC-BUF-A RSD-DOC-CAP RSD-BUF ;

: RSD-LLM-BUF ( -- ptr u8 )
   RSD-LLM-BUF-A RSD-DOC-CAP RSD-BUF ;

: RSD-OUT ( -- ptr u8 )
   RSD-OUT-A RSD-BUF-CAP RSD-BUF ;

: RSD-ERR ( -- ptr u8 )
   RSD-ERR-A RSD-BUF-CAP RSD-BUF ;

: RSD-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: RSD-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: RSD-ROOT ( -- ptr u8 n )
   RSD-ROOT-BUF RSD-ROOT-U @ ;

: RSD-SRC ( -- ptr u8 n )
   RSD-SRC-BUF RSD-SRC-U @ ;

: RSD-DIAG ( -- ptr u8 n )
   RSD-DIAG-BUF RSD-DIAG-U @ ;

: RSD-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: RSD-LF ( -- )
   10 SB-APPEND-C ;

: RSD-DQ ( -- )
   34 SB-APPEND-C ;

\ JBAD drives the scalar data-mismatch fields (expected/actual/declared_effect).
\ The zrc SUMTYPE + ZBAD drive the layout-mismatch `family` field: ZBAD declares
\ ( n -- zrc ) but leaves n, so expected zrc<> / actual n -> family "zrc".
\ CBAD drives the ADT `variant`/`tag`/`payload_pos` fields: `construct zrc keep`
\ wants n but CBAD supplies ptr u8, so the keep arm (tag 0, slot 0) is captured
\ at the mismatch. ABAD drives the `arity_expected`/`arity_actual` fields: zrc
\ declares arity 0 but ABAD writes zrc<n> -> E-WRONG-ARITY (0 vs 1).
\ (--all-errors re-drive emits an extra spurious E-BAD-DECLARATION duplicate-family
\ line for zrc; that is a separate registry-rollback gap, dotted, and does not
\ affect field-presence/schema parity here.)
: RSD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : JBAD ( i64 -- i64 ) dup ;" SB-APPEND RSD-LF
   s" SUMTYPE zrc 0 VARIANT keep n ;VARIANT ;SUMTYPE" SB-APPEND RSD-LF
   s" : ZBAD ( n -- zrc ) ;" SB-APPEND RSD-LF
   s" : CBAD ( ptr u8 -- zrc ) construct zrc keep ;" SB-APPEND RSD-LF
   s" : ABAD ( zrc<n> -- ) drop ;" SB-APPEND RSD-LF
   SB$ ;

: RSD-LOAD-DOCS ( -- )
   s" docs/repair-diagnostics.md" RSD-DOC-BUF RSD-DOC-CAP READ-ALL RSD-DOC-U !
   s" LLM.md" RSD-LLM-BUF RSD-DOC-CAP READ-ALL RSD-LLM-U ! ;

\ Emitter sources: render.f owns REPAIR-CLASS (data-stack/return/signature/dead
\ classes), check-core.f owns CHK-TYPE-JSON (fix_nominal_type, the deftype path).
: RSD-LOAD-EMITTERS ( -- )
   s" src/core/render.f" RSD-REND-BUF RSD-EMIT-CAP READ-ALL RSD-REND-U !
   s" tools/check-core.f" RSD-CHK-BUF RSD-EMIT-CAP READ-ALL RSD-CHK-U ! ;

: RSD-NEED-DOC ( ptr u8 n -- )
   RSD-DOC-BUF RSD-DOC-U @ 2swap CONTAINS? TTRUE ;

: RSD-NEED-LLM ( ptr u8 n -- )
   RSD-LLM-BUF RSD-LLM-U @ 2swap CONTAINS? TTRUE ;

: RSD-DOC-FIELD$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   SB-RESET
   s" | `" SB-APPEND
   a u SB-APPEND
   s" ` |" SB-APPEND
   SB$ ;

: RSD-DOC-CLASS$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   SB-RESET
   s" `" SB-APPEND
   a u SB-APPEND
   s" `" SB-APPEND
   SB$ ;

: RSD-NEED-DOC-FIELD ( ptr u8 n -- )
   RSD-DOC-FIELD$ RSD-NEED-DOC ;

: RSD-NEED-DOC-CLASS ( ptr u8 n -- )
   RSD-DOC-CLASS$ RSD-NEED-DOC ;

: RSD-TEST-DOC-ANCHORS ( -- )
   s" # Repair Diagnostics Schema" RSD-NEED-DOC
   s" ## Checker Diagnostic JSON" RSD-NEED-DOC
   s" ## Repair Packet JSON" RSD-NEED-DOC
   s" ## Benchmark Result Fields" RSD-NEED-DOC ;

: RSD-TEST-DOC-FIELDS ( -- )
   s" schema_version" RSD-NEED-DOC-FIELD
   s" code" RSD-NEED-DOC-FIELD
   s" repair_class" RSD-NEED-DOC-FIELD
   s" verdict" RSD-NEED-DOC-FIELD
   s" word" RSD-NEED-DOC-FIELD
   s" token" RSD-NEED-DOC-FIELD
   s" token_index" RSD-NEED-DOC-FIELD
   s" file" RSD-NEED-DOC-FIELD
   s" line" RSD-NEED-DOC-FIELD
   s" column" RSD-NEED-DOC-FIELD
   s" byte_start" RSD-NEED-DOC-FIELD
   s" byte_end" RSD-NEED-DOC-FIELD
   s" definition_source" RSD-NEED-DOC-FIELD
   s" declared_effect" RSD-NEED-DOC-FIELD
   s" declared_effect_source" RSD-NEED-DOC-FIELD
   s" inferred_effect" RSD-NEED-DOC-FIELD
   s" return_stack" RSD-NEED-DOC-FIELD
   s" expected" RSD-NEED-DOC-FIELD
   s" actual" RSD-NEED-DOC-FIELD
   s" family" RSD-NEED-DOC-FIELD
   s" variant" RSD-NEED-DOC-FIELD
   s" tag" RSD-NEED-DOC-FIELD
   s" payload_pos" RSD-NEED-DOC-FIELD
   s" arity_expected" RSD-NEED-DOC-FIELD
   s" arity_actual" RSD-NEED-DOC-FIELD
   s" suggestion" RSD-NEED-DOC-FIELD
   s" source_excerpt" RSD-NEED-DOC-FIELD
   s" reason" RSD-NEED-DOC-FIELD
   s" instruction" RSD-NEED-DOC-FIELD ;

\ Needle for an emitter site: the verbatim `s" class"` literal the emitters
\ write, so `s" fix_type"` cannot match `s" fix_signature_type"`.
: RSD-EMIT-NEEDLE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   115 SB-APPEND-C                             \ s
   RSD-DQ                                       \ "
   32 SB-APPEND-C                               \ space
   a u SB-APPEND
   RSD-DQ                                        \ "
   SB$ ;

: RSD-EMITTED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   RSD-REND-BUF RSD-REND-U @ a u CONTAINS?
   RSD-CHK-BUF RSD-CHK-U @ a u CONTAINS? or ;

: RSD-NEED-EMITTER ( ptr u8 n -- )
   RSD-EMIT-NEEDLE$ RSD-EMITTED? TTRUE ;

\ Completeness gate for one repair class across all four drift sites:
\   site 1 (emitters render.f/check-core.f) emit `s" class"`,
\   site 2 (GJA-SUGGEST-FOR) maps class -> suggestion (dies if unmapped) and
\           that suggestion text appears verbatim in the doc table (ties GJA<->doc),
\   site 3 (docs Repair Classes list) names `class`,
\   site 4 is this canonical list itself.
: RSD-NEED-CLASS ( ptr u8 n -- ) {: a:ptr u:n :}
   a u RSD-NEED-DOC-CLASS
   a u RSD-NEED-EMITTER
   a u GJA-SUGGEST-FOR RSD-NEED-DOC ;

\ Canonical repair-class enumeration (17). This is the single source of truth;
\ RSD-NEED-CLASS proves every downstream site carries each one. Reverse drift (a
\ class an emitter grows but this list omits) is caught at commit time by the
\ Forth gate, which re-runs this fixture after any emitter edit; a source-parsing
\ enumerator is deliberately out of scope (ADT/parser work, dot territory).
: RSD-TEST-DOC-CLASSES ( -- )
   s" remove_producer" RSD-NEED-CLASS
   s" add_producer" RSD-NEED-CLASS
   s" fix_type" RSD-NEED-CLASS
   s" fix_return_stack" RSD-NEED-CLASS
   s" trusted_boundary_required" RSD-NEED-CLASS
   s" factor_local_shape" RSD-NEED-CLASS
   s" factor_linear_local" RSD-NEED-CLASS
   s" remove_dead_code" RSD-NEED-CLASS
   s" fix_qualified_name" RSD-NEED-CLASS
   s" fix_signature_syntax" RSD-NEED-CLASS
   s" fix_signature_type" RSD-NEED-CLASS
   s" fix_signature_arity" RSD-NEED-CLASS
   s" fix_bare_ptr_element" RSD-NEED-CLASS
   s" fix_nominal_type" RSD-NEED-CLASS
   s" fix_family_declaration" RSD-NEED-CLASS
   s" rewrite_uncheckable" RSD-NEED-CLASS
   s" unknown_rejection" RSD-NEED-CLASS ;

: RSD-TEST-LLM-LINKS ( -- )
   s" docs/repair-diagnostics.md" RSD-NEED-LLM
   s" Repair diagnostic schema" RSD-NEED-LLM ;

: RSD-PREPARE ( -- )
   CLEANUP-RESET
   s" hb-repair-schema" TMPDIR-MKDIR {: a:ptr u :}
   a u RSD-ROOT-BUF RSD-ROOT-U RSD-COPY!
   RSD-ROOT CLEANUP-TREE+
   RSD-ROOT s" bad.f" RSD-SRC-BUF RSD-SRC-U RSD-PATH!
   RSD-ROOT s" bad.err" RSD-DIAG-BUF RSD-DIAG-U RSD-PATH!
   RSD-SRC RSD-SRC$ WRITE-ALL
   RSD-LOAD-DOCS
   RSD-LOAD-EMITTERS ;

: RSD-RUN-CHECK-ACT ( -- )
   RSD-SRC RSD-SRC CHECK-ALL-ERRORS-FILE ;

: RSD-RUN-CHECK ( -- n n n )
   RSD-ERR RSD-BUF-CAP RSD-OUT RSD-BUF-CAP CHECK-ALL-ERRORS-BUFFERS!
   0 0= CHECK-ALL-ERRORS-JSON!
   [: RSD-RUN-CHECK-ACT ;] catch {: rc:n :}
   0 CHECK-ALL-ERRORS-OUT$ nip rc ;

: RSD-EXPECT-EXIT ( n n n n -- n n ) {: outu:n erru:n code:n expect:n :}
   code expect T=
   outu erru ;

: RSD-DIAG-FIELD$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   SB-RESET
   RSD-DQ a u SB-APPEND RSD-DQ
   58 SB-APPEND-C
   SB$ ;

: RSD-REPAIR-CLASS$ ( -- ptr u8 n )
   SB-RESET
   RSD-DQ s" repair_class" SB-APPEND RSD-DQ
   58 SB-APPEND-C
   RSD-DQ s" remove_producer" SB-APPEND RSD-DQ
   SB$ ;

: RSD-NEED-DIAG-FIELD ( ptr u8 n -- )
   RSD-DIAG-FIELD$ RSD-ERR RSD-ERR-U @ 2swap CONTAINS? TTRUE ;

: RSD-TEST-DIAG-FIELDS ( -- )
   s" schema_version" RSD-NEED-DIAG-FIELD
   s" code" RSD-NEED-DIAG-FIELD
   s" repair_class" RSD-NEED-DIAG-FIELD
   s" verdict" RSD-NEED-DIAG-FIELD
   s" word" RSD-NEED-DIAG-FIELD
   s" token" RSD-NEED-DIAG-FIELD
   s" token_index" RSD-NEED-DIAG-FIELD
   s" file" RSD-NEED-DIAG-FIELD
   s" line" RSD-NEED-DIAG-FIELD
   s" column" RSD-NEED-DIAG-FIELD
   s" byte_start" RSD-NEED-DIAG-FIELD
   s" byte_end" RSD-NEED-DIAG-FIELD
   s" definition_source" RSD-NEED-DIAG-FIELD
   s" declared_effect" RSD-NEED-DIAG-FIELD
   s" declared_effect_source" RSD-NEED-DIAG-FIELD
   s" inferred_effect" RSD-NEED-DIAG-FIELD
   s" return_stack" RSD-NEED-DIAG-FIELD
   s" expected" RSD-NEED-DIAG-FIELD
   s" actual" RSD-NEED-DIAG-FIELD
   s" family" RSD-NEED-DIAG-FIELD
   s" variant" RSD-NEED-DIAG-FIELD
   s" tag" RSD-NEED-DIAG-FIELD
   s" payload_pos" RSD-NEED-DIAG-FIELD
   s" arity_expected" RSD-NEED-DIAG-FIELD
   s" arity_actual" RSD-NEED-DIAG-FIELD
   s" suggestion" RSD-NEED-DIAG-FIELD ;

: RSD-TEST-DIAG ( -- )
   RSD-RUN-CHECK 70 RSD-EXPECT-EXIT {: outu erru :}
   outu 0 T=
   erru RSD-ERR-U !
   RSD-DIAG RSD-ERR erru WRITE-ALL
   RSD-ERR RSD-ERR-U @ RSD-REPAIR-CLASS$ CONTAINS? TTRUE
   RSD-TEST-DIAG-FIELDS
   RSD-DIAG GJA-JSON-LINES-SCHEMA ;

: RSD-MAIN ( -- )
   T-RESET
   RSD-PREPARE
   RSD-TEST-DOC-ANCHORS
   RSD-TEST-DOC-FIELDS
   RSD-TEST-DOC-CLASSES
   RSD-TEST-LLM-LINKS
   RSD-TEST-DIAG
   CLEANUP-RUN
   RSD-ROOT EXISTS? TFALSE
   T-REPORT
   s" repair-schema-doc-test: ok" type cr ;

RSD-MAIN
