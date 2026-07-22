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

package REPAIR-SCHEMA-TEST

$40000 constant DOC-CAP
$10000 constant EMIT-CAP
8192 constant BUF-CAP

variable ROOT-U
variable SRC-U
variable DIAG-U
variable DOC-U
variable LLM-U
variable ERR-U
variable REND-U
variable CHK-U
variable DOC-BUF-A
variable LLM-BUF-A
variable OUT-A
variable ERR-A

create ROOT-BUF FS-PATH-CAP allot
create SRC-BUF FS-PATH-CAP allot
create DIAG-BUF FS-PATH-CAP allot
create REND-BUF EMIT-CAP allot
create CHK-BUF EMIT-CAP allot

: PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: PTR-U8@ ( ptr a -- ptr u8 )
   PTR-U8-FIELD @ ;

: PTR-U8! ( ptr u8 ptr a -- )
   PTR-U8-FIELD ! ;

: ALLOC-BUF ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: BUF ( ptr a n -- ptr u8 ) {: slot:ptr cap:n :}
   slot @ 0= if cap ALLOC-BUF slot PTR-U8! then
   slot PTR-U8@ ;

: DOC-BUF ( -- ptr u8 )
   DOC-BUF-A DOC-CAP BUF ;

: LLM-BUF ( -- ptr u8 )
   LLM-BUF-A DOC-CAP BUF ;

: OUT ( -- ptr u8 )
   OUT-A BUF-CAP BUF ;

: ERR ( -- ptr u8 )
   ERR-A BUF-CAP BUF ;

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu:n na:ptr nu:n dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: SRC ( -- ptr u8 n )
   SRC-BUF SRC-U @ ;

: DIAG ( -- ptr u8 n )
   DIAG-BUF DIAG-U @ ;

: EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: LF ( -- )
   10 SB-APPEND-C ;

: DQ ( -- )
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
: SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : JBAD ( i64 -- i64 ) dup ;" SB-APPEND LF
   s" SUMTYPE zrc 0 VARIANT keep n ;VARIANT ;SUMTYPE" SB-APPEND LF
   s" : ZBAD ( n -- zrc ) ;" SB-APPEND LF
   s" : CBAD ( ptr u8 -- zrc ) construct zrc keep ;" SB-APPEND LF
   s" : ABAD ( zrc<n> -- ) drop ;" SB-APPEND LF
   SB$ ;

: LOAD-DOCS ( -- )
   s" docs/repair-diagnostics.md" DOC-BUF DOC-CAP READ-ALL DOC-U !
   s" LLM.md" LLM-BUF DOC-CAP READ-ALL LLM-U ! ;

\ Emitter sources: render.f owns REPAIR-CLASS (data-stack/return/signature/dead
\ classes), check-core.f owns CHK-TYPE-JSON (fix_nominal_type, the nominal: path).
: LOAD-EMITTERS ( -- )
   s" src/core/render.f" REND-BUF EMIT-CAP READ-ALL REND-U !
   s" tools/check-core.f" CHK-BUF EMIT-CAP READ-ALL CHK-U ! ;

: NEED-DOC ( ptr u8 n -- )
   DOC-BUF DOC-U @ 2swap CONTAINS? TTRUE ;

: NEED-LLM ( ptr u8 n -- )
   LLM-BUF LLM-U @ 2swap CONTAINS? TTRUE ;

: DOC-FIELD$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" | `" SB-APPEND
   a u SB-APPEND
   s" ` |" SB-APPEND
   SB$ ;

: DOC-CLASS$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   s" `" SB-APPEND
   a u SB-APPEND
   s" `" SB-APPEND
   SB$ ;

: NEED-DOC-FIELD ( ptr u8 n -- )
   DOC-FIELD$ NEED-DOC ;

: NEED-DOC-CLASS ( ptr u8 n -- )
   DOC-CLASS$ NEED-DOC ;

: TEST-DOC-ANCHORS ( -- )
   s" # Repair Diagnostics Schema" NEED-DOC
   s" ## Checker Diagnostic JSON" NEED-DOC
   s" ## Repair Packet JSON" NEED-DOC
   s" ## Benchmark Result Fields" NEED-DOC ;

: TEST-DOC-FIELDS ( -- )
   s" schema_version" NEED-DOC-FIELD
   s" code" NEED-DOC-FIELD
   s" repair_class" NEED-DOC-FIELD
   s" verdict" NEED-DOC-FIELD
   s" word" NEED-DOC-FIELD
   s" token" NEED-DOC-FIELD
   s" token_index" NEED-DOC-FIELD
   s" file" NEED-DOC-FIELD
   s" line" NEED-DOC-FIELD
   s" column" NEED-DOC-FIELD
   s" byte_start" NEED-DOC-FIELD
   s" byte_end" NEED-DOC-FIELD
   s" definition_source" NEED-DOC-FIELD
   s" declared_effect" NEED-DOC-FIELD
   s" declared_effect_source" NEED-DOC-FIELD
   s" inferred_effect" NEED-DOC-FIELD
   s" return_stack" NEED-DOC-FIELD
   s" expected" NEED-DOC-FIELD
   s" actual" NEED-DOC-FIELD
   s" family" NEED-DOC-FIELD
   s" variant" NEED-DOC-FIELD
   s" tag" NEED-DOC-FIELD
   s" payload_pos" NEED-DOC-FIELD
   s" arity_expected" NEED-DOC-FIELD
   s" arity_actual" NEED-DOC-FIELD
   s" suggestion" NEED-DOC-FIELD
   s" source_excerpt" NEED-DOC-FIELD
   s" reason" NEED-DOC-FIELD
   s" instruction" NEED-DOC-FIELD ;

\ Needle for an emitter site: the verbatim `s" class"` literal the emitters
\ write, so `s" fix_type"` cannot match `s" fix_signature_type"`.
: EMIT-NEEDLE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   115 SB-APPEND-C                             \ s
   DQ                                           \ "
   32 SB-APPEND-C                               \ space
   a u SB-APPEND
   DQ                                            \ "
   SB$ ;

: EMITTED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   REND-BUF REND-U @ a u CONTAINS?
   CHK-BUF CHK-U @ a u CONTAINS? or ;

: NEED-EMITTER ( ptr u8 n -- )
   EMIT-NEEDLE$ EMITTED? TTRUE ;

\ Completeness gate for one repair class across all four drift sites:
\   site 1 (emitters render.f/check-core.f) emit `s" class"`,
\   site 2 (GJA-SUGGEST-FOR) maps class -> suggestion (dies if unmapped) and
\           that suggestion text appears verbatim in the doc table (ties GJA<->doc),
\   site 3 (docs Repair Classes list) names `class`,
\   site 4 is this canonical list itself.
: NEED-CLASS ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NEED-DOC-CLASS
   a u NEED-EMITTER
   a u GJA-SUGGEST-FOR NEED-DOC ;

\ Canonical repair-class enumeration (18). This is the single source of truth;
\ NEED-CLASS proves every downstream site carries each one. Reverse drift (a
\ class an emitter grows but this list omits) is caught at commit time by the
\ Forth gate, which re-runs this fixture after any emitter edit; a source-parsing
\ enumerator is deliberately out of scope (ADT/parser work, dot territory).
: TEST-DOC-CLASSES ( -- )
   s" remove_producer" NEED-CLASS
   s" add_producer" NEED-CLASS
   s" fix_type" NEED-CLASS
   s" fix_return_stack" NEED-CLASS
   s" trusted_boundary_required" NEED-CLASS
   s" model_compile_immediate" NEED-CLASS
   s" factor_local_shape" NEED-CLASS
   s" factor_linear_local" NEED-CLASS
   s" remove_dead_code" NEED-CLASS
   s" fix_qualified_name" NEED-CLASS
   s" fix_signature_syntax" NEED-CLASS
   s" fix_signature_type" NEED-CLASS
   s" fix_signature_arity" NEED-CLASS
   s" fix_bare_ptr_element" NEED-CLASS
   s" fix_nominal_type" NEED-CLASS
   s" fix_family_declaration" NEED-CLASS
   s" rewrite_uncheckable" NEED-CLASS
   s" unknown_rejection" NEED-CLASS ;

: TEST-LLM-LINKS ( -- )
   s" docs/repair-diagnostics.md" NEED-LLM
   s" Repair diagnostic schema" NEED-LLM ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" hb-repair-schema" TMPDIR-MKDIR {: a:ptr u :}
   a u ROOT-BUF ROOT-U COPY!
   ROOT CLEANUP-TREE+
   ROOT s" bad.f" SRC-BUF SRC-U PATH!
   ROOT s" bad.err" DIAG-BUF DIAG-U PATH!
   SRC SRC$ WRITE-ALL
   LOAD-DOCS
   LOAD-EMITTERS ;

: RUN-CHECK-ACT ( -- )
   SRC SRC CHECK-ALL-ERRORS-FILE ;

: RUN-CHECK ( -- n n n )
   ERR BUF-CAP OUT BUF-CAP CHECK-ALL-ERRORS-BUFFERS!
   0 0= CHECK-ALL-ERRORS-JSON!
   [: RUN-CHECK-ACT ;] catch {: rc:n :}
   0 CHECK-ALL-ERRORS-OUT$ nip rc ;

: EXPECT-EXIT ( n n n n -- n n ) {: outu:n erru:n code:n expect:n :}
   code expect T=
   outu erru ;

: DIAG-FIELD$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   DQ a u SB-APPEND DQ
   58 SB-APPEND-C
   SB$ ;

: REPAIR-CLASS$ ( -- ptr u8 n )
   SB-RESET
   DQ s" repair_class" SB-APPEND DQ
   58 SB-APPEND-C
   DQ s" remove_producer" SB-APPEND DQ
   SB$ ;

: NEED-DIAG-FIELD ( ptr u8 n -- )
   DIAG-FIELD$ ERR ERR-U @ 2swap CONTAINS? TTRUE ;

: TEST-DIAG-FIELDS ( -- )
   s" schema_version" NEED-DIAG-FIELD
   s" code" NEED-DIAG-FIELD
   s" repair_class" NEED-DIAG-FIELD
   s" verdict" NEED-DIAG-FIELD
   s" word" NEED-DIAG-FIELD
   s" token" NEED-DIAG-FIELD
   s" token_index" NEED-DIAG-FIELD
   s" file" NEED-DIAG-FIELD
   s" line" NEED-DIAG-FIELD
   s" column" NEED-DIAG-FIELD
   s" byte_start" NEED-DIAG-FIELD
   s" byte_end" NEED-DIAG-FIELD
   s" definition_source" NEED-DIAG-FIELD
   s" declared_effect" NEED-DIAG-FIELD
   s" declared_effect_source" NEED-DIAG-FIELD
   s" inferred_effect" NEED-DIAG-FIELD
   s" return_stack" NEED-DIAG-FIELD
   s" expected" NEED-DIAG-FIELD
   s" actual" NEED-DIAG-FIELD
   s" family" NEED-DIAG-FIELD
   s" variant" NEED-DIAG-FIELD
   s" tag" NEED-DIAG-FIELD
   s" payload_pos" NEED-DIAG-FIELD
   s" arity_expected" NEED-DIAG-FIELD
   s" arity_actual" NEED-DIAG-FIELD
   s" suggestion" NEED-DIAG-FIELD ;

: TEST-DIAG ( -- )
   RUN-CHECK 70 EXPECT-EXIT {: outu:n erru:n :}
   outu 0 T=
   erru ERR-U !
   DIAG ERR erru WRITE-ALL
   ERR ERR-U @ REPAIR-CLASS$ CONTAINS? TTRUE
   TEST-DIAG-FIELDS
   DIAG GJA-JSON-LINES-SCHEMA ;

: RUN ( -- )
   T-RESET
   PREPARE
   TEST-DOC-ANCHORS
   TEST-DOC-FIELDS
   TEST-DOC-CLASSES
   TEST-LLM-LINKS
   TEST-DIAG
   CLEANUP-RUN
   ROOT EXISTS? TFALSE
   T-REPORT
   s" repair-schema-doc-test: ok" type cr ;

RUN

;package
