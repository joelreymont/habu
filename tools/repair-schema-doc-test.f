\ repair-schema-doc-test.f - checked fixture for repair diagnostic docs.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/repair-schema-doc-test.f

$40000 constant RSD-DOC-CAP
8192 constant RSD-BUF-CAP
3000 constant RSD-TIMEOUT-MS

variable RSD-ROOT-U
variable RSD-SRC-U
variable RSD-DIAG-U
variable RSD-DOC-U
variable RSD-LLM-U
variable RSD-ERR-U

create RSD-ROOT-BUF FS-PATH-CAP allot
create RSD-SRC-BUF FS-PATH-CAP allot
create RSD-DIAG-BUF FS-PATH-CAP allot
create RSD-DOC-BUF RSD-DOC-CAP allot
create RSD-LLM-BUF RSD-DOC-CAP allot
create RSD-OUT RSD-BUF-CAP allot
create RSD-ERR RSD-BUF-CAP allot

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

: RSD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : JBAD ( i64 -- i64 ) dup ;" SB-APPEND RSD-LF
   SB$ ;

: RSD-LOAD-DOCS ( -- )
   s" docs/repair-diagnostics.md" RSD-DOC-BUF RSD-DOC-CAP READ-ALL RSD-DOC-U !
   s" LLM.md" RSD-LLM-BUF RSD-DOC-CAP READ-ALL RSD-LLM-U ! ;

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
   s" suggestion" RSD-NEED-DOC-FIELD
   s" source_excerpt" RSD-NEED-DOC-FIELD
   s" reason" RSD-NEED-DOC-FIELD ;

: RSD-TEST-DOC-CLASSES ( -- )
   s" remove_producer" RSD-NEED-DOC-CLASS
   s" add_producer" RSD-NEED-DOC-CLASS
   s" fix_type" RSD-NEED-DOC-CLASS
   s" fix_return_stack" RSD-NEED-DOC-CLASS
   s" trusted_boundary_required" RSD-NEED-DOC-CLASS
   s" fix_signature_syntax" RSD-NEED-DOC-CLASS
   s" rewrite_uncheckable" RSD-NEED-DOC-CLASS
   s" unknown_rejection" RSD-NEED-DOC-CLASS ;

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
   RSD-LOAD-DOCS ;

: RSD-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: RSD-RUN-CHECK ( -- n n n )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/vector.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+ s" tools/lint/token.f" >LEN PROC-ARGV+ s" tools/lint/lib.f" >LEN PROC-ARGV+
   s" tools/lint/json-writer.f"  >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/check-all-errors-core.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/check-all-errors.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" --json-errors"  >LEN PROC-ARGV+
   s" --label"  >LEN PROC-ARGV+
   RSD-SRC  >LEN PROC-ARGV+
   RSD-SRC  >LEN PROC-ARGV+
   s" bin/hb"  >LEN RSD-OUT RSD-BUF-CAP >LEN RSD-ERR RSD-BUF-CAP >LEN
   RSD-TIMEOUT-MS >MS RUN-ARGV-CAPTURE RSD-CAPTURE>N ;

: RSD-RUN-ASSERT ( ptr u8 n -- n n n ) {: mode:ptr modeu :}
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/gate-json-assert.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   mode modeu  >LEN PROC-ARGV+
   RSD-DIAG  >LEN PROC-ARGV+
   s" bin/hb"  >LEN RSD-OUT RSD-BUF-CAP >LEN RSD-ERR RSD-BUF-CAP >LEN
   RSD-TIMEOUT-MS >MS RUN-ARGV-CAPTURE RSD-CAPTURE>N ;

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
   s" suggestion" RSD-NEED-DIAG-FIELD ;

: RSD-TEST-DIAG ( -- )
   RSD-RUN-CHECK 70 T=
   {: outu erru :}
   outu 0 T=
   erru RSD-ERR-U !
   RSD-DIAG RSD-ERR erru WRITE-ALL
   RSD-ERR RSD-ERR-U @ RSD-REPAIR-CLASS$ CONTAINS? TTRUE
   RSD-TEST-DIAG-FIELDS
   s" json-lines-schema" RSD-RUN-ASSERT 0 T=
   {: aout aerr :}
   aout 0 T=
   aerr 0 T= ;

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
