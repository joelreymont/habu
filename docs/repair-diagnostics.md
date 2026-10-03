# Repair Diagnostics Schema

This is the stable machine contract for Habu checker repair feedback. The
implemented surface today is one JSON object per failed top-level definition from
the native `tools/check.f` runner with `--json-errors --all-errors`, plus the
records below for a refusal outside a definition. A repair packet is the normalized
LLM prompt object built from those checker diagnostics.

## Checker Diagnostic JSON

Checker diagnostics are newline-delimited JSON objects with
`schema_version: 1`. They are emitted on stderr and remain valid JSON object
lines even when the checker rejects the input.
The native gate enforces the required field set with `tools/gate-json-assert.f
diag-contract` over every checker JSONL fixture emitted by `test/gate-diagnostics.f`,
each record in the shape its `code` names: a declaration, a storage refusal, a
span, an input, or otherwise a definition.

Fields:

| Field | Type | Presence | Meaning |
| --- | --- | --- | --- |
| `schema_version` | integer | required | Current checker diagnostic schema version. |
| `code` | string | required | Stable error code such as `E-MISMATCH`, `E-REJECTED`, `E-UNDEFINED`, `E-UNSAFE`, `E-UNMODELED-IMMEDIATE`, `E-BAD-SIGNATURE`, `E-BAD-LOCAL-SHAPE`, `E-LOCAL-NAME-TOO-LONG`, `E-TOO-MANY-LOCALS`, `E-LINEAR-LOCAL`, `E-DEAD-CODE`, `E-INPUT-UNDERFLOW`, or `E-UNCHECKABLE`. |
| `repair_class` | string | required | Stable repair bucket used by LLM repair loops. |
| `verdict` | string | required | `rejected` or `uncheckable`; certification is not emitted as a diagnostic. |
| `word` | string | required | Failing definition name as seen by the checker. |
| `token` | string | required | Token that anchored the diagnostic. |
| `dead_owner` | string | dead-code only | Terminating token (`throw`, `die`, `exit`, `leave`, `again`, or a no-return word) that made the later token unreachable. |
| `token_index` | integer | required | Zero-based token index within the captured definition body. |
| `file` | string | required | Wrapper label or source path attached to the diagnostic. |
| `line` | integer | required | One-based line of the token's first byte in the labeled file; LF ends a line. |
| `column` | integer | required | One-based column of the token's first byte, counted in bytes. |
| `byte_start` | integer | required | Zero-based byte offset of the token's first byte in the labeled file. |
| `byte_end` | integer | required | Zero-based byte offset immediately after the token's last byte. |
| `definition_source` | string | required | Captured definition text without the leading colon and trailing semicolon. |
| `declared_effect` | string | required for signed definitions | Declared data and return-stack effect, normalized by the checker. |
| `declared_effect_source` | string | required for signed definitions | Declared effect as written between the signature parentheses, trimmed but preserving source row/type variable names. |
| `inferred_effect` | string | required | Inferred data and return-stack effect at the diagnostic point. |
| `return_stack` | object | required | Object with `expected` and `actual` return-stack rows. |
| `expected` | string | data mismatch only | Expected data-stack row. Absent when only the return stack or safety verdict failed. |
| `actual` | string | data mismatch only | Actual data-stack row. Absent when only the return stack or safety verdict failed. |
| `family` | string | layout mismatch only | Type-family name of the ADT layout value involved in the mismatch (expected side, else actual; else the captured variant's family for a `construct` payload mismatch). Carries the interned qualified spelling matching the expected/actual rows: folded `pkg:tail` for a foreign-package family, bare tail for the global package. Absent for pure-scalar mismatches. |
| `variant` | string | construct-arm mismatch only | Sum-variant name of the `construct family variant` arm being built when a payload type mismatched. Absent when no specific variant was in scope (boundary or pure-scalar mismatches). |
| `tag` | integer | construct-arm mismatch only | Declaration-order tag of the `variant` arm (0-based, matching the SUMTYPE declaration order). Present exactly when `variant` is. |
| `payload_pos` | integer | construct payload mismatch only | 0-based declaration-order index of the variant payload slot whose type failed to unify. Present only with `variant`, and only when the checker pinned the failure to a specific payload cell (absent for whole-row or post-expansion failures). |
| `arity_expected` | integer | wrong-arity signature only | The family's declared arity, on `E-WRONG-ARITY` / `fix_signature_arity` packets. |
| `arity_actual` | integer | wrong-arity signature only | The argument count actually written in the signature's family application. Present exactly when `arity_expected` is. |
| `reason` | string | storage refusal, or a definition refusal with a stated cause | Short cause: on every `E-BAD-STORAGE` record, and on a definition record whose refusal names one, such as `E-INPUT-UNDERFLOW` or a `match` or `construct` form. |
| `suggestion` | string | required | Human-readable repair hint derived from `repair_class`. |

The position fields locate the token in the labeled file's bytes. Where a
driver checks text it read out of the file, they are where its scanner read the
token, whatever the layout between tokens: verify-source
(`src/habu/verify-source.f`), which `tools/check.f` runs before it loads a file,
for `--all-errors` and as `CHECK:VERIFY-BYTES` (below), and `tools/check.f`'s
declaration pass. Text with no
file bytes behind it reports its definition's origin plus the token's offset in
the checked text: the engine's own load (`bin/hb --load` and `tools/check.f`'s
child run), whose captured definitions keep no source addresses, text built by
`evaluate`, and the definitions the checker generates for a declaration.

The checker JSON uses `definition_source` where a packet has `source_excerpt`.
It always carries `suggestion`; `reason` appears only on the records the
`reason` row names and on a declaration record. Packet builders must copy or
normalize these fields instead of requiring the checker to emit aliases.

Top-level type-family declaration failures (`NEWTYPE`/`SUMTYPE`) emit a
declaration-shaped object instead of the definition shape above: code
`E-BAD-DECLARATION`, repair class `fix_family_declaration`, `verdict`
`rejected`, plus `decl` (declaration kind), `family` (family name token),
`token` (offending token), `reason` (short cause), `file`, and `suggestion`.
When the token locates in the labeled file, `line`, `column`, `byte_start` and
`byte_end` follow `file`, with the meanings above. Declaration packets never
fabricate definition-only fields such as `word`, `declared_effect`,
`definition_source`, or `return_stack`.

A storage declaration its definer refuses (`LAYOUT-BUFFER`,
`DEFER-LAYOUT-BUFFER`, `TYPED-BUFFER`, `TYPED-VARIABLE`, `DYNAMIC-BUFFER`)
emits a storage-shaped object: code `E-BAD-STORAGE`, `verdict` `rejected`,
`word` (the declared name as written), `token` (the refused token), `reason`,
`file` and `suggestion`. The repair class follows the reason:
`fix_storage_type` for an unknown, malformed or unstorable type,
`fix_storage_name` for a name with more than one `:` or in a sealed package,
and `fix_storage_count` for a literal count outside the extent, a count token
that resolves to no `( -- n )` word, or no count. An
unknown type names its own token, any other type refusal the whole stored type.
`tools/check.f` reads the declaration before the run, so there the object also
carries the token's `line`, `column`, `byte_start` and `byte_end`; a run-time
definer under `bin/hb --load` has no record of its token's place and carries
none. It has no definition fields, and the checker continues past it under
`--all-errors`, counting it as a refusal.

A span record locates a refusal that is not a definition's. It carries `schema_version`, `code`, `repair_class`, `verdict`
`rejected`, the `token` with its `file`, `line`, `column`, `byte_start` and
`byte_end`, and `suggestion`, and no definition fields. There are three:

- `E-STATEMENT-THROW`, repair class `unknown_rejection`: a top-level statement
  threw while the checker checked it, with or without `--all-errors`, such as a
  `;using` with no `using` open (`E-USING-UNBALANCED`, 7142). The `token` is the
  one the checker read last, and the record adds the signed integer
  `throw_code` it raised. The checker does not continue past that statement in
  its source, and the run exits 70 as for a refusal. Without
  `--json-errors` it is the line `E-STATEMENT-THROW <file>:<line>:<column>:
  throw <throw_code> at '<token>'`.
- `E-UNTERMINATED-STRING`, repair class `close_string`: a string literal opened
  at `token` does not close in the checked source.
- `E-MALFORMED-REGISTRY-ROW`, repair class `close_primitive_row`: a `PRIM:` or
  `PPRIM:` primitive-axiom row opened at `token` does not close.

The lexer cannot read past either defect, so it is reported in place of
checking that source. `--all-errors` reports both classes for a file or
standard input; `tools/check.f` also reports an open string in a file in its
default mode, in the file that holds it (a required file included), since
source discovery stops there. Without `--json-errors` a file gets
`check.f: discovery rejected: unterminated string` and standard input the bare
code on its own line.

A second definition of a name in one wordlist, with or without
`--all-errors`, emits a definition-shaped object with code
`E-DUPLICATE-DEFINITION` and repair class `rename_duplicate`, whose `file` is
the source that defined the name again. Its `word`, `token` and
`definition_source` are the placeholder `duplicate-definition` at line 1,
column 1, not the duplicate's own name and place. Nothing after it is checked,
and the run exits 78 as the load does. Without `--json-errors` it is the line
`checker: duplicate definition in <file>`.

`tools/check.f` refuses, before checking anything, a single file or a
`--source-list` whose every input is a source the engine provides, since a run
loads nothing from such a source. Each input gets an input record: code
`E-ENGINE-PROVIDED`, repair class `rebuild_engine`, `verdict` `uncheckable`,
the input's `file` as given with `line` 1 and `column` 1, and `suggestion`;
without `--json-errors` it is the line `E-ENGINE-PROVIDED <file>:1:1:
<suggestion>`. The run exits 64. A list that also names a source the engine does
not provide is checked.

Two load-time refusals of a checker record emit a row-shaped object: no
definition encloses them, so they carry `schema_version`, `code`,
`repair_class`, `verdict` `rejected`, `token` (the name the record would have
described), `file` and `suggestion`, and no span or definition-only field.
`E-TRUST-UNRESOLVED` / `fix_stale_trust_row` is a `trust` row naming no word
where its record lands; `E-PKG-CONTEXT` / `use_storage_definer` is a checker
storage registrar called from source outside the engine's verifier window.

## Checking Without Running

`tools/check.f --verify-only FILE` reports what `bin/hb --load FILE` would
refuse of FILE's definitions and runs none of FILE or its closure: no lint, no
run stage. `--verify-only --stdin-path PATH` checks stdin's bytes as the file
at PATH, which need not exist. Both call `CHECK:VERIFY-BYTES`
(`tools/check-verify-core.f`), the operation a language server calls in its own
process.

- The require closure is discovered over the bytes, with PATH's directory as
  root and PATH as the subject's identity, so a dependency that requires PATH
  back meets the bytes, never the copy on disk. A closure that cannot be
  followed, through a missing file or one discovery refuses, is `refused`
  with that file named in the prose, and nothing is verified.
- The closure in dependency order, then the subject, is verified with all
  errors in one checker scope, in a child whose image is the engine's boot
  prefix plus the verifier, so no word of the caller or of an earlier check is
  visible. The subject's packets name PATH, canonical and absolute, with
  positions in the bytes; a dependency's name the dependency, with positions
  in its file.
- The child runs on `bin/hb` in the caller's working directory, which must
  be the tree root, as `check.f`'s run stage does.

| `CHECK:verdict` | Meaning | check.f exit |
| --- | --- | --- |
| `verified` | Nothing in the closure or the subject is refused. | 0 |
| `refused` | The subject, a file of its closure, or the closure itself is refused. | 70 |
| `engine-provided` | The engine provides PATH (`ENGINE-PROVIDES?`): nothing is verified, whatever the bytes hold. | 64 |
| `held` | The child's image holds PATH though the engine does not (`src/habu/verify-source.f`, `tools/check-verify-child.f`), so it cannot be verified there. | 69 |
| `incomplete` | The child ended without a result line; its `status` is the exit, signal or deadline, and the packets are those it made before. | 69 |

Under `--verify-only` check.f writes the packets on stderr, as schema-1 JSON
with or without `--json-errors`, and its prose on stdout, with a closing line
for `engine-provided`, `held` and `incomplete`. Child output beyond the
operation's capture exits 69 with the complete packets received before it, the
prose and a closing line. Usage errors (64), a missing FILE and an oversized
source (66) keep their exit codes and explain the failure on stdout.
An argument that exceeds the source path capacity exits 67 and explains the
limit on stdout. Ordinary checks explain it on stderr with the same status.
With `--verify-only`, a source list, a FILE beside `--stdin-path` and stdin
without it are usage errors; so is `--stdin-path` given twice or without
`--verify-only`.

`CHECK:VERIFY-BYTES ( ptr u8 n ptr u8 n ms -- CHECK:verdict )` takes the bytes,
PATH, a relative one read from the working directory, and the child's
deadline. `CHECK:VERIFY-OUT$` holds the packets, one JSON object per line, and
`CHECK:VERIFY-LOG$` the prose, until the next call. An empty PATH throws
`E-FS-PATH`; a closure of more than 128 files and a failed spawn throw as well.
Child output beyond the capture, 4 MiB on stdout or 256 KiB on stderr, kills
the child and throws `E-PROC-TRUNCATED`, with every complete packet received
before it in `CHECK:VERIFY-OUT$` and the stderr received in
`CHECK:VERIFY-LOG$`.
`tools/check-verify-test.f` prints what one check costs: about 50 ms for a
one-definition file and 230 ms for `tools/check-core.f`, whose closure is over
thirty files.

The child, `tools/check-verify-child.f`, is run only by the operation:

```text
ENGINE --load tools/check-verify-child.f -- SUBJECT [DEP ...] < BYTES
```

SUBJECT is canonical and absolute, and each DEP is a file of the closure,
canonical and absolute, in dependency order; a DEP the image holds is skipped,
as `require` skips it. stdout carries the packets in verification order, each
written as the checker makes it, so a child that dies has passed on every
packet made before; then one result line, `check-verify: verified`, `refused`
or `held`. stderr carries prose, including `PATH: verification stopped by
throw RC after N rejected definitions` for each file a throw stopped. The
verdict is read from the result line after a clean exit, never from the exit
status.

## Repair Packet JSON

Repair packets are the LLM-facing object passed back after a checker rejection.
They preserve the evidence present in the source diagnostic without inventing
fields that its shape cannot supply. `tools/repair-packet.f` builds one packet
from the first diagnostic, in the shape that diagnostic's record has: Schema 1
has definition, declaration, storage, span and input packet shapes.

Definition packet fields:

| Field | Type | Presence | Meaning |
| --- | --- | --- | --- |
| `schema_version` | integer | required | Repair packet schema version, currently `1`. |
| `kind` | string | required | Must be `habu_repair_packet`. |
| `word` | string | required | Failing definition name. |
| `token` | string | required | Diagnostic token. |
| `token_index` | integer | required | Zero-based token index within the definition body. |
| `file` | string | required | Source label or path. |
| `line` | integer | required | One-based source line. |
| `column` | integer | required | One-based source column. |
| `byte_start` | integer | required | Token start byte. |
| `byte_end` | integer | required | Token end byte. |
| `declared_effect` | string or null | required | Declared effect copied from the checker, or null if no checked signature existed. |
| `declared_effect_source` | string or null | required | Source-preserving declared effect copied from the checker, or null if no checked signature existed. |
| `inferred_effect` | string | required | Inferred effect copied from the checker. |
| `expected` | string or null | required | Expected data-stack row, or null when the checker did not emit a data-stack mismatch. |
| `actual` | string or null | required | Actual data-stack row, or null when the checker did not emit a data-stack mismatch. |
| `family` | string or null | required | Exact mismatched layout family, or null for non-layout failures. |
| `return_stack` | object | required | Object with `expected` and `actual` return-stack rows. |
| `code` | string | required | Stable checker error code. |
| `repair_class` | string | required | Stable repair bucket. |
| `reason` | string or null | required | Checker reason when present, otherwise null. |
| `suggestion` | string | required | Checker repair hint. |
| `source_excerpt` | string | required | Packet alias for checker `definition_source`. |
| `diagnostic_count` | integer | required | Number of diagnostics represented by the packet. |
| `instruction` | string | required | Definition-repair output constraint. |

Declaration packets carry only declaration evidence:

| Field | Type | Presence | Meaning |
| --- | --- | --- | --- |
| `schema_version` | integer | required | Repair packet schema version, currently `1`. |
| `kind` | string | required | Must be `habu_repair_packet`. |
| `decl` | string | required | Declaration kind (`newtype`, `sumtype`, `enum`, or `product`). |
| `family` | string | required | Family token; may be empty when the declaration omitted it. |
| `token` | string | required | Offending token; may be empty when one was missing. |
| `file` | string | required | Source label or path. |
| `code` | string | required | Must be `E-BAD-DECLARATION`. |
| `repair_class` | string | required | Must be `fix_family_declaration`. |
| `reason` | string | required | Short declaration failure cause. |
| `suggestion` | string | required | Stable declaration repair hint. |
| `diagnostic_count` | integer | required | Number of diagnostics represented by the packet. |
| `instruction` | string | required | Declaration-repair output constraint. |

Declaration packets do not fabricate `word`, source spans, effects, stack rows,
or `source_excerpt`.

Storage packets carry a storage record's evidence:

| Field | Type | Presence | Meaning |
| --- | --- | --- | --- |
| `schema_version` | integer | required | Repair packet schema version, currently `1`. |
| `kind` | string | required | Must be `habu_repair_packet`. |
| `word` | string | required | The declared name as written. |
| `token` | string | required | The refused token. |
| `reason` | string | required | Short refusal cause. |
| `file` | string | required | Source label or path. |
| `line` | integer | with a place | One-based source line. |
| `column` | integer | with a place | One-based source column. |
| `byte_start` | integer | with a place | Token start byte. |
| `byte_end` | integer | with a place | Token end byte. |
| `code` | string | required | `E-BAD-STORAGE`. |
| `repair_class` | string | required | `fix_storage_type`, `fix_storage_name` or `fix_storage_count`. |
| `suggestion` | string | required | Checker repair hint. |
| `diagnostic_count` | integer | required | Number of diagnostics represented by the packet. |
| `instruction` | string | required | `Fix the storage declaration so its definer accepts it. Output only corrected Habu code.` |

The packet copies the record's four place fields when it has them and none when
it has none, such as a declaration `evaluate` runs; it has no definition fields.

Span packets carry a span record's evidence:

| Field | Type | Presence | Meaning |
| --- | --- | --- | --- |
| `schema_version` | integer | required | Repair packet schema version, currently `1`. |
| `kind` | string | required | Must be `habu_repair_packet`. |
| `token` | string | required | Diagnostic token. |
| `file` | string | required | Source label or path. |
| `line` | integer | required | One-based source line. |
| `column` | integer | required | One-based source column. |
| `byte_start` | integer | required | Token start byte. |
| `byte_end` | integer | required | Token end byte. |
| `code` | string | required | `E-STATEMENT-THROW`, `E-UNTERMINATED-STRING` or `E-MALFORMED-REGISTRY-ROW`. |
| `throw_code` | integer or null | required | The code a statement threw; null for a lexer defect. |
| `repair_class` | string | required | Stable repair bucket. |
| `suggestion` | string | required | Checker repair hint. |
| `diagnostic_count` | integer | required | Number of diagnostics represented by the packet. |
| `instruction` | string | required | `Fix the source at this token so it checks. Output only corrected Habu code.` |

Input packets carry an input record's evidence, since no edit to the source
answers it:

| Field | Type | Presence | Meaning |
| --- | --- | --- | --- |
| `schema_version` | integer | required | Repair packet schema version, currently `1`. |
| `kind` | string | required | Must be `habu_repair_packet`. |
| `file` | string | required | The input as given. |
| `line` | integer | required | `1`. |
| `column` | integer | required | `1`. |
| `code` | string | required | `E-ENGINE-PROVIDED`. |
| `repair_class` | string | required | `rebuild_engine`. |
| `suggestion` | string | required | Checker repair hint. |
| `diagnostic_count` | integer | required | Number of diagnostics represented by the packet. |
| `instruction` | string | required | `Rebuild bin/hb to check this source; no code change answers this diagnostic.` |

When a packet aggregates multiple diagnostics, it must preserve deterministic
ordering from `--all-errors` and either include one packet per diagnostic or a
top-level array whose items each carry the fields above.

## Repair Classes

Current checker classes:

- `remove_producer`: the body leaves more data-stack values than declared.
- `add_producer`: the body leaves fewer data-stack values than declared.
- `fix_type`: data-stack arity matches, but one or more types differ.
- `fix_return_stack`: return-stack row differs from the declaration.
- `supply_missing_input`: a call inside the body consumed more cells than the
  declared inputs leave, so it reached under them into the caller's stack. The
  reason names both counts. Push the missing inputs before the call or declare
  them in the signature; `E-INPUT-UNDERFLOW` is a checker verdict and is
  unrelated to the runtime `E-UNDERFLOW` exit.
- `trusted_boundary_required`: checked code used a compiler or runtime boundary
  that requires audited `TRUST` or a modeled rewrite. This includes adversarial
  attempts to call `evaluate`, declare effects with `TRUST`, or disable/replace
  the checker hook with `set-check` from inside a checked definition.
- `model_compile_immediate`: a checked definition names an immediate whose
  compile-time expansion is not modeled as stack-neutral; declare its token
  consumption with `parse-imm` or remove it from the compiled body.
- `factor_local_shape`: locals were introduced inside active control flow, inside
  a quotation, or after a dead `exit` path; factor a helper or move locals before
  control opens.
- `shorten_local_name`: a local's bare name is wider than the 16 bytes the
  compiler's local record holds; the diagnostic states the name's width and the
  limit. Shorten the name.
- `reduce_local_count`: a `{: :}` group bound a 65th local; a definition binds
  at most 64. Bind fewer locals or factor a helper.
- `factor_linear_local`: a linear-counting value (a `deflinear` type) was bound to
  a `{: :}` local, where a reference could duplicate it and an unreferenced local
  could drop it; keep the linear value on the stack and factor instead.
- `remove_dead_code`: ordinary tokens appeared after a terminating control word;
  remove them or move the work before the terminating path.
- `fix_qualified_name`: a call used a malformed qualified name with more than one
  `:`; use a single `:` qualifier such as `PKG:WORD`.
- `fix_signature_syntax`: the stack-effect comment is malformed or incomplete.
- `fix_signature_type`: the stack-effect comment names an unknown multi-character
  type; use a known nominal type or a single-letter type variable.
- `fix_signature_arity`: a registered type family was applied to the wrong number
  of arguments; give it its exact declared arity.
- `fix_bare_ptr_element`: a signature named `ptr` with no element type; give it an
  element type, e.g. `ptr u8` or `ptr a`.
- `fix_nominal_type`: a `deftype` declaration used a reserved, duplicate, or
  syntactically invalid nominal type name.
- `fix_missing_name`: a definer (`:`, `DEFTYPE`, `package`, `NEWTYPE` and the
  rest) ended the source with no name after it (`E-MISSING-NAME`); the packet
  is located at the definer.
- `fix_record_field`: a `VALUE-RECORD` field had a bad or duplicate name or a
  missing or unknown type, or the record had no field (`E-BAD-RECORD-FIELD`);
  `reason` carries the registration's refusal and the packet is located at the
  field (at `END-VALUE-RECORD` for a record with no field).
- `fix_family_declaration`: a `NEWTYPE` or `SUMTYPE` declaration used a
  reserved, non-lowercase, or duplicate family/variant name, a bad arity token,
  an unknown payload type, or a malformed/unterminated `VARIANT` block.
- `fix_storage_type`: a storage declaration (`LAYOUT-BUFFER`,
  `DEFER-LAYOUT-BUFFER`, `TYPED-BUFFER`, `TYPED-VARIABLE`, `DYNAMIC-BUFFER`)
  names an unknown or malformed type, or one its definer cannot store; declare
  the type before the storage or store a type the definer admits.
- `fix_storage_name`: a storage declaration's name has more than one `:` or lies
  in a sealed package.
- `fix_storage_count`: a storage declaration's literal count is outside the
  buffer's extent, its count token resolves to no `( -- n )` word, or the
  declaration has no count.
- `rename_duplicate`: a name was defined a second time in one wordlist; rename
  it or `undefine` the first definition.
- `close_string`: a string literal does not close.
- `close_primitive_row`: a `PRIM:` or `PPRIM:` primitive-axiom row does not
  close.
- `rebuild_engine`: the input is a source the engine provides, so loading it
  checks nothing; rebuild `bin/hb` to check a change to it.
- `rewrite_uncheckable`: the checker could not model the word; rewrite with
  modeled words or use an audited boundary only when the primitive is intended.
- `unknown_rejection`: rejection did not fit a more specific class.

The checker `suggestion` field is stable short text derived only from
`repair_class`; it does not replace the raw `expected`, `actual`, or
`return_stack` evidence:

| `repair_class` | `suggestion` |
| --- | --- |
| `remove_producer` | `Remove an extra producer or drop the surplus value.` |
| `add_producer` | `Add the missing producer or stop consuming a required value.` |
| `fix_type` | `Change the body so produced types match the signature.` |
| `fix_return_stack` | `Balance return-stack transfers before the definition exits.` |
| `supply_missing_input` | `Push the missing inputs before the call, or declare them in the signature; a definition may not consume below its declared inputs.` |
| `trusted_boundary_required` | `Move this compiler or runtime boundary behind audited TRUST.` |
| `model_compile_immediate` | `Declare a stack-neutral parsing immediate with parse-imm, or remove it from the compiled body.` |
| `factor_local_shape` | `Move locals to a live top-level path or factor a helper.` |
| `shorten_local_name` | `Shorten the local name to at most 16 bytes.` |
| `reduce_local_count` | `Bind at most 64 locals in one definition, or factor a helper.` |
| `factor_linear_local` | `Keep the linear value on the stack; do not bind it to a local.` |
| `remove_dead_code` | `Remove tokens after the terminating control word, or move the work before it.` |
| `fix_qualified_name` | `Use one ':' qualifier, e.g. PKG:WORD.` |
| `fix_signature_syntax` | `Repair the stack-effect comment syntax, including --.` |
| `fix_signature_type` | `Use a known stack-signature type or a single-letter type variable.` |
| `fix_signature_arity` | `Give the type family its exact declared number of arguments.` |
| `fix_bare_ptr_element` | `Give 'ptr' an element type, e.g. 'ptr u8' or 'ptr a'.` |
| `fix_nominal_type` | `Choose a unique non-reserved nominal type name.` |
| `fix_missing_name` | `Give the definer a name: the next whitespace-delimited token.` |
| `fix_record_field` | `Declare at least one field, each with a unique name and a known type.` |
| `fix_family_declaration` | `Repair the family declaration: unique lowercase names, exact arity, closed VARIANT blocks.` |
| `fix_storage_type` | `Declare the type before the storage, or store a closed, copyable type this definer admits.` |
| `fix_storage_name` | `Name the storage with at most one inner ':', outside a sealed system package.` |
| `fix_storage_count` | `Put a positive count before the definer whose cells fit in memory: a literal, a constant or an expression.` |
| `rename_duplicate` | `Rename the word or undefine the old definition before redefining it.` |
| `close_string` | `Close the string literal before the definition ends.` |
| `close_primitive_row` | `Close the primitive-axiom row opened at this token: a bare row reads PRIM: name effect... PRIM;, and a package row reads PPRIM: package name effect... PPRIM; or CLOSE-PRIVATE.` |
| `rebuild_engine` | `The engine provides this source; rebuild bin/hb to check a change to it.` |
| `rewrite_uncheckable` | `Rewrite with modeled words or isolate an audited primitive.` |
| `unknown_rejection` | `Inspect the token, signature, and raw stack evidence.` |

The benchmark diagnostic fixtures include separate trusted-boundary rows for
`evaluate`, `TRUST`, and `set-check` misuse. Each must reject through
`tools/check.f --json-errors` as schema-1 JSON with
`repair_class: trusted_boundary_required` and the stable suggestion above.

## Benchmark Result Fields

Live benchmark JSONL rows use `schema_version: 2`. The native validator requires
identity fields `run_id`, `model_id`, `arm`, `task_id`, and `trial_id`, plus
`task_family`, `model`, `model_version`, `model_date`, trial/order metadata,
outcome/repair fields, token and wall-time fields, `source_chars`, runtime
fields, and replay artifacts. Unknown model version/date are represented by the
stable nonempty string `unknown`.

`checker_false_reject` is required on schema-2 rows. It is true only when the
first-pass checker rejected the candidate and execution confirmed the final
candidate passed; validators reject rows that set it on a certified checker pass
or on a failing execution row. Reports count these rows separately from model
failures so checker precision gaps do not depress language reliability.

Replay fields are `prompt`, `raw_response`, `extracted_candidate`,
`checker_diagnostics`, `repair_packet`, `test_output`, and `final_bundle`; every
one must have a paired `*_sha256` field. `prompt`, `raw_response`, and
`extracted_candidate` are nonempty. `final_bundle` is nonempty for rows where
`tests_passed` is true; error rows that cannot build a candidate may record an
empty `final_bundle` with the SHA-256 of the empty payload.

Benchmark rows score diagnostic quality with boolean fields derived from the
checker or repair packets:

| Field | Type | Presence | Meaning |
| --- | --- | --- | --- |
| `diagnostic_count` | integer | required | Number of checker diagnostics observed across repair attempts. |
| `diagnostic_token` | boolean | required | True when every diagnostic had token evidence. |
| `diagnostic_span` | boolean | required | True when every diagnostic had source span evidence. |
| `diagnostic_expected` | boolean | required | True when every relevant data-stack mismatch had expected-row evidence. |
| `diagnostic_actual` | boolean | required | True when every relevant data-stack mismatch had actual-row evidence. |
| `diagnostic_code` | boolean | required | True when every diagnostic had a stable error code. |
| `diagnostic_repair_class` | boolean | required | True when every diagnostic had a stable repair class. |
| `all_errors_stable` | boolean | required | True when repeated checker runs produced identical diagnostic JSONL. |
| `repair_class_stats` | array | optional when no diagnostics | Per-class diagnostic counts, repair success accounting, and first-seen repair packet order. |

Each `repair_class_stats` item is required to contain `repair_class`,
`diagnostic_count`, `repair_success`, `repair_iterations`, `first_round`,
`first_order`, and `token_delta`. `first_round` is the repair round where that
class first appeared in the diagnostic event stream; `first_order` is its
1-based first-seen order among classes in that row's first actionable repair
packet evidence. Lower `first_order` is better when multiple classes are present
because the first repair packet is what drives the next model attempt.

Benchmark validators must fail rows that claim diagnostic quality without the
corresponding evidence. Reports must keep diagnostic quality, repair success,
repair rounds, wall time, and generated-token cost as separate axes.
