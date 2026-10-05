# Repair Diagnostics Schema

This is the stable machine contract for Habu checker repair feedback. The
implemented surface today is one JSON object per failed top-level definition from
the native `tools/check.f` runner with `--json-errors --all-errors`, plus the
records below for a refusal in another shape, a deferral, and a warning outside
the contract. A repair packet is the normalized LLM prompt object built from those
checker diagnostics.

## Checker Diagnostic JSON

Checker diagnostics are newline-delimited JSON objects with
`schema_version: 1`. They are emitted on stderr and remain valid JSON object
lines even when the checker rejects the input.
The native gate enforces the required field set with `tools/gate-json-assert.f
diag-contract` over every checker JSONL fixture emitted by `test/gate-diagnostics.f`,
each record in the shape its `code` names: a declaration, a storage refusal, a
span, a deferral, an input, a refused record, a using refusal, a warning, or
otherwise a definition.
`tools/diag-code.f` holds a row for every code whose record is not a
definition's: its shape, the repair classes it names and the field a refused
record adds. That check and `tools/repair-packet.f` both read it.

Under `--all-errors`, and in `CHECK:VERIFY-BYTES` (below), every refused
definition is reported, rejected or uncheckable alike, one with an undefined
word included, and the check goes on at the next definition. A duplicate,
refused at its name before its body is checked, is reported by its record
(below) in both, and `CHECK:VERIFY-BYTES` goes on past it, skipping the
definition; `--all-errors` ends at it, as the load does. Both end at a
duplicate of a name a definer generates, after its record, and when recording
a refused definition throws, as a malformed name's record does (below):
`--all-errors` after writing its record, `CHECK:VERIFY-BYTES` with its stderr
stop line (below) and no packet for it. A later definition
sees a refused one by its declared signature when that parses: a use that fits
it gets no record, one that does not gets that definition's own; a use of one
whose signature does not parse is `E-UNDEFINED`. Without `--all-errors`
`tools/check.f` stops at the first refusal.

Fields:

| Field | Type | Presence | Meaning |
| --- | --- | --- | --- |
| `schema_version` | integer | required | Current checker diagnostic schema version. |
| `code` | string | required | Stable error code such as `E-MISMATCH`, `E-REJECTED`, `E-UNDEFINED`, `E-UNSAFE`, `E-UNMODELED-IMMEDIATE`, `E-BAD-SIGNATURE`, `E-BAD-LOCAL-SHAPE`, `E-LOCAL-NAME-TOO-LONG`, `E-TOO-MANY-LOCALS`, `E-LINEAR-LOCAL`, `E-DEAD-CODE`, `E-INPUT-UNDERFLOW`, or `E-UNCHECKABLE`. |
| `repair_class` | string | required | Stable repair bucket used by LLM repair loops. |
| `verdict` | string | required | `rejected` or `uncheckable`, or `deferred` on a deferral; certification is not emitted as a diagnostic. |
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
`word` (the declared name as written, or the definer when no name stands on
its line: as written under `tools/check.f`, in its canonical uppercase
spelling under `bin/hb --load`), `token` (the refused token), `reason`, `file` and `suggestion`. The
`reason` is one of the texts below and the repair class follows it.
`diag-contract` (tools/gate-json-assert-core.f `GJA-STORAGE-CLASS$`) holds a
record to this table and refuses one under another class or with a reason not
listed:

| `repair_class` | `reason` | Refusal |
| --- | --- | --- |
| `fix_storage_type` | `unknown type` | The type names nothing the checker knows. |
| `fix_storage_type` | `malformed type` | The type does not parse. |
| `fix_storage_type` | `type this definer cannot store` | The type parses, and this definer cannot store it. |
| `fix_storage_type` | `scheme in a stored type` | The type holds a scheme, which no storage holds. |
| `fix_storage_type` | `no type for` | No token follows the name on its line to be its type. |
| `fix_storage_name` | `more than one ':' in name` | The name has more than one inner `:`. |
| `fix_storage_name` | `name in a sealed package` | The name is qualified into a sealed package. |
| `fix_storage_name` | `no name for` | No token follows the definer on its line to be its name. |
| `fix_storage_count` | `count outside the buffer's extent` | The literal count is outside the definer's extent. |
| `fix_storage_count` | `count resolves to no ( -- n ) word` | The count token names no word that leaves the count. |
| `fix_storage_count` | `no count for` | No token precedes the definer to be its count. |

An unknown type names its own token, a missing one the name, any other type
refusal the whole stored type. `tools/check.f` reads the declaration before the
run, so there the object also carries the token's `line`, `column`, `byte_start`
and `byte_end`; a run-time definer under `bin/hb --load` has no record of its
token's place and carries none. It has no definition fields, and the checker
continues past it under `--all-errors`, counting it as a refusal.

A span record locates a refusal that is not a definition's. It carries `schema_version`, `code`, `repair_class`, `verdict`
`rejected`, the `token` with its `file`, `line`, `column`, `byte_start` and
`byte_end`, and `suggestion`, and no definition fields. There are six:

- `E-STATEMENT-THROW`, repair class `unknown_rejection`: a top-level statement
  threw while the checker checked it, with or without `--all-errors`, such as a
  `;using` with no `using` open (`E-USING-UNBALANCED`, 7142). The `token` is the
  one the checker read last, and the record adds the signed integer
  `throw_code` it raised. The checker does not continue past that statement in
  its source, and the run exits 70 as for a refusal. Without
  `--json-errors` it is the line `E-STATEMENT-THROW <file>:<line>:<column>:
  throw <throw_code> at '<token>'`. The pre-verifier stops the same way at the
  opener of a statement the source ends inside or that lacks a part it must
  have: a definition, its signature or a locals group never closed (7155; a
  `FUNCTION:` with no symbol), a definer's signature missing or never closed
  (7157; `FUNCTION:`'s declaration group), `TRUST` without the name and
  signature strings before it (7158), an `ENUM`, `STRUCTURE`,
  `BEGIN-STRUCTURE`, `PRODUCT` or `VALUE-RECORD` never ended
  (`TYPE-DECL:E-TDECL-SYNTAX`, 7107; the nominal pass refuses one in a file it
  reads first), and a `generates:` effect longer than the engine's row holds
  (7199). It stops at a `DEFLINEAR` or `VALUE-RECORD` name no type may take
  (7200) and at a `VALUE-RECORD` field the checker refuses, or the
  `END-VALUE-RECORD` of a record with none (7198); outside `--verify-only` the
  nominal pass reports those first as `E-BAD-NOMINAL-TYPE` and
  `E-BAD-RECORD-FIELD`. Its own tables grow with the source, so none of them
  stops it. Source discovery's stop at a `{:` group a file never closes is
  this record at the `{:`, with `throw_code` `E-DISC-UNTERM` (-4103).
- `E-GENERATES-ROW`: a `generates: D ( effect )` row the checker refused, its
  `token` D. Its repair class names the claim that failed: `fix_generates_row`
  when D names no word where the row stands, `delete_generates_row` when D
  already states what it makes (its `does>` clause, an earlier row, or the
  definer it wraps), and the signature refusal's own class
  (`fix_signature_type`, `fix_bare_ptr_element`, `fix_signature_arity` or
  `fix_signature_syntax`) when the effect does not parse. A load refuses the
  row with `hb: uncaught throw code 7153` and exits 67. `tools/check.f` reads
  the row before the run, so its record carries the token's place, and exits
  67 likewise (70 under `--verify-only`) or, under `--all-errors`, continues
  past it, counting it as a refusal. A row in text that `evaluate` runs is
  refused by the run alone, which has no record of its token's place, so that
  record carries none.
- `E-UNDEFINED-TOP-LEVEL`, repair class `unknown_rejection`, and
  `E-BAD-QUALIFIED-TOP-LEVEL`, repair class `fix_qualified_name`: a top-level
  token the load runs or ticks resolves nowhere, or is a malformed qualified
  name, where a body's reference to it is `E-UNDEFINED` or `E-BAD-QUALIFIED`
  under the same class. The source pre-pass asks the checker in source order,
  over the declarations before the token (`src/habu/verify-source.f`
  `TOP-TOKEN`, `checker.f` `CHECKER-VERIFY-TOP`); a number the engine's reader
  takes is no name. The refusal ends the check, as a definition's does, except
  under `--all-errors` and `--verify-only`, which count it and go on at the
  next statement the pre-pass reads. Without `--json-errors`, `--all-errors`
  writes the line `E-UNDEFINED-TOP-LEVEL habu: <file>:<line>:<column>:
  undefined word '<token>'`, or `E-BAD-QUALIFIED-TOP-LEVEL` with `malformed
  qualified name`.
- `E-UNTERMINATED-STRING`, repair class `close_string`: a string literal opened
  at `token` does not close in the checked source.
- `E-MALFORMED-REGISTRY-ROW`, repair class `close_primitive_row`: a `PRIM:` or
  `PPRIM:` primitive-axiom row opened at `token` does not close.

The lexer cannot read past either defect, so it is reported in place of
checking that source, in the file that holds it (a required file included,
standard input as `<stdin>`): source discovery stops at an open string, and the
pre-verifier at either defect where it reads one (`VERIFY:E-UNTERMINATED-STRING`,
`VERIFY:E-MALFORMED-REGISTRY-ROW`). Without `--json-errors` it is the line
`<code> <file>:<line>:<column>: string literal opened at '<token>' does not
close`, or `primitive-axiom row` in place of `string literal`. Under
`--verify-only` a string or group discovery stops at adds its record after the
packets, and its status line, `<file>: discovery rejected: unterminated string
or locals group`, is `CHECK:VERIFY-LOG$`, on stdout.

`tools/check.f` follows the subject's require closure, of any size, before it
checks anything, on every path: `--verify-only` and the language server's check
as well, and for standard input and `CHECK:SOURCE` bytes, whose requires resolve
as the run resolves them. Every pass after the walk visits the files it found,
those bytes under their label, so what a required file declares, such as the
type a structure's field names, is in scope there as it is for a named file.
Under `--json-errors` or `--verify-only` a closure
it cannot follow for any other reason is a refusal, exit 70, with one
source-span record in the file that holds the defect, a required file included,
and `CHECK:VERIFY-BYTES` answers `refused` with that record:

- `E-LOADER-FORM`, repair class `literal_loader_form`, at a loader form
  discovery cannot follow: a loader word with no literal path (a computed one,
  or a `c"` or `."` string before it), a path over 1024 bytes as written or as
  resolved, or the name of a definition that redefines or retires a loader
  word, unless `tools/dynamic-tail-manifest.f` lists the file.
- `E-MISSING-SOURCE`, repair class `fix_load_path`, at the loader word that
  names a file that is not there.
- `E-UNREADABLE-SOURCE`, repair class `make_source_readable`, at the loader
  word that names a file the file system will not read, such as one whose
  mode forbids it.

Under `--verify-only` its status line, `CHECK:VERIFY-LOG$`, is on stdout and
names the file that ended the walk: `<file>: discovery rejected: <reason>`,
`<file>: no such source` or `cannot read <file>`. Without either option the
line is `check.f: discovery rejected: <reason>`, exit 70, `check.f: no such
source`, exit 66, or `check.f: cannot read <path>`, exit 74.

A deferral record locates a stretch of top-level source the source pre-pass left
to the run, or a definition the checker left to it, and is no refusal: code
`W-CHECK-DEFERRED`, repair class `rewrite_uncheckable`, `verdict` `deferred`,
the `token` with its `file`, `line`, `column`, `byte_start` and `byte_end`, and
`suggestion`, with no `throw_code` or definition fields. The token runs a word
that may read the source after it (it parses or is deferred), or names a word
only a rendering statement before it may define, so the tokens from it to the
next statement the pre-pass reads are the run's to know, and none of them is
verified. A word that renders source opens no stretch: it reads only the text it
renders. A definition's body names a word only such a statement, or an unlearned
create caller, may define (`CHECK` verdict 2, `docs/forth.md`), and the token is
where the checker's judgment of the body stops. Only `--verify-only` reports it,
once per such stretch that holds anything but blanks and comments and once per
such definition; see Checking Without Running for the file's verdict. It counts
as no refusal and has no repair packet.

A definition by `:`, `CAST:`, `EXPORT` or a typed storage definer of a name
its wordlist already holds emits, with or without `--all-errors`, a
definition-shaped object with code `E-DUPLICATE-DEFINITION` and repair class
`rename_duplicate`, whose `file` is the source that defined the name again.
Its `word`, `token` and `definition_source` are the name as written there,
with `token_index` 0, and its `line`, `column`, `byte_start` and `byte_end`
place that name. Nothing after it is checked, and the run exits 78 as the load
does; under `--verify-only` the check goes on past it and exits 70, as for any
refused definition. Without `--json-errors` it is the line
`duplicate definition: <name> at <file>:<line>`, the line `--load` writes. When
the name taken is one a definer generates instead of writing, the `-RESERVE` or
`-RELEASE` word of a `DYNAMIC-BUFFER` or the `-BIND` or `-GROW` word of a
`DEFER-LAYOUT-BUFFER`, there is no written name to place: the record keeps the
placeholder `duplicate-definition` at line 1, column 1, and the line
`checker: duplicate definition in <file>`. A second definition by a definer the
checker only trusts, such as `variable`, `create`, `defer` or `TRUSTED:`, is
left to the run, which writes the `--load` line in every mode.

`tools/check.f` refuses, before checking anything, a single file or a
`--source-list` whose every input is a source the engine provides, since a run
loads nothing from such a source. Each input gets an input record: code
`E-ENGINE-PROVIDED`, repair class `rebuild_engine`, `verdict` `uncheckable`,
the input's `file` as given with `line` 1 and `column` 1, and `suggestion`;
without `--json-errors` it is the line `E-ENGINE-PROVIDED <file>:1:1:
<suggestion>`. The run exits 64. A list that also names a source the engine does
not provide is checked.

`tools/check.f` runs a checked program in a child, the run stage, with a
deadline of 120 s, or the milliseconds `--deadline-ms N` gives, from 1 to
2147483647. A run still going at its deadline is killed with every process it
started, what it wrote is not replayed, and the one line
`check.f: <label>: the run passed its deadline of <N> ms` goes to stderr, or to
stdout under `--json-errors`.
The label is `<stdin>`, `<source-list>`, or a named file as given, canonical
under `--json-errors` as in its packets. The run exits 70. A program that runs
longer is checked with a longer `--deadline-ms`.

A refusal of a named checker record emits a refused-record object: `schema_version`, `code`, `repair_class`, `verdict` `rejected`, `token`,
`file`, `suggestion` and the field its code adds. When the checked text
locates its named token, all four source position fields are present; otherwise
none are. It has no `throw_code` or definition-only field. The code names its repair class and that field:

| `code` | `repair_class` | Added field | Refusal |
| --- | --- | --- | --- |
| `E-TRUST-UNRESOLVED` | `fix_stale_trust_row` | none | A `trust` row names no word where its record lands; `token` is the row's name. |
| `E-PKG-CONTEXT` | `use_storage_definer` | none | A checker storage registrar was called from source, outside the engine's verifier window; `token` is the name it would have recorded. |
| `E-BAD-QUALIFIED-RECORD` | `fix_qualified_name` | none | A checker record was asked for a malformed qualified name, which keys no word; `token` is that name. A call to such a name is refused in its definition as `E-BAD-QUALIFIED`, under the same class with a definition's fields. |
| `E-BAD-STORED-SIGNATURE` | `fix_signature_type`, `fix_bare_ptr_element`, `fix_signature_arity` or `fix_signature_syntax`, as for a definition's signature; `fix_signature_size` for a row too deep or too wide to record | `signature`, as written, empty when the row stored no text; and `reason` for a row too deep or too wide to record, naming the bound with the row's count and the limit | A stored signature, a `trust` row's or a `TRUSTED:` definition's, does not parse, or is more than 4096 levels deep or takes more than 255 cells, more than its record holds; `token` is the name it is stored for. |
| `E-SHADOWED-ARITY` | `match_shadowed_private_effect` | `package` | The package public `token` moves another number of cells than the private word of its package with the same tail. |

A named checker record refuses before its load can continue. If the run
reaches one, its refusal throws its code past every handler, so the
load ends on `hb: uncaught throw code <code>` and exits 70 for a refusal the
checker rendered ([debugging.md](debugging.md)). A malformed name in a
definition or declaration (`defer P:Q:R ( -- )`) is refused by the engine itself
under `--load` with exit 75. The source pre-pass can ask the checker for its
record and report the statement that asked for it as one that threw. A record
entry called at run time with that name (`s" P:Q:R" CHECKER-DEFER`) throws
`E-BAD-QUALIFIED` (7152); the load exits 70 and no later statement runs.

`E-BAD-STORED-SIGNATURE` is rendered in every mode. Under `--all-errors`, a
source `trust` row is counted and checking continues, even if it names the
definition just checked. Otherwise the load throws 7156 where the signature
was stored; the pre-pass places a following `E-STATEMENT-THROW` span when it
meets that statement. A row reached only at run time, such as through
`evaluate`, also exits 70. Its prose is `habu: in <token>: bad stored
signature '<signature>'`, with a reason for a row too deep or too wide to
record. Named definition and using refusals place their own token when the
checked text locates it, without a following statement-throw packet.
`tools/check.f` exits with the run's status.

`W-EFFECT-NOT-RECORDED` is a warning, outside this contract: a definition with
no declared signature certified, but its inferred effect has more than 23 type
variables or binders, or a type the checker does not model, so no effect is
recorded for it and a later call to it is refused as undefined. The definition
loads and the run's status stands. The object carries `schema_version`, `code`,
`word`, `file`, optional source positions and `reason`, and no `verdict`,
`repair_class` or `suggestion`.

Two packets name a definition, not a token of its body, and are placed at the
definition's name as written: where the checked text locates in the labeled
file, `line`, `column`, `byte_start` and `byte_end` follow `file`, with the
meanings above. Elsewhere they carry `file` alone, since a definition the
engine's own load captured keeps no source address for its name and one built
by `evaluate` or generated has no bytes in the file.

- `E-SHADOWED-ARITY`, repair class `match_shadowed_private_effect`, `verdict`
  `rejected`: a package's public definition moves other cells than the private
  word of that package whose tail it shares (forth.md **Packages**). It
  carries `token`, the tail as the checker folds it, `package`, `file` and
  `suggestion`, and refuses its definition as the using refusals below do.
- `W-EFFECT-NOT-RECORDED`, with no `repair_class` or `verdict`: a definition
  without a declared effect certified, but its inferred effect cannot be
  recorded, so a later caller does not find it. It carries `word`, the name as
  the checker folds it, `file` and `reason`, and the check goes on.
  `tools/check.f` writes it once in every mode, from its check before the run:
  the run loads what that check checked and writes no warning (`WARN-DIAGS`,
  `src/core/render.f`), so a definition only the run builds, with `evaluate`,
  gets none.

The checker refuses a bare token in a definition or at top level that resolves
in a used package and somewhere else as well, at the token, with a using
object. Each code
names its repair class: `E-USING-SHADOW-GLOBAL` / `disambiguate_using_shadow`
is a token a global and a used package's public both export, and
`E-USING-AMBIGUOUS` / `disambiguate_using_ambiguous` one the publics of more
than one used package export. The object carries `schema_version`, `code`,
`repair_class`, `verdict` `rejected`, `token` as written, `file`, the token's
`line`, `column`, `byte_start` and `byte_end` when it locates in the file,
`used_packages` and `suggestion`, and no `throw_code` or definition-only field.
`used_packages` names each used package the token resolves in, folded, once
each, in the order `using` searches them: one for the shadow, two or more for
the ambiguity. Under `--all-errors` without `--json-errors` it is a line that
begins with the code and names the token and each candidate, `PKG:TOK` for a
package's. The refusal refuses its definition, a definer when the token is in
its `does>` clause, as an `E-UNDEFINED` does, and `tools/check.f` exits 70.
Without `--all-errors` or `--verify-only` the check stops there. Under either,
which report every refused definition, the definition keeps its declared effect
for later callers and the check goes on and reports every later refusal; the
refusal is its definition's report, not an `E-STATEMENT-THROW` record or a
`verification stopped by throw` line. `bin/hb --load` stops at the shadow, rc
70, as at `E-SHADOWED-ARITY`. The engine refuses an ambiguous use earlier,
before the checker, at top level and under `bin/hb --load` in a definition
too, and exits 94 (`docs/forth.md`, Packages). A top-level token's refusal
refuses the token by the same rule: `tools/check.f` exits 70 on every path and
stops there or goes on as above, where `bin/hb --load` exits 105 at the shadow
(`ENGINE-ERROR:USING-SHADOW-GLOBAL`) and 94 at the ambiguity. The refused
token runs no word, so one naming a renderer such as `evaluate` leaves no
later name to the run (`W-CHECK-DEFERRED`).

## Checking Without Running

`tools/check.f --verify-only FILE` reports what `bin/hb --load FILE` would
refuse of FILE's definitions and top-level tokens and runs none of FILE or its
closure: no lint, no run stage. `--verify-only --stdin-path PATH` checks stdin's bytes as the file
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
- The child runs, in the caller's working directory, on the engine
  `lib/engine-candidate.f` names (`HABU_UNDER_TEST` if set, else the running
  engine), as `check.f`'s run stage does, and loads
  `ROOT/tools/check-verify-child.f` by its absolute path, ROOT being the tree
  `tools/check-verify-core.f` was loaded from.
- The child has the run stage's deadline, `--deadline-ms` included.

| `CHECK:verdict` | Meaning | check.f exit |
| --- | --- | --- |
| `verified` | Nothing in the closure or the subject is refused. | 0 |
| `refused` | The subject, a file of its closure, or the closure itself is refused. | 70 |
| `engine-provided` | The engine provides PATH (`ENGINE-PROVIDES?`): nothing is verified, whatever the bytes hold. | 64 |
| `held` | The child's image holds PATH though the engine does not (`src/habu/verify-source.f`, `tools/check-verify-child.f`), so it cannot be verified there. | 69 |
| `incomplete` | The child ended without a result line; its `status` is the exit, signal or deadline, and the packets are those it made before. | 69 |
| `deferred` | Nothing is refused, but the source pre-pass left a stretch of top-level source or a definition in the closure or the subject to the run: a `W-CHECK-DEFERRED` packet locates each, and its tokens are not verified. | 0 |

Any refusal makes the verdict `refused`, deferrals or not: the load fails at
the refusal whatever the run would make of what was deferred. Without one, a
deferred stretch or definition makes it `deferred`, which exits 0 because the
load may accept it, and whose packets and closing line keep a caller from
reading that 0 as `verified`; the language server publishes those packets at
Information severity. Only this child reports a deferral: check.f's default
mode runs the program after its pre-pass, so the run judges what the pre-pass
left to it.

Under `--verify-only` check.f writes the packets on stderr, as schema-1 JSON
with or without `--json-errors`, and its prose on stdout, with a closing line
for `engine-provided`, `held`, `incomplete` and `deferred`, for which it is
`check.f: a stretch or definition deferred to the run is not verified`. A
verification that stopped, at a word with no name after it, a string or
primitive-axiom row its file never closes, a statement that threw, or where
discovery stopped, adds the record the other modes write for it, at that place,
after the packets made before it. Child output beyond the
operation's capture exits 69 with the complete packets received before it, the
prose and a closing line. Usage errors (64), a missing FILE (66) and a FILE the
file system will not read (74, `check.f: cannot read <path>` with its canonical
path, as the closure walk says it) keep their exit codes and explain the failure
on stdout. A source of any size is checked: check.f reads a file to its end, its
size only the first room, and holds the source, and the text it builds from it,
in buffers that grow to what they hold; the engine loads it in a frame sized to
the file.
An argument that exceeds the source path capacity is a usage error, exits 64
and explains the limit on stdout. A check with neither `--verify-only` nor
`--json-errors` explains it on stderr with the same status. Under
`--json-errors`, with or without `--all-errors`, stderr likewise carries the
packets alone and the prose goes to stdout: the closing lines, the run's
deadline line, usage errors, a missing or unreadable FILE and, when the
run stage writes no packet on stderr, what it writes there, whether the run
passed or failed. The engine reports a top-level
refusal of the run, such as `hb: interpret stack underdepth: <word>` (70) or
`hb: uncaught throw code <n>` (67), as that prose, with no packet. An engine
selection `lib/engine-candidate.f` refuses (`HABU_UNDER_TEST`, else the running
engine) exits 67 in every mode, whether its path names no executable or is one
the file system refuses, such as one over 1024 bytes. Under `--json-errors`
stdout says `check.f: the selected engine (HABU_UNDER_TEST, else the running
engine) is not a usable executable` and stderr is empty; otherwise, by default
and with `--verify-only` alone, stderr holds only `hb: uncaught throw code <n>`,
the resolver's code (`E-FS-OPEN` -2102, `E-FS-PATH` -2100), naming neither the
engine nor `HABU_UNDER_TEST`. The selection is judged where the check starts a
child, the verifier or the run, so a check that ends before either, such as one
whose FILE cannot be read, keeps its own outcome whatever the selection is.
Any other throw that reaches check.f's command line uncaught, such as a failure
of its scratch directory under `HB_TMP` (`E-FS-IO` -2105 making it, `E-FS-OPEN`
-2102 writing the source into it), exits 67 too: under `--json-errors` stdout
says `check.f: uncaught throw code <n>`, with no packet, and stderr is empty;
otherwise stderr holds `hb: uncaught throw code <n>`.
With `--verify-only`, a source list, a FILE beside `--stdin-path` and stdin
without it are usage errors; so is `--stdin-path` given twice or without
`--verify-only`.

`CHECK:VERIFY-BYTES ( ptr u8 n ptr u8 n ms -- CHECK:verdict )` takes the bytes,
PATH, a relative one read from the working directory, and the child's
deadline. `CHECK:VERIFY-OUT$` holds the packets, one JSON object per line, and
`CHECK:VERIFY-LOG$` the prose, until the next call. A duplicate definition,
for which the checker writes no packet, is the record `--all-errors` writes
for it (`CHECK-ALL-ERRORS:DUP-RECORD$`), placed in the bytes or in the file of
the closure that defined the name again. A throw that ended the verification
refuses it: `CHECK:VERIFY-STOP` is its code, 0 for none, and
`CHECK:VERIFY-STOP-AT`, `CHECK:VERIFY-STOP-SUBJECT?` and
`CHECK:VERIFY-STOPPED$` say where, as for `CHECK:PREVERIFY-BYTES`. Discovery's
stop at a string or a `{:` group a file of the closure never closes refuses it
too, with `E-DISC-UNTERM` at the opener in that file. Either stop's record, the
one `check.f --verify-only` writes, is the last line of `CHECK:VERIFY-OUT$`:
for a definer or parsing word with nothing after it the nominal pass's
`E-MISSING-NAME` packet, for an open string or row the lexer's record, else
the record `--all-errors` writes for a statement that throws, at the stop, so
a language server publishes it with the packets before it. Any other closure
the walk cannot follow is its record (above), and the verdict is `refused`. An
empty PATH throws `E-FS-PATH`, and a failed spawn throws as well.
Child output beyond the capture, 4 MiB on stdout or 256 KiB on stderr, kills
the child and throws `E-PROC-TRUNCATED`, with every complete packet received
before it in `CHECK:VERIFY-OUT$` and the stderr received in
`CHECK:VERIFY-LOG$`.
`tools/check-verify-test.f` prints what one check costs: about 50 ms for a
one-definition file and 230 ms for `tools/check-core.f`, whose closure is over
thirty files.

The child, `tools/check-verify-child.f`, is run only by this operation and by
`CHECK:PREVERIFY-BYTES`, check.f's pre-pass:

```text
ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT < BYTES
ENGINE --load ROOT/tools/check-verify-child.f -- SUBJECT LABEL < BYTES
```

SUBJECT is canonical and absolute. The child follows the loader forms of
BYTES and of the files they load; a file the image holds is skipped, as
`require` skips it. stdout carries the packets in verification order, each
written as the checker makes it, so a child that dies has passed on every
packet made before; then one result line. The first form verifies with all
errors, going past a duplicate definition; for each it writes the second
form's stopped line, code 78, among the packets, which the operation replaces
by the duplicate's record. It answers `check-verify: verified`, `refused`,
`deferred` or `held`, or the second form's `stopped` line for a throw that
ended it; stderr
carries prose, including `PATH: verification stopped by throw RC after N
rejected definitions` for each file a throw stopped. The second form stops at
the first refused definition or top-level token, as the load does, names the
subject LABEL in its packets and answers `check-verify: verified` or `check-verify: stopped RC BYTE
DUP-AT DUP-LEN IN-SUBJECT FILE`: the code it stopped with, where the token it
stopped at starts (the one it read last, or the opener of the statement it was
in), where the name it refused as a duplicate starts and its length (0 when it
kept none), 1 when the stop is in BYTES, and the file it is in, SUBJECT or
LABEL for BYTES. The answer is
read from the result line after a clean exit, never from the exit status.

Because check.f's default pre-pass runs in this child, it resolves the
engine's words and those the subject loads, the words its run has. A word only
the checking process loaded, such as `lib/fs.f`'s `FILE-SIZE` under check.f or
`lib/test.f`'s `T=` in a harness that checks in its own process, is
`E-UNDEFINED` to it, `E-UNDEFINED-TOP-LEVEL` at top level, and check.f reports
it there in every mode. The pre-pass
child has the run stage's deadline, `--deadline-ms` included; one that ends
without its result line fails the check with 69, as `incomplete` fails
`--verify-only`, with its closing line on stderr, or on stdout under
`--json-errors`.

## Repair Packet JSON

Repair packets are the LLM-facing object passed back after a checker rejection.
They preserve the evidence present in the source diagnostic without inventing
fields that its shape cannot supply. `tools/repair-packet.f` builds one packet
from the first refusal, in the shape that refusal's record has, and counts only
refusals: a deferral or a warning has no packet. Schema 1 has definition, declaration,
storage, span, input, refused-record and using packet shapes.

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
| `word` | string | required | The declared name as written; the definer when it has none (as written under `tools/check.f`, uppercase under `bin/hb --load`). |
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
| `code` | string | required | `E-STATEMENT-THROW`, `E-UNTERMINATED-STRING`, `E-MISSING-SOURCE`, `E-UNREADABLE-SOURCE`, `E-LOADER-FORM`, `E-MALFORMED-REGISTRY-ROW`, `E-UNDEFINED-TOP-LEVEL` or `E-BAD-QUALIFIED-TOP-LEVEL`. |
| `throw_code` | integer or null | required | The code a statement threw; null for any other span. |
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

Refused-record packets carry the object's evidence, including all four source
position fields when its named token locates in the checked text:

| Field | Type | Presence | Meaning |
| --- | --- | --- | --- |
| `schema_version` | integer | required | Repair packet schema version, currently `1`. |
| `kind` | string | required | Must be `habu_repair_packet`. |
| `token` | string | required | The record's token, as its code describes it. |
| `signature` or `package` | string | the field its code adds | Copied from the record. |
| `file` | string | required | Source label or path. |
| `line`, `column`, `byte_start`, `byte_end` | integer | all four or none | Copied source positions when known. |
| `code` | string | required | A refused-record code or `E-GENERATES-ROW`. |
| `repair_class` | string | required | One its code names. |
| `suggestion` | string | required | Checker repair hint. |
| `diagnostic_count` | integer | required | Number of diagnostics represented by the packet. |
| `instruction` | string | required | `Fix the statement that names this token so it loads. Output only corrected Habu code.` |

Using packets carry a using record's evidence:

| Field | Type | Presence | Meaning |
| --- | --- | --- | --- |
| `schema_version` | integer | required | Repair packet schema version, currently `1`. |
| `kind` | string | required | Must be `habu_repair_packet`. |
| `token` | string | required | The bare token as written. |
| `file` | string | required | Source label or path. |
| `line` | integer | with a place | One-based source line. |
| `column` | integer | with a place | One-based source column. |
| `byte_start` | integer | with a place | Token start byte. |
| `byte_end` | integer | with a place | Token end byte. |
| `used_packages` | array of strings | required | Each used package the token resolves in. |
| `code` | string | required | `E-USING-SHADOW-GLOBAL` or `E-USING-AMBIGUOUS`. |
| `repair_class` | string | required | `disambiguate_using_shadow` or `disambiguate_using_ambiguous`, the one the code names. |
| `suggestion` | string | required | Checker repair hint. |
| `diagnostic_count` | integer | required | Number of diagnostics represented by the packet. |
| `instruction` | string | required | `Qualify this token as PKG:WORD for the package word meant, or rename the collision. Output only corrected Habu code.` |

The packet copies the record's four place fields when it has them and none when
it has none.

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
- `fix_qualified_name`: a call, or a record of a definition or declaration, used
  a malformed qualified name with more than one `:`; use a single `:` qualifier
  such as `PKG:WORD`.
- `fix_signature_syntax`: the stack-effect comment is malformed or incomplete.
- `fix_signature_type`: the stack-effect comment names an unknown multi-character
  type; use a known nominal type or a single-letter type variable.
- `fix_signature_arity`: a registered type family was applied to the wrong number
  of arguments; give it its exact declared arity.
- `fix_bare_ptr_element`: a signature named `ptr` with no element type; give it an
  element type, e.g. `ptr u8` or `ptr a`.
- `fix_generates_row`: a `generates: D ( effect )` row's D names no word where
  the row stands; write the row after D's definition, spelled as it spells D.
- `delete_generates_row`: a `generates: D ( effect )` row's D already states
  what it makes: its `does>` clause, an earlier row, or the definer it wraps.
  Delete the row.
- `fix_signature_size`: a stored signature is more than 4096 levels deep or takes
  more than 255 cells, more than its record holds; `reason` names the bound, the
  row's count and the limit. Keep bulk values in a buffer.
- `fix_nominal_type`: a `deftype` declaration used a reserved, duplicate, or
  syntactically invalid nominal type name.
- `fix_missing_name`: a definer (`:`, `DEFTYPE`, `package`, `create`, a
  learned `create … does>` definer and the rest) or a parsing word (`char`,
  `'`, a field word) ended the source with no name after it
  (`E-MISSING-NAME`); the packet is located at that word.
- `fix_record_field`: a `VALUE-RECORD` field had a bad or duplicate name, a
  missing or unknown type or one with a scoped dependency, or the record had
  no field (`E-BAD-RECORD-FIELD`);
  `reason` carries the registration's refusal and the packet is located at the
  field (at `END-VALUE-RECORD` for a record with no field).
- `fix_family_declaration`: a `NEWTYPE` or `SUMTYPE` declaration used a
  reserved, non-lowercase, or duplicate family/variant name, a bad arity token,
  an unknown payload type, or a malformed/unterminated `VARIANT` block.
- `fix_storage_type`: a storage declaration (`LAYOUT-BUFFER`,
  `DEFER-LAYOUT-BUFFER`, `TYPED-BUFFER`, `TYPED-VARIABLE`, `DYNAMIC-BUFFER`)
  names an unknown or malformed type, one its definer cannot store, or no type
  on the name's line; declare the type before the storage or store a type the
  definer admits.
- `fix_storage_name`: a storage declaration has no name on its definer's line,
  or its name has more than one `:` or lies in a sealed package.
- `fix_storage_count`: a storage declaration's literal count is outside the
  buffer's extent, its count token resolves to no `( -- n )` word, or the
  declaration has no count.
- `rename_duplicate`: a name was defined a second time in one wordlist; rename
  it or `undefine` the first definition.
- `close_string`: a string literal does not close.
- `fix_load_path`: a loader word names a file that is not there.
- `make_source_readable`: a loader word names a file the file system will not
  read.
- `literal_loader_form`: a loader form names no literal path, or redefines or
  retires a loader word, so the require closure cannot be read from the source.
- `close_primitive_row`: a `PRIM:` or `PPRIM:` primitive-axiom row does not
  close.
- `rebuild_engine`: the input is a source the engine provides, so loading it
  checks nothing; rebuild `bin/hb` to check a change to it.
- `fix_stale_trust_row`: a `trust` row names no word in the wordlist its record
  lands in; delete the row, correct the name, or write the row in the section
  that defines the word.
- `use_storage_definer`: a checker storage registrar was called from source,
  outside the engine's verifier window; define the storage with its definer.
- `disambiguate_using_shadow`: a bare token resolves to a global while a used
  package's public exports it too; qualify the package word as `PKG:WORD`, or
  rename the collision to reach the global.
- `disambiguate_using_ambiguous`: a bare token resolves in the publics of more
  than one used package; qualify the one meant as `PKG:WORD`, or rename the
  collision.
- `match_shadowed_private_effect`: a package public moves another number of
  cells than the private word of its package that owns the same tail; give the
  public definition the private word's effect, or rename one of the two.
- `rewrite_uncheckable`: the checker could not model the word; rewrite with
  modeled words or use an audited boundary only when the primitive is intended.
- `unknown_rejection`: rejection did not fit a more specific class.

The checker `suggestion` field is stable short text; for each class in this
table it is derived only from `repair_class`. It does not replace the raw
`expected`, `actual`, or `return_stack` evidence:

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
| `fix_signature_size` | `Declare fewer cells: keep bulk values in a buffer, not on the stack.` |
| `fix_nominal_type` | `Choose a unique non-reserved nominal type name.` |
| `fix_missing_name` | `Give the definer a name: the next whitespace-delimited token.` |
| `fix_record_field` | `Declare at least one field, each with a unique name and a known type.` |
| `fix_family_declaration` | `Repair the family declaration: unique lowercase names, exact arity, closed VARIANT blocks.` |
| `fix_storage_type` | `Declare the type before the storage, or store a closed, copyable type this definer admits.` |
| `fix_storage_name` | `Name the storage with at most one inner ':', outside a sealed system package.` |
| `fix_storage_count` | `Put a positive count before the definer whose cells fit in memory: a literal, a constant or an expression.` |
| `rename_duplicate` | `Rename the word or undefine the old definition before redefining it.` |
| `close_string` | `Close the string literal before the definition ends.` |
| `fix_load_path` | `No file is at the path this loader word names. Correct the path, or create the file.` |
| `make_source_readable` | `The file this loader word names cannot be read. Make it readable, or correct the path.` |
| `literal_loader_form` | `Load a file by a literal path of at most 1024 bytes, as written and as resolved, through a loader word no definition redefines or retires, or list this file in tools/dynamic-tail-manifest.f.` |
| `close_primitive_row` | `Close the primitive-axiom row opened at this token: a bare row reads PRIM: name effect... PRIM;, and a package row reads PPRIM: package name effect... PPRIM; or CLOSE-PRIVATE.` |
| `fix_generates_row` | `This generates: row names no word here. Write it after the definer's definition, spelled as the definition spells it.` |
| `delete_generates_row` | `This definer already states what it makes: its does> clause, an earlier generates: row or the definer it wraps. Delete the row.` |
| `rebuild_engine` | `The engine provides this source; rebuild bin/hb to check a change to it.` |
| `fix_stale_trust_row` | `This trust row names no word in the wordlist its record lands in: the open section's, the global wordlist outside a package, or PKG's public wordlist for PKG:TAIL. Delete the row if the word is gone, correct the spelling, or write the row in the section that defines the word.` |
| `use_storage_definer` | `A checker storage registrar records a definer's accessor only inside the engine's verifier window. Define the storage with its definer (TYPED-VARIABLE, TYPED-BUFFER, LAYOUT-BUFFER, DYNAMIC-BUFFER) instead of calling the registrar.` |
| `disambiguate_using_shadow` | `A global word and a used package public share this name. Qualify the package word as PKG:WORD, or rename the collision; the global has no bare qualifier.` |
| `disambiguate_using_ambiguous` | `Used publics of more than one package share this name. Qualify the one meant as PKG:WORD, or rename the collision.` |
| `match_shadowed_private_effect` | `A private word of this package owns the same tail, and a bare tail binds the private word first, so the native compiler reads this definition's arity from it. Give the public definition the private word's effect, or rename one of the two.` |
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
