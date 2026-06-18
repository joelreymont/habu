# Repair Diagnostics Schema

This is the stable machine contract for Habu checker repair feedback. The
implemented surface today is one JSON object per failed top-level definition from
`tools/check.sh --json-errors --all-errors`. A repair packet is the normalized
LLM prompt object built from those checker diagnostics.

## Checker Diagnostic JSON

Checker diagnostics are newline-delimited JSON objects with
`schema_version: 1`. They are emitted on stderr and remain valid JSON object
lines even when the checker rejects the input.

Fields:

| Field | Type | Presence | Meaning |
| --- | --- | --- | --- |
| `schema_version` | integer | required | Current checker diagnostic schema version. |
| `code` | string | required | Stable error code such as `E-MISMATCH`, `E-REJECTED`, `E-UNSAFE`, `E-BAD-SIGNATURE`, or `E-UNCHECKABLE`. |
| `repair_class` | string | required | Stable repair bucket used by LLM repair loops. |
| `verdict` | string | required | `rejected` or `uncheckable`; certification is not emitted as a diagnostic. |
| `word` | string | required | Failing definition name as seen by the checker. |
| `token` | string | required | Token that anchored the diagnostic. |
| `token_index` | integer | required | Zero-based token index within the captured definition body. |
| `file` | string | required | Wrapper label or source path attached to the diagnostic. |
| `line` | integer | required | One-based source line for the token. |
| `column` | integer | required | One-based source column for the token. |
| `byte_start` | integer | required | Zero-based byte offset of the token start in the labeled source. |
| `byte_end` | integer | required | Zero-based byte offset immediately after the token. |
| `definition_source` | string | required | Captured definition text without the leading colon and trailing semicolon. |
| `declared_effect` | string | required for signed definitions | Declared data and return-stack effect, normalized by the checker. |
| `inferred_effect` | string | required | Inferred data and return-stack effect at the diagnostic point. |
| `return_stack` | object | required | Object with `expected` and `actual` return-stack rows. |
| `expected` | string | data mismatch only | Expected data-stack row. Absent when only the return stack or safety verdict failed. |
| `actual` | string | data mismatch only | Actual data-stack row. Absent when only the return stack or safety verdict failed. |
| `suggestion` | string | required | Human-readable repair hint derived from `repair_class`. |

The current checker JSON intentionally uses `definition_source` rather than
`source_excerpt`, and `suggestion` rather than `reason`. Packet builders must
copy or normalize these fields instead of requiring the checker to emit aliases.

## Repair Packet JSON

Repair packets are the LLM-facing object passed back after a checker rejection.
They are derived from checker diagnostics and must preserve the same token,
span, stack, code, class, and source evidence. Packet builders may include
additional prompt-oriented text, but must not remove the stable fields below.

Fields:

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
| `inferred_effect` | string | required | Inferred effect copied from the checker. |
| `expected` | string or null | required | Expected data-stack row, or null when the checker did not emit a data-stack mismatch. |
| `actual` | string or null | required | Actual data-stack row, or null when the checker did not emit a data-stack mismatch. |
| `return_stack` | object | required | Object with `expected` and `actual` return-stack rows. |
| `code` | string | required | Stable checker error code. |
| `repair_class` | string | required | Stable repair bucket. |
| `reason` | string | nullable | Short packet-level explanation. Current checker JSON does not emit this field. |
| `suggestion` | string | required | Checker repair hint. |
| `source_excerpt` | string | required | Packet alias for checker `definition_source`. |
| `diagnostic_count` | integer | required | Number of diagnostics represented by the packet. |

When a packet aggregates multiple diagnostics, it must preserve deterministic
ordering from `--all-errors` and either include one packet per diagnostic or a
top-level array whose items each carry the fields above.

## Repair Classes

Current checker classes:

- `remove_producer`: the body leaves more data-stack values than declared.
- `add_producer`: the body leaves fewer data-stack values than declared.
- `fix_type`: data-stack arity matches, but one or more types differ.
- `fix_return_stack`: return-stack row differs from the declaration.
- `trusted_boundary_required`: checked code used a compiler or runtime boundary
  that requires audited `TRUST` or a modeled rewrite.
- `fix_signature_syntax`: the stack-effect comment is malformed or incomplete.
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
| `trusted_boundary_required` | `Move this compiler or runtime boundary behind audited TRUST.` |
| `fix_signature_syntax` | `Repair the stack-effect comment syntax, including --.` |
| `rewrite_uncheckable` | `Rewrite with modeled words or isolate an audited primitive.` |
| `unknown_rejection` | `Inspect the token, signature, and raw stack evidence.` |

## Benchmark Result Fields

Live benchmark JSONL rows use `schema_version: 2`. The native validator requires
identity fields `run_id`, `model_id`, `arm`, `task_id`, and `trial_id`, plus
`task_family`, `model`, `model_version`, `model_date`, trial/order metadata,
outcome/repair fields, token and wall-time fields, `source_chars`, runtime
fields, and replay artifacts. Unknown model version/date are represented by the
stable nonempty string `unknown`.

Replay fields are `prompt`, `raw_response`, `extracted_candidate`,
`checker_diagnostics`, `repair_packet`, `test_output`, and `final_bundle`; every
one must have a paired `*_sha256` field.

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
| `repair_class_stats` | array | optional when no diagnostics | Per-class diagnostic counts and repair success accounting. |

Benchmark validators must fail rows that claim diagnostic quality without the
corresponding evidence. Reports must keep diagnostic quality, repair success,
repair rounds, wall time, and generated-token cost as separate axes.
