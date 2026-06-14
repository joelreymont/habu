# Evaluation Against Habu LLM Recommendations

Date: 2026-06-14

Source: `/tmp/codex-remote-attachments/019ec27a-ead9-74b2-b0bd-754dd2e03df9/327B0850-3A1C-41B9-B230-672124ADA4E8/1-habu_llm_recommendations.md`

## Verdict

The recommendation document is directionally right, but it is now partly stale.
The current repo has already implemented most of the suggested LLM-facing
infrastructure:

- `LLM.md` exists and gives a coding-agent operating protocol.
- `STATUS.md` is the canonical self-check status source.
- `TRUSTED.md` exists and is enforced by `tools/trust-lint.py`.
- `tools/stale-status-lint.py` prevents duplicated status-count drift.
- Native diagnostics support `JSON-DIAGS ON`.
- `examples/llm/` exists.
- `bench/llm/` exists and is run by `tools/oracle.sh`.
- Default `hb-build.sh` now verifies user definitions through `CHECK!` and fails
  closed on checker rejection.

Habu is therefore already past the baseline described by the recommendation file.
The remaining high-value work is not broad new architecture; it is sharpening
diagnostics and turning the benchmark harness into measured multi-model data.

## Recommendation Status

| Recommendation | Status | Evidence | Remaining Gap |
|---|---|---|---|
| Add `LLM.md` | Done | `LLM.md` covers read-first protocol, checked words, `CHECK!`, body-not-signature repair, `TRUST`, tests, gate, and scope. | None found. |
| Add structured checker errors | Partial | `src/core/render.f` implements `JSON-DIAGS ON`; `test/t-sh-jdiag.fs` pins JSON output with `code`, `word`, and `suggestion`. | JSON lacks declared effect, inferred effect, token index, return-stack state, and verdict classification. There is no CLI flag such as `bin/habu --json-errors`. |
| Separate `CHECK` vs `CHECK!` in docs | Done | `README.md`, `STATUS.md`, and `LLM.md` explicitly distinguish infer mode from verify mode and tell LLM-authored definitions to use declared signatures plus `CHECK!`. | None found. |
| Make trust auditable | Done | `TRUSTED.md` exists; `tools/trust-lint.py` requires every `TRUST` site in `src/` to have a manifest row and tests. Current lint reports `17 TRUST site(s), 17 manifest row(s), 0 finding(s)`. | The lint does not yet compare effect-string drift against manifest rows, but the manifest/test requirement is enforced. |
| Add LLM benchmark suite | Partial | `bench/llm/tasks.md`, `solutions.f`, `validate.fs`, and `run.sh` exist. `tools/oracle.sh` runs it. Current answer key validates `23/23` certified, `0` rejected. | It is a reference validation harness, not yet a measured benchmark run across models. The recommendation asks for 30-50 tasks and recorded metrics such as repair iterations/tokens/signature weakening. |
| Add stale-doc detection around status numbers | Done | `STATUS.md` is the single count source; `tools/stale-status-lint.py` is in the native gate and reports `0 finding(s)`. | Date freshness is not linted; `STATUS.md` was updated here to `2026-06-14`. |
| Add "do not fix by weakening the type" rule | Done | `LLM.md` section 4 says to fix the body, not the signature; JSON diagnostic suggestions also say not to weaken the signature for type mismatches. | None found. |
| Add examples for model prompting | Done | `examples/llm/good.f` and `examples/llm/bad.f` cover accepted and rejected idioms with reasons. | Could add the JSON diagnostic expected output beside each bad example, but the base examples exist. |
| Keep language small and opinionated | Aligned | Current docs and gates keep the target narrow: macOS arm64 native engine, checked Forth subset, small examples, strict style. | Portability remains intentionally narrow. |
| Make checker loop the product | Mostly done | `README.md`, `LLM.md`, `STATUS.md`, `bench/llm/PROTOCOL.md`, and JSON diagnostics all center the write-check-repair loop. | Needs measured LLM runs and richer diagnostics to fully close the loop. |

## Remaining Priorities

1. **Complete JSON diagnostics.** Add declared effect, inferred effect, token
   index, return-stack state when relevant, and verdict (`rejected` vs
   `uncheckable`). Add a CLI/user-facing switch rather than only `JSON-DIAGS ON`.
2. **Turn `bench/llm` into data.** Expand toward 30-50 tasks and record actual
   model runs: first-pass checker success, test success, repair iterations,
   token use, final size, `TRUST` use, and signature weakening.
3. **Tighten trust drift checks.** Extend `tools/trust-lint.py` to compare code
   effect strings against `TRUSTED.md`, not only presence/test cells.
4. **Keep status dates fresh.** Counts are linted; verification date is not.

## Verification Performed

- `./tools/stale-status-lint.py`
- `./tools/trust-lint.py`
- `./bench/llm/run.sh`

Relevant previously-run gates for the current committed stack:

- `./test/run.sh`
- `./tools/oracle.sh`
- `HB_TMP=$(mktemp -d) ./tools/bootstrap.sh`
