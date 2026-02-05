# ANSI Functional Parity Signoff

## Goal

Provide an objective go/no-go record for claiming functional parity against the pinned `ansi-test` corpus.

## Inputs

- Corpus pin: `docs/ansi-parity-baseline.json`
- Latest SBCL results: `docs/ansi/results/sbcl-latest.json`
- Latest Habu results: `docs/ansi/results/habu-latest.json`
- Failure map: `docs/ansi-failure-map.md`
- CI workflow: `.github/workflows/ansi-conformance.yml`

## Required Artifacts

1. Baseline metadata (`docs/ansi-parity-baseline.json`) with pinned corpus revision.
2. Normalized latest run outputs for both runtimes (`sbcl-latest.json`, `habu-latest.json`).
3. Raw logs for both runtimes under `docs/ansi/raw/`.
4. Regression check output proving no new failures vs baseline.
5. Green CI evidence for:
   - PR smoke gate
   - Nightly full conformance gate

## Signoff Checklist

- [ ] Corpus revision is pinned and matches baseline metadata.
- [ ] Latest normalized results exist for both runtimes.
- [ ] `tools/ansi/check_regression.py` reports no regressions.
- [ ] No open `fix-*` parity dots remain in `dot tree habu-reach-ansi-functional-32946029`.
- [ ] PR smoke gate is green.
- [ ] Nightly full gate is green.
- [ ] Exception count is zero (no waived failures).

## Decision Record

- Signoff date (UTC): `TBD`
- Corpus revision: `TBD`
- SBCL fail count: `TBD`
- Habu fail count: `TBD`
- Regression status: `TBD`
- CI smoke status: `TBD`
- CI nightly status: `TBD`
- Decision: `NOT READY`

## Commands

```bash
# Produce latest normalized results
tools/ansi/run.sh sbcl --input /tmp/habu-ansi/ansi-test/doit.lsp --tag latest
tools/ansi/run.sh habu --input /tmp/habu-ansi/ansi-test/doit.lsp --tag latest

# Parse logs (if run.sh did not already normalize)
tools/ansi/parse_results.py docs/ansi/raw/sbcl-latest.log
tools/ansi/parse_results.py docs/ansi/raw/habu-latest.log

# Compare latest against baseline
python3 tools/ansi/check_regression.py \
  --baseline docs/ansi-parity-baseline.json \
  --sbcl docs/ansi/results/sbcl-latest.json \
  --habu docs/ansi/results/habu-latest.json

# Verify remaining parity work
dot tree habu-reach-ansi-functional-32946029
dot ready
```
