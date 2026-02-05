# ANSI Functional Parity

## Corpus Source (Pinned)

- Upstream: `https://gitlab.common-lisp.net/ansi-test/ansi-test.git`
- Pinned commit: `a1107c9564833680c72946f1cd87c9c3bbe0de5a`
- Pin date: `2026-02-05`

## Reproducible Fetch

```bash
mkdir -p /tmp/habu-ansi
cd /tmp/habu-ansi
git clone https://gitlab.common-lisp.net/ansi-test/ansi-test.git
cd ansi-test
git checkout a1107c9564833680c72946f1cd87c9c3bbe0de5a
git rev-parse HEAD
```

Expected `git rev-parse HEAD`:

```text
a1107c9564833680c72946f1cd87c9c3bbe0de5a
```

## Artifact Conventions

- Raw run logs: `docs/ansi/raw/<runtime>-<timestamp>.log`
- Normalized results: `docs/ansi/results/<runtime>-<timestamp>.json`
- Baseline snapshot: `docs/ansi-parity-baseline.json`

## Next Required Dot

- `habu-add-ansi-test-05377306`: add deterministic runner for `sbcl` and `habu`.
