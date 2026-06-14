# prop-corpus — frozen false-cert counterexamples

When `tools/prop-test.py` finds a FALSE-CERT (a definition the checker certifies
whose real runtime out-arity differs from its declared sig), freeze the
**minimized** program here so the bug can never silently return.

## Format

One counterexample per line in a `*.txt` file:

```
n_in|declared_out|body
```

e.g. a (hypothetical) `: G ( i64 -- i64 i64 ) dup ;` that was wrongly certified:

```
1|2|dup
```

Lines beginning `#` are comments.

## Replay (regression)

```sh
python3 tools/prop-test.py --replay test/prop-corpus
```

Asserts every frozen line is **no longer** a false-cert — i.e. the checker now
rejects it, or certifies it and it runs with the declared arity. Wired into the
gate (`test/run.sh`). Empty corpus passes trivially.

The minimizer in `prop-test.py` produces the smallest failing body; paste its
`min:` line (as `n_in|declared|min-body`) here.
