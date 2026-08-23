# Code-generator parity

The judge compiles each corpus subject through the engine emitter and the native
chain from one source text. Where a C twin is available it also runs the host
compiler. The gate checks result and witness agreement; byte counts, stack
traffic, reference sizes, and costs are live measurements, not baselines.

Run the live report from the repository root:

```sh
bin/hb --load tools/judge.f
```

The focused functional test, also registered in the native suite, is:

```sh
bin/hb --load tools/judge-test.f
```

The report has two parts. The first records the two Habu generators, including a
measured refusal code when the chain cannot compile a subject. The second records
host-dependent reference and cost measurements. Neither part is compared with a
committed snapshot.

The main components are:

| File | Responsibility |
|---|---|
| `tools/judge.f` | render the live report |
| `tools/judge/check.f` | run every corpus into the live table |
| `tools/judge/pass.f` | compile and execute one subject through each available generator |
| `tools/judge/row.f` | hold live results and measurements |
| `tools/judge/report.f` | render the table |
| `tools/judge-test.f` | assert semantic agreement, refusal coverage, inputs, witnesses, and traffic |
| `tools/judge-fuzz-test.f` | run the seeded differential oracle |
| `tools/judge/corpus*.f` | define the finite hand-written subjects |

The hand-written corpus is finite. The seeded fuzz gate adds generated
straight-line integer programs and compares their answers on boundary and
generated inputs. Both generators still share the source reader, so
`tools/judge/src-test.f` separately attacks that parser with misleading
fixtures.

A reference column is present only when its host toolchain can build it. Its
machine-code size and timing data are useful evidence in the report, but they do
not decide whether the gate passes. Semantic disagreement does.
