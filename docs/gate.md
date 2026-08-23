# Native test suite

Build and install the exact current engine pair before running the suite:

```sh
bin/hb --load tools/build-fixpoint-refresh.f -- install --force
```

Then run the one native registry:

```sh
bin/hb --load test/run.f
```

`test/gate-stdlib-cases.f` is the registry. Each `SUITE` row runs its listed
files through the installed `bin/hb`; there is no second test inventory.

Failures print the suite label, exit outcome, and captured stdout and stderr.
On failure, the full capture files remain under the printed temporary root;
successful runs remove it.
