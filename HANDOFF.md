# Handoff: Intel x86-64 port onto master

State on 2026-10-21. Delete this file when the port lands on `master`.

## Where things are

| Ref | Revision | Meaning |
| --- | --- | --- |
| `intel` | merge `40f36d55` + this file | The x86-64 checkpoint line (`4db1d989`, its second parent) merged onto `master@5999ba5f`. Every conflict is resolved. **Never built or run.** The branch was moved sideways from `4db1d989`, which stays reachable as the merge's parent. |
| `intel-checkpoint-2026-10-07` | draft release | `hb-linux-x86_64` + `.names`, source `b16066cd`. It cannot host the merged tree: its baked prefix is the old `lib/` layout, so the build dies at once on `duplicate definition: E-A-FIRST at src/lib/errors.f:20` (exit 78). |
| `master@origin` | `40860f6f` | Two commits past the merge's master parent (`42873837`, `40860f6f`); merge them into `intel` before gating. |

The merge rule was: where both sides changed the same thing, master's design wins and the x86 capability is re-expressed on it. Nothing x86 was dropped silently; what changed or was lost is listed below.

## What the merge decided

- **Declared return-stack cells across calls:** intel's bridging (53b94c0b, a264b43e, return rows of cbc55b8a) is removed. Master refuses such signatures since 69e6c964 (no non-test word used them; ANS 3.2.3.3 forbids the form). `>r … r>` inside one definition still works; intel's whole-value return-stack cells (RGLUE, RSTACK-CK, RSTACK-CELLS by operand width) are kept.
- **Libraries** follow master's `src/lib/`; `lib/` is gone. `lib/source-syntax.f` (intel-only) is now `src/lib/source-syntax.f`.
- **Checker owner record** is $550 bytes / 170 cells: intel's fields are appended after master's in `CHECKER-OWNER-EXT` (MULTI-ERROR $528, RESET-REPORT $530, JSON-REPORTED $538, REFUSAL-TOKEN $540, RECOVERY-USED $548). `E-NCOMP-RECOVERY` is -8576; the x86 disassembler's codes are -9500..-9509.
- **Compiler:** fixed/defer native paths, trusted and immediate ticks, undefined-tick refusal at the operand, the remat mark (a64ir MINOR 19, K-REMAT-MARK 21, KEYS 22; x64ir MINOR 4, K-REMAT-MARK 15, KEYS 16), MB-PIN (pinned class to the frame for idiv/rax, shl/rcx), reloads never re-spilled, x86 RELOC-CALLS, icode fixup tables at $2000. Master's MB-RUN-HELD? replaces intel's MB-DIVIDE/CL-ANCH/EVICTED-ROOTS; master's TFW replaces intel's nested generic widths. Intel's multi-error recovery binding is re-expressed as RECOVERY-BIND under master's LIVE-BIND; the used-import ambiguity among recovery rows is simplified (first used import's row wins).
- **Runtime:** occurrence numbering now covers x86 boot (`OCC-INIT,` numbers existing records) and unit publication on both arches (intel handoff defects 1 and 4). Capture keeps master's prepare/complete split with shared ACAP-START/ACAP-FINISH; `aot-shadow.f` keeps intel's x86-edge reach (format v15) and names master's TARGET-CAPTURE routines with their private WID. The task ALTSTACK is removed (master's task ABI, `TCB-BYTES` $170).
- **Tools:** `native-build-core.f` CAPTURE has three paths (native x86 shadow, ARM host with dual-emission shadow → `AOT-CAPTURE:TARGET-CAPTURE`, plain). `native-unit-capture.f` enforces PROFILE-CK itself; the definition cache declines x86 quietly. `aot-data-cell.f` reads x86 address sites through `SITES:EACH-IN-SPAN`.
- `handoff.md` and `docs/branch-work.md` from the intel line are deleted; durable facts moved into `docs/x86-64.md`.

## Known gaps (work, not merge leftovers)

1. x86 replay writers and the source-exec observer still refuse (intel handoff defect 2; master refuses too).
2. x86 `does-patch` does not reissue occurrence numbers for patched records and aliases (defect 3; `test/does-empty-clause.f:183` expects pre-patch handles to refuse `E-STALE`).
3. After a throw inside the Habu `evaluate`, master's eval-frame rows are not retracted.
4. `AOT-CAPTURE:TARGET-CAPTURE` does not set `ACAP-ALL-NAMES` from `ACAP-WHITEBOX?` as `CAPTURE` does; a whitebox x86 cross-build would miss names.
5. The comment at `src/habu/aot-closure.f:715` (OWNER asks the ARM bitmap) is stale.
6. New x86 FFI float32 stubs in `src/lib/ffi-abi.f` were written by hand and never ran.
7. Not adapted: bundle builds to the x86 writer; incremental builds may not record every x86 writer input; an x86 cached object with a DATA payload is refused.
8. Coverage lost with tests master deleted: 9 x86 golden rows (`target-native-layout-goldens.f`), the ARM-only PROFILE-CK of `native-unit-image.f` (now in the tool), `engine-writers.f`'s def-open tier and kind seal cases, the engine-suite x86 raw FFI leaf, imgdump/native-unit-profile/pre-trust/aot-shadow anonymous x86 cases. Master's REDEFINE-CASE (tier 0) runs on ARM only.
9. `test/task-prepare-failure.f`'s 11-page limit was sized with the altstack page; check it still makes preparation fail.
10. Intel's last full suite (on frozen 1f98c905) was 614 suites, 68 red; `test/xt-effect-test.f` F34/F36 red on the checkpoint. Expect reds; classify each, do not raise budgets.
11. `intel-aot-wip` (97b4ddab) holds portable AOT graph/storage repairs with two requested review corrections (see `git show 4db1d989:handoff.md`, "Retained AOT work"); not merged.

## Needs a build to confirm

Compiler and checker load (RECOVERY-BIND, CHECKER-PREFIX:BIND?, publish.f COMMIT-DEFER, dict.f REC-CODE, fixed/defer paths); the 170-cell owner record including a retained master host; layout offsets against DATA-CLAIMS; occurrence numbering at boot and unit publication; `evaluate-range` on x86; x86 snapshot boot with master's zeroed hook cells; `test/x86-64-link-records.f` strip/export/quot cases; error codes using-test and tier.f expect; x86 pinned spills; native-multi-error-recovery; C2 refusals, closed-unit-suite, match-factor-pin, does-clause-record on x86; sealed-source routing on x86.

## How to continue

The x86 engine has no cold route: it is cross-built from a working ARM64 engine. Real ARM64 Linux hardware is the fast route.

Without ARM64 hardware, an x86-64 Linux host can run the ARM64 engine under qemu user emulation. Measured in the cloud session that produced this branch (2 cores):

- `apt install qemu-user-static libc6-arm64-cross`; register binfmt (`mount -t binfmt_misc binfmt_misc /proc/sys/fs/binfmt_misc`, then write `/usr/lib/binfmt.d/qemu-aarch64.conf` with interpreter `/usr/bin/qemu-aarch64-static` into `/proc/sys/fs/binfmt_misc/register`); export `QEMU_LD_PREFIX=/usr/aarch64-linux-gnu` (the stage engines link libc dynamically). PostgreSQL and zip rows need ARM64 `libpq5`/`libzip` unpacked from ports.ubuntu.com into that prefix; master's zip rows expect `libzip.so.5`, which Ubuntu 24.04 does not ship.
- Gforth 0.7.9 builds from the complang snapshot tarball (needs `libtool-bin`). `HABU_TARGET=linux-aarch64` must be set explicitly on an x86 host.
- The seed's self-check gates are the slow part: each gate binary ran about ten minutes because the JIT defeats qemu's translation cache. Without them (a copy of `tools/bootstrap.sh` whose `bootstrap_*_gate` calls are disabled — a host-preparation shortcut, never a qualification) the Gforth seed reached `hb-stage0` within a minute and the stage-1 engine in about 15 minutes. The stage fixpoint, cross-build and ARM gate under qemu were not completed in that session.
- The x86 checkpoint engine (`intel-checkpoint-2026-10-07`) cannot host this tree: its baked prefix is the old `lib/` layout (`duplicate definition: E-A-FIRST at src/lib/errors.f:20`, exit 78).

On an ARM64 Linux host, in an isolated copy of `intel` (`jj workspace add` under `.jj-ws.noindex/`):

```sh
# 0. Bring in master's newer commits first.
jj new intel master@origin -m "Merge master into the Intel port"   # resolve, describe, continue

# 1. ARM64 engine from the merged tree. Host: a qualified ARM64 hb of master
#    (~/.local/lib/habu/current/bin/hb), or recover through Gforth 0.7.9:
#    HABU_ALLOW_BOOTSTRAP=1 HABU_TARGET=linux-aarch64 HB_TMP=$TMP GFORTH=$(command -v gforth) tools/bootstrap.sh
HOST=~/.local/lib/habu/current/bin/hb
HABU_UNDER_TEST=$HOST HABU_FIXPOINT_ENGINE=$HOST HB_TMP=$TMP \
  $HOST --load tools/native-build.f -- $OUT/arm/hb
cp $OUT/arm/hb $OUT/arm/hb.map bin/
bin/hb --load test/run.f </dev/null                     # ARM gate: master must not regress

# 2. Cross-build x86-64 from that ARM engine.
bin/hb --load src/arch/x86-64/passes.f tools/native-build.f -- $OUT/x64/hb --target linux-x86-64
```

Then on x86-64 Linux, in a copy of the same tree with `$OUT/x64/hb` and its `hb.map` as `bin/`:

```sh
HB=$PWD/bin/hb
# 3. Self-host to a byte fixpoint: three generations, cmp each pair (executable and map).
HABU_UNDER_TEST=$HB HABU_FIXPOINT_ENGINE=$HB HB_TMP=$TMP $HB --load tools/native-build.f -- $OUT/g1/hb
# ... g2 from g1, g3 from g2; cmp g1/g2 and g2/g3.
# 4. Full native suite on the x86 candidate.
bin/hb --load test/run.f </dev/null
```

Fix what fails at the responsible layer, closing gaps 1–4 above as they surface; get an independent adversarial review of the merge and the fixes (Fable for Claude-written code); then land on `master` (gate and push on separate lines) and delete this file and the `intel*` refs once accounted for (`intel-rebase` and `intel-rebase-wip` on origin are superseded copies of this branch; delete them). Keep `INTEL.md` and `docs/x86-64.md` current: master's `INTEL.md` still describes the 2026-09-29 state.
