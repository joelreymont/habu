# Intel-host execution evidence

19 September 2026. Baseline: `515463165054d552899add949e62183f6f25808f`.

## What is working

The genuine ARM64 Habu recovery engine builds on x86-64 Linux and runs under QEMU user mode. It loads the actual checker/runtime prefix and compiles and executes Habu definitions. This is not a native x86-64 backend, a Python source replay, or an accepted production selfbuild.

The recovery patch is `.github/patches/intel-hosted-recovery.patch`. Apply with `git apply --check` followed by `git apply`; the patch is stored rather than silently applied to this branch's tracked bootstrap files. The workflow applies it in a read-only-permission job.

Five bootstrap files repair missing atomic primitives and LSE encoders, the Linux seed's dynamic-loader/realpath bridge, the cold image's 64-KiB alignment calculation, and provided rows for preloaded dependencies. macOS recovery and full snapshot roundtrips were not validated.

## Executed CI

- Commit: `3ca7967f65a1cf2f0f57e76d6b5510f6f41e1987`.
- Successful run: https://github.com/joelreymont/habu/actions/runs/35441654392
- Job: `105893382716`; artifact: `10583563516`.
- Actual gates: typed arithmetic/recursion/loops; atomics and path handling; file loading and require deduplication; lib/string.f; records, inferred wide locals and tagged-union MATCH; wrong stack effect rejected with exit 70; protected atomic write rejected with exit 83; stdin program returns 42.
- Engine: ARM64 ELF, 196800 bytes; SHA-256 `698c6d5a129fdd231d4431939058bd6940d1b6409fcb65b76acc909f497b6bd0`.
- The CI engine is byte-identical to the independently generated local engine. This is not a self-host fixpoint claim.
- Gforth tag `0.7.9_20260610`, commit `be0316636de19aed535564874b807be68ec47a11`.

Locally, 11533 selected LDADDAL/CASAL/DMB encodings from the actual Gforth bootstrap assembler matched Clang's AArch64 assembler. Runtime tests do not establish a concurrency stress result.

## Existing x86 work and new real-engine check

The recovered Library artifact `habu-x86-64-51546316.zip` contains an unpushed Phase A encoder/emitter candidate, not a hosted port. Its original source replay evidence did not establish Habu acceptance.

The actual recovered Habu fixture `test/compiler/x86-64-asm.f` was executed locally through the recovery engine. First it rejected the typed wide local `insn:instruction`. Applying the small arity-0 wide annotation change passed that point and exposed unsupported `u<`/`u>` words. Replacing those with checked sign-bit-XOR order helpers yielded `test: ok`, exit 0.

The additive follow-up is `.github/patches/x86-real-engine-followup.patch`; it requires the original Phase A patch from the recovered artifact first. Both patches and raw local results are in the delivered recovery kit. This optional experiment is separate from the clean CI recovery baseline. It does not establish complete typed-wide/parametric/linear-local acceptance.

## Not done

Native x86 source-to-code lowering, runtime/ABI/syscalls, JIT and image/capture integration, complete selfbuild, the full native suite, and Rocq gates remain outstanding. The earlier complete Omabox planner and parametric/linear-local changes have not been validated by these smoke tests. QEMU recovery timing must not be compared to native Rust as a language benchmark.
