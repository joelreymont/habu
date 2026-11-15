#!/usr/bin/env bash
# Native ARM execution of the same checked T0 stages used during recovery.
set -euo pipefail

test "$(uname -m)" = aarch64
ci_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
task_root="${RUNNER_TEMP:-/tmp}/habu-t0-recovery"
mkdir -p "$task_root/results"
(cd "$ci_root" && sha256sum -c seed.sha256)
seed="$ci_root/hb"

phase() {
    local name="$1"
    shift
    local started="$SECONDS"
    if (cd "$source" && TMPDIR="$stage/tmp" HABU_OUT="$stage/objects" \
        timeout 300 "$seed" "$@" </dev/null >"$result/$name.log" 2>&1); then
        printf '%s %s passed in %s seconds\n' "$revision" "$name" "$((SECONDS-started))"
    else
        local status="$?"
        cat "$result/$name.log"
        return "$status"
    fi
}

for revision in c5a99531f3f535bd7540b1448af6a54924bda64e 0c1e6b799379ed0f16dc55c272309cc53cff90c6; do
    stage="$task_root/$revision"
    source="$stage/source"
    result="$task_root/results/$revision"
    mkdir -p "$stage/tmp" "$stage/objects" "$result"
    git fetch --depth=1 origin "$revision"
    git worktree add --detach "$source" "$revision"
    git -C "$source" rev-parse HEAD HEAD^{tree} >"$result/source.txt"
    phase kernel --load "$ci_root/kernel.f" -- "$source" "$result/kernel.hko"
    phase engine --load "$ci_root/engine.f" -- "$source" "$result/hb" "$result/kernel.hko"
    (cd "$result" && sha256sum hb hb.map >sha256.txt)
    seed="$result/hb"
    phase smoke --load "$ci_root/smoke.f"
done
