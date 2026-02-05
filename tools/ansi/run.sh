#!/usr/bin/env bash
set -euo pipefail

usage() {
    cat <<'EOF'
Usage: tools/ansi/run.sh <sbcl|habu> [options]

Options:
  --input <file>      Lisp file to load/run (default: <ansi-dir>/doit.lsp)
  --ansi-dir <dir>    ANSI test checkout root (default: /tmp/habu-ansi/ansi-test)
  --tag <name>        Deterministic artifact suffix (default: latest)
  --out <path>        Explicit output log path
  --habu-bin <path>   Habu binary (default: ./zig-out/bin/habu)
  -h, --help          Show this help
EOF
}

die() {
    echo "error: $*" >&2
    exit 1
}

if [[ $# -lt 1 ]]; then
    usage
    exit 2
fi

mode="$1"
shift

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ansi_dir="${ANSI_TEST_DIR:-/tmp/habu-ansi/ansi-test}"
input=""
tag="latest"
out=""
habu_bin="${HABU_BIN:-$repo_root/zig-out/bin/habu}"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --input)
            [[ $# -ge 2 ]] || die "--input requires a value"
            input="$2"
            shift 2
            ;;
        --ansi-dir)
            [[ $# -ge 2 ]] || die "--ansi-dir requires a value"
            ansi_dir="$2"
            shift 2
            ;;
        --tag)
            [[ $# -ge 2 ]] || die "--tag requires a value"
            tag="$2"
            shift 2
            ;;
        --out)
            [[ $# -ge 2 ]] || die "--out requires a value"
            out="$2"
            shift 2
            ;;
        --habu-bin)
            [[ $# -ge 2 ]] || die "--habu-bin requires a value"
            habu_bin="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            die "unknown argument: $1"
            ;;
    esac
done

if [[ -z "$input" ]]; then
    input="$ansi_dir/doit.lsp"
fi

[[ -f "$input" ]] || die "input not found: $input"
input="$(cd "$(dirname "$input")" && pwd)/$(basename "$input")"
ansi_dir="$(cd "$ansi_dir" && pwd)"

if [[ "${habu_bin#/}" == "$habu_bin" ]]; then
    habu_bin="$repo_root/$habu_bin"
fi

if [[ -z "$out" ]]; then
    out="$repo_root/docs/ansi/raw/${mode}-${tag}.log"
fi

mkdir -p "$(dirname "$out")"

case "$mode" in
    sbcl)
        cmd=(sbcl --noinform --disable-debugger --load "$input" --quit)
        ;;
    habu)
        if [[ ! -x "$habu_bin" ]]; then
            (cd "$repo_root" && zig build >/dev/null)
        fi
        [[ -x "$habu_bin" ]] || die "habu binary not executable: $habu_bin"
        if [[ ! -f "$ansi_dir/lib/stdlib.habu" ]]; then
            mkdir -p "$ansi_dir/lib"
            ln -sf "$repo_root/lib/stdlib.habu" "$ansi_dir/lib/stdlib.habu"
        fi
        cmd=("$habu_bin" "$input")
        ;;
    *)
        die "mode must be sbcl or habu"
        ;;
esac

{
    echo "# mode: $mode"
    echo "# input: $input"
    echo "# ansi_dir: $ansi_dir"
    echo "# started_utc: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "# command: ${cmd[*]}"
    echo
} >"$out"

if (cd "$ansi_dir" && "${cmd[@]}") >>"$out" 2>&1; then
    echo "# exit_code: 0" >>"$out"
else
    code=$?
    echo "# exit_code: $code" >>"$out"
    exit "$code"
fi

echo "$out"
