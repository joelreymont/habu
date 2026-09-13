#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
GF="${GFORTH:-gforth}"

if [[ -z "${HABU_TARGET:-}" ]]; then
  case "$(uname -s)-$(uname -m)" in
    Darwin-arm64|Darwin-aarch64) HABU_TARGET=macos-aarch64 ;;
    Linux-aarch64|Linux-arm64) HABU_TARGET=linux-aarch64 ;;
    *) printf 'nf-path-test: unsupported host\n' >&2; exit 64 ;;
  esac
fi
export HABU_TARGET

work="$(mktemp -d /tmp/habu-nf-path-test.XXXXXX)"
trap 'rm -rf -- "$work"' EXIT

prefix="$work/"
fill=$((97 - ${#prefix}))
if (( fill < 1 )); then
  printf 'nf-path-test: temporary root is too long\n' >&2
  exit 64
fi
long_root="$prefix$(printf '%*s' "$fill" '' | tr ' ' x)"
quoted_root="$work/lane one 'quote; dollar\$ \"double\" [brackets]"
mkdir -p -- "$long_root" "$quoted_root"

cd "$ROOT"
HB_TMP="$long_root" "$GF" test/bootstrap-wide-memory.fs >"$work/long.log" 2>&1 &
long_pid=$!
HB_TMP="$quoted_root" "$GF" test/bootstrap-wide-memory.fs >"$work/quoted.log" 2>&1 &
quoted_pid=$!

set +e
wait "$long_pid"; long_rc=$?
wait "$quoted_pid"; quoted_rc=$?
set -e
if (( long_rc != 0 || quoted_rc != 0 )); then
  cat "$work/long.log" "$work/quoted.log" >&2
  printf 'nf-path-test: concurrent fixtures failed: long=%s quoted=%s\n' \
    "$long_rc" "$quoted_rc" >&2
  exit 1
fi

HB_TMP="$long_root" "$GF" -e \
  'require test/nf.fs NF-REPL-CMD$ nip 256 <= abort" nf-path-test: combined command did not exceed the old bound" bye' \
  >"$work/command.log" 2>&1

overflow_prefix="$work/"
overflow_fill=$((122 - ${#overflow_prefix}))
overflow_root="$overflow_prefix$(printf '%*s' "$overflow_fill" '' | tr ' ' y)"
set +e
HB_TMP="$overflow_root" "$GF" -e 'require test/nf.fs bye' \
  >"$work/overflow.out" 2>"$work/overflow.err"
overflow_rc=$?
set -e
if (( overflow_rc == 0 )) ||
   ! grep -q 'nf.fs: scratch path exceeds NF-PATH-CAP' "$work/overflow.err"; then
  cat "$work/overflow.err" >&2
  printf 'nf-path-test: missing named path overflow refusal\n' >&2
  exit 1
fi

printf 'nf-path-test: ok\n'
