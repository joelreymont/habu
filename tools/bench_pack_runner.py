#!/usr/bin/env python3
"""Shared cross-runtime benchmark runners for bench-pack tooling."""

from __future__ import annotations

import json
import os
import re
import shlex
import subprocess
import sys
from typing import Any

_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
_CORPUS_PATH = os.path.join(_ROOT, "bench", "pack", "corpus.json")
_OCAML_ENV = {
    "micro": "HABU_OCAML_MICRO_CMD",
    "maxima": "HABU_OCAML_MAXIMA_CMD",
    "gc": "HABU_OCAML_GC_CMD",
}
_TIMEOUT_SEC = {
    "micro": 900,
    "maxima": 3600,
    "gc": 1800,
}


def repo_root() -> str:
    return _ROOT


def load_corpus(path: str = _CORPUS_PATH) -> dict[str, Any]:
    with open(path, "r", encoding="utf-8") as fh:
        return json.load(fh)


def suite_workload_names(corpus: dict[str, Any], suite: str) -> list[str]:
    suites = corpus.get("suites", {})
    node = suites.get(suite, {})
    rows = node.get("workloads", [])
    out: list[str] = []
    for row in rows:
        if isinstance(row, dict):
            name = row.get("name")
            if isinstance(name, str) and name:
                out.append(name)
    return out


def parse_mixed_json(text: str, label: str, required_keys: tuple[str, ...] = ()) -> dict[str, Any]:
    starts = [i for i, ch in enumerate(text) if ch == "{"]
    dec = json.JSONDecoder()
    for i in reversed(starts):
        frag = text[i:].lstrip()
        try:
            val, _ = dec.raw_decode(frag)
        except json.JSONDecodeError:
            continue
        if isinstance(val, dict) and all(k in val for k in required_keys):
            return val
    raise RuntimeError(f"{label}: missing JSON payload\\n{text}")


def _parse_rss_bytes(stderr_text: str) -> int | None:
    for line in stderr_text.splitlines():
        if "maximum resident set size" not in line.lower():
            continue
        match = re.search(r"(\d+)", line)
        if not match:
            continue
        raw = int(match.group(1))
        if sys.platform == "darwin":
            # macOS /usr/bin/time -l reports bytes.
            return raw
        # GNU time reports kbytes.
        return raw * 1024
    return None


def run_cmd(
    argv: list[str],
    *,
    timeout: int,
    env: dict[str, str] | None = None,
    capture_rss: bool = False,
) -> tuple[subprocess.CompletedProcess[str], int | None]:
    if capture_rss and os.path.exists("/usr/bin/time"):
        proc = subprocess.run(
            ["/usr/bin/time", "-l", *argv],
            capture_output=True,
            text=True,
            timeout=timeout,
            cwd=_ROOT,
            env=env,
        )
        if proc.returncode == 0:
            return proc, _parse_rss_bytes(proc.stderr)
        if "illegal option" not in proc.stderr.lower():
            return proc, _parse_rss_bytes(proc.stderr)
    proc = subprocess.run(
        argv,
        capture_output=True,
        text=True,
        timeout=timeout,
        cwd=_ROOT,
        env=env,
    )
    return proc, None


def _default_workload_map(names: list[str], status: str, error: str) -> dict[str, dict[str, Any]]:
    out: dict[str, dict[str, Any]] = {}
    for name in names:
        out[name] = {
            "name": name,
            "status": status,
            "ns": 0,
            "error": error,
        }
    return out


def _to_int(value: Any) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return 0


def _extract_benches(payload: dict[str, Any]) -> list[dict[str, Any]]:
    benches = payload.get("benches")
    if isinstance(benches, list):
        return [row for row in benches if isinstance(row, dict)]

    workloads = payload.get("workloads")
    if isinstance(workloads, list):
        out = []
        for row in workloads:
            if isinstance(row, dict):
                out.append(row)
        return out
    if isinstance(workloads, dict):
        out = []
        for name, row in workloads.items():
            item: dict[str, Any] = {"name": name}
            if isinstance(row, dict):
                item.update(row)
            else:
                item["ns"] = row
            out.append(item)
        return out
    return []


def _normalize_workloads(
    names: list[str], payload: dict[str, Any]
) -> tuple[dict[str, dict[str, Any]], list[dict[str, Any]]]:
    rows = _extract_benches(payload)
    seen: dict[str, dict[str, Any]] = {}
    for row in rows:
        name = row.get("name")
        if not isinstance(name, str) or not name:
            continue
        ns = _to_int(row.get("ns", 0))
        err = row.get("error")
        status = "error" if err else ("ok" if ns > 0 else "zero")
        seen[name] = {
            "name": name,
            "status": status,
            "ns": ns,
            "error": err,
        }

    workloads: dict[str, dict[str, Any]] = {}
    for name in names:
        if name in seen:
            workloads[name] = seen[name]
        else:
            workloads[name] = {
                "name": name,
                "status": "missing",
                "ns": 0,
                "error": "missing",
            }

    extras = [row for name, row in seen.items() if name not in workloads]
    return workloads, extras


def _has_flag(argv: list[str], flag: str) -> bool:
    if any(arg == flag for arg in argv):
        return True
    if flag.endswith("="):
        return any(arg.startswith(flag) for arg in argv)
    return any(arg.startswith(flag + "=") for arg in argv)


def _append_kv(argv: list[str], key: str, value: int) -> None:
    probe = f"{key}="
    if _has_flag(argv, probe):
        return
    argv.append(f"{key}={value}")


def _build_ocaml_cmd(
    suite: str,
    *,
    iters: int,
    scale: int,
    heap_mb: int,
    nursery_mb: int,
    live_mb: int,
) -> tuple[list[str] | None, str | None]:
    env_key = _OCAML_ENV[suite]
    template = os.environ.get(env_key, "").strip()
    if not template:
        return None, f"{env_key} is not set"

    params = {
        "iters": iters,
        "scale": scale,
        "heap_mb": heap_mb,
        "nursery_mb": nursery_mb,
        "live_mb": live_mb,
    }
    has_template = "{" in template and "}" in template
    if has_template:
        try:
            rendered = template.format(**params)
        except KeyError as exc:
            return None, f"{env_key} missing placeholder: {exc}"
    else:
        rendered = template

    argv = shlex.split(rendered)
    if not argv:
        return None, f"{env_key} resolved to an empty command"

    if not _has_flag(argv, "--json"):
        argv.append("--json")

    if not has_template:
        if suite == "micro":
            _append_kv(argv, "--iters", iters)
        elif suite == "maxima":
            _append_kv(argv, "--scale", scale)
            _append_kv(argv, "--heap-mb", heap_mb)
            _append_kv(argv, "--nursery-mb", nursery_mb)
        elif suite == "gc":
            _append_kv(argv, "--iters", iters)
            _append_kv(argv, "--live-mb", live_mb)
            _append_kv(argv, "--heap-mb", heap_mb)

    return argv, None


def _build_workload_cmd(
    runtime: str,
    suite: str,
    *,
    iters: int,
    scale: int,
    heap_mb: int,
    nursery_mb: int,
    mode: str,
) -> tuple[list[str] | None, str | None]:
    if runtime == "habu":
        if suite == "micro":
            cmd = [
                "zig",
                "build",
                "-Duse-hoist=true",
                "bench-comp",
                "--",
                "--json",
                f"--iters={iters}",
            ]
            if mode == "interp":
                cmd.append("--interp")
            return cmd, None
        if suite == "maxima":
            use_hoist = "false" if mode == "interp" else "true"
            return [
                "zig",
                "build",
                f"-Duse-hoist={use_hoist}",
                "bench-maxima",
                "--",
                "--json",
                f"--scale={scale}",
                f"--heap-mb={heap_mb}",
                f"--nursery-mb={nursery_mb}",
            ], None
        return None, f"unsupported suite for habu runner: {suite}"

    if runtime == "sbcl":
        if suite == "micro":
            return [
                "sbcl",
                "--script",
                "bench/comprehensive.lisp",
                "--json",
                f"--iters={iters}",
            ], None
        if suite == "maxima":
            return [
                "sbcl",
                "--script",
                "bench/maxima_workload.lisp",
                "--json",
                f"--scale={scale}",
            ], None
        return None, f"unsupported suite for sbcl runner: {suite}"

    if runtime == "ocaml":
        return _build_ocaml_cmd(
            suite,
            iters=iters,
            scale=scale,
            heap_mb=heap_mb,
            nursery_mb=nursery_mb,
            live_mb=8,
        )

    return None, f"unknown runtime: {runtime}"


def _build_gc_cmd(
    runtime: str,
    *,
    iters: int,
    live_mb: int,
    heap_mb: int,
) -> tuple[list[str] | None, str | None]:
    if runtime == "habu":
        return [
            "zig",
            "build",
            "bench",
            "--",
            "--json",
            f"--iters={iters}",
            f"--live-mb={live_mb}",
            f"--heap-mb={heap_mb}",
        ], None

    if runtime == "sbcl":
        return [
            "sbcl",
            "--script",
            "bench/sbcl_gc.lisp",
            "--json",
            f"--iters={iters}",
            f"--live-mb={live_mb}",
        ], None

    if runtime == "ocaml":
        return _build_ocaml_cmd(
            "gc",
            iters=iters,
            scale=1,
            heap_mb=heap_mb,
            nursery_mb=0,
            live_mb=live_mb,
        )

    return None, f"unknown runtime: {runtime}"


def run_workload_runtime(
    runtime: str,
    suite: str,
    *,
    iters: int = 1,
    scale: int = 1,
    heap_mb: int = 1024,
    nursery_mb: int = 32,
    mode: str = "jit",
    timeout: int | None = None,
    env: dict[str, str] | None = None,
) -> dict[str, Any]:
    corpus = load_corpus()
    workload_names = suite_workload_names(corpus, suite)

    cmd, cmd_err = _build_workload_cmd(
        runtime,
        suite,
        iters=iters,
        scale=scale,
        heap_mb=heap_mb,
        nursery_mb=nursery_mb,
        mode=mode,
    )
    if cmd is None:
        return {
            "runtime": runtime,
            "suite": suite,
            "status": "unavailable",
            "error": cmd_err,
            "workload_order": workload_names,
            "workloads": _default_workload_map(workload_names, "unavailable", cmd_err or "unavailable"),
            "extras": [],
            "payload": {},
            "rss_bytes": None,
            "command": [],
        }

    timeout_sec = timeout if timeout is not None else _TIMEOUT_SEC.get(suite, 1800)
    proc, rss_bytes = run_cmd(cmd, timeout=timeout_sec, env=env)
    if proc.returncode != 0:
        err = f"cmd failed ({runtime}/{suite}): {' '.join(cmd)}"
        return {
            "runtime": runtime,
            "suite": suite,
            "status": "error",
            "error": err,
            "stdout": proc.stdout,
            "stderr": proc.stderr,
            "workload_order": workload_names,
            "workloads": _default_workload_map(workload_names, "error", err),
            "extras": [],
            "payload": {},
            "rss_bytes": rss_bytes,
            "command": cmd,
        }

    try:
        payload = parse_mixed_json(proc.stdout, f"{runtime}/{suite}", required_keys=("benches",))
    except RuntimeError:
        payload = parse_mixed_json(proc.stdout, f"{runtime}/{suite}")
    workloads, extras = _normalize_workloads(workload_names, payload)
    return {
        "runtime": runtime,
        "suite": suite,
        "status": "ok",
        "error": None,
        "workload_order": workload_names,
        "workloads": workloads,
        "extras": extras,
        "payload": payload,
        "rss_bytes": rss_bytes,
        "command": cmd,
    }


def run_gc_runtime(
    runtime: str,
    *,
    iters: int = 100,
    live_mb: int = 8,
    heap_mb: int = 64,
    timeout: int | None = None,
) -> dict[str, Any]:
    cmd, cmd_err = _build_gc_cmd(runtime, iters=iters, live_mb=live_mb, heap_mb=heap_mb)
    if cmd is None:
        return {
            "runtime": runtime,
            "suite": "gc",
            "status": "unavailable",
            "error": cmd_err,
            "payload": {},
            "rss_bytes": None,
            "command": [],
        }

    timeout_sec = timeout if timeout is not None else _TIMEOUT_SEC["gc"]
    proc, rss_bytes = run_cmd(cmd, timeout=timeout_sec, capture_rss=True)
    if proc.returncode != 0:
        err = f"cmd failed ({runtime}/gc): {' '.join(cmd)}"
        return {
            "runtime": runtime,
            "suite": "gc",
            "status": "error",
            "error": err,
            "stdout": proc.stdout,
            "stderr": proc.stderr,
            "payload": {},
            "rss_bytes": rss_bytes,
            "command": cmd,
        }

    try:
        payload = parse_mixed_json(proc.stdout, f"{runtime}/gc", required_keys=("avg_pause_ns",))
    except RuntimeError:
        payload = parse_mixed_json(proc.stdout, f"{runtime}/gc")
    return {
        "runtime": runtime,
        "suite": "gc",
        "status": "ok",
        "error": None,
        "payload": payload,
        "rss_bytes": rss_bytes,
        "command": cmd,
    }
