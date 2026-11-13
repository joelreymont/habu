#!/usr/bin/env python3
"""Run the unchanged W3C comparison policy through Habu and PostgreSQL.

Habu alone parses SPARQL and compiles SQL. PostgreSQL alone evaluates it.
This adapter supplies fixtures and serializes returned rows for the existing
comparison code. It does not execute the public Habu result writer or MCP.
"""
from __future__ import annotations

import argparse
from collections import Counter
import hashlib
import json
import os
from pathlib import Path
import subprocess
import time

import psycopg
from psycopg.pq import ExecStatus
from rdflib import BNode, Graph, Literal, URIRef

from fixtures import HERE, run_one

HABU = HERE.parents[1]
SCHEMA = "pg_temp"
# A compile limit has not established syntax acceptance. Only SERVICE policy
# follows a completed parse; other compiler failures fail the syntax audit.
ERROR_STATUS = {-10001: 1, -10002: 3}


class SyntaxOnly:
    def load(self, dataset):
        pass

    def close(self):
        pass


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def graph_term(term):
    if term["type"] == "uri":
        return URIRef(term["value"])
    if term["type"] == "bnode":
        return BNode(term["value"])
    return Literal(term["value"], lang=term.get("xml:lang"),
                   datatype=term.get("datatype"), normalize=False)


def result_text(plan, rows):
    form = plan["form"]
    if form == "select":
        return json.dumps({"head": {"vars": plan["vars"]},
                           "results": {"bindings": rows}}, ensure_ascii=False)
    if form == "ask":
        if len(rows) != 1 or type(rows[0]) is not bool:
            raise ValueError("ASK did not return one JSON boolean")
        return json.dumps({"head": {}, "boolean": rows[0]})
    graph = Graph()
    for row in rows:
        graph.add(tuple(graph_term(row[key]) for key in "spo"))
    return graph.serialize(format="nt")


class Habu:
    def __init__(self, executable, tests, artifacts, timeout):
        self.executable = executable.resolve()
        self.executable_sha256 = sha256(self.executable)
        artifacts.mkdir(parents=True, exist_ok=True)
        self.batch = artifacts / "queries.json"
        self.stdout = artifacts / "plans.jsonl"
        self.stderr = artifacts / "compiler.stderr"
        pairs = [(t["query_text"], t["base_uri"] or "") for t in tests]
        self.batch.write_text(json.dumps(pairs, ensure_ascii=False) + "\n")
        with self.stdout.open("w") as out, self.stderr.open("w") as err:
            process = subprocess.run(
                [str(self.executable), "--load", "tools/sparql-compile.f", "--",
                 str(self.batch.resolve()), SCHEMA], cwd=HABU,
                stdin=subprocess.DEVNULL, stdout=out, stderr=err, timeout=timeout)
        if process.returncode:
            raise RuntimeError(f"Habu load/compile process exited {process.returncode}; see {self.stderr}")
        compiled = [json.loads(line) for line in self.stdout.read_text().splitlines() if line.strip()]
        if len(compiled) != len(pairs):
            raise RuntimeError(f"Habu emitted {len(compiled)} records for {len(pairs)} queries")
        self.plans = dict(zip(pairs, compiled))

    def run(self, query, base, provider, **limits):
        plan = self.plans[(query, base or "")]
        if "error" in plan:
            code = plan["error"]
            return ERROR_STATUS.get(code, 5), "", f"Habu error {code}", {}, []
        if isinstance(provider, SyntaxOnly):
            return 0, "", "", {}, []  # syntax-only audit: no evaluation result is claimed
        try:
            with provider.snapshot():
                rows, batches = provider.execute(plan)
            return 0, result_text(plan, rows), "", {"query": 1, "fetch": batches}, []
        except psycopg.Error as error:
            return 3, "", str(error), {"query": 1}, []
        except QueryError as error:
            return 3, "", str(error), {"query": 1}, []


class QueryError(Exception):
    pass


class Postgres:
    def __init__(self, projection, terms):
        self.connection = psycopg.connect(os.environ["ZEN_VALIDATION_CONNINFO"], autocommit=True)
        self.version = self.connection.execute("SELECT version()").fetchone()[0]
        self.connection.execute("CREATE TEMP TABLE audit_quad(s jsonb,p jsonb,o jsonb,g jsonb)")
        self.connection.execute("CREATE INDEX ON audit_quad(p,s,o,g)")
        self.connection.execute("CREATE TEMP TABLE audit_graph(g jsonb PRIMARY KEY)")
        prefix = projection.read_text().split("CREATE FUNCTION knowledge.sparql_vocabulary", 1)
        if len(prefix) != 2:
            raise ValueError("Projection foundation boundary is absent; do not guess helper definitions")
        self.connection.execute(prefix[0].replace("knowledge.", "pg_temp."))
        self.connection.execute(terms.read_text().replace("knowledge.", "pg_temp."))
        self.connection.execute("""
            CREATE FUNCTION pg_temp.sparql_match(ws jsonb,wp jsonb,wo jsonb,wg jsonb)
            RETURNS TABLE(s jsonb,p jsonb,o jsonb,g jsonb)
            LANGUAGE sql STABLE AS $$
                SELECT s,p,o,g FROM pg_temp.audit_quad
                 WHERE (ws IS NULL OR s=ws) AND (wp IS NULL OR p=wp)
                   AND (wo IS NULL OR o=wo) AND g IS NOT DISTINCT FROM wg
            $$;
            CREATE FUNCTION pg_temp.sparql_graphs() RETURNS TABLE(g jsonb)
            LANGUAGE sql STABLE AS $$ SELECT g FROM pg_temp.audit_graph $$
        """)

    def load(self, dataset):
        with self.connection.transaction():
            self.connection.execute("TRUNCATE audit_quad,audit_graph")
            with self.connection.cursor() as cursor:
                with cursor.copy("COPY audit_quad(s,p,o,g) FROM STDIN") as copy:
                    for row in dataset.quads:
                        copy.write_row(row)
                with cursor.copy("COPY audit_graph(g) FROM STDIN") as copy:
                    for graph in dataset.graphs:
                        copy.write_row((graph,))
            self.connection.execute("""
                UPDATE audit_quad SET s=pg_temp.sparql_normalize(s),p=pg_temp.sparql_normalize(p),
                    o=pg_temp.sparql_normalize(o),g=pg_temp.sparql_normalize(g)
            """)

    def snapshot(self):
        from contextlib import contextmanager

        @contextmanager
        def scope():
            with self.connection.transaction():
                self.connection.execute("SET TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY")
                self.connection.execute("SET LOCAL statement_timeout='10s'")
                self.connection.execute("SET LOCAL search_path=pg_catalog,pg_temp")
                yield
        return scope()

    def execute(self, plan):
        sql = "DECLARE habu_audit NO SCROLL CURSOR FOR " + plan["sql"]
        reply = self.connection.pgconn.exec_params(sql.encode(), [p.encode() for p in plan["params"]])
        if reply.status != ExecStatus.COMMAND_OK:
            raise QueryError(reply.error_message.decode(errors="replace"))
        rows, batches = [], 0
        while True:
            batch = self.connection.execute("FETCH FORWARD 64 FROM habu_audit").fetchall()
            batches += 1
            if not batch:
                return rows, batches
            rows.extend(row[0] for row in batch)

    def close(self):
        self.connection.close()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--hb", type=Path, required=True)
    parser.add_argument("--projection", type=Path, required=True)
    parser.add_argument("--terms", type=Path, required=True)
    parser.add_argument("--manifest", type=Path, default=HERE / "readonly-manifest.json")
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--syntax-only", action="store_true")
    parser.add_argument("--id", action="append", default=[])
    parser.add_argument("--limit", type=int)
    parser.add_argument("--compile-timeout", type=int, default=600)
    args = parser.parse_args()
    manifest = json.loads(args.manifest.read_text())
    tests = [t for t in manifest["tests"] if t["group"] in ("query_syntax", "query_evaluation")]
    if args.syntax_only:
        tests = [t for t in tests if t["group"] == "query_syntax"]
    if args.id:
        tests = [t for t in tests if any(value in t["id"] for value in args.id)]
    if args.limit is not None:
        tests = tests[:args.limit]
    args.output.parent.mkdir(parents=True, exist_ok=True)
    artifacts = args.output.parent / (args.output.stem + "-artifacts")
    habu = Habu(args.hb, tests, artifacts, args.compile_timeout)
    provider = SyntaxOnly() if args.syntax_only else Postgres(args.projection, args.terms)
    descriptions = json.loads((HERE / "dataset-descriptions.json").read_text())["tests"]
    records, started = [], time.perf_counter()
    try:
        for index, test in enumerate(tests, 1):
            record = run_one(habu, provider, test, Path(manifest["suite_root"]), descriptions)
            records.append(record)
            if index % 50 == 0 or record["status"] != "pass":
                print(json.dumps({"completed": index, "total": len(tests),
                                  "status": record["status"], "test": test["id"]}), flush=True)
    finally:
        provider.close()
        result = {
            "subject": "Habu SPARQL compiler with PostgreSQL evaluation",
            "habu_language_compiler_executed": True,
            "habu_language_wrapper_executed": False,
            "syntax_method": "Habu parse/compile; E-SYNTAX rejects; execution-policy errors do not reject syntax",
            "evaluation_method": "One PostgreSQL statement and read-only snapshot per query; 64-row cursor batches",
            "timeout_ms": 10000, "intermediate_read_cap": None, "result_row_cap": None,
            "provider": "none" if args.syntax_only else "postgres",
            "postgres_version": getattr(provider, "version", None),
            "habu": str(habu.executable), "habu_sha256": habu.executable_sha256,
            "plans": str(habu.stdout), "plans_sha256": sha256(habu.stdout),
            "compiler_stderr": str(habu.stderr),
            "harness_sha256": sha256(__file__), "comparison_harness_sha256": sha256(HERE / "fixtures.py"),
            "projection_sha256": sha256(args.projection), "terms_sha256": sha256(args.terms),
            "source_hashes": {str(path.relative_to(HABU)): sha256(path)
                              for path in sorted((HABU / "src/lib/sparql").glob("*.f"))},
            "driver_sha256": sha256(HABU / "tools/sparql-compile.f"),
            "manifest_sha256": sha256(args.manifest),
            "fixture_metadata_sha256": sha256(HERE / "dataset-descriptions.json"),
            "suite_revision": manifest["revision"], "planned": len(tests), "completed": len(records),
            "counts": dict(Counter(r["status"] for r in records)),
            "counts_by_group": {group: dict(Counter(r["status"] for r in records if r["group"] == group))
                                for group in sorted(set(r["group"] for r in records))},
            "elapsed_seconds": round(time.perf_counter() - started, 3),
            "comparison": "Unchanged fixture/comparison functions: exact RDF 1.1 terms, bags, global blank-node bijection, rs:index order, REDUCED lax cardinality; no numeric lexical normalization",
            "records": records,
        }
        args.output.write_text(json.dumps(result, indent=2, ensure_ascii=False) + "\n")
        print(json.dumps(result["counts"]))


if __name__ == "__main__":
    main()
