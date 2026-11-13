#!/usr/bin/env python3
"""Inventory the pinned W3C SPARQL suite by following mf:include/mf:entries.

Requires rdflib. This enumerates tests; it does not execute or score them.
"""
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
from urllib.parse import unquote, urlparse

from rdflib import BNode, Graph, Literal, Namespace, RDF, URIRef

REVISION = "7b5afb91857a9c282c03d2e45d3f76485797c901"
MF = Namespace("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#")
QT = Namespace("http://www.w3.org/2001/sw/DataAccess/tests/test-query#")
UT = Namespace("http://www.w3.org/2009/sparql/tests/test-update#")
SD = Namespace("http://www.w3.org/ns/sparql-service-description#")
DAWG = Namespace("http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#")


def local_path(uri):
    parsed = urlparse(str(uri))
    if parsed.scheme != "file":
        raise ValueError(f"Expected local manifest URI: {uri}")
    return Path(unquote(parsed.path))


def inventory(root, manifest):
    seen, tests = set(), {}

    def value(g, term, visited=()):
        if term is None:
            return None
        if isinstance(term, BNode):
            if term in visited:
                return {"cycle": str(term)}
            if (term, RDF.first, None) in g:
                return [value(g, v, visited + (term,)) for v in g.items(term)]
            data = {}
            for p, o in g.predicate_objects(term):
                data.setdefault(str(p), []).append(value(g, o, visited + (term,)))
            return data
        if isinstance(term, Literal):
            return {"value": str(term), "datatype": str(term.datatype) if term.datatype else None,
                    "language": term.language}
        parsed = urlparse(str(term))
        if parsed.scheme == "file":
            p = local_path(term)
            try:
                rel = p.relative_to(root).as_posix()
            except ValueError:
                return str(term)
            return rel + ("#" + parsed.fragment if parsed.fragment else "")
        return str(term)

    def walk(path):
        path = path.resolve()
        if path in seen:
            return
        seen.add(path)
        g = Graph().parse(path, format="turtle")
        for entries in g.objects(None, MF.entries):
            for t in g.items(entries):
                key = value(g, t)
                action = g.value(t, MF.action)
                tests[key] = {
                    "id": key,
                    "types": [str(v).split("#")[-1] for v in g.objects(t, RDF.type)],
                    "name": str(g.value(t, MF.name) or ""),
                    "manifest": path.relative_to(root).as_posix(),
                    "action": value(g, action),
                    "result": value(g, g.value(t, MF.result)),
                    "requires": [value(g, v) for v in g.objects(t, MF.requires)],
                    "approval": value(g, g.value(t, DAWG.approval)),
                    "result_cardinality": value(g, g.value(t, MF.resultCardinality)),
                    "entailment": value(g, g.value(action, SD.entailmentRegime)) if action else None,
                }
        for includes in g.objects(None, MF.include):
            for included in g.items(includes):
                walk(local_path(included))

    walk(root / manifest)
    return {
        "root_manifest": manifest,
        "test_count": len(tests),
        "counts_by_type": dict(sorted(Counter(ty for t in tests.values() for ty in t["types"]).items())),
        "manifests": {p.relative_to(root).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
                      for p in sorted(seen)},
        "tests": sorted(tests.values(), key=lambda t: t["id"]),
    }


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--suite", type=Path, default=Path(__file__).parent / f"rdf-tests-{REVISION}")
    ap.add_argument("--output", type=Path, default=Path(__file__).parent / "inventory.json")
    args = ap.parse_args()
    root = args.suite.resolve()
    manifests = ["sparql/sparql10/manifest.ttl", "sparql/sparql11/manifest.ttl",
                 "sparql/sparql11/manifest-all.ttl", "sparql/sparql12/manifest.ttl",
                 "sparql/manifest-all.ttl"]
    inventories = [inventory(root, m) for m in manifests]
    result = {"repository": "https://github.com/w3c/rdf-tests", "revision": REVISION,
              "license": "W3C Test Suite License OR W3C 3-clause BSD License",
              "method": "Recursive mf:include and mf:entries traversal; unique test IRIs; no execution",
              "inventories": inventories}
    args.output.write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps([{k: i[k] for k in ("root_manifest", "test_count", "counts_by_type")}
                      for i in inventories], indent=2))


if __name__ == "__main__":
    main()
