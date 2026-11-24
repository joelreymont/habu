#!/usr/bin/env python3
"""Export official SPARQL 1.0+1.1 manifests for a separate engine harness.

This parses manifests, not SPARQL queries; it never runs a reference engine.
"""
import json
from collections import Counter
from pathlib import Path
from rdflib import BNode, Graph, Namespace, RDF
from inventory import REVISION, MF, QT, UT, SD, DAWG, local_path

ROOT = Path(__file__).parent / f"rdf-tests-{REVISION}"
ROOT = ROOT.resolve()
ROOTS = [ROOT / "sparql/sparql10/manifest.ttl", ROOT / "sparql/sparql11/manifest-all.ttl"]
seen, tests = set(), {}


def resource(term):
    if term is None:
        return None
    uri = str(term)
    if uri.startswith("file:"):
        path = local_path(term)
        return {"uri": uri, "path": str(path), "relative_path": path.relative_to(ROOT).as_posix()}
    return {"uri": uri, "path": None, "relative_path": None}


def walk(path):
    if path in seen:
        return
    seen.add(path)
    g = Graph().parse(path, format="turtle")
    for entries in g.objects(None, MF.entries):
        for t in g.items(entries):
            types = [str(ty).split("#")[-1] for ty in g.objects(t, RDF.type)]
            action = g.value(t, MF.action)
            syntax = any("Syntax" in ty for ty in types)
            update = any("Update" in ty for ty in types)
            query = g.value(action, UT.request if update else QT.query)
            if syntax:
                query = action
            query_resource = resource(query)
            result = g.value(t, MF.result)
            result_resource = None if isinstance(result, BNode) else resource(result)
            dirname = path.parent.name
            group = "unclassified"
            if syntax and not update:
                group = "query_syntax"
            elif "QueryEvaluationTest" in types:
                group = "entailment_evaluation" if dirname == "entailment" else (
                    "federated_evaluation" if dirname == "service" else "query_evaluation")
            elif update:
                group = "update"
            elif "CSVResultFormatTest" in types:
                group = "csv_result_format"
            elif "ServiceDescriptionTest" in types:
                group = "service_description"
            elif "ProtocolTest" in types:
                group = "protocol"
            elif "GraphStoreProtocolTest" in types:
                group = "graph_store_protocol"
            default_data = [resource(v) for v in g.objects(action, QT.data)]
            named_data = []
            for v in g.objects(action, QT.graphData):
                # In this pinned query suite qt:graphData is always an IRI.
                if isinstance(v, BNode):
                    raise ValueError(f"New graphData structure must be supported explicitly: {t}")
                named_data.append({"name": str(v), "data": resource(v)})
            services = []
            for s in g.objects(action, QT.serviceData):
                services.append({"endpoint": str(g.value(s, QT.endpoint)),
                                 "data": [resource(v) for v in g.objects(s, QT.data)]})
            # Preserve list or scalar requirements; RDF-list order is not meaningful here.
            requirements = []
            for v in g.objects(t, MF.requires):
                requirements.extend(str(z) for z in (g.items(v) if isinstance(v, BNode) else [v]))
            tests[str(t)] = {
                "id": str(t), "name": str(g.value(t, MF.name) or ""),
                "types": types, "group": group,
                "manifest": str(path), "category": dirname,
                "query": query_resource,
                "query_text": Path(query_resource["path"]).read_text() if query_resource and query_resource["path"] else None,
                "base_uri": str(query) if query else None,
                "default_data": default_data, "named_data": named_data,
                "service_data": services, "result": result_resource,
                "requires": requirements,
                "approval": str(g.value(t, DAWG.approval) or ""),
                "result_cardinality": str(g.value(t, MF.resultCardinality) or ""),
                "is_negative_syntax": any(ty.startswith("Negative") for ty in types),
                "note": "Unbound results omit variables; preserve duplicate rows; bnode labels compare by a global bijection.",
            }
    for includes in g.objects(None, MF.include):
        for included in g.items(includes):
            walk(local_path(included))


for root in ROOTS:
    walk(root)
result = {
    "repository": "https://github.com/w3c/rdf-tests", "revision": REVISION,
    "suite_root": str(ROOT), "method": "Recursive official manifests; no query execution",
    "total": len(tests),
    "counts_by_group": dict(sorted(Counter(t["group"] for t in tests.values()).items())),
    "read_only_core": {
        "groups": ["query_syntax", "query_evaluation"],
        "test_count": sum(t["group"] in {"query_syntax", "query_evaluation"} for t in tests.values()),
        "query_syntax_count": sum(t["group"] == "query_syntax" for t in tests.values()),
        "query_evaluation_count": sum(t["group"] == "query_evaluation" for t in tests.values()),
        "note": "Includes three valid federation syntax tests; syntax acceptance is distinct from execution policy.",
    },
    "tests": sorted(tests.values(), key=lambda t: t["id"]),
}
output = Path(__file__).parent / "readonly-manifest.json"
output.write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps({k:v for k,v in result.items() if k != "tests"}, indent=2))
