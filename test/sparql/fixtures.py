"""Pinned W3C fixture loading and result comparison, independent of the engine.

These functions are preserved verbatim from the historical audit. RDFLib reads
RDF/results and compares terms and graph isomorphism; it never runs SPARQL.
"""
from __future__ import annotations

from collections import Counter
import hashlib
import json
from pathlib import Path
import time
import traceback
from urllib.parse import unquote, urlparse, quote

import rdflib
rdflib.NORMALIZE_LITERALS = False
from rdflib import BNode, Graph, Literal, Namespace, RDF, URIRef, Dataset as RDFDataset
from rdflib.compare import isomorphic
from rdflib.query import Result

HERE = Path(__file__).resolve().parent
XSD = "http://www.w3.org/2001/XMLSchema#"
RS = Namespace("http://www.w3.org/2001/sw/DataAccess/tests/result-set#")
HX = Namespace("urn:habu:sparql:conformance:")
STATUS = {0:"success", 1:"syntax", 2:"source", 3:"evaluation", 4:"argument",
          5:"engine_error", 6:"limit"}


def normalized_term(t):
    """RDF 1.1 identity: plain = xsd:string; language tags are case-insensitive.

    Numeric/boolean/date lexical forms are deliberately NOT normalized.
    """
    if t is None:
        return None
    kind = t["type"]
    if kind in ("uri", "bnode"):
        return {"type": kind, "value": t["value"]}
    if kind in ("literal", "typed-literal"):
        language = t.get("xml:lang") or t.get("lang")
        if language:
            return {"type":"literal", "value":t["value"], "xml:lang":language.lower()}
        return {"type":"literal", "value":t["value"], "datatype":t.get("datatype") or XSD+"string"}
    raise ValueError(f"Unsupported RDF term kind {kind!r}")



def term_key(t):
    return json.dumps(normalized_term(t), sort_keys=True, ensure_ascii=False, separators=(",", ":"))



def to_json_term(t):
    if isinstance(t, URIRef):
        return {"type":"uri", "value":str(t)}
    if isinstance(t, BNode):
        return {"type":"bnode", "value":str(t)}
    if isinstance(t, Literal):
        v = {"type":"literal", "value":str(t)}
        if t.language:
            v["xml:lang"] = t.language.lower()
        else:
            v["datatype"] = str(t.datatype) if t.datatype else XSD+"string"
        return v
    raise TypeError(f"Unsupported RDFLib term: {t!r}")



def to_rdf_term(t, bnodes=None):
    t = normalized_term(t)
    if t["type"] == "uri":
        return URIRef(t["value"])
    if t["type"] == "bnode":
        if bnodes is None:
            return BNode(t["value"])
        return bnodes.setdefault(t["value"], BNode())
    return Literal(t["value"], lang=t.get("xml:lang"), datatype=t.get("datatype"), normalize=False)



class Dataset:
    def __init__(self):
        self.quads = {}
        self.graphs = {}

    def graph(self, name):
        if name is not None:
            self.graphs[term_key(name)] = normalized_term(name)

    def add(self, s,p,o,g):
        terms = tuple(normalized_term(x) for x in (s,p,o,g))
        self.quads[tuple(term_key(x) if x is not None else None for x in terms)] = dict(zip("spog",terms))
        self.graph(g)



def fixture_graph(path, cache):
    path = Path(path).resolve()
    if path not in cache:
        suffix = path.suffix.lower()
        fmt = {".ttl":"turtle", ".n3":"turtle", ".rdf":"xml", ".nt":"nt"}.get(suffix)
        if fmt is None:
            raise ValueError(f"Unsupported RDF input format {suffix}")
        cache[path] = Graph().parse(path, format=fmt, publicID=path.as_uri())
    return cache[path]



def load_dataset(test, description, suite_root):
    dataset, cache = Dataset(), {}

    def add_file(path, name):
        dataset.graph(name)
        for s,p,o in fixture_graph(path,cache):
            dataset.add(to_json_term(s),to_json_term(p),to_json_term(o),name)

    for d in test["default_data"]:
        path = Path(d["path"]).resolve()
        if path.suffix.lower() in (".trig", ".nq"):
            # Current upstream manifests also use qt:data for a whole dataset.
            source = RDFDataset()
            source.parse(path,format="trig" if path.suffix.lower() == ".trig" else "nquads",publicID=path.as_uri())
            for graph in source.graphs():
                name = None if graph.identifier == source.default_graph.identifier else to_json_term(graph.identifier)
                dataset.graph(name)
                for s,p,o in graph:
                    dataset.add(to_json_term(s),to_json_term(p),to_json_term(o),name)
        else:
            add_file(path,None)
    for d in test["named_data"]:
        add_file(d["data"]["path"],{"type":"uri","value":d["name"]})
    if description:
        # Supply named fixture graphs for FROM and FROM NAMED. Selection/merge
        # belongs to the native evaluator and remains under test.
        for iri in set(description["default"] + description["named"]):
            graph_name = {"type":"uri","value":iri}
            if term_key(graph_name) in dataset.graphs:
                continue
            uri = urlparse(iri)
            if uri.scheme != "file":
                raise ValueError(f"Fixture graph is unavailable locally: {iri}")
            path = Path(unquote(uri.path)).resolve()
            if not path.is_relative_to(suite_root):
                raise ValueError(f"Fixture path is outside pinned suite: {path}")
            add_file(path,graph_name)
    return dataset



def load_expected(test):
    path = Path(test["result"]["path"])
    suffix = path.suffix.lower()
    if suffix in (".srx", ".srj", ".tsv"):
        fmt = {".srx":"xml", ".srj":"json", ".tsv":"tsv"}[suffix]
        with path.open("rb") as f:
            result = Result.parse(f,format=fmt)
        if result.type == "ASK":
            return {"kind":"boolean", "boolean":bool(result.askAnswer)}
        return {"kind":"solutions", "vars":[str(v) for v in result.vars],
                "rows":[{str(k):to_json_term(v) for k,v in row.items() if v is not None} for row in result.bindings],
                "ordered":False}
    graph = fixture_graph(path,{})
    rs = next(iter(graph.subjects(RDF.type,RS.ResultSet)),None)
    if rs is None:
        return {"kind":"graph", "graph":graph}
    answer = graph.value(rs,RS.boolean)
    if answer is not None:
        return {"kind":"boolean", "boolean":str(answer) in ("true","1")}
    rows, indices = [], []
    for solution in graph.objects(rs,RS.solution):
        row = {}
        for binding in graph.objects(solution,RS.binding):
            variable = graph.value(binding,RS.variable)
            value = graph.value(binding,RS.value)
            if variable is not None and value is not None:
                row[str(variable)] = to_json_term(value)
        rows.append(row)
        index = graph.value(solution,RS.index)
        indices.append(int(index) if index is not None else None)
    ordered = any(i is not None for i in indices)
    if ordered:
        if any(i is None for i in indices) or len(set(indices)) != len(indices):
            raise ValueError("Expected result has incomplete/duplicate rs:index")
        rows = [row for _,row in sorted(zip(indices,rows),key=lambda pair:pair[0])]
    return {"kind":"solutions", "vars":[str(v) for v in graph.objects(rs,RS.resultVariable)],
            "rows":rows, "ordered":ordered}



def row_key(row):
    return tuple(sorted((v,term_key(t)) for v,t in row.items()))



def solution_graph(rows, ordered):
    result = Graph()
    bnodes = {}
    for index,row in enumerate(rows):
        solution = BNode()
        result.add((HX.root,HX.solution,solution))
        if ordered:
            result.add((solution,HX.index,Literal(index)))
        for variable,term in row.items():
            result.add((solution,URIRef(str(HX)+"variable:"+quote(variable,safe="")),to_rdf_term(term,bnodes)))
    return result



def compare_result(form, text, expected, lax):
    if form in (3,4):
        if expected["kind"] != "graph":
            return False,{"reason":"Result form mismatch","expected":expected["kind"],"actual":"graph"}
        graph = Graph().parse(data=text,format="nt")
        equal = isomorphic(graph,expected["graph"])
        return equal,{"kind":"graph","actual_triples":len(graph),"expected_triples":len(expected["graph"])}
    actual = json.loads(text)
    if form == 2:
        ok = expected["kind"] == "boolean" and actual.get("boolean") is expected.get("boolean")
        return ok,{"kind":"boolean","actual":actual.get("boolean"),"expected":expected.get("boolean")}
    if expected["kind"] != "solutions":
        return False,{"reason":"Result form mismatch","expected":expected["kind"],"actual":"solutions"}
    variables = actual["head"].get("vars",[])
    rows = [{v:normalized_term(t) for v,t in r.items()} for r in actual["results"]["bindings"]]
    exp_rows = [{v:normalized_term(t) for v,t in r.items()} for r in expected["rows"]]
    details = {"kind":"solutions", "actual_rows":len(rows), "expected_rows":len(exp_rows),
               "ordered":expected["ordered"], "lax_cardinality":lax,
               "actual_vars":variables,"expected_vars":expected["vars"]}
    if set(variables) != set(expected["vars"]):
        details["reason"] = "Projected variable mismatch"
        return False,details
    bnodes = any(t["type"] == "bnode" for row in rows+exp_rows for t in row.values())
    if lax:
        if bnodes:
            raise ValueError("No pinned REDUCED fixture uses bnodes; support explicitly if suite changes")
        actual_counts, expected_counts = Counter(map(row_key,rows)), Counter(map(row_key,exp_rows))
        ok = actual_counts.keys() == expected_counts.keys() and all(1 <= actual_counts[k] <= n for k,n in expected_counts.items())
    elif bnodes:
        ok = isomorphic(solution_graph(rows,expected["ordered"]),solution_graph(exp_rows,expected["ordered"]))
    elif expected["ordered"]:
        ok = list(map(row_key,rows)) == list(map(row_key,exp_rows))
    else:
        ok = Counter(map(row_key,rows)) == Counter(map(row_key,exp_rows))
    if not ok:
        details["reason"] = "Result terms/multiplicities/order differ"
        details["actual_rows_preview"] = rows[:12]
        details["expected_rows_preview"] = exp_rows[:12]
    return ok,details



def run_one(native, provider, test, suite_root, descriptions):
    started = time.perf_counter()
    record = {k:test[k] for k in ("id","name","group","category","requires","approval")}
    record["query_sha256"] = hashlib.sha256(test["query_text"].encode()).hexdigest()
    try:
        syntax = test["group"] == "query_syntax"
        description = None
        if not syntax:
            # Input-only metadata from the identical pinned parser's previous run.
            # It contains no expected answer and does not judge query correctness.
            item = descriptions[test["id"].removeprefix(suite_root.as_uri()+"/")]
            if item["query_sha256"] != record["query_sha256"]:
                raise ValueError("Fixture dataset metadata belongs to another query")
            if item["dataset"] is not None:
                description = {k:[u.replace("fixture:",suite_root.as_uri()+"/") for u in v]
                               for k,v in item["dataset"].items()}
        dataset = Dataset() if syntax else load_dataset(test,description,suite_root)
        provider.load(dataset)
        status,result,diagnostic,counts,errors = native.run(test["query_text"],test["base_uri"],provider)
        record.update(parse_status="syntax" if status==1 else "success",diagnostic=diagnostic,
                      source_requests=counts,source_errors=errors)
        if syntax:
            passed = status==1 if test["is_negative_syntax"] else status not in (1,4,5)
            record.update(status="pass" if passed else "fail",
                          expected_parse="reject" if test["is_negative_syntax"] else "accept",
                          execution_status=STATUS.get(status,str(status)))
            return record
        form = 3
        if status == 0 and result.lstrip().startswith("{"):
            form = 2 if "boolean" in json.loads(result) else 1
        record.update(input_quads=len(dataset.quads),input_named_graphs=len(dataset.graphs),
                      dataset_description=description,query_status=STATUS.get(status,str(status)),
                      form=form,actual_result=result)
        if status != 0:
            record.update(status="fail",phase="parse" if status==1 else "evaluation")
            return record
        if errors:
            record.update(status="fail",phase="source",diagnostic="Native evaluation returned success after provider failure")
            return record
        expected = load_expected(test)
        ok,details = compare_result(form,result,expected,test["result_cardinality"].endswith("#LaxCardinality"))
        record["comparison"] = details
        record["status"] = "pass" if ok else "fail"
        return record
    except Exception as exc:
        record.update(status="harness_error",diagnostic=type(exc).__name__+": "+str(exc),traceback=traceback.format_exc())
        return record
    finally:
        record["elapsed_ms"] = round(1000*(time.perf_counter()-started),3)
