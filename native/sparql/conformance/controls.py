#!/usr/bin/env python3
"""Focused native paging/control checks. Does not execute Habu or the MCP host."""
import argparse
import ctypes as C
import json
from pathlib import Path
import selectors
import subprocess
import sys
import time

from run_native import Native, SOURCE, WRITE


def term(value):
    return {"type":"uri","value":value}


class Source:
    def __init__(self, total=1000, delay=0, fail=False):
        self.total,self.delay,self.fail=total,delay,fail
        self.requests=[]
        self.rows=0

    def __call__(self, request):
        self.requests.append(request)
        if self.delay:
            time.sleep(self.delay)
        if self.fail:
            raise RuntimeError("source unavailable")
        if request["op"]=="graphs":
            return {"graphs":[],"next":None}
        if request["op"]=="contains":
            return {"contains":False}
        start,limit=int(request.get("cursor") or 0),request["limit"]
        end=min(start+limit,self.total)
        rows=[{"s":term("urn:s:"+str(i)),"p":term("urn:p"),
               "o":{"type":"literal","value":"value-"+str(i)},"g":None}
              for i in range(start,end)]
        self.rows+=len(rows)
        return {"quads":rows,"next":str(end) if end<self.total else None}


def invoke(native, query, source, refuse=False, **limits):
    chunks,diagnostics=[],[]

    @SOURCE
    def read(context,data,length,reply):
        try:
            result=source(json.loads(C.string_at(data,length)))
            status=0
        except Exception as error:
            result={"error":str(error)}
            status=1
        encoded=json.dumps(result,separators=(",", ":")).encode()
        native.lib.hb_sparql_reply_set(reply,encoded,len(encoded))
        return status

    @WRITE
    def write(context,status,data,length):
        value=C.string_at(data,length)
        (diagnostics if status else chunks).append(value)
        return int(refuse and status==0)

    request=json.dumps({"query":query,"timeout_ms":5000,"max_rows":10000,
                        "max_bytes":1048576,"max_scan_rows":10000,**limits}).encode()
    started=time.monotonic()
    status=native.lib.hb_sparql_run(request,len(request),None,read,write)
    return {"status":status,"result":b"".join(chunks).decode(),
            "diagnostic":b"".join(diagnostics).decode(),
            "source_calls":len(source.requests),"source_rows":source.rows,
            "output_bytes":sum(map(len,chunks)),"chunks":len(chunks),
            "elapsed_ms":round((time.monotonic()-started)*1000,3)}


def cpu_query():
    values=" ".join(map(str,range(100)))
    return "SELECT (COUNT(*) AS ?count) WHERE { "+" ".join(
        "VALUES ?"+v+" { "+values+" }" for v in "abcdef")+" }"


def hard_stop(native):
    command=[sys.executable,__file__,"--library",str(native.path),"--cpu-child"]
    process=subprocess.Popen(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True)
    selector=selectors.DefaultSelector()
    selector.register(process.stdout,selectors.EVENT_READ)
    try:
        if not selector.select(10) or process.stdout.readline().strip()!="native ready":
            raise RuntimeError("CPU probe did not reach the native boundary")
        started=time.monotonic()
        try:
            process.wait(timeout=0.2)
            return {"pass":False,"reason":"CPU query completed before the hard-stop deadline"}
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait(timeout=5)
            return {"pass":process.returncode<0,"returncode":process.returncode,
                    "elapsed_ms":round((time.monotonic()-started)*1000,3),
                    "scope":"OS termination of actual native evaluator; Habu worker not executed"}
    finally:
        selector.close()
        if process.poll() is None:
            process.kill()
            process.wait()
        process.communicate()


def main():
    parser=argparse.ArgumentParser()
    parser.add_argument("--library",required=True,type=Path)
    parser.add_argument("--output",type=Path,default=Path(__file__).with_name("controls-results.json"))
    parser.add_argument("--cpu-child",action="store_true")
    args=parser.parse_args()
    native=Native(args.library)
    if args.cpu_child:
        print("native ready",flush=True)
        invoke(native,cpu_query(),Source(0),timeout_ms=60000)
        return
    records=[]

    def check(name,result,condition):
        result={"name":name,"pass":bool(condition),**result}
        records.append(result)
        print(json.dumps(result),flush=True)

    query="SELECT ?s WHERE { ?s <urn:p> ?o }"
    result=invoke(native,query+" LIMIT 1",Source(100000))
    check("LIMIT 1 stops after first page",result,
          result["status"]==0 and result["source_calls"]==1 and result["source_rows"]==256
          and len(json.loads(result["result"])["results"]["bindings"])==1)

    result=invoke(native,"SELECT (COUNT(*) AS ?count) WHERE { ?s <urn:p> ?o }",Source())
    check("Aggregate reads successive pages",result,
          result["status"]==0 and result["source_calls"]==4 and result["source_rows"]==1000
          and json.loads(result["result"])["results"]["bindings"][0]["count"]["value"]=="1000")

    for name,limit in [("Result row cap",{"max_rows":1}),
                       ("Result byte cap",{"max_bytes":100}),
                       ("Source row cap",{"max_scan_rows":255})]:
        selected="SELECT (1 AS ?x) WHERE {}" if "max_bytes" in limit else query
        result=invoke(native,selected,Source(),**limit)
        check(name,result,result["status"]==6 and
              ("max_bytes" not in limit or result["output_bytes"]<=limit["max_bytes"]) and
              ("max_scan_rows" not in limit or result["source_rows"]<=limit["max_scan_rows"]))

    result=invoke(native,query,Source(delay=0.05),timeout_ms=20)
    check("Cooperative deadline after source callback",result,
          result["status"]==6 and result["source_calls"]==1 and result["elapsed_ms"]>=50)

    result=invoke(native,"SELECT (EXISTS { ?s <urn:p> ?o } AS ?found) WHERE {}",Source(fail=True))
    check("Source error survives EXISTS",result,result["status"]==2)

    result=invoke(native,query,Source(),refuse=True)
    check("Output refusal cancels",result,result["status"]==2)

    result=hard_stop(native)
    check("Hard CPU cancellation in separate process",result,result["pass"])
    output={"library_sha256":native.sha256,"habu_wrapper_executed":False,
            "passed":sum(r["pass"] for r in records),"total":len(records),"records":records}
    args.output.write_text(json.dumps(output,indent=2)+"\n")
    raise SystemExit(0 if output["passed"]==output["total"] else 1)


if __name__=="__main__":
    main()
