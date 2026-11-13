#!/usr/bin/env python3
"""Focused evaluation of the BNODE scope defect through the native ABI."""
import argparse, hashlib, json, sys
from pathlib import Path
from run_native import Native, Dataset, MemoryProvider

def run(library):
    native = Native(library)
    records = []
    def query(name, text, check, dataset=None):
        source = MemoryProvider(); source.load(dataset or Dataset())
        status, raw, diagnostic, requests, errors = native.run(text, 'urn:probe:', source)
        result = json.loads(raw) if status == 0 else None
        ok = status == 0 and not errors and check(result)
        records.append(dict(name=name, query=text, pass_=ok, status=status,
                            result=result, diagnostic=diagnostic, requests=requests))
    def rows(result): return result['results']['bindings']
    def number(result, name='n'): return int(rows(result)[0][name]['value'])
    def pairs(result):
        rs=rows(result)
        return len(rs)==2 and all(r['b']==r['c'] for r in rs) and rs[0]['b']!=rs[1]['b']
    query('Duplicate rows get distinct scopes',
          'SELECT (COUNT(DISTINCT ?b) AS ?n) WHERE { VALUES ?row { 1 1 } BIND(BNODE("x") AS ?b) }',
          lambda r:number(r)==2)
    query('Sequential BINDs share each row scope',
          'SELECT ?b ?c WHERE { VALUES ?row { 1 2 } BIND(BNODE("x") AS ?b) BIND(BNODE("x") AS ?c) }',pairs)
    query('Filter and SELECT retain each row scope',
          'SELECT ?b (BNODE("x") AS ?c) WHERE { VALUES ?row { 1 2 } BIND(BNODE("x") AS ?b) FILTER(sameTerm(?b,BNODE("x"))) } ORDER BY ?row',pairs)
    query('Arbitrary string keys',
          'SELECT (BNODE("") AS ?a) (BNODE("has space") AS ?b) (BNODE("has space") AS ?c) WHERE {}',
          lambda r:set(rows(r)[0])=={'a','b','c'} and rows(r)[0]['b']==rows(r)[0]['c'] and rows(r)[0]['a']!=rows(r)[0]['b'])
    query('Join preserves bound nodes and creates result scopes',
          'SELECT (COUNT(DISTINCT ?before) AS ?beforeCount) (COUNT(DISTINCT ?after) AS ?afterCount) WHERE { { BIND(BNODE("x") AS ?before) } VALUES ?l { 1 1 } VALUES ?r { 1 1 } BIND(BNODE("x") AS ?after) }',
          lambda r:number(r,'beforeCount')==1 and number(r,'afterCount')==4)
    query('Union branches do not share scope',
          'SELECT (COUNT(DISTINCT ?b) AS ?n) WHERE { { BIND(BNODE("x") AS ?b) } UNION { BIND(BNODE("x") AS ?b) } }',
          lambda r:number(r)==2)
    query('Subquery projection retains duplicate occurrences',
          'SELECT (COUNT(DISTINCT ?b) AS ?n) WHERE { { SELECT ?x WHERE { VALUES (?x ?hidden) { (1 2) (1 3) } } } BIND(BNODE("x") AS ?b) }',
          lambda r:number(r)==2)
    query('Blank-node scope does not affect tuple equality',
          'SELECT (COUNT(DISTINCT *) AS ?n) WHERE { VALUES ?row { 1 1 } FILTER(isBlank(BNODE("x"))) }',
          lambda r:number(r)==1)
    query('No-argument calls remain fresh',
          'SELECT (BNODE() AS ?a) (BNODE() AS ?b) WHERE { VALUES ?row { 1 1 } }',
          lambda r:len(rows(r))==2 and len({t['value'] for row in rows(r) for t in row.values()})==4)
    dataset=Dataset()
    dataset.add({'type':'bnode','value':'x'},{'type':'uri','value':'urn:p'},
                {'type':'literal','value':'1','datatype':'http://www.w3.org/2001/XMLSchema#integer'},None)
    query('Generated nodes differ from same-spelled source nodes',
          'SELECT (sameTerm(?s,BNODE("x")) AS ?same) WHERE { ?s <urn:p> 1 }',
          lambda r:rows(r)[0]['same']['value']=='false',dataset)
    return {'library_sha256':native.sha256,'passed':sum(r['pass_'] for r in records),'total':len(records),'records':records}

if __name__=='__main__':
    parser=argparse.ArgumentParser();parser.add_argument('--library',required=True);parser.add_argument('--output',required=True);args=parser.parse_args()
    result=run(args.library);Path(args.output).write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps({k:v for k,v in result.items() if k!='records'}))
    for record in result['records']:
        if not record['pass_']: print(json.dumps(record))
    sys.exit(result['passed']!=result['total'])
