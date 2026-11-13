//! Regression queries run through the same C ABI used by Habu.
use habu_sparql::{HbSparqlReply, hb_sparql_reply_set, hb_sparql_run};
use serde_json::{Value, json};
use std::collections::HashSet;
use std::ffi::c_void;

const XSD: &str = "http://www.w3.org/2001/XMLSchema#";

#[derive(Default)]
struct Call {
    source: bool,
    output: Vec<u8>,
    diagnostic: Vec<u8>,
}

unsafe extern "C" fn read(
    context: *mut c_void, bytes: *const u8, length: usize, reply: *mut HbSparqlReply,
) -> i32 {
    let call = unsafe { &mut *context.cast::<Call>() };
    // An unused EXISTS branch must not reach the source, even if its error
    // would otherwise be swallowed by SPARQL expression evaluation.
    if !call.source { return 1; }
    let Ok(request) = serde_json::from_slice::<Value>(unsafe {
        std::slice::from_raw_parts(bytes, length)
    }) else { return 1 };
    if request["op"] != "scan" { return 1; }
    let quad = json!({
        "s":{"type":"bnode","value":"x"},
        "p":{"type":"uri","value":"urn:p"},
        "o":{"type":"literal","value":"1","datatype":format!("{XSD}integer")},
        "g":null
    });
    let matches = request["graph"]["kind"] == "default"
        && ["s", "p", "o"].iter().all(|key|
            request[*key].is_null() || request[*key] == quad[*key]);
    let response = json!({"quads":if matches { vec![quad] } else { vec![] },"next":null})
        .to_string();
    unsafe { hb_sparql_reply_set(reply, response.as_ptr(), response.len()); }
    0
}

unsafe extern "C" fn write(
    context: *mut c_void, status: i32, bytes: *const u8, length: usize,
) -> i32 {
    let call = unsafe { &mut *context.cast::<Call>() };
    let output = if status == 0 { &mut call.output } else { &mut call.diagnostic };
    output.extend_from_slice(unsafe { std::slice::from_raw_parts(bytes, length) });
    0
}

fn query(query: &str, source: bool) -> Value {
    let request = json!({
        "query":format!("PREFIX xsd: <{XSD}> {query}"),
        "timeout_ms":5000,"max_rows":100,"max_bytes":65536,"max_scan_rows":1000
    }).to_string();
    let mut call = Call { source, ..Call::default() };
    let status = unsafe {
        hb_sparql_run(request.as_ptr(), request.len(),
            (&mut call as *mut Call).cast(), Some(read), Some(write))
    };
    assert_eq!(status, 0, "{query}: {}", String::from_utf8_lossy(&call.diagnostic));
    serde_json::from_slice(&call.output).expect("standard SPARQL Results JSON")
}

fn literal(value: &str, datatype: &str) -> Value {
    json!({"type":"literal","value":value,"datatype":format!("{XSD}{datatype}")})
}

#[test]
fn extrema_and_sample_preserve_selected_terms() {
    for (lexical, datatype) in [("01.00", "decimal"), ("003", "integer"), ("1.0e0", "double")] {
        let result = query(&format!(
            "SELECT (MIN(?v) AS ?lo) (MAX(?v) AS ?hi) (SAMPLE(?v) AS ?sample)
             WHERE {{ VALUES ?v {{ \"{lexical}\"^^xsd:{datatype} }} }}"
        ), false);
        for variable in ["lo", "hi", "sample"] {
            assert_eq!(result["results"]["bindings"][0][variable], literal(lexical, datatype));
        }
    }
    let result = query(
        "SELECT (MIN(DISTINCT ?v) AS ?lo) (MAX(DISTINCT ?v) AS ?hi)
         WHERE { VALUES ?v { \"01.00\"^^xsd:decimal \"02.00\"^^xsd:decimal \"01.00\"^^xsd:decimal } }",
        false,
    );
    assert_eq!(result["results"]["bindings"][0]["lo"], literal("01.00", "decimal"));
    assert_eq!(result["results"]["bindings"][0]["hi"], literal("02.00", "decimal"));
}

#[test]
fn conditional_selection_preserves_terms_and_short_circuits() {
    for aggregate in ["", "MIN", "MAX", "SAMPLE"] {
        let expressions = [
            "IF(?choose, ?v, EXISTS { ?s ?p ?o })",
            "COALESCE(?missing, ?v, EXISTS { ?s ?p ?o })",
            "COALESCE(1 / 0, ?v)",
        ];
        let projection = expressions.iter().enumerate().map(|(index, expression)| {
            format!("({aggregate}({expression}) AS ?v{index})")
        }).collect::<Vec<_>>().join(" ");
        let result = query(&format!(
            "SELECT {projection} WHERE {{ VALUES (?v ?choose) {{ (\"01.00\"^^xsd:decimal true) }} }}"
        ), false);
        for index in 0..expressions.len() {
            assert_eq!(result["results"]["bindings"][0][format!("v{index}")], literal("01.00", "decimal"),
                "{aggregate}: {}", expressions[index]);
        }
    }
}

#[test]
fn blank_nodes_share_one_solution_scope() {
    let result = query(
        "SELECT (BNODE(\"has space\") AS ?a) (BNODE(\"has space\") AS ?b) (BNODE(\"\") AS ?c)
         WHERE { VALUES ?row { 1 1 } }", false,
    );
    let rows = result["results"]["bindings"].as_array().unwrap();
    assert_eq!(rows.len(), 2);
    let mut nodes = HashSet::new();
    for row in rows {
        assert_eq!(row["a"], row["b"]);
        for variable in ["a", "c"] {
            assert_eq!(row[variable]["type"], "bnode");
            assert!(nodes.insert(row[variable]["value"].as_str().unwrap()));
        }
    }
    let result = query("SELECT ?s (BNODE(\"x\") AS ?b) WHERE { ?s <urn:p> 1 }", true);
    let row = &result["results"]["bindings"][0];
    assert_eq!(row["b"]["type"], "bnode");
    assert_ne!(row["s"], row["b"]);
}
