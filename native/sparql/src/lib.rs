//! Standard SPARQL evaluation over a synchronous, caller-owned dataset.
//! The C header defines the ownership and callback protocol.

use oxrdf::{BlankNode, Literal, NamedNode, Term, Variable};
use serde_json::{Map, Value, json};
use spareval::{InternalQuad, QueryEvaluator, QueryResults, QueryableDataset};
use spargebra::{Query, SparqlParser};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ffi::c_void;
use std::fmt;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::rc::Rc;
use std::slice;
use std::str;

type SourceCallback = unsafe extern "C" fn(*mut c_void, *const u8, usize, *mut HbSparqlReply) -> i32;
type Failure = (i32, String);

#[derive(Default)]
struct State {
    query: Option<Query>,
    bindings: HashMap<Variable, Term>,
    result: Vec<u8>,
    error: Vec<u8>,
    dataset: Vec<u8>,
}

/// Opaque caller-owned query. RefCell refuses recursive access during callbacks.
pub struct HbSparql(RefCell<State>);

#[derive(Default)]
pub struct HbSparqlReply {
    bytes: Vec<u8>,
    error: Option<String>,
}

#[derive(Clone, Debug)]
struct SourceError(String);

impl fmt::Display for SourceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
impl std::error::Error for SourceError {}

fn invalid(message: impl Into<String>) -> Failure {
    (4, message.into())
}

// The caller supplies a valid readable span. Null is accepted only for length 0.
unsafe fn input<'a>(pointer: *const u8, length: usize) -> Result<&'a [u8], Failure> {
    if length == 0 {
        return Ok(&[]);
    }
    if pointer.is_null() || length > isize::MAX as usize {
        return Err(invalid("invalid byte span"));
    }
    Ok(unsafe { slice::from_raw_parts(pointer, length) })
}

unsafe fn text<'a>(pointer: *const u8, length: usize) -> Result<&'a str, Failure> {
    str::from_utf8(unsafe { input(pointer, length)? }).map_err(|e| invalid(e.to_string()))
}

fn change(pointer: *mut HbSparql, action: impl FnOnce(&mut State) -> Result<(), Failure>) -> i32 {
    let Some(handle) = (unsafe { pointer.as_ref() }) else { return 4 };
    let Ok(mut state) = handle.0.try_borrow_mut() else { return 4 };
    state.error.clear();
    state.result.clear();
    let outcome = catch_unwind(AssertUnwindSafe(|| action(&mut state)));
    match outcome {
        Ok(Ok(())) => 0,
        Ok(Err((code, message))) => {
            state.result.clear();
            state.error = message.into_bytes();
            code
        }
        Err(_) => {
            state.query = None;
            state.result.clear();
            state.error = b"native SPARQL engine panicked".to_vec();
            5
        }
    }
}

fn term_json(term: &Term) -> Value {
    match term {
        Term::NamedNode(node) => json!({"type":"uri","value":node.as_str()}),
        Term::BlankNode(node) => json!({"type":"bnode","value":node.as_str()}),
        Term::Literal(literal) => {
            let mut value = json!({"type":"literal","value":literal.value()});
            if let Some(language) = literal.language() {
                value["xml:lang"] = json!(language);
            } else if literal.datatype().as_str() != "http://www.w3.org/2001/XMLSchema#string" {
                value["datatype"] = json!(literal.datatype().as_str());
            }
            value
        }
    }
}

fn member<'a>(value: &'a Value, name: &str) -> Result<&'a str, SourceError> {
    value.get(name).and_then(Value::as_str)
        .ok_or_else(|| SourceError(format!("term requires string {name}")))
}

fn parse_term(value: &Value) -> Result<Term, SourceError> {
    let lexical = member(value, "value")?;
    let invalid = |e: String| SourceError(e);
    match member(value, "type")? {
        "uri" => {
            if value.get("xml:lang").is_some() || value.get("datatype").is_some() {
                return Err(SourceError("IRI cannot carry literal metadata".into()));
            }
            NamedNode::new(lexical.to_owned()).map(Into::into).map_err(|e| invalid(e.to_string()))
        }
        "bnode" => {
            if value.get("xml:lang").is_some() || value.get("datatype").is_some() {
                return Err(SourceError("blank node cannot carry literal metadata".into()));
            }
            BlankNode::new(lexical.to_owned()).map(Into::into).map_err(|e| invalid(e.to_string()))
        }
        "literal" => {
            if let Some(language) = value.get("xml:lang") {
                if value.get("datatype").is_some() {
                    return Err(SourceError("literal cannot have both language and datatype".into()));
                }
                let language = language.as_str().ok_or_else(|| invalid("language must be a string".into()))?;
                Literal::new_language_tagged_literal(lexical.to_owned(), language.to_owned())
                    .map(Into::into).map_err(|e| invalid(e.to_string()))
            } else if let Some(datatype) = value.get("datatype") {
                let datatype = datatype.as_str().ok_or_else(|| invalid("datatype must be an IRI string".into()))?;
                let datatype = NamedNode::new(datatype.to_owned()).map_err(|e| invalid(e.to_string()))?;
                if datatype.as_str() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString" {
                    return Err(invalid("rdf:langString requires a language tag".into()));
                }
                Ok(Literal::new_typed_literal(lexical.to_owned(), datatype).into())
            } else {
                Ok(Literal::new_simple_literal(lexical.to_owned()).into())
            }
        }
        other => Err(SourceError(format!("unknown RDF term type {other}"))),
    }
}

fn named_term(value: &Value) -> Result<Term, SourceError> {
    let term = parse_term(value)?;
    if matches!(term, Term::Literal(_)) {
        return Err(SourceError("subject or graph name cannot be a literal".into()));
    }
    Ok(term)
}

#[derive(Clone)]
struct Source {
    context: *mut c_void,
    callback: SourceCallback,
    failure: Rc<RefCell<Option<String>>>,
}

impl Source {
    fn remember<T>(&self, result: Result<T, SourceError>) -> Result<T, SourceError> {
        if let Err(error) = &result {
            self.failure.borrow_mut().get_or_insert_with(|| error.to_string());
        }
        result
    }

    fn call(&self, request: Value) -> Result<Value, SourceError> {
        self.remember((|| {
            if let Some(error) = self.failure.borrow().as_ref() {
                return Err(SourceError(error.clone()));
            }
            let bytes = serde_json::to_vec(&request).map_err(|e| SourceError(e.to_string()))?;
            let mut reply = HbSparqlReply::default();
            let status = unsafe { (self.callback)(self.context, bytes.as_ptr(), bytes.len(), &mut reply) };
            if let Some(error) = reply.error {
                return Err(SourceError(error));
            }
            let value: Value = serde_json::from_slice(&reply.bytes)
                .map_err(|e| SourceError(format!("invalid source reply (status {status}): {e}")))?;
            if status != 0 || value.get("error").is_some() {
                return Err(SourceError(value.get("error").and_then(Value::as_str)
                    .unwrap_or("source callback failed").to_owned()));
            }
            Ok(value)
        })())
    }
}

impl<'a> QueryableDataset<'a> for Source {
    type InternalTerm = Term;
    type Error = SourceError;

    fn internal_quads_for_pattern(
        &self, subject: Option<&Term>, predicate: Option<&Term>, object: Option<&Term>,
        graph_name: Option<Option<&Term>>,
    ) -> impl Iterator<Item = Result<InternalQuad<Term>, SourceError>> + use<'a> {
        let graph = match graph_name {
            Some(None) => json!({"kind":"default"}),
            Some(Some(name)) => json!({"kind":"named","name":term_json(name)}),
            None => json!({"kind":"any"}),
        };
        let result = self.remember((|| {
            let reply = self.call(json!({"op":"scan","s":subject.map(term_json),
                "p":predicate.map(term_json),"o":object.map(term_json),"graph":graph}))?;
            let values = reply.get("quads").and_then(Value::as_array)
                .ok_or_else(|| SourceError("source reply requires quads array".into()))?;
            let mut seen = HashSet::new();
            let mut quads = Vec::new();
            for value in values {
                let s = named_term(&value["s"])?;
                let p = parse_term(&value["p"])?;
                if !matches!(p, Term::NamedNode(_)) {
                    return Err(SourceError("predicate must be an IRI".into()));
                }
                let o = parse_term(&value["o"])?;
                let graph_value = value.get("g")
                    .ok_or_else(|| SourceError("quad requires explicit g (null for default graph)".into()))?;
                let g = if graph_value.is_null() { None } else { Some(named_term(graph_value)?) };
                let matches_graph = match graph_name {
                    Some(None) => g.is_none(),
                    Some(Some(expected)) => g.as_ref() == Some(expected),
                    None => g.is_some(),
                };
                if !matches_graph || subject.is_some_and(|v| v != &s)
                    || predicate.is_some_and(|v| v != &p) || object.is_some_and(|v| v != &o) {
                    return Err(SourceError("source returned a quad outside the requested pattern".into()));
                }
                if seen.insert((s.clone(), p.clone(), o.clone(), g.clone())) {
                    quads.push(InternalQuad { subject:s, predicate:p, object:o, graph_name:g });
                }
            }
            Ok(quads)
        })());
        match result {
            Ok(quads) => quads.into_iter().map(Ok).collect::<Vec<_>>(),
            Err(error) => vec![Err(error)],
        }.into_iter()
    }

    fn internal_named_graphs(&self)
        -> impl Iterator<Item = Result<Term, SourceError>> + use<'a> {
        let result = self.remember((|| {
            let reply = self.call(json!({"op":"graphs"}))?;
            let graphs = reply.get("graphs").and_then(Value::as_array)
                .ok_or_else(|| SourceError("source reply requires graphs array".into()))?;
            graphs.iter().map(named_term).collect::<Result<HashSet<_>, _>>()
        })());
        match result {
            Ok(graphs) => graphs.into_iter().map(Ok).collect::<Vec<_>>(),
            Err(error) => vec![Err(error)],
        }.into_iter()
    }

    fn contains_internal_graph_name(&self, graph_name: &Term) -> Result<bool, SourceError> {
        if matches!(graph_name, Term::Literal(_)) { return Ok(false); }
        self.remember(self.call(json!({"op":"contains","graph":term_json(graph_name)}))?
            .get("contains").and_then(Value::as_bool)
            .ok_or_else(|| SourceError("source reply requires contains boolean".into())))
    }

    fn internalize_term(&self, term: Term) -> Result<Term, SourceError> { Ok(term) }
    fn externalize_term(&self, term: Term) -> Result<Term, SourceError> { Ok(term) }
}

fn encode_results(results: QueryResults<'_>) -> Result<Vec<u8>, Failure> {
    let evaluation = |error: spareval::QueryEvaluationError| (3, error.to_string());
    match results {
        QueryResults::Boolean(boolean) => Ok(json!({"head":{},"boolean":boolean}).to_string().into_bytes()),
        QueryResults::Solutions(solutions) => {
            let vars = solutions.variables().iter().map(|v| v.as_str().to_owned()).collect::<Vec<_>>();
            let mut rows = Vec::new();
            for solution in solutions {
                let solution = solution.map_err(evaluation)?;
                let row: Map<String, Value> = solution.iter()
                    .map(|(variable, term)| (variable.as_str().to_owned(), term_json(term))).collect();
                rows.push(row);
            }
            Ok(json!({"head":{"vars":vars},"results":{"bindings":rows}}).to_string().into_bytes())
        }
        QueryResults::Graph(triples) => {
            let mut seen = HashSet::new();
            let mut result = String::new();
            for triple in triples {
                let triple = triple.map_err(evaluation)?;
                if seen.insert(triple.clone()) {
                    result.push_str(&format!("{triple} .\n"));
                }
            }
            Ok(result.into_bytes())
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn hb_sparql_new() -> *mut HbSparql {
    Box::into_raw(Box::new(HbSparql(RefCell::new(State::default()))))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hb_sparql_free(pointer: *mut HbSparql) {
    if pointer.is_null() { return; }
    // Freeing an active query is a caller error. Keep it alive through callback unwind.
    if unsafe { &*pointer }.0.try_borrow_mut().is_err() { return; }
    drop(unsafe { Box::from_raw(pointer) });
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hb_sparql_parse(
    pointer: *mut HbSparql, source: *const u8, length: usize, base: *const u8, base_length: usize,
) -> i32 {
    change(pointer, |state| {
        state.query = None;
        state.bindings.clear();
        state.dataset.clear();
        let source = unsafe { text(source, length)? };
        let base = unsafe { text(base, base_length)? };
        let mut parser = SparqlParser::new();
        if !base.is_empty() {
            parser = parser.with_base_iri(base).map_err(|e| invalid(e.to_string()))?;
        }
        let query = parser.parse_query(source).map_err(|e| (1, e.to_string()))?;
        let dataset = match query.dataset() {
            None => Value::Null,
            Some(dataset) => json!({
                "default":dataset.default.iter().map(|n| n.as_str()).collect::<Vec<_>>(),
                "named":dataset.named.as_ref().map(|names| names.iter().map(|n| n.as_str()).collect::<Vec<_>>()).unwrap_or_default(),
            }),
        };
        state.dataset = dataset.to_string().into_bytes();
        state.query = Some(query);
        Ok(())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hb_sparql_form(pointer: *const HbSparql) -> i32 {
    let Some(handle) = (unsafe { pointer.as_ref() }) else { return 0 };
    let Ok(state) = handle.0.try_borrow() else { return 0 };
    match state.query {
        Some(Query::Select(_)) => 1,
        Some(Query::Ask(_)) => 2,
        Some(Query::Construct(_)) => 3,
        Some(Query::Describe(_)) => 4,
        None => 0,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hb_sparql_bind(
    pointer: *mut HbSparql, name: *const u8, name_length: usize, value: *const u8, value_length: usize,
) -> i32 {
    change(pointer, |state| {
        if state.query.is_none() { return Err((6, "no parsed query".into())); }
        let name = unsafe { text(name, name_length)? };
        let variable = Variable::new(name.to_owned()).map_err(|e| invalid(e.to_string()))?;
        let value: Value = serde_json::from_slice(unsafe { input(value, value_length)? })
            .map_err(|e| invalid(e.to_string()))?;
        let term = parse_term(&value).map_err(|e| invalid(e.to_string()))?;
        state.bindings.insert(variable, term);
        Ok(())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hb_sparql_query(
    pointer: *mut HbSparql, context: *mut c_void, callback: Option<SourceCallback>,
) -> i32 {
    change(pointer, |state| {
        let query = state.query.as_ref().ok_or((6, "no parsed query".into()))?;
        let callback = callback.ok_or_else(|| invalid("missing dataset source"))?;
        let source = Source { context, callback, failure:Rc::new(RefCell::new(None)) };
        let failure = source.failure.clone();
        let evaluator = QueryEvaluator::new();
        let mut prepared = evaluator.prepare(query);
        for (variable, term) in &state.bindings {
            prepared = prepared.substitute_variable(variable.clone(), term.clone());
        }
        let results = prepared.execute(source).map_err(|e| (3, e.to_string())).and_then(encode_results);
        if let Some(error) = failure.borrow_mut().take() { return Err((2, error)); }
        state.result = results?;
        Ok(())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hb_sparql_reply_set(pointer: *mut HbSparqlReply, data: *const u8, length: usize) {
    let Some(reply) = (unsafe { pointer.as_mut() }) else { return };
    let result = catch_unwind(AssertUnwindSafe(|| unsafe { input(data, length) }.map(<[u8]>::to_vec)));
    match result {
        Ok(Ok(bytes)) => { reply.bytes = bytes; reply.error = None; }
        Ok(Err((_, message))) => reply.error = Some(message),
        Err(_) => reply.error = Some("native panic copying source reply".into()),
    }
}

macro_rules! view {
    ($pointer:ident, $length:ident, $field:ident) => {
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $pointer(pointer: *const HbSparql) -> *const u8 {
            let Some(handle) = (unsafe { pointer.as_ref() }) else { return std::ptr::null() };
            let Ok(state) = handle.0.try_borrow() else { return std::ptr::null() };
            state.$field.as_ptr()
        }
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $length(pointer: *const HbSparql) -> usize {
            let Some(handle) = (unsafe { pointer.as_ref() }) else { return 0 };
            let Ok(state) = handle.0.try_borrow() else { return 0 };
            state.$field.len()
        }
    }
}
view!(hb_sparql_result_ptr, hb_sparql_result_len, result);
view!(hb_sparql_error_ptr, hb_sparql_error_len, error);
view!(hb_sparql_dataset_ptr, hb_sparql_dataset_len, dataset);
