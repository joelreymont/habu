//! Pull one bounded source page when the evaluator needs another item.
use crate::{Control, Failure, HbSparqlReply, Read, Result};
use crate::terms::{named_term, parse_term, term_json};
use oxrdf::Term;
use serde_json::{Value, json};
use spareval::{InternalQuad, QueryableDataset};
use std::ffi::c_void;
use std::rc::Rc;

const PAGE_SIZE: usize = 256;
fn bad(message: impl Into<String>) -> Failure { Failure(2, message.into()) }

#[derive(Clone)]
pub(crate) struct Source {
    pub context: *mut c_void,
    pub read: Read,
    pub control: Rc<Control>,
}

impl Source {
    fn call(&self, request: &Value) -> Result<Value> {
        self.control.remember((|| {
            self.control.check()?;
            let bytes = serde_json::to_vec(request).map_err(|e| bad(e.to_string()))?;
            let mut reply = HbSparqlReply {
                bytes: Vec::new(), max_bytes: self.control.max_bytes, failure: None,
            };
            let status = unsafe { (self.read)(self.context, bytes.as_ptr(), bytes.len(), &mut reply) };
            self.control.check()?;
            if let Some(error) = reply.failure { return Err(error); }
            let value: Value = serde_json::from_slice(&reply.bytes)
                .map_err(|e| bad(format!("invalid source reply (status {status}): {e}")))?;
            if status != 0 || value.get("error").is_some() {
                return Err(bad(value.get("error").and_then(Value::as_str).unwrap_or("source callback failed")));
            }
            Ok(value)
        })())
    }

    fn pages(&self, request: Value, field: &'static str) -> Pages {
        Pages { source: self.clone(), request, field, items: Vec::new().into_iter(), done: false }
    }
}

struct Pages {
    source: Source,
    request: Value,
    field: &'static str,
    items: std::vec::IntoIter<Value>,
    done: bool,
}

impl Pages {
    fn fetch(&mut self) -> Result<()> {
        let remaining = self.source.control.max_scan_rows - self.source.control.scanned.get();
        if remaining == 0 { return Err(Failure(6, "source read limit exceeded".into())); }
        let limit = PAGE_SIZE.min(remaining);
        self.request["limit"] = json!(limit);
        let mut reply = self.source.call(&self.request)?;
        let next = reply.get("next").ok_or_else(|| bad("source page requires next cursor or null"))?;
        if !next.is_null() && (next.as_str().is_none_or(str::is_empty) || *next == self.request["cursor"]) {
            return Err(bad("source cursor must advance"));
        }
        let next = next.clone();
        let Some(Value::Array(items)) = reply.get_mut(self.field).map(Value::take) else {
            return Err(bad(format!("source page requires {} array", self.field)));
        };
        if items.len() > limit || (items.is_empty() && !next.is_null()) {
            return Err(bad("invalid source page length"));
        }
        self.source.control.scan(items.len().max(1))?;
        self.done = next.is_null();
        self.request["cursor"] = next;
        self.items = items.into_iter();
        Ok(())
    }
}

impl Iterator for Pages {
    type Item = Result<Value>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.items.next() {
            return Some(self.source.control.check().map(|()| item));
        }
        if self.done { return None; }
        if let Err(error) = self.fetch() {
            self.done = true;
            return Some(self.source.control.remember(Err(error)));
        }
        self.items.next().map(Ok)
    }
}

struct Pattern {
    subject: Option<Term>,
    predicate: Option<Term>,
    object: Option<Term>,
    graph: Option<Option<Term>>,
}

impl Pattern {
    fn quad(&self, value: Value) -> Result<InternalQuad<Term>> {
        let subject = named_term(&value["s"])?;
        let predicate = parse_term(&value["p"])?;
        if !matches!(predicate, Term::NamedNode(_)) { return Err(bad("predicate must be an IRI")); }
        let object = parse_term(&value["o"])?;
        let graph = value.get("g").ok_or_else(|| bad("quad requires explicit g (null for default graph)"))?;
        let graph_name = if graph.is_null() { None } else { Some(named_term(graph)?) };
        let matches_graph = match &self.graph {
            Some(None) => graph_name.is_none(),
            Some(Some(expected)) => graph_name.as_ref() == Some(expected),
            None => graph_name.is_some(),
        };
        if !matches_graph || self.subject.as_ref().is_some_and(|v| v != &subject)
            || self.predicate.as_ref().is_some_and(|v| v != &predicate)
            || self.object.as_ref().is_some_and(|v| v != &object) {
            return Err(bad("source returned a quad outside the requested pattern"));
        }
        Ok(InternalQuad { subject, predicate, object, graph_name })
    }
}

impl<'a> QueryableDataset<'a> for Source {
    type InternalTerm = Term;
    type Error = Failure;

    fn internal_quads_for_pattern(
        &self, subject: Option<&Term>, predicate: Option<&Term>, object: Option<&Term>,
        graph: Option<Option<&Term>>,
    ) -> impl Iterator<Item = Result<InternalQuad<Term>>> + use<'a> {
        let selection = match graph {
            Some(None) => json!({"kind":"default"}),
            Some(Some(name)) => json!({"kind":"named","name":term_json(name)}),
            None => json!({"kind":"any"}),
        };
        let pages = self.pages(json!({"op":"scan","s":subject.map(term_json),
            "p":predicate.map(term_json),"o":object.map(term_json),"graph":selection,"cursor":null}), "quads");
        let pattern = Pattern { subject:subject.cloned(), predicate:predicate.cloned(),
            object:object.cloned(), graph:graph.map(|v| v.cloned()) };
        let control = self.control.clone();
        pages.map(move |item| control.remember(item.and_then(|v| pattern.quad(v))))
    }

    fn internal_named_graphs(&self) -> impl Iterator<Item = Result<Term>> + use<'a> {
        let control = self.control.clone();
        self.pages(json!({"op":"graphs","cursor":null}), "graphs")
            .map(move |item| control.remember(item.and_then(|v| named_term(&v))))
    }

    fn contains_internal_graph_name(&self, graph: &Term) -> Result<bool> {
        if matches!(graph, Term::Literal(_)) { return Ok(false); }
        self.control.scan(1)?;
        let reply = self.call(&json!({"op":"contains","graph":term_json(graph)}))?;
        self.control.remember(reply.get("contains").and_then(Value::as_bool)
            .ok_or_else(|| bad("source reply requires contains boolean")))
    }

    fn internalize_term(&self, term: Term) -> Result<Term> { Ok(term) }
    fn externalize_term(&self, term: Term) -> Result<Term> { Ok(term) }
}
