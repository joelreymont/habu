//! One SPARQL call: a paged RDF source and a streamed result.
mod source;
mod terms;

use oxrdf::Variable;
use serde_json::{Map, Value, json};
use spareval::{QueryEvaluator, QueryResults};
use spargebra::SparqlParser;
use std::cell::{Cell, RefCell};
use std::collections::HashSet;
use std::ffi::c_void;
use std::fmt;
use std::io::{self, Write};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::rc::Rc;
use std::time::{Duration, Instant};
use terms::{parse_term, term_json};

type Read = unsafe extern "C" fn(*mut c_void, *const u8, usize, *mut HbSparqlReply) -> i32;
type WriteCallback = unsafe extern "C" fn(*mut c_void, i32, *const u8, usize) -> i32;
type Result<T> = std::result::Result<T, Failure>;

#[derive(Clone, Debug)]
struct Failure(i32, String);
impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.1.fmt(f) }
}
impl std::error::Error for Failure {}
fn invalid(message: impl Into<String>) -> Failure { Failure(4, message.into()) }

struct Control {
    deadline: Instant,
    max_rows: usize,
    max_bytes: usize,
    max_scan_rows: usize,
    scanned: Cell<usize>,
    failure: RefCell<Option<Failure>>,
}

impl Control {
    fn new(request: &Value) -> Result<Self> {
        let number = |name: &str| -> Result<usize> {
            request.get(name).and_then(Value::as_u64)
                .and_then(|v| usize::try_from(v).ok()).filter(|v| *v > 0)
                .ok_or_else(|| invalid(format!("{name} must be a positive integer")))
        };
        let timeout = number("timeout_ms")?;
        Ok(Self {
            deadline: Instant::now().checked_add(Duration::from_millis(timeout as u64))
                .ok_or_else(|| invalid("timeout_ms is too large"))?,
            max_rows: number("max_rows")?, max_bytes: number("max_bytes")?,
            max_scan_rows: number("max_scan_rows")?, scanned: Cell::new(0),
            failure: RefCell::new(None),
        })
    }

    fn remember<T>(&self, result: Result<T>) -> Result<T> {
        if let Err(error) = &result {
            self.failure.borrow_mut().get_or_insert_with(|| error.clone());
        }
        result
    }

    fn check(&self) -> Result<()> {
        if let Some(error) = self.failure.borrow().as_ref() { return Err(error.clone()); }
        if Instant::now() >= self.deadline {
            return self.remember(Err(Failure(6, "query deadline exceeded".into())));
        }
        Ok(())
    }

    fn scan(&self, count: usize) -> Result<()> {
        self.check()?;
        let total = self.scanned.get().checked_add(count)
            .filter(|v| *v <= self.max_scan_rows)
            .ok_or_else(|| Failure(6, "source read limit exceeded".into()));
        self.scanned.set(self.remember(total)?);
        Ok(())
    }
}

/// Borrowed by one source callback; never retained or freed by the caller.
pub struct HbSparqlReply {
    bytes: Vec<u8>,
    max_bytes: usize,
    failure: Option<Failure>,
}

// A nonempty span must point to readable memory for the duration of the call.
unsafe fn input<'a>(pointer: *const u8, length: usize) -> Result<&'a [u8]> {
    if length == 0 { return Ok(&[]); }
    if pointer.is_null() || length > isize::MAX as usize { return Err(invalid("invalid byte span")); }
    Ok(unsafe { std::slice::from_raw_parts(pointer, length) })
}

struct Output {
    context: *mut c_void,
    write: WriteCallback,
    control: Rc<Control>,
    bytes: usize,
}

impl Output {
    fn send(&mut self, bytes: &[u8]) -> Result<()> {
        self.control.check()?;
        let result = (|| {
            let total = self.bytes.checked_add(bytes.len()).filter(|v| *v <= self.control.max_bytes)
                .ok_or_else(|| Failure(6, "result byte limit exceeded".into()))?;
            if unsafe { (self.write)(self.context, 0, bytes.as_ptr(), bytes.len()) } != 0 {
                return Err(Failure(2, "result callback stopped the query".into()));
            }
            self.bytes = total;
            self.control.check()
        })();
        self.control.remember(result)
    }

    fn json(&mut self, value: &Value) -> Result<()> {
        serde_json::to_writer(&mut *self, value)
            .map_err(|e| self.control.failure.borrow().clone().unwrap_or(Failure(3, e.to_string())))
    }

    fn row(&self, number: usize) -> Result<()> {
        self.control.check()?;
        if number > self.control.max_rows {
            return self.control.remember(Err(Failure(6, "result row limit exceeded".into())));
        }
        Ok(())
    }
}

impl Write for Output {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        self.send(bytes).map(|()| bytes.len()).map_err(io::Error::other)
    }
    fn flush(&mut self) -> io::Result<()> { Ok(()) }
}

fn emit(results: QueryResults<'_>, output: &mut Output) -> Result<()> {
    let evaluation = |e: spareval::QueryEvaluationError| Failure(3, e.to_string());
    match results {
        QueryResults::Boolean(value) => output.json(&json!({"head":{},"boolean":value})),
        QueryResults::Solutions(rows) => {
            output.send(b"{\"head\":")?;
            output.json(&json!({"vars":rows.variables().iter().map(|v| v.as_str()).collect::<Vec<_>>()}))?;
            output.send(b",\"results\":{\"bindings\":[")?;
            for (index, row) in rows.enumerate() {
                output.row(index + 1)?;
                let row = row.map_err(evaluation)?;
                if index != 0 { output.send(b",")?; }
                let row: Map<String, Value> = row.iter()
                    .map(|(v, term)| (v.as_str().to_owned(), term_json(term))).collect();
                output.json(&Value::Object(row))?;
            }
            output.send(b"]}}")
        }
        QueryResults::Graph(triples) => {
            let mut seen = HashSet::new();
            for triple in triples {
                output.control.check()?;
                let triple = triple.map_err(evaluation)?;
                if seen.insert(triple.clone()) {
                    output.row(seen.len())?;
                    output.send(format!("{triple} .\n").as_bytes())?;
                }
            }
            Ok(())
        }
    }
}

fn run(request: &[u8], context: *mut c_void, read: Read, write: WriteCallback) -> Result<()> {
    let request: Value = serde_json::from_slice(request).map_err(|e| invalid(e.to_string()))?;
    let control = Rc::new(Control::new(&request)?);
    let query = request.get("query").and_then(Value::as_str).ok_or_else(|| invalid("query must be a string"))?;
    if query.len() > control.max_bytes { return Err(Failure(6, "query byte limit exceeded".into())); }
    let mut parser = SparqlParser::new();
    if let Some(base) = request.get("base_iri") {
        parser = parser.with_base_iri(base.as_str().ok_or_else(|| invalid("base_iri must be a string"))?)
            .map_err(|e| invalid(e.to_string()))?;
    }
    let query = parser.parse_query(query).map_err(|e| Failure(1, e.to_string()))?;
    control.check()?;
    let evaluator = QueryEvaluator::new();
    let mut prepared = evaluator.prepare(&query);
    if let Some(bindings) = request.get("bindings") {
        for (name, value) in bindings.as_object().ok_or_else(|| invalid("bindings must be an object"))? {
            let variable = Variable::new(name.to_owned()).map_err(|e| invalid(e.to_string()))?;
            let term = parse_term(value).map_err(|e| invalid(e.1))?;
            prepared = prepared.substitute_variable(variable, term);
        }
    }
    let source = source::Source { context, read, control: control.clone() };
    let mut output = Output { context, write, control: control.clone(), bytes: 0 };
    let outcome = prepared.execute(source).map_err(|e| Failure(3, e.to_string()))
        .and_then(|results| emit(results, &mut output));
    // SPARQL expression errors can be swallowed by EXISTS/OPTIONAL. Source
    // failures and resource limits must still fail the whole query.
    if let Some(error) = control.failure.borrow().as_ref() { return Err(error.clone()); }
    outcome?;
    control.check()
}

/// One synchronous call. On failure, discard all status-0 output chunks.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hb_sparql_run(
    request: *const u8, length: usize, context: *mut c_void,
    read: Option<Read>, write: Option<WriteCallback>,
) -> i32 {
    let Some(write) = write else { return 4 };
    let outcome = catch_unwind(AssertUnwindSafe(|| {
        run(unsafe { input(request, length)? }, context,
            read.ok_or_else(|| invalid("missing source callback"))?, write)
    })).unwrap_or_else(|_| Err(Failure(5, "native SPARQL engine panicked".into())));
    match outcome {
        Ok(()) => 0,
        Err(Failure(code, message)) => {
            unsafe { write(context, code, message.as_ptr(), message.len()); }
            code
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hb_sparql_reply_set(reply: *mut HbSparqlReply, data: *const u8, length: usize) {
    let Some(reply) = (unsafe { reply.as_mut() }) else { return };
    if reply.failure.is_some() { return; }
    let outcome = catch_unwind(AssertUnwindSafe(|| {
        if length > reply.max_bytes { return Err(Failure(6, "source page byte limit exceeded".into())); }
        Ok(unsafe { input(data, length)? }.to_vec())
    })).unwrap_or_else(|_| Err(Failure(5, "native panic copying source reply".into())));
    match outcome {
        Ok(bytes) => reply.bytes = bytes,
        Err(error) => reply.failure = Some(error),
    }
}
