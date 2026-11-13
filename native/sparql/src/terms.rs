//! RDF terms at the JSON callback boundary.
use oxrdf::{BlankNode, Literal, NamedNode, Term};
use serde_json::{Value, json};
use crate::Failure;

fn source_error(message: impl Into<String>) -> Failure { Failure(2, message.into()) }

pub(crate) fn term_json(term: &Term) -> Value {
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

fn member<'a>(value: &'a Value, name: &str) -> Result<&'a str, Failure> {
    value.get(name).and_then(Value::as_str)
        .ok_or_else(|| source_error(format!("term requires string {name}")))
}

pub(crate) fn parse_term(value: &Value) -> Result<Term, Failure> {
    let lexical = member(value, "value")?;
    let invalid = |e: String| source_error(e);
    match member(value, "type")? {
        "uri" => {
            if value.get("xml:lang").is_some() || value.get("datatype").is_some() {
                return Err(source_error("IRI cannot carry literal metadata"));
            }
            NamedNode::new(lexical.to_owned()).map(Into::into).map_err(|e| invalid(e.to_string()))
        }
        "bnode" => {
            if value.get("xml:lang").is_some() || value.get("datatype").is_some() {
                return Err(source_error("blank node cannot carry literal metadata"));
            }
            BlankNode::new(lexical.to_owned()).map(Into::into).map_err(|e| invalid(e.to_string()))
        }
        "literal" => {
            if let Some(language) = value.get("xml:lang") {
                if value.get("datatype").is_some() {
                    return Err(source_error("literal cannot have both language and datatype"));
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
        other => Err(source_error(format!("unknown RDF term type {other}"))),
    }
}

pub(crate) fn named_term(value: &Value) -> Result<Term, Failure> {
    let term = parse_term(value)?;
    if matches!(term, Term::Literal(_)) {
        return Err(source_error("subject or graph name cannot be a literal"));
    }
    Ok(term)
}

