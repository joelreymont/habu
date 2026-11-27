---
title: "Bind a SPARQL fact's value to its predicate's column"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-27T06:36:43.324500+01:00"
closed-at: "2026-11-27T09:49:00.076135+01:00"
---

Problem: a constant predicate over a cols slot (Zen's fact value, seven columns) leaves every column alive, so its value is COALESCEd and generic: SUM/AVG/MIN/MAX and comparisons over facts never go native. Design: a cols entry may carry "only": {"col": k, "set": name} (member name ruled by Joel 2026-11-27: only), the host's promise that a row whose column k holds an IRI some entry's set lists has its value only in entries whose set lists it; an IRI no set lists is unrestricted. A compile-time pass in GROUP before PRUNE kills the choices that promise excludes, for ?x P ?w and for two triples sharing one unique-subject row (?f rdf:predicate P; rdf:object ?w). Acceptance: on the test host's typed source, answers equal with and without only; one column and no COALESCE for both forms; statement unchanged for a variable predicate; an IRI in two sets keeps two columns; an unlisted IRI keeps all; store-test rejects only on a plain column slot and an unknown set. Depends: none.
