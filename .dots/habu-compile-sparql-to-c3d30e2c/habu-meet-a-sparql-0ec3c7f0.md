---
title: Meet a SPARQL term with a native binding by its column
status: closed
priority: 2
issue-type: task
created-at: "2026-11-27T14:34:40.259499+01:00"
closed-at: "2026-11-27T16:30:02.495982+01:00"
---

Problem: src/lib/sparql/emit.f SAME-SQL (:787-800) meets a native binding with a generic one through the native shape's in function, so an object variable that meets a subject compiles to t2.s = knowledge.sparql_id(COALESCE(<seven term functions of t4>)): Zen runs a regular expression per row and PostgreSQL has no column statistics for the join, where t2.s = t4.o_ref answers the same (sparql_id answers NULL for every term but a urn:zen: uri). Acceptance: a meeting of a native binding with a generic one whose columns include one of the native shape compares that column wherever that answers the same; answers unchanged on sparql-test.f and w3c-test.f; nest-opt-64's statement shrinks. Depends: none.
