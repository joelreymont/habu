---
title: Write each WHERE condition only when not implied
status: closed
priority: 2
issue-type: task
created-at: "2026-11-28T03:51:50.081874+01:00"
closed-at: "2026-11-28T09:48:09.674815+01:00"
---

Problem: src/lib/sparql/emit.f's header (about 88-97) rules no IS NOT NULL where a strict condition of the same WHERE reads the column, but whether a null test is written is a flag on each binding, cleared only in the lists the code writing a strict condition holds (COLUMN-TESTED, OCCUR's answer through SLOTS-SQL). Three review rounds found leaks: a unique-subject source with two triples on one kept row inside an EXISTS keeps t2.c IS NOT NULL beside t2.c = t1.id; SELECT ?y WHERE { ?y t:99 ?z . ?y t:99 ?w } writes t1.id IS NOT NULL twice (COVERED? at about 1846 ignores the kept row's subject when first is 1); a maybe-bound outer variable in an EXISTS writes t4.id IS NOT NULL twice (VAR-COVERS? about 1823); an outer variable in two slots on one column writes t2.id = t1.id twice. Acceptance: no compiled WHERE holds a null test of a column one of its strict conditions reads, nor any condition twice; sparql-test.f covers each case above; answers unchanged; the shapes dump changes only by such removals; emit-test.f's 1 MiB bound holds. Depends: none.
