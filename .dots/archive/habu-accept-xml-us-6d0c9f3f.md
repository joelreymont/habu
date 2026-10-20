---
title: Accept XML US-ASCII wire encoding
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T04:53:37.534331+03:00"
closed-at: "2026-10-15T05:12:52.135370+03:00"
close-reason: Accept declared US-ASCII XML at whole-source, legacy and chunked boundaries; retain exact owner byte ranges and reject non-ASCII wire/output without partial writes. Independent Astra and lead review clear. XML source, reader and roundtrip tests, installed owner E2E source/native, Office baseline/ASCII pair and unchanged TRAIN 536610 source/stripped/REPL all pass on unchanged e440 engine and matching 78e2 map. Exact source-only release installed at b685c051-xml-zip-ascii-1f0b72ec.
---

Valid XML customXml parts declared encoding=us-ascii fail XML:E-ENCODING -9214; public Office pair in ~/.cache/tmp/tender-office-encoding-repro differs only in declaration. Accept case-insensitive US-ASCII at source and reader boundaries, reject literal non-ASCII bytes in source/legacy/chunked input, preserve owner coordinates and reject non-ASCII output without writes. Verify through existing XML/source/owner consumers, public repros, and available gates; document XML 1.0 §4.3.3 behavior.
