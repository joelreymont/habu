---
title: Expose fresh curl connections
status: closed
priority: 1
issue-type: task
created-at: "2026-10-16T08:47:51.007921+03:00"
closed-at: "2026-10-16T09:12:12.004273+03:00"
close-reason: "Expose CURL:FRESH-CONNECT! through CURLOPT_FRESH_CONNECT74; independent Astra review CLEAR after peer synchronization repair. Public600 engine source suite passes, default reuse2POST/fresh1; stripped native consumer builds and passes2freshprocesses with true/false option and realHTTP200 bodies. No engine change; native/source qualification uses exact600compatible delta."
---

Tender OSCE local reproducer /Users/joel/.cache/tmp/libcurl-post-retry-repro-20261016.py: GET/form then one POST on same CURL multi loop can reuse a keepalive connection, server consumes body and closes with no response, libcurl retriesPOST internally so server observes2posts. Public600eb464 lacks CURLOPT_FRESH_CONNECT even on fresh easyhandle. Add per-handle CURL:FRESH-CONNECT! ( handle bool -- status ) using existing setopt long boundary, option CURLOPT_FRESH_CONNECT74, normal lease/lifecycle rules, no globalpolicy or Tender workaround. Libcurl docs https://curl.se/libcurl/c/CURLOPT_FRESH_CONNECT.html define force new nexttransfer when1, default reusewhen0. This prevents this reused-connection retry scenario, not a blanket exactly-once delivery guarantee. Before code select existing CURL realnetwork tests and add distinct local consumed-body/no-response regression where practical; preserve standard GET/reuse controls, source/native consumer and independentAstrareview. Library-only qualified source release may ship without waiting for CELL-BODY engine batch; no fullTender serverbuild.
