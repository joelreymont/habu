---
title: Connect to PostgreSQL with a password apart from the conninfo
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T19:30:09.724890+01:00"
---

Goal: phil (/tmp/phil-prod1-zen-plan.md section 2): sync and async PG connect words that take the password apart from the conninfo (PQconnectStartParams, expand_dbname=1), for an Entra token as the password; Zen (server.f:219, worker.f:2154) and Maki (src/db.f:72) connect this way. Today PG:CONNECT-START and PG:CONNECT (src/lib/pg.f:1581, :1652) take only a conninfo. Acceptance: a connect with a conninfo and a separate password signs in to the test cluster; the password never appears in a libpq error the word reports. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-pgpw.
