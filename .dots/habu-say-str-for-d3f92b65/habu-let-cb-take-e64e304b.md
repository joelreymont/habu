---
title: "Let CALLBACK: take a str"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T23:42:17.524328+01:00"
---

Problem: src/lib/ffi-callback.f IN-TOKEN (:307) admits only n, i32, u32, r and ^u8, so a callback whose C caller hands it bytes to read must say ^u8 n: src/lib/net/tls.f:161 CALLBACK: BIO-SEND ( n ^u8 n -- i32 ), whose body SEND-BODY is stored into a [ n ^u8 n -- n ] cell, keeps tls.f marked split-str. FUNCTION: already takes str for an input (src/lib/net/datagram.f). Acceptance: CALLBACK: takes str for a pointer and a length as FUNCTION: does; BIO-SEND and SEND-BODY say str; tls.f carries no split-str. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-cbstr.
