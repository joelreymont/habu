---- MODULE HttpcTable ----
\* The lines of LIFECYCLE, src/lib/net/httpc.f, as <<state, input,
\* next state>>. A test fails unless this file is that table.
Lines == {
    <<"starting", "marked", "down">>,
    <<"starting", "none", "resolving">>,
    <<"starting", "idle", "probing">>,
    <<"starting", "busy", "resolving">>,
    <<"probing", "quiet", "sending">>,
    <<"probing", "stale", "resolving">>,
    <<"resolving", "found", "connecting">>,
    <<"resolving", "failed", "down">>,
    <<"connecting", "up", "greeting">>,
    <<"connecting", "next", "connecting">>,
    <<"connecting", "failed", "down">>,
    <<"greeting", "up", "sending">>,
    <<"greeting", "failed", "down">>,
    <<"sending", "sent", "sent">>,
    <<"sending", "failed", "down">>,
    <<"heading", "interim", "heading">>,
    <<"heading", "head", "reading">>,
    <<"heading", "failed", "down">>,
    <<"reading", "some", "partway">>,
    <<"reading", "end", "settling">>,
    <<"reading", "failed", "down">>,
    <<"settling", "keep", "idle">>,
    <<"settling", "shut", "closed">>
}
====
