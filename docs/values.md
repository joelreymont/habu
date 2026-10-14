# Scoped typed values

`require src/lib/values.f` provides an opaque append/read collection for one
fixed-cell type. A trusted caller runs source inside `VALUE-SCOPE:WITH`; source
that is admitted only package `VALUES` can use:

```forth
VALUES:MAKE ( n -- VALUES:values<a> )
VALUES:ADD  ( a VALUES:values<a> -- )
VALUES:GET  ( VALUES:values<a> n -- a )
```

`MAKE` reserves a known capacity, including zero. `ADD` appends one typed value;
`GET` reads a zero-based index in the initialized prefix. `GET` never exposes
unused capacity. The native checker preserves the element's nominal type and
rejects wider by-value layouts. The opaque representation, pointer and owner
are inaccessible through the `VALUES` package.

`VALUE-SCOPE:WITH ( R [ R -- S ] -- S )` is for the trusted source loader only.
One scope can be active in a process. It releases all collections after normal
return or throw. A collection returned by the callback becomes stale; `ADD` and
`GET` reject it before touching its former allocation, including while a later
scope is active. The loader must admit only `VALUES` to untrusted source, and
must not admit `VALUE-SCOPE`, raw memory or representation access.

Invalid scope use or a stale handle throws `E-VALUES-STATE`; invalid capacity or
append past capacity throws `E-VALUES-CAPACITY`; an unreadable index throws
`E-VALUES-BOUNDS`. New scopes reject exhausted generation numbers with
`E-VALUES-EPOCH`. Scope storage is process-wide and is intended for a single
source-load process with no concurrent tasks.
