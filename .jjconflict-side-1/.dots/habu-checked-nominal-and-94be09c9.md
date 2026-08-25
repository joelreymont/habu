---
title: Bind linear handles atomically
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:44:49.846693+02:00"
---

Problem: `CAST:` can currently mention a `DEFLINEAR` type, so any package can
forge, dismantle, or retype a use-once owner. Merely restricting `CAST:` to the
type owner is still unsound: the owner could declare acquire through `ptr u8`
and take through `ptr cell`, laundering the pointee class. BPE and MODEL-ASSET
must not replace their lifetime trust with independently declared casts.

Result: the checker rejects every `CAST:` whose input or output transitively
contains linear ownership, including linear-to-linear casts. Add one fixed
top-level declarer which may run only in the owning package's private wordlist:

```
LINEAR-HANDLE BPE:builder AS ptr a
   ACQUIRE MINT-BUILDER
   BORROW BUILDER>BLOCK
   TAKE TAKE-BUILDER
;LINEAR-HANDLE
```

The type is an existing `DEFLINEAR` owned by the current package. The raw
representation is exactly one of `n`, `ptr u8`, or `ptr a` and becomes one
immutable fact for that type. The declarer accepts exactly one complete
ACQUIRE/BORROW/TAKE triple, no user effects, bodies, or aliases, and generates
fixed effects `( raw -- owner )`, `( owner -- owner raw )`, and
`( owner -- raw )`. It preflights the type owner, private destination names,
duplicate binding, representation grammar, and complete block before publishing
anything; failure leaves no type binding or word. A second binding for the same
type rejects. Existing abstract `DEFLINEAR` remains valid without a raw bridge.

Owner: `src/core/roles.f` owns declaration syntax and publication;
`src/core/checker.f` owns transitive-linear `CAST:` refusal and the single
type-to-representation authority record; native/source verification must accept
the complete caller chain. BPE builder-to-state conversion composes generated
TAKE then ACQUIRE over their common `ptr a` representation; it is never a
linear cast. Current landed `CAST:` definitions are non-linear role conversions,
so the hard cut requires no compatibility path.

Acceptance: a foreign package cannot acquire, borrow, take, or cast another
package's linear type; the owner cannot declare mismatched raw representations;
linear-to-linear and nested-linear casts reject; public/global declaration,
missing, duplicate, reordered, user-bodied, and partially failing blocks publish
nothing; generated borrow preserves the exact owner and cannot enable
dup/drop/take/reacquire; `n`, `ptr u8`, and `ptr a` representations work; the
landed non-linear cast suite is unchanged. Use the real declaration and checker
load paths, with a production foreign-package forge as the red. No `TRUSTED:`,
generic effect declarer, exact-effect escape hatch, ABI version, compatibility
alias, manifest, lint, or framework.

First publishable result is the declarer, authority record, universal cast ban,
and focused tests. The dot remains open until the 18 PTY mint/erase bridges in
`lib/process-pty-handle.f` migrate coherently; BPE and MODEL-ASSET migrate under
their own active product dots. Smallest owning checks: `test/cast-negative-suite.f`
and the focused linear-handle declaration suite.

Landed subset: universal may-linear `CAST:` rejection and rollback proof at `a36d1700287c860f7d728d73062fca35ef64aea9`. The remaining `LINEAR-HANDLE` and PTY migration work is unclaimed.

Claim: unassigned (RELEASED 2026-08-21: leaf carried status active with no claim line at all, and no live lane owns it - gc)
