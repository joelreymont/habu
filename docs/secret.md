# Desktop credentials

`require src/lib/secret.f` exposes `SECRET:READ ( service$ account$ destination -- n found? )`.
`destination` is a caller-owned `SPAN:span<u8>`. A found item writes exactly `n`
bytes and returns `true`, including `0 true` for an empty value. A missing item
returns `0 false`. The destination is unchanged on a miss or failure. The caller
owns the returned bytes and their lifetime. No key material enters diagnostics.

The macOS backend selects a generic password by its service and account and
copies `kSecReturnData` from [SecItemCopyMatching](https://developer.apple.com/documentation/security/secitemcopymatching(_:_:)).
The Linux backend searches the Secret Service by `service` and `account` attributes.
It uses [libsecret's item search](https://gnome.pages.gitlab.gnome.org/libsecret/method.Service.search_sync.html)
so a matching item whose unlock is dismissed reports an access failure. It reads
the returned [SecretValue](https://gnome.pages.gitlab.gnome.org/libsecret/method.Value.get.html)
by length to preserve embedded NUL bytes. These calls are synchronous and may
block while the desktop store responds.

Empty, NUL-containing, or malformed UTF-8 identifiers fail with `SECRET:E-CONFIG`. A failed store
access throws `SECRET:E-ACCESS`; unavailable native support or macOS
`errSecNotAvailable` throws `SECRET:E-UNAVAILABLE`. A value larger than the destination throws
`SECRET:E-CAPACITY` before copying any bytes. There is no plaintext fallback.

The end-to-end fixture in `src/lib/secret-test.f` writes unique dummy items to
the actual OS store, exercises lookup and failure behavior, then removes them.
Run it with `bin/hb --load src/lib/secret-test.f`. The same fixture can run from
a saved application image after loading `src/lib/secret.f` into that image.
