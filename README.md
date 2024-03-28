xxhash-ffi
==========

[![Hackage](https://img.shields.io/hackage/v/xxhash-ffi.svg?maxAge=2592000)](https://hackage.haskell.org/package/xxhash-ffi)
[![Stackage Nightly](http://stackage.org/package/xxhash-ffi/badge/nightly)](http://stackage.org/nightly/package/xxhash-ffi)
[![Stackage LTS](http://stackage.org/package/xxhash-ffi/badge/lts)](http://stackage.org/lts/package/xxhash-ffi)

Haskell bindings and high-level helpers for [xxHash](https://xxhash.com) family
of extremely fast non-cryptographic hash functions.

Benchmarks against `hashable-1.4.3.0` on `x86_64`:

```
10B
  Data.Digest.XXHash.FFI.XXH3:
    20.2 ns ± 692 ps
  Data.Hashable:
    20.3 ns ± 722 ps
1kB
  Data.Digest.XXHash.FFI.XXH3:
    80.8 ns ± 2.0 ns
  Data.Hashable:
    1.27 μs ±  16 ns
4kB
  Data.Digest.XXHash.FFI.XXH3:
    236  ns ± 7.9 ns
  Data.Hashable:
    5.09 μs ± 135 ns
1MB
  Data.Digest.XXHash.FFI.XXH3:
    138  μs ± 3.4 μs
  Data.Hashable:
    3.19 ms ±  60 μs
```

Benchmarks against `hashable-1.4.3.0` on `aarch64`:

```
10B
  Data.Digest.XXHash.FFI.XXH3:
    10.2 ns ± 208 ps
  Data.Hashable:
    12.1 ns ± 404 ps
1kB
  Data.Digest.XXHash.FFI.XXH3:
    71.7 ns ± 1.8 ns
  Data.Hashable:
    1.14 μs ±  27 ns
4kB
  Data.Digest.XXHash.FFI.XXH3:
    289  ns ± 7.0 ns
  Data.Hashable:
    4.71 μs ± 187 ns
1MB
  Data.Digest.XXHash.FFI.XXH3:
    175  μs ± 6.9 μs
  Data.Hashable:
    2.87 ms ± 108 μs
```
