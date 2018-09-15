# SimpleRNG

This repository contains implementations of the 32-bit Xorshift-RNG that was first introduced by George Marsaglia (www.jstatsoft.org/v08/i14/paper , doi:10.18637/jss.v008.i14).

It:

- Is **not** cryptographically secure.
- Is **not** statistically uniformly distributed. Most importantly, it will never output '0'.
- Will produce (when given the same seed) the exact same results in C, Java, Python and Haskell, regardless of platform, environment or compiler versions. This means that code which uses this RNG for some deterministic procedures work across languages and environments.



