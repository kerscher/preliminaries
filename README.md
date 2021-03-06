Preliminaries
=============

[![Build Status](https://travis-ci.org/kerscher/preliminaries.svg?branch=master)](https://travis-ci.org/kerscher/preliminaries)
[![Hackage](https://img.shields.io/hackage/v/preliminaries.svg)](https://hackage.haskell.org/package/preliminaries)
![BSD3 license](https://img.shields.io/badge/licence-BSD%203--clause-blue.svg)

The Haskell Report specifies the [Prelude](https://www.haskell.org/onlinereport/standard-prelude.html) with a minimal amount of definitions that are always available in scope for application writers. Due to its simplicity and frugality, multiple alternatives and support libraries were devised to improve upon it, including:

* [`classy-prelude`](https://github.com/snoyberg/mono-traversable/tree/master/classy-prelude)
* [`base-prelude`](https://github.com/nikita-volkov/base-prelude)
* [`basic-prelude`](https://github.com/snoyberg/basic-prelude)
* [`prelude-extras`](https://github.com/ekmett/prelude-extras)
* [`protolude`](https://github.com/sdiehl/protolude)

`preliminaries` is one of such alternatives and builds upon [`classy-prelude-conduit`](https://github.com/snoyberg/mono-traversable/tree/master/classy-prelude-conduit), with the following functionality out-of-the-box:

* Data manipulation and structures — i.e. [`microlens`](https://github.com/aelve/microlens) + [`mono-traversable`](https://github.com/snoyberg/mono-traversable)
* Streaming
* Concurrency
* Parallelism
* Read-only, write-only and read-write environments — i.e. [`mtl`](https://github.com/ekmett/mtl)

Contrary to most other alternatives, there is no attempt at being minimal.
Nevertheless, it's shipped with `microlens` instead of the full-blown `lens`.

Usage
-----

To use it, put the following in your `.cabal` file, ignoring the “…” for omited parts:

```
executable your-executable
  language:           Haskell2010
  default-extensions: NoImplicitPrelude
  build-depends:      preliminaries >= 0.1.6 < 1
  …
```

And on each file, add `import Preliminaries`.

You might also want to look at this project’s Cabal file to check on useful GHC extensions to enable alongside this change.

Contributing
------------

In case something does not build or you find other unpleasant aspects of the library, please send a pull request or contact the maintainer.

Licence
-------

This package uses a 3-clause BSD-like licence. You can check it [here](LICENCE).
