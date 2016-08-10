{-|
Module      : Preliminaries
Copyright   : © Yghor Kerscher, 2016
Licence     : BSD-3
Maintainer  : kerscher@acm.org
Stability   : experimental

The Haskell Report specifies the <https://www.haskell.org/onlinereport/standard-prelude.html Prelude> with a minimal amount of definitions that are always available in scope for application writers. Due to its simplicity and frugality, multiple alternatives and support libraries were devised to improve upon it, including:

* <https://github.com/snoyberg/mono-traversable/tree/master/classy-prelude classy-prelude>
* <https://github.com/nikita-volkov/base-prelude base-prelude>
* <https://github.com/snoyberg/basic-prelude basic-prelude>
* <https://github.com/ekmett/prelude-extras prelude-extras>
* <https://github.com/sdiehl/protolude protolude>

@Preliminaries@ is one of such alternatives and builds upon <https://hackage.haskell.org/package/classy-prelude-0.12.8 classy-prelude>, with the following functionality out-of-the-box:

* Data manipulation — i.e. <https://github.com/aelve/microlens microlens>
* Streaming
* Concurrency
* Parallelism
* Read-only, write-only and read-write environments — i.e. <https://github.com/ekmett/mtl mtl>

To use it, put the following on your @.cabal@ file, ignoring the “…” for ommited parts:

@
…
default-extensions: NoImplicitPrelude
build-depends:      preliminaries >= 0.1.1 < 2
@

And on each file, add @import Preliminaries@.

You might also want to look at this project’s Cabal file to check on useful GHC extensions to enable alongside this change.
In case something does not build or you find other unpleasant aspects of the library, please contact the maintainer.

-}
module Preliminaries
( -- * Data manipulation
  module Lens.Micro.Platform
, module Lens.Micro.Contra
  -- * Concurrency
, module Data.Conduit.Async
, module Data.Conduit.TQueue
  -- * Parallelism
, module Control.Monad.Par
, parFork
, parNew
, parNewFull
, parGet
, parPut
, parSpawn
, parParMap
, module Control.Monad.Parallel
, module Control.Parallel
, module Control.Parallel.Strategies
, thru
  -- * Environments
, module Control.Monad.Reader
, module Control.Monad.State.Lazy
, module Control.Monad.Writer.Lazy
  -- * Re-exports
, module ClassyPrelude.Conduit
, module Data.Biapplicative
, module Data.Bifoldable
, module Data.Bitraversable
, module Data.MonoTraversable.Instances
  -- * Utilities
, type ($)
)
where

import Control.Monad.Reader      (MonadReader, ask, asks, ReaderT (..), Reader, runReaderT, runReader)
import Control.Monad.State.Lazy  (MonadState, get, put, modify, StateT(..), State, runStateT, runState)
import Control.Monad.Writer.Lazy (MonadWriter, tell, listen, listens, WriterT(..), Writer, runWriterT, runWriter)
import Control.Monad.Par         as Par
import Control.Monad.Par.Class   (ParFuture)
import Control.Monad.Par         (Par, runPar, runParIO, IVar, parMapM, parMapReduceRange, InclusiveRange(..), parFor)
import Control.Monad.Parallel    (MonadFork, forkExec)
import Control.Parallel
import Control.Parallel.Strategies as Strategies
import Control.Parallel.Strategies
  ( Strategy, withStrategy
  , rseq, rdeepseq
  , rpar, rparWith
  , evalTraversable, parTraversable, parMap
  , ($|), ($||), (.|), (.||), (-|), (-||)
  , Eval, runEval
  )
import ClassyPrelude.Conduit
import Data.Biapplicative        (Biapplicative, bipure, (<<*>>))
import Data.Bifoldable           (Bifoldable, bifoldr, bifold, bifoldMap, bitraverse_, bisequenceA_, bifor_)
import Data.Bitraversable        (Bitraversable, bitraverse, bisequenceA, bifor)
import Data.Conduit.Async
import Data.Conduit.TQueue
import Data.MonoTraversable.Instances ()
import Lens.Micro.Platform
import Lens.Micro.Contra

parFork :: Par () -> Par ()
parFork = Par.fork

parNew :: Par (IVar a)
parNew = Par.new

parNewFull :: NFData a => a -> Par (IVar a)
parNewFull = Par.newFull

parGet :: IVar a -> Par a
parGet = Par.get

parPut :: NFData a => IVar a -> a -> Par ()
parPut = Par.put

parSpawn :: NFData a => Par a -> Par (IVar a)
parSpawn = Par.spawn

parParMap :: (Traversable t, NFData b, ParFuture iv p) => (a -> b) -> t a -> p (t b)
parParMap = Par.parMap

thru :: a -> Strategy a -> a
x `thru` strat = x `Strategies.using` strat

type f $ x = f x
