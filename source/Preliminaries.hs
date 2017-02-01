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

To use it, put the following in your @.cabal@ file, ignoring the “…” for omited parts:

@
…
default-extensions: NoImplicitPrelude
build-depends:      preliminaries >= 0.1.6 < 1
@

And on each file, add @import Preliminaries@.

You might also want to look at this project’s Cabal file to check on useful GHC extensions to enable alongside this change.
In case something does not build or you find other unpleasant aspects of the library, please contact the maintainer.

-}
module Preliminaries
( -- * Data manipulation
  {- |
Lenses provide unified and first-class means to access and modify data structures. 'Lens.Micro.Platform', included here, provides a lightweight alternative to the much larger 'Control.Lens' module, while remaining for the most part compatible.

Use <http://hackage.haskell.org/package/lens-tutorial-1.0.1/docs/Control-Lens-Tutorial.html this tutorial> as an introduction, minding the slightly different module names.
  -}
  module Lens.Micro.Platform
, module Lens.Micro.Contra
  -- * Concurrency
  {- |
Structure programs so that different threads are controlled independently. Whenever you need to have distinct functionality happening “at the same time”, you probably want to use the functionality here. The core 'Async' functionality is provided by 'ClassyPrelude'. Modules below provide helpers to execute things asynchronously in streaming 'Conduit's and a transactional queue to transfer data between threads.
  -}
, module Data.Conduit.Async
, module Data.Conduit.TQueue
  -- * Parallelism
  {- |
Using multiple available resources in a device to compute a result is what parallelism is about. Whenever you want to chop your data so that many cores calculate parts of it and bring about a result, you want what the imports here.

'Control.Monad.Par' provides fine-grained control, while 'Control.Monad.Parallel' provides a simple interface to create 'Control.Parallel.Strategies' to parallelise execution. In general it's easier to start with 'Parallel' and switch to 'Par' when more control is needed.

Since the names used by both modules are similar, this module prefixes `par` to all 'Control.Monad.Par' functions that would conflict with 'Control.Parallel'.
  -}
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
  {- |
If your programs end up repeatedly passing parameters around for configuration, state or logging, you will benefit from the monads below.

- 'Control.Reader' provides a read-only environment, useful to ensure configuration invariants are kept.
- 'Control.State' helps deal with scenarios where a variable is passed around many functions to “update” its state.
- 'Control.Writer' provides a write-only environment, useful for logging and auditing purposes.
  -}
, module Control.Monad.Reader
, module Control.Monad.State.Lazy
, module Control.Monad.Writer.Lazy
  -- * System interface
  {- |
Terminate your programs with 'exitFailure' or 'exitSuccess'.

You should ensure any scarce resources that outlive program termination are freed with appropriate 'Control.Exception.Safe' functions such as 'onException', 'bracket', 'bracket_', 'finally', 'withException', 'bracketOnError' or 'bracketOnError_'.
  -}
, module System.Environment
, getEnvironmentMap
, module System.Exit
  -- * Re-exports
, module ClassyPrelude.Conduit
, module Data.Biapplicative
, module Data.Bifoldable
, module Data.Bitraversable
, module Data.MonoTraversable.Instances
, module Data.Default
, module Data.String.Conversions
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
  , Eval, runEval
  )
import ClassyPrelude.Conduit
import Data.Biapplicative        (Biapplicative, bipure, (<<*>>))
import Data.Bifoldable           (Bifoldable, bifoldr, bifold, bifoldMap, bitraverse_, bisequenceA_, bifor_)
import Data.Bitraversable        (Bitraversable, bitraverse, bisequenceA, bifor)
import Data.Conduit.Async
import Data.Conduit.TQueue
import Data.Default
import Data.MonoTraversable.Instances ()
import Data.String.Conversions   (ConvertibleStrings, cs)
import Lens.Micro.Platform
import Lens.Micro.Contra
import qualified System.Environment as SE
import System.Environment        (getEnv, lookupEnv, setEnv, unsetEnv)
import System.Exit               (exitFailure, exitSuccess)

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

-- | A synonym for 'Strategies.using'.
thru :: a -> Strategy a -> a
x `thru` strat = x `Strategies.using` strat

-- | Retrieves the current list of environment variables as a 'Map' of keys for variable names and values for current assignment of each variable.
--
-- This is a single action. If you need to keep this structure in sync with system environment, it's your responsibility to call it again. Consider either calling a specific variable with 'getEnv' when you need it, or keep this structure in a 'TVar' and refresh it manually.
getEnvironmentMap :: IO (Map String String)
getEnvironmentMap = SE.getEnvironment >>= pure . mapFromList

-- | This allows you to avoid parentheses in type declarations:
--
-- > f :: h (g (f a b)) -> g (h (f a b))
-- > f :: h $ g $ f a b -> g $ h $ f a b
type f $ x = f x
