{-|
Module      : Preliminaries
Copyright   : © Yghor Kerscher, 2016
Licence     : BSD-3
Maintainer  : kerscher@acm.org
Stability   : experimental

The Haskell Report specifies the <https://www.haskell.org/onlinereport/standard-prelude.html Prelude> with a well-thought but minimal amount of definitions that are always available in scope for application writers. Due to its simplicity and frugality, multiple alternatives and support libraries were devised to improve upon it, including:

* <https://hackage.haskell.org/package/base-prelude-1.0.1.1 base-prelude>
* <https://hackage.haskell.org/package/basic-prelude-0.5.2 basic-prelude>
* <https://hackage.haskell.org/package/classy-prelude-0.12.8 classy-prelude>
* <https://hackage.haskell.org/package/prelude-extras-0.4.0.3 prelude-extras>
* <https://hackage.haskell.org/package/protolude-0.1.5 protolude>

@Preliminaries@ is one of such alternatives, with the following goals in mind:

* Imports that you use most of the time should be available by default.
* Useful abstractions are included, even if obscure.
* Documentation should be first-class.

To use it, put the following on your @.cabal@ file, ignoring the “…” for ommited parts:

@
…
build-depends: preliminaries
@

You might also want to look at this project’s Cabal file to check on useful GHC extensions to enable alongside this change.

In case something does not build or you find other unpleasant aspects of the library, please contact the maintainer.

-}
module Preliminaries
( -- * Basics
  {- |
     * 'Eq'ualities between values of a type;
     * 'Ord'erings;
     * 'Bool'eans;
     * 'Default' values.
  -}
  module Prelude.Unicode
, module Data.Eq
, module Data.Eq.Unicode
, module Data.Ord
, module Data.Ord.Unicode
, module Data.Bool
, module Data.Bool.Unicode
-- , module Data.Unique
-- , module Data.Universe
-- , module Control.DeepSeq
-- , module Data.Version
, module Data.Default.Class
, module Data.Default.Instances.Base
  -- ** Functions
, module Data.Function
, identity
, applyN
  -- * Structural abstractions
  -- ** Algebraic structures
  -- | Types with an associative operation ('Semigroup') and optionally identities ('Monoid'), inverses ('Group') or a “zero” element ('Zero').
, module Data.Semigroup
, module Data.Monoid.Unicode
, module Data.Group
, module Data.Zero
  -- ** Functors
  -- | Computational contexts and mappings over them.
, module Data.Functor
, module Data.Copointed
, module Data.Bifunctor
, module Data.Profunctor
, module Data.Align
, module Data.Functor.Identity
, module Data.Functor.Invariant
, module Data.Functor.Contravariant
, module Data.Functor.Adjunction
, module Data.Functor.Yoneda
, module Data.Functor.Kan.Lan
, module Data.Functor.Kan.Lift
, module Data.Functor.Kan.Ran
, module Data.Functor.Kan.Rift
, module Data.Functor.Rep
  -- ** Applicatives
  -- | Effectful computations where steps are independent of each other.
, module Control.Applicative
, module Data.Pointed
, module Data.Biapplicative
, module Control.Applicative.Free
, module Control.Applicative.Unicode
, module Control.Alternative.Free
  -- ** Folds
  -- | Reduction of a type to an aggregate value of a (possibly) different type.
, module Data.Foldable
, module Data.Foldable.Unicode
, module Data.Bifoldable
, module Data.Functor.Foldable
, FoldableFunctor
, foldF
, gunfoldF
, unfoldF
  -- ** Traversals
  -- | Commuting two functors — e.g.: turning a list of trees into a tree of lists.
  -- A beefed-up '<$>' applied within an 'Applicative' context.
, module Data.Traversable
, module Data.Bitraversable
, module Data.Distributive
  -- ** Lenses
  -- | A generalisation of 'Getter' and 'Setter' patterns from other languages with the addition of 'Prism's for coproducts. Also features a unified interface to do 'Fold's, 'Traversal's, 'Iso'morphisms and 'Equality'.
  -- A gigantic library with many operators available by default on scope re-exported here.
  --
  -- Start by watching a video intro, reading the lens website and visiting the wiki.
, module Control.Lens
  -- ** Monads
  -- | Effectful computations where steps may be dependent of previous bindings.
, module Control.Monad
, module Control.Monad.Unicode
, module Control.Monad.Fix
, module Control.Monad.Base
  -- *** Free
  -- | “Bare minimum” monads. Useful to prototype interpreters for DSLs.
, module Control.Monad.Free
, module Control.Monad.Free.Class
, module Control.Monad.Codensity
  -- *** Continuation
  -- | Allows interrupting and resuming computations.
, module Control.Monad.Cont
, module Control.Monad.Cont.Class
  -- *** Monad transformers
  -- | Composable management of state, read-only and write-only types.
, module Control.Monad.Identity
, module Control.Monad.Reader
, module Control.Monad.State
, module Control.Monad.ST
, module Control.Monad.Writer
, module Control.Monad.RWS
, module Control.Monad.List
, module Control.Monad.Trans
, module Control.Monad.Morph
, module Control.Monad.Trans.Control
  -- *** Helpers
, whenM
, unlessM
, ifM
, guardM
, liftM'
, liftM2'
  -- ** Comonads
  -- | Computations that generate data or layers of further computation.
, module Control.Comonad
, module Control.Comonad.Cofree
, module Control.Comonad.Cofree.Class
, module Control.Comonad.Store
, module Control.Comonad.Density
  -- ** Categories
  -- | Composable types that associate and have an identity.
, module Data.Semigroupoid 
, module Data.Groupoid
, module Control.Category
  -- ** Arrows
  -- | A “general interface to computation” useful to structure pipelines of functions and their arguments.
, module Control.Arrow
, module Control.Arrow.Unicode
  -- * Error handling
  {- |
     Types for values that are:

     * 'Maybe' optional, and 'mappend' only when 'Both' are 'Just';
     * 'Either' one value of type `a` or type `b`, or also 'These' two at the same time;
     * computations that end only when 'EitherR' any successes.
  -}
, module Data.Maybe
, module Data.Either
, module Data.EitherR
, module Data.Either.Combinators
, module Data.These
, module Data.Both
, leftToMaybe
, rightToMaybe
, maybeToRight
, maybeToLeft
, maybeToEither
  -- ** Exceptions
  -- | Ensuring unsafe code does not spread and crash your application is only guaranteed by dealing with exceptions directly.
, module Control.Exception.Enclosed
, module Control.Monad.Catch.Pure
, module Control.Monad.Except
, module System.IO.Error
  -- * Concurrency and parallelism
  -- ** Concurrency
  -- | Decomposing components of execution of a program for use in different orders. Useful to split long lasting computations from others with tighter soft deadlines and other asynchronous scenarios.
  --
  -- Includes facilities to share data between these partially-ordered sequences of execution safely with software transactional memory ('Control.Concurrent.STM.STM').
, module Control.Concurrent
, module Control.Concurrent.Async
, module Control.Concurrent.Thread
, module Control.Concurrent.STM
, module Control.Monad.STM
, module Control.Concurrent.STM.TArray
, module Control.Concurrent.STM.TBQueue
, module Control.Concurrent.STM.TChan
, module Control.Concurrent.STM.TMVar
, module Control.Concurrent.STM.TQueue
, module Control.Concurrent.STM.TSem
, module Control.Concurrent.STM.TVar
, module Control.Concurrent.STM.TBChan
, module Control.Concurrent.STM.TBMChan
, module Control.Concurrent.STM.TBMQueue
, module Control.Concurrent.STM.TMChan
, module Control.Concurrent.STM.TMQueue
  -- ** Parallelism
  -- | Computations that (can) happen simultaneously and the 'Control.Parallel.Strategies.Strategy' used to express them.
, module Control.Parallel
, module Control.Parallel.Strategies
  -- * Data structures
  -- ** Numbers
, module GHC.Enum
, module GHC.Num
, module GHC.Real
, module GHC.Float
, module Data.Bits
, module Data.Complex
, module Data.Fixed
, module Data.Int
, module Data.Ratio
, module Data.Word
, module Numeric
  -- ** Time
, module Data.Time.Calendar
, module Data.Time.Clock
, module Data.Time.LocalTime
, module Data.Time.Format
  -- ** Containers
, module GHC.Exts
, module Data.IntMap
, module Data.IntSet
, module Data.Default.Instances.Containers
, module Data.Ix
, module Data.Map
, module Data.HashMap.Lazy
, module Data.Set
, module Data.HashSet
, module Data.Sequence
, module Data.Tuple
, swapTuple
, module Data.Vector
, module Data.Vector.Instances
, headMay
, lastMay
, unconsVector
  -- ** Text
, module Data.Char
, module Data.String
, module Data.Text
, module Data.Text.Encoding
, module Data.ByteString
, module Codec.Binary.UTF8.String
, module Text.Show
, module Data.Text.IO
, module Data.String.Conversions
, reverseText
, replaceText
, justifyTextLeft
, justifyTextRight
, centerText
, intersperseText
, intercalateText
, encodeWord8
, decodeWord8
, packByteString
, unpackByteString
, fromUtf8String
  -- * System interface
, module System.CPUTime
, module System.Environment
, module System.Exit
, module System.IO
, module System.Info
, module System.Timeout
  -- ** Memory
, module Data.IORef
, module Data.STRef
, module System.Mem
, module System.Mem.StableName
, module System.Mem.Weak
)
where

-- Structural abstractions
--- Functors
import Data.Align
import Data.Functor
import Data.Functor.Identity
import Data.Functor.Invariant
import Data.Functor.Contravariant
import Data.Functor.Adjunction
import Data.Functor.Yoneda
import Data.Functor.Kan.Lan
import Data.Functor.Kan.Lift
import Data.Functor.Kan.Ran
import Data.Functor.Kan.Rift
import Data.Functor.Rep                  hiding (index)
import Data.Bifunctor
import Data.Profunctor                   hiding (WrappedArrow, WrapArrow, unwrapArrow)
--- Applicatives
import Control.Applicative
import Control.Applicative.Free
import Control.Applicative.Unicode       ((⊛))
import Data.Biapplicative
import Control.Alternative.Free          hiding (Alt, Ap, Pure)
import Control.Alternative.Free          as FAlt
--- Monads
import Control.Monad
import Control.Monad.Base
import Control.Monad.Free                hiding (Pure, unfold)
import Control.Monad.Free.Class
import Control.Monad.Unicode
import Control.Monad.Cont
import Control.Monad.Cont.Class
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.List
import Control.Monad.RWS                 hiding ((<>), First, Last, getFirst, getLast)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer              hiding ((<>), First, Last, getFirst, getLast)
import Control.Monad.Morph               hiding (embed)
import Control.Monad.Codensity
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.Trans.Control       hiding (embed)
--- Comonads
import Control.Comonad                   hiding (Functor, (<$>), ($>))
import Control.Comonad.Cofree            hiding ((:<), unfold, unfoldM)
import Control.Comonad.Cofree.Class
import Control.Comonad.Store
import Control.Comonad.Density
--- Categories, arrows and functions
import Control.Arrow                     hiding (first, second)
import Control.Arrow.Unicode
import Control.Category                  hiding ((.))
import Data.Function                     hiding (id)
--- Folds
import Data.Foldable                     hiding (toList)
import Data.Foldable.Unicode
import Data.Bifoldable
import Data.Functor.Foldable             hiding (Foldable, gunfold, fold)
import Data.Functor.Foldable             as RecursionSchemes
--- Traversals
import Data.Bitraversable
import Data.Distributive
import Data.Traversable
--- Lenses
import Control.Lens                      hiding ( universe, both, index, (??), para)
-- Type-level programming
import Data.Void
import Data.Type.Coercion                hiding (sym, trans)
import Data.Type.Equality
import Data.Type.Bool
import Data.Tagged
import Data.Proxy
import Data.Typeable
import Data.Coerce
import Data.Reflection
import Data.Data
import GHC.Generics                      (Generic)
-- Error handling
import Control.Error.Util                hiding (isLeft, isRight, bool, tryIO)
import Control.Monad.Catch
import Control.Monad.Catch.Pure
import Control.Exception.Enclosed
import System.IO.Error                   hiding (catchIOError)
-- Concurrency and parallelism
--- Concurrency
import Control.Concurrent                hiding (forkOS, forkIO, forkIOWithUnmask, forkOn, forkOnWithUnmask)
import Control.Concurrent.Async
import Control.Concurrent.Thread
import Control.Concurrent.STM
import Control.Concurrent.STM.TArray
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TSem
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBChan
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.TMQueue
import Control.Monad.STM
--- Parallelism
import Control.Parallel
import Control.Parallel.Strategies
-- Data structures
--- Basics
import Data.Default.Class
import Data.Default.Instances.Base
import Data.Eq
import Data.Eq.Unicode
import Data.Bool
import Data.Bool.Unicode
import Data.Pointed
import Data.Copointed
-- import Data.Monoid
import Data.Monoid.Unicode
import Data.Group
import Data.Groupoid
import Data.Semigroupoid
import Data.Semigroup
import Data.Zero
import Data.Maybe
import Data.Either
import Data.EitherR
import Data.Either.Combinators           hiding (isLeft, isRight, leftToMaybe, rightToMaybe)
import Data.These
import Data.Both
import Data.Unique
import Data.Universe
import Control.DeepSeq                   (NFData(..), ($!!), deepseq, force)
import Data.Version
import qualified Prelude                 as Prelude hiding ((.))
import Prelude.Unicode                   hiding ((∈), (⧺), (∉))
--- Numbers
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Float
import Data.Ord
import Data.Ord.Unicode
import Data.Bits
import Data.Complex
import Data.Fixed
import Data.Int
import Data.Ratio
import Data.Word
import Numeric
--- Time
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
--- Containers
import GHC.Exts                          (IsList, fromList, fromListN, toList)
import Data.IntMap                       (IntMap)
import Data.IntSet                       (IntSet)
import Data.Default.Instances.Containers
import Data.Ix
import Data.Map                          (Map)
import Data.HashMap.Lazy                 (HashMap)
import Data.Set                          (Set)
import Data.HashSet                      (HashSet)
import Data.Sequence                     (Seq)
import Data.Tuple                        hiding (swap)
import Data.Vector                       (Vector, take, dropWhile, drop, splitAt, filter, zipWith, unzip, partition, (!))
import Data.Vector.Instances
import qualified Data.Vector             as Vector
--- Text
import Data.Char                         hiding (toUpper, toLower, toTitle)
import Data.String                       (String, IsString, fromString)
import Data.Text                         (Text, toCaseFold, toLower, toUpper, toTitle, transpose, lines, words, unlines, unwords, isPrefixOf, isSuffixOf, isInfixOf, stripPrefix, stripSuffix, commonPrefixes, pack, unpack)
import Data.Text.Encoding
import Data.ByteString                   (ByteString)
import Data.ByteString                   as BS (pack, unpack)
import Codec.Binary.UTF8.String          (decodeString, encodeString)
import Codec.Binary.UTF8.String          as UTF8
import qualified Data.Text               as Text
import Text.Show
import Data.Text.IO
import Data.String.Conversions           hiding (ST, SBS, LBS, LT, (<>))
-- System interface
import System.CPUTime
import System.Environment
import System.Exit
import System.IO                         hiding (hGetContents, hGetLine, hPutStr, hPutStrLn, appendFile, getContents, getLine, interact, putStr, putStrLn, readFile, writeFile)
import System.Info
import System.Timeout
--- Memory
import Data.IORef
import Data.STRef
import System.Mem
import System.Mem.StableName
import System.Mem.Weak

-- Helper functions

fromUtf8String ∷ String → ByteString
fromUtf8String = packByteString . encodeWord8 . decodeString

packByteString ∷ [Word8] → ByteString
packByteString   = BS.pack

unpackByteString ∷ ByteString → [Word8]
unpackByteString = BS.unpack

encodeWord8 ∷ String → [Word8]
encodeWord8 = UTF8.encode

decodeWord8 ∷ [Word8] → String
decodeWord8 = UTF8.decode

reverseText      ∷ Text → Text
replaceText      ∷ Text → Text   → Text → Text
justifyTextLeft  ∷ Int  → Char   → Text → Text
justifyTextRight ∷ Int  → Char   → Text → Text
centerText       ∷ Int  → Char   → Text → Text
intersperseText  ∷ Char → Text   → Text
intercalateText  ∷ Text → [Text] → Text
reverseText      = Text.reverse
replaceText      = Text.replace
justifyTextLeft  = Text.justifyLeft
justifyTextRight = Text.justifyRight
centerText       = Text.center
intersperseText  = Text.intersperse
intercalateText  = Text.intercalate

-- | The trivial identity function. 'id' is used for 'Control.Category.Category' instead.
--
-- prop> identity ≡ Prelude.id
-- >>> identity 1
-- 1
identity ∷ a → a
identity   x = x

-- | Swaps the elements of pairs.
--
-- >>> swapTuple (1,0)
-- (0,1)
swapTuple ∷ (a,b) → (b,a)
swapTuple   (x,y) = (y,x)

-- | A safe 'head' for Vectors.
--
-- >>> headMay [1,2,3]
-- Just 1
-- >>> headMay []
-- Nothing
headMay ∷ Vector a → Maybe a
headMay xs = xs Vector.!? 0

-- | A safe 'last' for Vectors.
--
-- >>> lastMay [1,2,3]
-- Just 3
lastMay ∷ Vector a → Maybe a
lastMay xs | length xs ≥ 1 = Just $ Vector.last xs
           | otherwise     = Nothing

-- | Decompose a Vector into its head and tail. If the Vector is empty,
-- returns 'Nothing'. If non-empty, returns @'Just' (x,xs)@,
-- where @x@ is the head of the Vector and @xs@ its tail.
--
-- >>> unconsVector [1,2,3]
-- Just (1,[2,3])
unconsVector ∷ Vector a → Maybe (a, Vector a)
unconsVector xs = bool Nothing (Just (Vector.head xs, Vector.tail xs)) ((length xs) > 0)

-- | Takes a function and a count and applies it n times.
--
-- >>> applyN 5 (mappend "a") "b"
-- "aaaaab"
applyN ∷ Int → (a → a) → a → a
applyN n f = foldr (∘) identity (Vector.replicate n f)

whenM ∷ Monad m ⇒ m Bool → m () → m ()
whenM p m = p >>= flip when m

unlessM ∷ Monad m ⇒ m Bool → m () → m ()
unlessM p m = p >>= flip unless m

ifM ∷ Monad m ⇒ m Bool → m a → m a → m a
ifM p x y = p >>= \b → bool y x b

guardM ∷ MonadPlus m ⇒ m Bool → m ()
guardM f = guard =<< f

leftToMaybe ∷ Either l r → Maybe l
leftToMaybe = either Just (const Nothing)

rightToMaybe ∷ Either l r → Maybe r
rightToMaybe = either (const Nothing) Just

maybeToRight ∷ l → Maybe r → Either l r
maybeToRight l = maybe (Left l) Right

maybeToLeft ∷ r → Maybe l → Either l r
maybeToLeft r = maybe (Right r) Left

maybeToEither ∷ Monoid b => (a → b) → Maybe a → b
maybeToEither = maybe mempty

liftM' ∷ Monad m ⇒ (a → b) → m a → m b
liftM' = (<$!>)
{-# INLINE liftM' #-}

liftM2' ∷ (Monad m) ⇒ (a → b → c) → m a → m b → m c
liftM2' f a b = do
  x ← a
  y ← b
  let z = f x y
  z `Prelude.seq` return z
{-# INLINE liftM2' #-}

type FoldableFunctor = RecursionSchemes.Foldable

foldF ∷ (FoldableFunctor t) ⇒ (Base t a → a) → t → a
foldF = RecursionSchemes.fold

unfoldF ∷ (Unfoldable t) ⇒ (a → Base t a) → a → t
unfoldF = RecursionSchemes.unfold

gunfoldF ∷ (Unfoldable t, Monad m)
         ⇒ (∀ b. m (Base t b) → Base t (m b))
         → (a → Base t (m a))
         → a
         → t
gunfoldF = RecursionSchemes.gunfold
