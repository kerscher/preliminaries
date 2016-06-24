module Preliminaries
( -- * Structural abstractions
  -- ** Functors
  module Data.Align
, module Data.Functor
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
, module Data.Bifunctor
, module Data.Profunctor
  -- ** Applicatives
, module Control.Applicative
, module Control.Applicative.Free
, module Control.Applicative.Unicode
, module Data.Biapplicative
, module Control.Alternative.Free
, module Control.Alternative.Free
  -- ** Monads
, module Control.Monad
, module Control.Monad.Base
, module Control.Monad.Free
, module Control.Monad.Free.Class
, module Control.Monad.Unicode
, module Control.Monad.Cont
, module Control.Monad.Cont.Class
, module Control.Monad.Except
, module Control.Monad.Identity
, module Control.Monad.List
, module Control.Monad.RWS
, module Control.Monad.Reader
, module Control.Monad.State
, module Control.Monad.Trans
, module Control.Monad.Writer
, module Control.Monad.Morph
, module Control.Monad.Codensity
, module Control.Monad.Fix
, module Control.Monad.ST
, module Control.Monad.Trans.Control
, applyN
, whenM
, unlessM
, ifM
, guardM
, liftM'
, liftM2'
  -- ** Comonads
, module Control.Comonad
, module Control.Comonad.Cofree
, module Control.Comonad.Cofree.Class
, module Control.Comonad.Store
, module Control.Comonad.Density
  -- ** Categories
, module Control.Arrow
, module Control.Arrow.Unicode
, module Control.Category
, module Data.Function
, module Data.Isomorphism
, Isomorphism
, identity
, (▶)
  -- ** Folds
, module Data.Foldable
, module Data.Foldable.Unicode
, module Data.Bifoldable
, module Data.Functor.Foldable
, FoldableFunctor
, foldF
, gunfoldF
, unfoldF
  -- ** Traversals
, module Data.Bitraversable
, module Data.Distributive
, module Data.Traversable
  -- ** Lenses
, module Control.Lens
  -- * Error handling
, module Control.Monad.Catch.Pure
, module Control.Exception.Enclosed
, module System.IO.Error
  -- * Concurrency and parallelism
  -- ** Concurrency
, module Control.Concurrent
, module Control.Concurrent.Async
, module Control.Concurrent.Thread
, module Control.Concurrent.STM
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
, module Control.Monad.STM
  -- * Data structures
  -- ** Basics
, module Data.Default.Class
, module Data.Default.Instances.Base
, module Data.Eq
, module Data.Eq.Unicode
, module Data.Bool
, module Data.Bool.Unicode
, module Data.Pointed
, module Data.Copointed
, module Data.Monoid
, module Data.Monoid.Unicode
, module Data.Group
, module Data.Groupoid
, module Data.Semigroupoid -- , module Data.Semigroup
, module Data.Zero
, module Data.Maybe
, module Data.Either
, module Data.EitherR
, module Data.Either.Combinators
, leftToMaybe
, rightToMaybe
, maybeToRight
, maybeToLeft
, maybeToEither
, module Data.These
, module Data.Both
, module Data.Unique
, module Data.Universe
, module Control.DeepSeq
, module Data.Version
, module Prelude.Unicode
  -- ** Numbers
, module GHC.Enum
, module GHC.Num
, module GHC.Real
, module GHC.Float
, module Data.Ord
, module Data.Ord.Unicode
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
  -- ** Text
, module Data.Char
, module Data.String
, module Data.Text
, module Data.Text.Encoding
, module Data.ByteString
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
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
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
import Data.Isomorphism                  hiding (Iso, embed, project)
import Data.Isomorphism                  as Isomorphism
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
import System.IO.Error                   hiding (catchIOError, tryIO)
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
import Data.Monoid
import Data.Monoid.Unicode
import Data.Group
import Data.Groupoid
import Data.Semigroupoid -- import Data.Semigroup
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
import Data.String.Conversions           hiding (ST, SBS, LBS, LT)
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

type Isomorphism = Isomorphism.Iso

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

-- | 'swap' for pairs, as the name is taken by 'concatenative'.
swapTuple ∷ (α,β) → (β,α)
swapTuple   (x,y) = (y,x)

infixl 1 ▶
(▶) ∷ α → (α → β) → β
x ▶ f = f x

identity ∷ α → α
identity   x = x

-- | A safe 'head' for Vectors.
headMay ∷ Vector α → Maybe α
headMay xs = xs Vector.!? 0

-- | A safe 'last' for Vectors.
lastMay ∷ Vector α → Maybe α
lastMay xs | length xs ≥ 1 = Just $ Vector.last xs
           | otherwise     = Nothing

-- | Decompose a Vector into its head and tail. If the Vector is empty,
-- returns 'Nothing'. If non-empty, returns @'Just' (x, xs)@,
-- where @x@ is the head of the Vector and @xs@ its tail.
uncons ∷ Vector α → Maybe (α, Vector α)
uncons xs = bool Nothing (Just (Vector.head xs, Vector.tail xs)) ((length xs) > 0)

-- | Takes a function and a count and applies it n times.
applyN ∷ Int → (α → α) → α → α
applyN n f = foldr (∘) identity (Vector.replicate n f)

whenM ∷ Monad m ⇒ m Bool → m () → m ()
whenM p m = p >>= flip when m

unlessM ∷ Monad m ⇒ m Bool → m () → m ()
unlessM p m = p >>= flip unless m

ifM ∷ Monad m ⇒ m Bool → m α → m α → m α
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

maybeToEither ∷ Monoid β => (α → β) → Maybe α → β
maybeToEither = maybe mempty

liftM' ∷ Monad m ⇒ (α → β) → m α → m β
liftM' = (<$!>)
{-# INLINE liftM' #-}

liftM2' ∷ (Monad m) ⇒ (α → β → γ) → m α → m β → m γ
liftM2' f a b = do
  x ← a
  y ← b
  let z = f x y
  z `Prelude.seq` return z
{-# INLINE liftM2' #-}

type FoldableFunctor = RecursionSchemes.Foldable

foldF ∷ (FoldableFunctor τ) ⇒ (Base τ α → α) → τ → α
foldF = RecursionSchemes.fold

unfoldF ∷ (Unfoldable τ) ⇒ (α → Base τ α) → α → τ
unfoldF = RecursionSchemes.unfold

gunfoldF ∷ (Unfoldable τ, Monad m)
         ⇒ (∀ β. m (Base τ β) → Base τ (m β))
         → (α → Base τ (m α))
         → α
         → τ
gunfoldF = RecursionSchemes.gunfold
