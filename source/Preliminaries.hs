-- |
-- This module reexports a very large amount of commonly used types and functions,
-- and is meant to be used instead of Prelude.
module Preliminaries
(
  module Exports
, (▶)
, reverseText
, replaceText
, justifyTextLeft
, justifyTextRight
, centerText
, intersperseText
, intercalateText
, swapTuple
, identity
, headMay
, lastMay
, applyN
, whenM
, unlessM
, ifM
, guardM
, leftToMaybe
, rightToMaybe
, maybeToRight
, maybeToLeft
, maybeToEither
, liftM'
, liftM2'
, FoldableFunctor
, foldF
, gunfoldF
, unfoldF
)
where

-- * Abstract nonsense

-- ** Functors
import Data.Align                        as Exports
import Data.Functor                      as Exports
import Data.Functor.Identity             as Exports
import Data.Functor.Invariant            as Exports
import Data.Functor.Contravariant        as Exports
import Data.Functor.Adjunction           as Exports
import Data.Functor.Yoneda               as Exports
import Data.Functor.Kan.Lan              as Exports
import Data.Functor.Kan.Lift             as Exports
import Data.Functor.Kan.Ran              as Exports
import Data.Functor.Kan.Rift             as Exports
import Data.Functor.Rep                  as Exports hiding (index)
import Data.Bifunctor                    as Exports
import Data.Profunctor                   as Exports hiding (WrappedArrow, WrapArrow, unwrapArrow)
 
-- ** Applicatives
import Control.Applicative               as Exports
import Control.Applicative.Free          as Exports
import Control.Applicative.Unicode       as Exports ((⊛))
import Data.Biapplicative                as Exports
import Control.Alternative.Free          as Exports hiding (Alt, Ap, Pure)
import Control.Alternative.Free          as FAlt

-- ** Monads
import Control.Monad                     as Exports
import Control.Monad.Base                as Exports
import Control.Monad.Free                as Exports hiding (Pure, unfold)
import Control.Monad.Free.Class          as Exports
import Control.Monad.Unicode             as Exports
import Control.Monad.Cont                as Exports
import Control.Monad.Cont.Class          as Exports
import Control.Monad.Except              as Exports
import Control.Monad.Identity            as Exports
import Control.Monad.List                as Exports
import Control.Monad.RWS                 as Exports
import Control.Monad.Reader              as Exports
import Control.Monad.State               as Exports
import Control.Monad.Trans               as Exports
import Control.Monad.Writer              as Exports
import Control.Monad.Morph               as Exports hiding (embed)
import Control.Monad.Codensity           as Exports
import Control.Monad.Fix                 as Exports
import Control.Monad.ST                  as Exports

-- ** Comonads
import Control.Comonad                   as Exports hiding (Functor, (<$>), ($>))
import Control.Comonad.Cofree            as Exports hiding ((:<), unfold, unfoldM)
import Control.Comonad.Cofree            as Cofree
import Control.Comonad.Cofree.Class      as Exports
import Control.Comonad.Store             as Exports
import Control.Comonad.Density           as Exports
       
-- ** Categories, arrows and functions
import Control.Arrow                     as Exports hiding (first, second)
import Control.Arrow.Unicode             as Exports
import Control.Category                  as Exports hiding ((.))
import Data.Function                     as Exports hiding (id)
import Data.Isomorphism                  as Exports hiding (Iso, embed, project)
import Data.Isomorphism                  as Isomorphism


-- * Concurrency and parallelism

-- ** Concurrency
import Control.Concurrent                as Exports hiding (forkOS, forkIO, forkIOWithUnmask, forkOn, forkOnWithUnmask)
import Control.Concurrent.Async          as Exports
import Control.Concurrent.Thread         as Exports
import Control.Concurrent.STM            as Exports
import Control.Concurrent.STM.TArray     as Exports
import Control.Concurrent.STM.TBQueue    as Exports
import Control.Concurrent.STM.TChan      as Exports
import Control.Concurrent.STM.TMVar      as Exports
import Control.Concurrent.STM.TQueue     as Exports
import Control.Concurrent.STM.TSem       as Exports
import Control.Concurrent.STM.TVar       as Exports
import Control.Concurrent.STM.TBChan     as Exports
import Control.Concurrent.STM.TBMChan    as Exports
import Control.Concurrent.STM.TBMQueue   as Exports
import Control.Concurrent.STM.TMChan     as Exports
import Control.Concurrent.STM.TMQueue    as Exports
import Control.Monad.STM                 as Exports

-- ** Parallelism
import Control.Parallel                  as Exports
import Control.Parallel.Strategies       as Exports


-- * Data structures

-- ** Basics
import Data.Typeable                     as Exports
import Data.Coerce                       as Exports
import Data.Reflection                   as Exports
import Data.Data                         as Exports
import Data.Default.Class                as Exports
import Data.Default.Instances.Base       as Exports
import GHC.Generics                      as Exports (Generic)

import Data.Eq                           as Exports
import Data.Eq.Unicode                   as Exports
import Data.Bool                         as Exports
import Data.Bool.Unicode                 as Exports

import Data.Monoid                       as Exports
import Data.Monoid.Unicode               as Exports
import Data.Group                        as Exports
import Data.Groupoid                     as Exports
import Data.Semigroupoid                 as Exports
-- import Data.Semigroup        as Exports
import Data.Pointed                      as Exports
import Data.Zero                         as Exports

import Data.Maybe                        as Exports
import Data.Either                       as Exports
import Data.EitherR                      as Exports
import Data.Either.Combinators           as Exports hiding (isLeft, isRight, leftToMaybe, rightToMaybe)
import Data.These                        as Exports
import Data.Both                         as Exports

import Data.Unique                       as Exports
import Data.Universe                     as Exports

import Control.DeepSeq                   as Exports (NFData(..), ($!!), deepseq, force)
import Data.Version                      as Exports

import qualified Prelude                 as Prelude hiding ((.))
import Prelude.Unicode                   as Exports hiding ((∈), (⧺), (∉))


-- ** Type-level programming
import Data.Void                         as Exports
import Data.Type.Bool                    as Exports
import Data.Type.Coercion                as Exports hiding (sym, trans)
import Data.Type.Equality                as Exports
import Data.Tagged                       as Exports
import Data.Proxy                        as Exports


-- ** Manipulate

-- *** Folds
import Data.Foldable                     as Exports hiding (toList)
import Data.Foldable.Unicode             as Exports
import Data.Bifoldable                   as Exports
import Data.Functor.Foldable             as Exports hiding (Foldable, gunfold, fold)
import Data.Functor.Foldable             as RecursionSchemes

-- *** Traversals
import Data.Bitraversable                as Exports 
import Data.Distributive                 as Exports
import Data.Traversable                  as Exports

-- *** Comonad coalgebra yadda-yadda-yadda
import Control.Lens                      as Exports hiding ( universe
                                                           , both
                                                           , index
                                                           , (??)
                                                           , para
                                                           )


-- ** Numbers
import GHC.Enum                          as Exports
import GHC.Num                           as Exports
import GHC.Real                          as Exports
import GHC.Float                         as Exports
import Data.Ord                          as Exports
import Data.Ord.Unicode                  as Exports
import Data.Bits                         as Exports
import Data.Complex                      as Exports
import Data.Fixed                        as Exports
import Data.Int                          as Exports
import Data.Ratio                        as Exports
import Data.Word                         as Exports
import Numeric                           as Exports

-- ** Time
import Data.Time.Calendar                as Exports
import Data.Time.Clock                   as Exports
import Data.Time.LocalTime               as Exports
import Data.Time.Format                  as Exports

-- ** Containers
import GHC.Exts                          as Exports (IsList, fromList, fromListN, toList)
import Data.IntMap                       as Exports (IntMap)
import Data.IntSet                       as Exports (IntSet)
import Data.Default.Instances.Containers as Exports
import Data.Ix                           as Exports
import Data.Map                          as Exports (Map)
import Data.HashMap.Lazy                 as Exports (HashMap)
import Data.Set                          as Exports (Set)
import Data.HashSet                      as Exports (HashSet)
import Data.Sequence                     as Exports (Seq)
import Data.Tuple                        as Exports hiding (swap)
import Data.Vector                       as Exports (Vector, take, dropWhile, drop, splitAt)
import qualified Data.Vector             as Vector

-- ** Memory
import Data.IORef
import Data.STRef                        as Exports
import System.Mem                        as Exports
import System.Mem.StableName             as Exports
import System.Mem.Weak                   as Exports

-- ** Text
import Data.Char                         as Exports hiding (toUpper, toLower, toTitle)
import Data.String                       as Exports (String, IsString, fromString)
import Data.Text                         as Exports (Text, toCaseFold, toLower, toUpper, toTitle, transpose, lines, words, unlines, unwords, isPrefixOf, isSuffixOf, isInfixOf, stripPrefix, stripSuffix, commonPrefixes)
import Data.Text.Encoding                as Exports
import Data.ByteString                   as Exports (ByteString)
import qualified Data.Text               as Text
import Text.Show                         as Exports
import Data.Text.IO                      as Exports
import Data.String.Conversions           as Exports hiding (ST, SBS, LBS, LT)


-- * System interface
import System.CPUTime                    as Exports
import System.Environment                as Exports
import System.Exit                       as Exports
import System.IO                         as Exports hiding (hGetContents, hGetLine, hPutStr, hPutStrLn, appendFile, getContents, getLine, interact, putStr, putStrLn, readFile, writeFile)
import System.IO.Error                   as Exports
import System.Info                       as Exports
import System.Timeout                    as Exports

-- * Error handling
import Control.Error.Util                as Exports hiding (isLeft, isRight, bool)


-- * Helper functions

type Isomorphism = Isomorphism.Iso

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
-- | Easier to remember alias for '&'.
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

-- -- | Decompose a Vector into its head and tail. If the Vector is empty,
-- -- returns 'Nothing'. If non-empty, returns @'Just' (x, xs)@,
-- -- where @x@ is the head of the Vector and @xs@ its tail.
-- uncons ∷ Vector α -> Maybe (α, Vector α)
-- uncons []     = Nothing
-- uncons (x:xs) = Just (x, xs)

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
