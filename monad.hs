{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
import Control.Monad
import Data.Functor.Identity
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.State hiding (get)
--import Web.Scotty
import Web.Scotty.Trans
import Data.IORef
import qualified Data.Map as M
import Data.Text.Lazy as TL
import Data.Text.Lazy (Text)
import System.Environment (getArgs)
import Data.Monoid (mconcat)
import Data.Maybe (fromJust)

newtype Compose f g a = Compose {getCompose :: f (g a) } deriving (Eq, Show)
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga
instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose (pure (pure x))
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  Compose f <*> Compose a = Compose $ (<*>) <$> f <*> a  -- HARD!
instance (Foldable f, Foldable g) => Foldable (Compose f g) where -- HARD!!!
--  foldMap f (Compose fga) = foldMap f $ foldMap f <$> fga -- my answer -! wrong
  foldMap f (Compose fga) = foldMap (foldMap f) fga
instance (Traversable f, Traversable g) => Traversable (Compose f g) where -- HARD!
--  traverse  :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse afb (Compose fga) = Compose <$> traverse (traverse afb) fga

class Bifunctor' p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a->b) -> (c->d) -> p a c -> p b d
  bimap f g = first f . second g
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  second :: (b->c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b deriving (Eq, Show)
instance Bifunctor' Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)
data Const a b = Const a deriving (Eq, Show)
instance Bifunctor' Const where
  bimap f g (Const a) = Const (f a)
data Drei a b c = Drei a b c deriving (Eq, Show)
instance Bifunctor' (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)
data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)
instance Bifunctor' (SuperDrei a) where
  bimap f g (SuperDrei a b) = SuperDrei a (f b)
data SemiDrei a b c = SemiDrei a deriving (Eq, Show)
instance Bifunctor' (SemiDrei a) where
  bimap f g (SemiDrei a) = SemiDrei a
data Quadriceps a b c d = Quadzzz a b c d deriving (Eq, Show)
instance Bifunctor' (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)
data Either' a b = Left' a | Right' b deriving (Eq, Show)
instance Bifunctor' (Either') where
  bimap f g (Left' a) = Left' (f a)
  bimap f g (Right' b) = Right' (g b)

newtype Identity' a = Identity' { runIdentity' :: a } deriving (Eq, Show)
newtype IdentityT' f a = IdentityT' { runIdentityT' :: f a } deriving (Eq, Show)
instance Functor Identity' where
  fmap f (Identity' a) = Identity' (f a)
instance (Functor m) => Functor (IdentityT' m) where
  fmap f (IdentityT' fa) = IdentityT' (fmap f fa)
instance Applicative Identity' where
  pure = Identity'
  (Identity' f) <*> (Identity' a) = Identity' $ f a
instance (Applicative m) => Applicative (IdentityT' m) where
  pure = IdentityT' . pure
  (IdentityT' fab) <*> (IdentityT' fa) = IdentityT' $ fab <*> fa
instance Monad Identity' where
  return = pure
  (Identity' a) >>= f = f a
instance (Monad m) => Monad (IdentityT' m) where
  return = pure
  (>>=) :: IdentityT' m a -> (a -> IdentityT' m b) -> IdentityT' m b
--  (IdentityT' ma) >>= f = IdentityT' $ ma >>= (runIdentityT'.f)
  (IdentityT' ma) >>= f
--    = IdentityT' $ join $ fmap runIdentityT' $ fmap f ma -- Functor fmap Law
--    = IdentityT' $ join $ fmap (runIdentityT' . f) ma -- x >>= f = join (fmap f x)
    = IdentityT' $ ma >>= runIdentityT' . f

newtype MaybeT' m a = MaybeT' { runMaybeT' :: m (Maybe a) }
instance (Functor m) => Functor (MaybeT' m) where
  fmap f (MaybeT' ma) -- = MaybeT' $ (fmap (fmap f) ma)
    = MaybeT' $ (fmap . fmap) f ma
instance (Applicative m) => Applicative (MaybeT' m) where
  pure = MaybeT' . pure . pure
  (MaybeT' fab) <*> (MaybeT' mma) =
    MaybeT' $ (<*>) <$> fab <*> mma
instance (Monad m) => Monad (MaybeT' m) where
  return = pure
  (>>=) :: MaybeT' m a -> (a -> MaybeT' m b) -> MaybeT' m b
  (MaybeT' ma) >>= f =
    MaybeT' $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just x  -> runMaybeT' $ f x
    
newtype EitherT' e m a = EitherT' { runEitherT' :: m (Either e a) }
instance (Functor m) => Functor (EitherT' e m) where
  fmap f (EitherT' ma) = EitherT' $ (fmap . fmap) f ma
instance (Applicative m) => Applicative (EitherT' e m) where
  pure = EitherT' . pure . pure
  (EitherT' fab) <*> (EitherT' x) =
    EitherT' $ (<*>) <$> fab <*> x
instance (Monad m) => Monad (EitherT' e m) where
  return = pure
  (>>=) :: EitherT' e m a -> (a -> EitherT' e m b) -> EitherT' e m b
  EitherT' ma >>= f =
    EitherT' $ do
    v <- ma
    case v of
      Left e -> return $ Left e
      Right x -> runEitherT' $ f x

swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT' :: (Functor m) => EitherT' e m a -> EitherT' a m e
swapEitherT' x = EitherT' $ swapEither <$> runEitherT' x

eitherT' :: Monad m => (a -> m c) -> (b -> m c) -> EitherT' a m b -> m c
eitherT' f g (EitherT' x) = join $ fmap (either f g) x
-- eitherT f g (EitherT m) = m >>= either

newtype ReaderT' r m a = ReaderT' { runReaderT' :: r -> m a }
instance (Functor m) => Functor (ReaderT' r m) where
  fmap f (ReaderT' ma) = ReaderT' $ (fmap . fmap) f ma
instance (Applicative m) => Applicative (ReaderT' r m) where
  pure = ReaderT' . pure . pure
  (ReaderT' fab) <*> (ReaderT' x) =
    ReaderT' $ (<*>) <$> fab <*> x
instance (Monad m) => Monad (ReaderT' r m) where
  return = pure
  (>>=) :: ReaderT' r m a -> (a -> ReaderT' r m b) -> ReaderT' r m b
  ReaderT' ma >>= f = ReaderT' $ \r -> do
    v <- ma r
    (runReaderT' . f) v r

newtype StateT' s m a = StateT' { runStateT' :: s -> m (a, s) }
instance (Functor m) => Functor (StateT' s m) where
  fmap f (StateT' ma) = StateT' $ \s ->
    fmap (\(a,s') -> (f a,s')) (ma s)
instance (Monad m) => Applicative (StateT' s m) where -- !MONAD precondition
  pure a = StateT' $ \s -> pure (a, s)
  StateT' mfab <*> StateT' ma =
    StateT' $ \s -> do
    (fab, s') <- mfab s
    (a, s'') <- ma s'
    return (fab a, s'')
instance (Monad m) => Monad (StateT' s m) where
  return = pure
  StateT' ma >>= f = StateT' $ \s -> do
    (a, s') <- ma s
    (runStateT' . f) a s'

foo, foo' :: MaybeT IO Int
foo = return 1
foo' = MaybeT $ return (Just 1)
bar, bar' :: MaybeT (ExceptT String IO) Int
bar = return 1
bar' = MaybeT $ ExceptT $ return $ Right (Just 1)

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1
embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT $ ExceptT $ ReaderT $ return <$> (const (Right (Just 1)))
-- DANG HARD!!

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded
eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap
readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap
fooo = readerUnwrap ()
{-
scottyTest' = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    lift $ putStrLn "hello"
    html $ mconcat ["<h1>Scotty, ", beam, "me up! </h1>"]
-}
{-
instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

  lift :: (Monad m) => m a -> t m a
  (ReaderT . const) :: m a -> ReaderT r m a
    
  const :: a -> b -> a
  ReaderT :: (Monad m) => (r -> m a) -> ReaderT r m a
-}
-- newtype EitherT' e m a = EitherT' { runEitherT' :: m (Either e a) }
instance MonadTrans (EitherT' e) where
--  lift :: (Monad m) => m a -> t m a
--       ::   Monad m => m a -> EitherT' e m a
  lift = EitherT' . fmap Right

--newtype StateT' s m a = StateT' { runStateT' :: s -> m (a, s) }
instance MonadTrans (StateT' s) where
--  lift :: (Monad m) => m a -> t m a
--         :: (Monad m) => m a -> StateT' s m a
  lift ma = StateT' $ \s -> ma >>= return . (,s)

rDec :: Num a => Reader a a -- ReaderT a Idenity a
--newtype ReaderT' r m a = ReaderT' { runReaderT' :: r -> m a }
--rDec = ReaderT (\x -> return (x - 1))
rDec = ReaderT $ return . (+ (-1))

rShow :: Show a => ReaderT a Identity String -- (== Reader a String)
--rShow = ReaderT (\x -> return (show x))
rShow = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a -- (== Reader a (IO a))
rPrintAndInc = ReaderT (\x -> do
                           putStrLn $ "Hi: " ++ show x
                           return (x+1))

sPrintIncAccum :: (Num a, Show a) => StateT a IO String -- (== State a (IO String))
sPrintIncAccum = StateT (\s -> do
                           putStrLn $ "Hi: " ++ show s
                           return (show s, s+1))

isValid :: String -> Bool
isValid v = '!' `elem` v

--newtype MaybeT' m a = MaybeT' { runMaybeT' :: m (Maybe a) }
maybeExcite :: MaybeT IO String -- IO (Maybe String)
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something exciting!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "hmm"
    Just e -> putStrLn ("Good! " ++ e)

data Config = Config { counts :: IORef (M.Map Text Integer)
                     , prefix :: Text
                     }
type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = let m' = M.insertWith (+) k 1 m
                in (m', fromJust $ M.lookup k m')

app :: Scotty ()
app =
  get "/:key" $ do
  config <- lift ask -- DANG HARD!!
  unprefixed <- param "key"
  m <- (lift . lift) $ readIORef $ counts config -- HARD!!
  let key' = mappend (prefix config) (unprefixed :: Text)
      (m', newInteger) = bumpBoomp key' m
  (lift . lift) $ writeIORef (counts config)  m'
  html $ mconcat [ "<h1>Success! Count was: "
                 , TL.pack $ show (newInteger :: Integer)
                 , key'
                 , "</h1>"
                 ]
main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter $ TL.pack prefixArg
      runR x = runReaderT x config -- (m Response -> IO Response)
  scottyT 3000 runR app
  
