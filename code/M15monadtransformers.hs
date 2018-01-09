module M15monadtransformers where
import qualified Data.Map.Strict as M
    
-- | A Manual combination of Maybe and State

data MaybeState s a = MS { runMS :: s -> Maybe (a, s) }

instance Functor (MaybeState s) where
 fmap f ms = MS (\s -> mapFst f <$> runMS ms s)
   where mapFst f (x, y) = (f x, y)


instance Applicative (MaybeState s) where
 pure a =  MS (\s -> Just (a, s))
 f <*> ms = MS (\s -> do
                  (f, s') <- runMS f s
                  (a, s'') <- runMS ms s'
                  Just (f a, s''))

instance Monad (MaybeState s) where
 return a = MS (\s -> Just (a, s))
 ms >>= f  = MS (\s -> case runMS ms s of
                        Nothing -> Nothing
                        Just (a,s') -> runMS (f a) s')

-- | Monad Transformer definition

class MonadTrans t where
  lift :: Monad m => m a -> t m a

-- | The MaybeT monad transformer

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance Monad m => Applicative (MaybeT m) where
    pure = MaybeT . return . Just
    mf <*> mx = MaybeT $ do
        mb_f <- runMaybeT mf
        case mb_f of
            Nothing -> return Nothing
            Just f  -> do
                mb_x <- runMaybeT mx
                case mb_x of
                    Nothing -> return Nothing
                    Just x  -> return (Just (f x))


instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  (MaybeT mmx) >>= f = MaybeT $ do
    mx <- mmx
    case mx of
      Nothing -> return Nothing
      Just x  -> runMaybeT (f x)

instance MonadTrans MaybeT where
  lift ma = MaybeT $ ma >>= return . Just

mfail :: Monad m => MaybeT m a
mfail = MaybeT $ return Nothing

-- We can recover Maybe by applying MaybeT to Id

data Id x = Id x
instance Functor Id where
    fmap f (Id x) = Id $ f x
instance Applicative Id where
    pure = Id
    (Id f) <*> (Id x) = Id $ f x
instance Monad Id where
    return = Id
    Id x >>= f = f x

runId (Id x) = x

type MaybeLike = MaybeT Id

someint :: MaybeLike Integer
someint = return 3

test0 :: (Monad m) => Integer -> MaybeT m Integer
test0 x = do
  if x /= 0 then return x
  else mfail

-- | StateT

newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s) }

get :: Monad m => StateT s m s
get = StateT (\s -> return (s, s))

set :: Monad m => s -> StateT s m s
set x = StateT (\s -> return (x, x))


instance Functor m => Functor (StateT s m) where
    fmap f m = StateT $ \s -> fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> return (a, s)
    StateT mf <*> StateT mx = StateT $ \ s -> do
                                (f, s') <- mf s
                                (x, s'') <- mx s'
                                return (f x, s'')

instance Monad m => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  m >>= f  = StateT $ \s -> do
      (a, s') <- runStateT m s
      runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do { a <- ma ; return (a, s) }


-- Maybe+State
type MS2 s a = StateT s Maybe a

test2 :: MS2 Integer Integer
test2 = do
  s <- get
  if s > 0 then lift $ Just $ s * s else lift Nothing

-- State+Maybe ?
type MS3 s a = MaybeT (StateT s Id) a

test3 :: MS3 Integer Integer
test3 = do
  s <- lift $ get
  if s > 0 then return $ s * s else mfail

runMS3 :: MS3 s a -> s -> (Maybe a, s)
runMS3 mma s =
    let ma = runMaybeT mma in
    let a = runStateT ma s in
    runId a

          
-- | ReaderT
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

-- Retrieves the monad environment.
ask :: (Monad m) => ReaderT r m r
ask = ReaderT return

-- Runs computation in modified environment.
local :: Monad m => (r -> r) -> ReaderT r m a -> ReaderT r m a
local f (ReaderT rma) = ReaderT (rma . f)

test4 :: ReaderT [String] Id String
test4 = do
  l <- ask
  return $ head l

computeWith :: String -> ReaderT [String] Id a -> ReaderT [String] Id a
computeWith s m = do
  local ((:)s) m

test5 = do
  v <- computeWith "foo" test4
  v2 <- test4
  return (v, v2)
        

instance Functor m => Functor (ReaderT r m) where
    fmap f m = ReaderT $ fmap f . runReaderT m

instance Applicative m => Applicative (ReaderT r m) where
    pure    = ReaderT . const . pure
    f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r

instance Monad m => Monad (ReaderT r m) where
    return  = lift . return
    m >>= k = ReaderT $ \r -> do
                 a <- runReaderT m r
                 runReaderT (k a) r

instance MonadTrans (ReaderT r) where
    lift m = ReaderT (\_ -> m)


-- | Monadic interpreter

type Ident = String
data Term  = Con Integer
           | Div Term Term
           | Var Ident | Let Ident Term Term
           | Read | Write Term
             deriving (Eq, Show)

type Env = M.Map Ident Integer
type InterpM = StateT Integer (ReaderT Env (MaybeT Id))

run :: InterpM a -> Maybe (a, Integer)
run m = runId $ runMaybeT $ runReaderT (runStateT m 0) M.empty 

eval :: Term -> InterpM Integer
eval (Con i) = return i
eval (Div t t') = do
  v <- eval t
  v' <- eval t'
  if v' == 0 then lift $ lift $ mfail else return (v `div` v')
eval (Var id) = do
  env <- lift ask
  lift $ lift $ MaybeT $ return $ M.lookup id env
eval (Let id t tbody) = do
  v <- eval t
  liftState (local (M.insert id v)) $ eval tbody

eval Read = get
eval (Write t) =
  eval t >>= \v ->  
  set v >>= \_ ->
  return v
          
liftState f m =
    StateT $ \s -> f $ runStateT m s 
              
             
