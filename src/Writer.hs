-- {-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Writer where

-- import Text.LaTeX.Base hiding((<>))
-- import Text.LaTeX.Base.Syntax hiding((<>))
-- import Text.LaTeX.Base.Class
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
-- import Text.LaTeX.Packages.AMSMath
-- import Text.LaTeX.Packages.AMSFonts
-- import Text.LaTeX.Packages.AMSSymb
-- import Data.Semigroup
-- import Control.Monad.Writer.Strict hiding((<>))
-- import Syntax
-- import Control.Monad.Identity
-- import Control.Monad

-- newtype DiagramT m a = DGT{unwrapDGT :: WriterT Diagram m a}

-- instance (Functor f) => Functor (DiagramT f) where
--     fmap f = DGT . fmap f . unwrapDGT

-- instance (Applicative a) => Applicative (DiagramT a) where
--     pure  = DGT . pure
--     (DGT f) <*> (DGT x) = DGT $ f <*> x

-- instance (Monad m) => Monad (DiagramT m) where
--     return = pure
--     (DGT x) >>= f = DGT $ do --DiagramT値のコンストラクタの中で、mのモナド計算を行うのが実態。
--         y <- x --以下のdo文はmに関するモナド計算。一方で関数fは戻り値にDiagramT m値をとるので、let束縛とパターンマッチで中身のm値を取り出さざるを得ない。
--         let DGT z = f y
--         z

-- instance (Monad m) => DiagramC (DiagramT m a) where
--     liftListD f xs = mapM extractDiagram_ xs >>= dgtell . f --これがWriterTモナドのdo文で自然に各行のモノイド値をtellして結合できる秘訣だと思われる

-- instance MonadTrans DiagramT where
--    lift = DGT . lift 


-- type DiagramM = DiagramT Identity 

-- runDGT :: DiagramT m a -> m (a,Diagram)
-- runDGT = runWriterT . unwrapDGT

-- execDGT :: Monad m => DiagramT m a -> m Diagram
-- execDGT = liftM snd . runDGT

-- runDGM :: DiagramM a -> (a,Diagram)
-- runDGM = runIdentity . runDGT

-- execDGM :: DiagramM a -> Diagram
-- execDGM = runIdentity . execDGT

-- extractDiagram :: Monad m => DiagramT m a -> DiagramT m (a,Diagram)
-- extractDiagram = DGT . lift . runWriterT . unwrapDGT

-- extractDiagram_ :: Monad m => DiagramT m a -> DiagramT m Diagram
-- extractDiagram_ = liftM snd .extractDiagram

-- dgtell :: Monad m => Diagram -> DiagramT m ()
-- dgtell = DGT . tell