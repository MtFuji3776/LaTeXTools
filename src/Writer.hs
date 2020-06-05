{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Writer where

import Text.LaTeX.Base hiding((<>))
import Text.LaTeX.Base.Syntax hiding((<>))
import Text.LaTeX.Base.Class
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSFonts
import Text.LaTeX.Packages.AMSSymb
import Data.Semigroup
import Control.Monad.Writer.Strict hiding((<>))
import Syntax

newtype DiagramT m a = DGT{unwrapDGT :: WriterT Diagram m a}

instance (Functor f) => Functor (DiagramT f) where
    fmap f = DGT . fmap f . unwrapDGT