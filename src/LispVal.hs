{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable (Typeable)
import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Exception
import Control.Monad.Reader

type ValCtx = Map.Map T.Text LispVal
type FnCtx = Map.Map T.Text LispVal

data EnvCtx = EnvCtx
    {
        env :: ValCtx,
        fenv :: FnCtx
    } deriving (Eq)

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
      deriving (Monad, Functor, Applicative, MonadReader EnvCtx,  MonadIO)

-- representation of the S-Expression
data LispVal 
    = Atom T.Text
    | List [LispVal]
    | Number Integer
    | String T.Text
    | Fun IFunc
    | Lambda IFunc EnvCtx
    | Nil
    | Bool Bool
    deriving (Typeable, Eq)

instance Show LispVal where
    show = T.unpack . showVal

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }
    deriving (Typeable) 

showVal :: LispVal -> T.Text
showVal val =
    case val of
        (Atom atom)     -> atom
        (String txt)    -> T.concat [ "\"" , txt, "\""]
        (Number num)    -> T.pack $ show num
        (Bool True)     -> "#t"
        (Bool False)    -> "#f"
        Nil             -> "'()"
        (List contents) -> T.concat ["(", unwordsList contents, ")"]
        (Fun _ )        -> "(internal function)"
        (Lambda _ _)    -> "(lambda function)"