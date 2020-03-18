import qualified Data.Text as T
import qualified Data.Map as Map

type ValCtx = Map.Map T.Text LispVal
type FnCtx = Map.Map T.Text LispVal

data EnvCtx = EnvCtx
    {
        env :: ValCtx,
        fenv :: FnCtx
    } deriving (Eq)

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