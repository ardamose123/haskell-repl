import System.Exit
import System.IO
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Maybe

type Name = String
type Value = [String]
type Function = (Value -> Value)

data Ambient = Ambient
	{ vars :: [(Name, Value   )]
	, funs :: [(Name, Function)]
	}

addVar :: Name -> Value -> Ambient -> Ambient
addVar k v a = a { vars = (k,v) : vars a }

addFun :: Name -> Function -> Ambient -> Ambient
addFun k v a = a { funs = (k,v) : funs a }

type Ambiented = StateT Ambient

safeApply :: (Value -> b) -> Value -> Maybe b
safeApply _ [] = Nothing
safeApply f l  = Just $ f l

rotr :: Function
rotr l  = last l : init l

rotl :: Function
rotl l  = tail l ++ [head l]

construct :: [Function] -> Function
construct l = map unwords . zipWith ($) l . repeat

printAmbient :: Ambiented IO ()
printAmbient = do
	lift $ putStrLn "Variables"
	variables <- gets vars
	forM_ variables $ \(k,v) -> lift $ putStrLn $ k ++ " = " ++ unwords v
	lift $ putStrLn ""
	lift $ putStrLn "Funciones"
	functions <- gets funs
	lift $ putStrLn $ unwords $ map fst functions

defineFunction :: Name -> [String] -> Ambiented IO ()
defineFunction n ["o", f1, f2] = do
	fs  <- gets funs
	f1' <- return $ fromJust $ lookup f1 fs
	f2' <- return $ fromJust $ lookup f2 fs
	modify $ addFun n $ (f1' . f2')

defineFunction n ("[]":args) = do
	fs  <- gets funs
	modify $ addFun n $ construct $ map (fromJust . flip lookup fs) args

defineFunction n ("_" :args)   = modify $ addFun n $ const args

substitute :: Value -> Ambiented IO Value
substitute value = do
	variables <- gets vars
	return $ concatMap (\string -> maybe [string] id $ lookup string variables) value

execFun :: Value -> Function -> Ambiented IO Value
execFun value function = liftM function $ substitute value

runCmd :: [String] -> Ambiented IO Value
runCmd args@(h:t) = do
	functions <- gets funs
	maybe (substitute args) (execFun t) $ lookup h functions

defineVariable :: Name -> Value -> Ambiented IO ()
defineVariable n v = runCmd v >>= modify . addVar n

execM :: [String] -> Ambiented IO ()
execM [] = lift $ putStrLn ""
execM ("defun"  : name : args) = defineFunction name args
execM ("defvar" : name : args) = defineVariable name args
execM ["amb"] = printAmbient
execM ["fin"] = lift exitSuccess
execM args = runCmd args >>= lift . putStrLn . unwords

initAmbient :: Ambient
initAmbient = Ambient 
	{ vars = []
	, funs = 
		[ ("first" , maybe [] (:[]) . safeApply head)
		, ("last"  , maybe [] (:[]) . safeApply last)
		, ("tail"  , maybe [] id . safeApply tail)
		, ("chop"  , maybe [] id . safeApply init)
		, ("rotr"  , maybe [] id . safeApply rotr)
		, ("rotl"  , maybe [] id . safeApply rotl)
		, ("length", (:[]) . show . length)
		, ("ident" , id)
		]
	}

main :: IO ()
main = do
	hSetBuffering stdin  LineBuffering
	hSetBuffering stdout   NoBuffering
	evalStateT (forever run) initAmbient
	where
		run = do
			lift $ putStr "%> "
			lift getLine >>= execM . words