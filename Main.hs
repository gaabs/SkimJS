import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (PrefixExpr op expr) = do
    v <- evalExpr env expr
    prefixOp env op v
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    stateLookup env var -- crashes if the variable doesn't exist
    e <- evalExpr env expr
    setVar var e
	
--evalExpr env (CallExpr Expression [Expression]) 
	-- falta usar args
	-- assumi que a Expr é sempre VarRef
	-- escopo de variaveis incorre
evalExpr env (CallExpr (VarRef (Id name)) exps) = do {
	(Function (Id name) ids sts) <- stateLookup env name; -- crashes if the variable doesn't exist
	argsLookup ids env exps;
	myEvaluate env sts;
	
}


argsLookup :: [Id] -> StateT-> [Expression] -> StateTransformer Value
argsLookup [] _ _ = return Nil
argsLookup ((Id local):ids) env ((VarRef (Id parametro)):exps) = do
	val <- myStateLookup env local
	x <- myStateLookup env parametro
	case val of 
		Nil -> setVar local x
		otherwise -> do
			tryToSave local val env 0
			setVar local x	
	argsLookup ids env exps

tryToSave :: String->Value->StateT->Int->StateTransformer Value
tryToSave _ Nil _ _ = return Nil
tryToSave s v env i = do{
	val <- myStateLookup env (s++"Temp"++(show i));
	case val of
	Nil -> setVar (s++"Temp"++(show i)) v
	otherwise -> tryToSave s v env (i+1)
	;
}

myStateLookup :: StateT -> String -> StateTransformer Value
myStateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var (union s env) of
        Nothing -> (Nil, s)
        Just val -> (val, s)
	;

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env (BlockStmt sts) = myEvaluate env sts
evalStmt env EmptyStmt = return Nil
evalStmt env (ExprStmt expr) = evalExpr env expr
evalStmt env (IfStmt expr st1 st2) = do {
	v <- evalExpr env expr;
	if (boolAux v) 
		then evalStmt env st1;
		else evalStmt env st2;
}
evalStmt env (IfSingleStmt expr st) = do {
	v <- evalExpr env expr;
	if (boolAux v)
		then evalStmt env st;
		else return Nil;
}
evalStmt env (WhileStmt expr st) = do {
	v <- evalExpr env expr;
	if (boolAux v) 
		-- n entendi como transformou de StateTransformer Value para StateT
		then do { 
		x <- evalStmt env st;
		if(isBreak x)
			then return Nil;
		else evalStmt env (WhileStmt expr st);
		}
	else return Nil;
}

-- DoWhileStmt Statement Expression
-- ForInStmt ForInInit Expression Statement 
-- LabelledStmt Id Statement
	

evalStmt env (ForStmt init (test) (inc) st) = do {
	case init of
		NoInit -> return Nil
		VarInit l -> evalStmt env (VarDeclStmt l)
		ExprInit expr -> evalExpr env expr
	;	
	case test of
		Nothing ->	do {x <- evalStmt env st;
						if (isBreak x)
							then return Nil;
						else 
							case inc of
								Nothing ->	evalStmt env (ForStmt NoInit (test) (inc) st);
								Just expr -> do {evalExpr env expr >> evalStmt env (ForStmt NoInit (test) (inc) st)};	
					};
		Just expr -> do {
						v<-evalExpr env expr;
						if(boolAux v)
							then do{
								x <- evalStmt env st;
								if (isBreak x)
									then return Nil;
								else 
									case inc of
										Nothing ->	evalStmt env (ForStmt NoInit (test) (inc) st);
										Just expr -> do {evalExpr env expr >> evalStmt env (ForStmt NoInit (test) (inc) st)};	
							};
						else
							return Nil;
					};
	;
}

evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)

--evalStmt env (FunctionStmt (Id name) args sts) = ST $ (\s -> (Function (Id name) args sts, insert name (Function (Id name) args sts) s));

evalStmt env (FunctionStmt (Id name) args sts) = do {
	let f = Function (Id name) args sts;
	in ST $ (\s -> (f, insert name f s));
}


--falta fazer break e continue com label
--break funciona em qq coisa
myEvaluate :: StateT -> [Statement] -> StateTransformer Value
myEvaluate st [] = return Nil
myEvaluate st ((BreakStmt Nothing):sts) = return (String "break")
myEvaluate st ((ContinueStmt Nothing):sts) = return (String "continue")
myEvaluate st ((ReturnStmt Nothing):sts) = return Nil;
myEvaluate st ((ReturnStmt (Just expr)) :sts) = evalExpr st expr
myEvaluate st (s:sts) = evalStmt st s >> myEvaluate st sts


isContinue :: Value -> Bool
isContinue (String "continue") = True
isContinue _ = False

isBreak :: Value -> Bool
isBreak (String "break") = True
isBreak _ = False

boolAux :: Value -> Bool
boolAux (Bool x) = x
boolAux (Int x) = x /= 0
boolAux (String x) = x /= []
boolAux (Var x) = x /= []
boolAux Nil = False

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

prefixOp :: StateT -> PrefixOp ->  Value -> StateTransformer Value
prefixOp env PrefixMinus (Int  v) = return $ Int  $ -v
{-
data PrefixOp = PrefixLNot -- ^ @!@
              | PrefixBNot -- ^ @~@
              | PrefixPlus -- ^ @+@
              | PrefixMinus -- ^ @-@
              | PrefixTypeof -- ^ @typeof@
              | PrefixVoid -- ^ @void@
              | PrefixDelete -- ^ @delete@
-}

--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = Map.empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var (union s env) of
        Nothing -> error $ "Variable " ++ show var ++ " not defined."
        Just val -> (val, s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

		
		
instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) =    show val ++ "\n" ++ show (toList $ union defs environment) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f Map.empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
