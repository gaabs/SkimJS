import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map
import Debug.Trace
import Value

--
-- Evaluate functions
--



evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (StringLit str) = return $ String str
evalExpr env (NullLit) = return $ Nil
evalExpr env (NumLit double) = return $ Double double
evalExpr env (ArrayLit []) = return $ Array []
evalExpr env (ArrayLit (exp:exps)) = do {
	(Array x) <- (evalExpr env (ArrayLit exps));
	y <- evalExpr env exp;
	return $ Array ( [y] ++ x )
}

evalExpr env (FuncExpr (Nothing) args sts) = return $ Function (Id "") args sts;
evalExpr env (FuncExpr (Just id) args sts) = return $ Function (id) args sts;

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

evalExpr env (AssignExpr OpAssign (LBracket container keyExp) expr) = do{
	vector <- evalExpr env container;
	case vector of
		(Array array) -> do {
			(Int key)<-evalExpr env keyExp;
			if(key<0||key>(count(Array array)))
			then error $ "Array out_of_bounds. ";
			else
				case container of
				(VarRef (Id id))-> do{
									e <- evalExpr env expr;
									if(key==0)
										then let x = Array ([e]++(drop (key+1) array));
												in setVar id x;			
									else 
										let x = Array ((take key array)++[e]++(drop (key+1) array));
										in setVar id x;
									}
				otherwise -> do{
								e <- evalExpr env expr;
								if(key==0)
									then return $ Array ([e]++(drop (key+1) array));			
								else 
									return $ Array ((take key array)++[e]++(drop (key+1) array));
								}
				;
			}
		(String array) -> return Nil
}



evalExpr env (AssignExpr op (LVar var) expr) = do
    case op of
		OpAssignAdd -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpAdd (VarRef (Id var)) expr))
		OpAssignSub -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpSub (VarRef (Id var)) expr))
		OpAssignMul -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpMul (VarRef (Id var)) expr))
		OpAssignDiv -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpDiv (VarRef (Id var)) expr))
		OpAssignMod -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpMod (VarRef (Id var)) expr))

evalExpr env (UnaryAssignExpr unaryAssignOp (LVar var)) = do
	case unaryAssignOp of
		PrefixInc -> evalExpr env (AssignExpr OpAssignAdd (LVar var) (IntLit 1))
		PostfixInc -> evalExpr env (AssignExpr OpAssignAdd (LVar var) (IntLit 1))
		PrefixDec -> evalExpr env (AssignExpr OpAssignSub (LVar var) (IntLit 1))
		PostfixDec -> evalExpr env (AssignExpr OpAssignSub (LVar var) (IntLit 1))

evalExpr env (CallExpr (VarRef (Id name)) exps) = do{
	(Function (Id name) ids sts) <- stateLookup env name;
	ST $ \s -> do{
		let (ST funcaoLocais) = addLocals env (BlockStmt sts);
			(_, varLocais) = funcaoLocais env;
			(ST funcaoGlobais) = addGlobal varLocais (BlockStmt sts);
			(_, varGlobais) = funcaoGlobais s;
			(ST funcaoArgs) = mapM (evalExpr env) exps;
			(params, _) = funcaoArgs s;
			parametros = fromList (zip (Prelude.map (\(Id a) -> a) ids) (params));
			locais = union parametros s;
			(ST rodarFuncao) = myEvaluate env sts;
			(val, estadoFinal) = rodarFuncao locais;
		in do
			if (isReturn(val))
				then (getReturn(val), union (difference estadoFinal (union varLocais parametros)) varGlobais)
			else
				(val, union (difference estadoFinal (union varLocais parametros)) varGlobais)
	};
}
	
evalExpr env (CallExpr (DotRef e (Id function) ) exps) = do {
	x<-evalExpr env e;
	case function of
		"concat" -> myConcat env x exps
		"len" -> return $ (myLen env x)
		"length" -> return $ (myLen env x)
		"head" -> myHead env x
		"tail" -> myTail env x
	;
}

evalExpr env (DotRef e (Id function) ) = do {
	x<-evalExpr env e;
	case function of
		"len" -> return $ (myLen env x)
		"length" -> return $ (myLen env x)
		"head" -> myHead env x
		"tail" -> myTail env x
	;
}

evalExpr env (BracketRef container key) = do{
	v <- evalExpr env container;
	
	case v of
		(Array array) -> do {
								(Int chave) <- evalExpr env key;
								return $ (array!!chave);
							}
		
		(String string) -> do {
								(Int chave) <- evalExpr env key;
								return $ String [(string!!chave)];
							}
		_ -> return $ Nil
	;
	
}

myConcat :: StateT->Value->[Expression]->StateTransformer Value
myConcat env (Array l) [] = return $ Array l
myConcat env (Array l) (h:exps) = do{
 x <- evalExpr env h;
 (Array y) <- myConcat env x exps;
 return $ Array (l++y);
}
myConcat env (String l) [] = return $ String l
myConcat env (String l) (h:exps) = do{
  x <- evalExpr env h;
 (String y) <- myConcat env x exps;
 return $ String (l++y);

}

myLen :: StateT->Value->Value
myLen env e = Int (count e);
 
myHead :: StateT->Value->StateTransformer Value
myHead env (Array []) = return $ (Array [])
myHead env (Array (e:es)) = return $ e
myHead env (String []) = return $ (String [])
myHead env (String (e:es)) = return $ (String [e])

myTail :: StateT->Value->StateTransformer Value
myTail env (Array []) = return $ (Array [])
myTail env (Array (e:es)) = return $ (Array es)
myTail env (String []) = return $ (String [])
myTail env (String (e:es)) = return $ (String es)

count :: Value->Int
count (Array []) = 0
count (Array (e:ex)) = 1 + count(Array ex)
count (String []) = 0
count (String (e:ex)) = 1 + count(String ex)

addLocals :: StateT-> Statement -> StateTransformer Value
addLocals env (BlockStmt []) = return $ Nil
addLocals env (VarDeclStmt []) = return $ Nil
addLocals env (VarDeclStmt (decl:ds)) = do
	varDecl env decl
	addLocals env (VarDeclStmt ds)
addLocals env (BlockStmt (s:sts))= do
	case s of
			(IfStmt expr ifBlock elseBlock) -> do
				addLocals env ifBlock
				addLocals env elseBlock
				addLocals env (BlockStmt sts)
			(IfSingleStmt expr ifBlock) -> do
				addLocals env ifBlock
				addLocals env (BlockStmt sts)
			(ForStmt initialize expr1 expr2 stmt) -> do
				addLocals env stmt
				addLocals env (BlockStmt sts)
			(VarDeclStmt (y:ys)) -> do
				varDecl env y
				addLocals env (BlockStmt sts)
			(ExprStmt (CallExpr nameExp args)) -> do
				res <- evalExpr env (nameExp)
				case res of
					(Vazia _) -> addLocals env (BlockStmt sts)
					(Function name argsName stmts) -> do
						addLocals env (BlockStmt stmts)
						addLocals env (BlockStmt sts)
					_ -> do
						addLocals env (BlockStmt sts)
					
			_ -> addLocals env (BlockStmt sts)
addLocals env _ = return $ Nil

addGlobal :: StateT-> Statement -> StateTransformer Value
addGlobal env (BlockStmt []) = return Nil
addGlobal env (BlockStmt ((ExprStmt (AssignExpr OpAssign (LVar var) expr)):xs)) = do
    v <- stateLookup env var
    case v of
        (Vazia _) -> do
            evalStmt env (VarDeclStmt [(VarDecl (Id var) (Nothing))])
            addGlobal env (BlockStmt xs)
        _ -> addGlobal env (BlockStmt xs)
addGlobal env (BlockStmt (x:xs)) = do
    case x of
        (IfStmt expr ifBlock elseBlock) -> do
            addGlobal env ifBlock
            addGlobal env elseBlock
            addGlobal env (BlockStmt xs)
        (IfSingleStmt expr ifBlock) -> do
            addGlobal env ifBlock
            addGlobal env (BlockStmt xs)
        (ForStmt initialize expr1 expr2 stmt) -> do
            case initialize of
                (ExprInit e) -> do
                    addGlobal env (BlockStmt [ExprStmt e])
                    addGlobal env stmt
                    addGlobal env (BlockStmt xs)
                _ -> do
                    addGlobal env stmt
                    addGlobal env (BlockStmt xs)
        (ExprStmt (CallExpr nameExp args)) -> do
            res <- evalExpr env (nameExp)
            case res of
                (Vazia _) -> addGlobal env (BlockStmt xs)
                (Function name argsName stmts) -> do
                    addGlobal env (BlockStmt stmts)
                    addGlobal env (BlockStmt xs)
        _ -> addGlobal env (BlockStmt xs)
addGlobal env _ = return $ Nil

		
evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env (BlockStmt sts) = myEvaluate env sts; 
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
		then do { 
			x <- evalStmt env st;
			if(isBreak x)
				then return Nil;
			else 
				if (isReturn x)
					then return $ x;
				else evalStmt env (WhileStmt expr st);
		}
	else return Nil;
}


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
							if (isReturn x)
								then return $ x;
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
									if (isReturn x)
										then return $ x;	
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

evalStmt env (FunctionStmt (Id name) args sts) = do {
	let f = Function (Id name) args sts;
	in ST $ (\s -> (f, insert name f s));
}

evalStmt env ((ReturnStmt Nothing)) = return (Return Nil)
evalStmt env ((ReturnStmt (Just expr))) = evalExpr env expr >>= \x -> return (Return x)
evalStmt env ((BreakStmt Nothing)) = return (Break)

myEvaluate :: StateT -> [Statement] -> StateTransformer Value
myEvaluate st [] = return Nil
myEvaluate st ((BreakStmt Nothing):sts) = return Break
myEvaluate st ((ContinueStmt Nothing):sts) = return Continue
myEvaluate st ((ReturnStmt Nothing):sts) = return (Return Nil)
myEvaluate st ((ReturnStmt (Just expr)) :sts) = evalExpr st expr >>= \x -> return (Return x)
myEvaluate st (s:sts) = do {
	x <- evalStmt st s;
	if (isBreak x)
		then return Break;
	else 
		if (isReturn x)
			then return $ x
			else myEvaluate st sts
}

getReturn :: Value -> Value
getReturn (Return v) = v
getReturn _ = Nil

isReturn :: Value -> Bool
isReturn (Return _) = True
isReturn _ = False

isContinue :: Value -> Bool
isContinue Continue = True
isContinue _ = False

isBreak :: Value -> Bool
isBreak Break = True
isBreak _ = False

isNil :: Value -> Bool
isNil Nil = True
isNil _ = False

boolAux :: Value -> Bool
boolAux Nil = False
boolAux Break = False
boolAux (Bool x) = x
boolAux (Int x) = x /= 0
boolAux (String x) = x /= []
boolAux (Var x) = x /= []

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ Prelude.map (evalStmt env) stmts

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
infixOp env OpNEq  (Int v1) (Int v2) = return $ Bool $ v1 /= v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2
infixOp env OpEq  (v1) (v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (v1) (v2) = return $ Bool $ v1 /= v2
--infixOp env op (Return v1) (v2) = (infixOp env op v1 v2)
--infixOp env op (v1) (Return v2) = (infixOp env op v1 v2)


prefixOp :: StateT -> PrefixOp ->  Value -> StateTransformer Value
prefixOp env PrefixMinus (Int  v) = return $ Int  $ -v
prefixOp env PrefixPlus (Int  v) = return $ Int  $ v
prefixOp env PrefixLNot (Bool  v) = return $ Bool  $ not v
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
environment = insert "." (Global Map.empty) Map.empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var env of
        Nothing -> case Map.lookup var s of
						Nothing -> ((Vazia Nil), s)
						Just val -> (val, s)
        Just val -> (val, env)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr;
            setVar id val;


removeLocals :: StateT->[Id]->StateTransformer Value			
removeLocals env [] = return $ Nil 
removeLocals env (Id id:ids) = do{

	--let (val, state) = getResult (removeLocalsEach env id)
	let state = (Map.intersection (Map.delete id env) env)
	in removeLocals state ids; 

}
			
			
				
setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

setVarLocal :: StateT -> String -> Value -> StateTransformer Value
setVarLocal env var val = ST $ \s -> (val, insert var val (union s env))


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
showResult (val, defs) =    show val ++ "\n" ++ show (toList $ defs ) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f Map.empty

main :: IO ()
main = do
    --js <- Parser.parseFromFile "Main.js"
    filename <- getLine
    js <- Parser.parseFromFile filename
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
