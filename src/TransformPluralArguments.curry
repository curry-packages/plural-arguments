--------------------------------------------------------------------
--- A tool to support plural arguments by a transformation
--- on Curry programs.
---
--- @author Michael Hanus
--- @version July 2024
--------------------------------------------------------------------

import Curry.Compiler.Distribution ( curryCompiler, installDir )

import Control.Monad        ( when )
import System.Environment   ( getArgs, getEnv, setEnv )

import AbstractCurry.Files
import AbstractCurry.Types
import AbstractCurry.Select
import AbstractCurry.Build
import AbstractCurry.Pretty
import System.CurryPath     ( stripCurrySuffix )
import System.Directory     ( renameFile )
import System.FilePath      ( (</>), searchPathSeparator )
import System.FrontendExec  ( FrontendTarget(..), callFrontendWithParams
                            , rcParams, setQuiet )
import System.Process       ( system )

import ConfigPluralPackage  ( getPackagePath )

--------------------------------------------------------------------

banner :: String
banner = unlines [bannerLine,bannerText,bannerLine]
 where
   bannerText = "Curry-Plural Transformation Tool (Version of 23/07/24)"
   bannerLine = take (length bannerText) (repeat '=')

------------------------------------------------------------------------
-- Data type for transformation parameters
data TParam = TParam Bool -- work quietly?
                     Bool -- compile the transformed program?
                     Bool -- load and execute transformed program?

defaultTParam :: TParam
defaultTParam = TParam False False False

setRunQuiet :: TParam -> TParam
setRunQuiet (TParam _ cmp ep) = TParam True cmp ep

setCompile :: TParam -> TParam
setCompile (TParam wq _ ep) = TParam wq True ep

setExec :: TParam -> TParam
setExec  (TParam wq _ _) = TParam wq True True

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  processArgs defaultTParam args
 where
  processArgs tparam args = case args of
     ["-h"]          -> putStrLn $ banner ++ usageInfo
     ("-q":moreargs) -> processArgs (setRunQuiet  tparam) moreargs
     ("-c":moreargs) -> processArgs (setCompile   tparam) moreargs
     ("-r":moreargs) -> processArgs (setExec      tparam) moreargs
     [mname]         -> transformPlural tparam (stripCurrySuffix mname)
     _ -> putStrLn $ banner ++
           "\nERROR: Illegal arguments for transformation: " ++
           unwords args ++ "\n" ++ usageInfo

usageInfo :: String
usageInfo =
  "Usage: curry-plural [-q|-c|-r] <module_name>\n"++
  "-q : work quietly\n"++
  "-c : compile the transformed program\n"++
  "-r : load the transformed program into the Curry system '" ++
  curryCompiler ++ "' (implies -c)\n"

transformPlural :: TParam -> String -> IO ()
transformPlural (TParam quiet compile execprog) progname = do
  let progfname      = progname ++ ".curry"
      saveprogfname  = progname ++ "_ORG.curry"
      transprogfname = progname ++ "_TRANSPLURAL.curry"
      putStrNQ s   = if quiet then return () else putStr s
      putStrLnNQ s = if quiet then return () else putStrLn s
  putStrLnNQ banner
  ppath <- fmap (</> "src") getPackagePath
  cp <- getEnv "CURRYPATH"
  setEnv "CURRYPATH" (if null cp then ppath
                                 else ppath ++ searchPathSeparator : cp)
  uc <- readUntypedCurry progname
  let pargs = (pluralArgsOfProg uc)
  if null pargs
   then putStrLnNQ $ "No plural arguments found."
   else do
     putStrNQ "Plural arguments:"
     putStrLnNQ (concatMap (\ ((_,f),args) -> " "++f++"/"++show args) pargs)
     system $ "cleancurry " ++ progname
     ac <- readCurry progname
     let transprog = showCProg (tPluralProg pargs ac)
     putStrLnNQ "Transformed module:"
     putStrLnNQ transprog
     when compile $ do
       renameFile progfname saveprogfname
       writeFile progfname transprog
       compileAcyFcy quiet progname
       renameFile progfname transprogfname
       renameFile saveprogfname progfname
       putStrLnNQ $
         "Transformed program written into '" ++ transprogfname ++ "'"
       when execprog $ do
         system $ unwords [installDir </> "bin" </> "curry", ":load", progname]
         return ()

compileAcyFcy :: Bool -> String -> IO ()
compileAcyFcy quiet progname = do
  params <- rcParams >>= return . setQuiet quiet
  callFrontendWithParams ACY params progname
  callFrontendWithParams FCY params progname

------------------------------------------------------------------------
-- Extract plural arguments:
pluralArgsOfProg :: CurryProg -> [(QName, [Int])]
pluralArgsOfProg (CurryProg _ _ _ _ _ _ funs _) =
  concatMap pluralArgsOfFunc funs

pluralArgsOfFunc :: CFuncDecl -> [(QName, [Int])]
pluralArgsOfFunc (CFunc mf _ _ ctype _) =
  let pargs = pluralArgsOfType 1 (typeOfQualType ctype)
   in if null pargs then [] else [(mf,pargs)]
pluralArgsOfFunc (CmtFunc _ mf ar vis ctype rs) =
  pluralArgsOfFunc (CFunc mf ar vis ctype rs)


pluralArgsOfType :: Int-> CTypeExpr -> [Int]
pluralArgsOfType argnum ty = case ty of
  CFuncType (CTApply (CTCons tc) _) t2 ->
                    (if tc==tcPlural then (argnum:) else id)
                       (pluralArgsOfType (argnum+1) t2)
  CFuncType _ t2 -> pluralArgsOfType (argnum+1) t2
  _ -> []

-- Transform a name into a qualified name of the Plural module.
toPluralModName :: String -> QName
toPluralModName s = ("Language.Curry.Plural", s)

tcPlural :: QName
tcPlural = toPluralModName "Plural"

tcPluralArg :: QName
tcPluralArg = toPluralModName "PluralArg"

tcplural :: QName
tcplural = toPluralModName "plural"

------------------------------------------------------------------------
-- Transform a program containing plural arguments:
tPluralProg :: [(QName, [Int])] -> CurryProg -> CurryProg
tPluralProg pargs (CurryProg mname imps dflts cls insts tdecls funs ops) =
  CurryProg mname imps dflts cls insts tdecls
            (map (tPluralFunc mname pargs) funs) ops

tPluralFunc :: String -> [(QName, [Int])] -> CFuncDecl -> CFuncDecl
tPluralFunc mname pargs (CFunc mf ar vis (CQualType ctxt ctype) rs) =
  let fpargs = maybe [] id (lookup mf pargs)
   in CFunc mf ar vis (CQualType ctxt (tPluralType fpargs 1 ctype))
            (map (tPluralRule mname pargs fpargs) rs)
tPluralFunc mname pargs (CmtFunc cmt mf ar vis ctype rs) =
  let (CFunc mf' ar' vis' ctype' rs') =
                          tPluralFunc mname pargs (CFunc mf ar vis ctype rs)
   in (CmtFunc cmt mf' ar' vis' ctype' rs')

tPluralType :: [Int] -> Int -> CTypeExpr -> CTypeExpr
tPluralType fpargs argnum ty = case ty of
  CFuncType t1 t2 -> CFuncType (if argnum `elem` fpargs
                                then CTApply (CTCons tcPluralArg) t1
                                else t1)
                               (tPluralType fpargs (argnum+1) t2)
  _ -> ty

tPluralRule :: String -> [(QName, [Int])] -> [Int] -> CRule -> CRule
tPluralRule mname pargs fpargs (CRule pats (CSimpleRhs exp locals)) =
  tPluralRule mname pargs fpargs
              (CRule pats (CGuardedRhs [(preTrue,exp)] locals))
tPluralRule mname pargs fpargs (CRule pats (CGuardedRhs condrules locals)) =
  CRule (map (replacePluralCPatterns fpargs) numpats)
        (CGuardedRhs (map tPluralCondRule condrules)
                     (locals ++ map CLocalFunc (concat pllocals)))
 where
  numpats = zip [1..] pats

  (plvars,pllocals) = unzip (map (pluralVarsOfPattern mname fpargs) numpats)

  tPluralCondRule (cond,exp) =
    (list2conj
         (concatMap (matchForPluralCPatterns mname fpargs) numpats ++
          if cond == preTrue
          then []
          else [tPluralExp pargs (concat plvars) cond]),
     tPluralExp pargs (concat plvars) exp)

-- Replace plural constructor patterns by fresh variables.
replacePluralCPatterns :: [Int] -> (Int, CPattern) -> CPattern
replacePluralCPatterns fpargs (n,pat) = case pat of
  CPVar _     -> pat
  CPLit _     -> pat
  CPComb _ _  -> if n `elem` fpargs then CPVar (freshVar n) else pat
  CPAs v _    -> if n `elem` fpargs then CPVar v else pat
  CPFuncComb _ _ -> funPatError
  CPLazy _       -> lazyPatError
  CPRecord _ _   -> recPatError

funPatError :: _
funPatError =
  error "Plural arguments with functional patterns not yet supported!"

lazyPatError :: _
lazyPatError =
  error "Plural arguments with lazy patterns not yet supported!"

recPatError :: _
recPatError =
  error "Plural arguments with record patterns not yet supported!"

-- Create a "fresh" variable with an index n (should be improved...):
freshVar :: Int -> CVarIName
freshVar n = (142+n,"newvar"++show n)

-- Generate match calls for fresh variables introduced
-- for plural constructor patterns.
matchForPluralCPatterns :: String -> [Int] -> (Int, CPattern) -> [CExpr]
matchForPluralCPatterns mname fpargs (n,pat) = case pat of
  CPVar _     -> []
  CPLit _     -> []
  CPComb _ _  -> if n `elem` fpargs
                 then [applyF (mname,"match_"++show n)
                              [applyF tcplural [CVar (freshVar n)]]]
                 else []
  CPAs v apat -> if n `elem` fpargs
                 then case apat of
                       CPComb _ _ -> [applyF (mname,"match_"++show n)
                                             [applyF tcplural [CVar v]]]
                       CPFuncComb _ _ -> funPatError
                       CPAs _ _ -> error "Nested as patterns not supported!"
                       _ -> []
                 else []
  CPFuncComb _ _ -> funPatError
  CPLazy _       -> lazyPatError
  CPRecord _ _   -> recPatError

-- Extract the plural arguments from a list of patterns.
-- The second argument is the list of plural argument positions.
-- The result is a renaming of variables into expressions (to be
-- performed in the right-hand side) and the list of new local
-- match and projection functions.
pluralVarsOfPattern :: String -> [Int] -> (Int,CPattern)
                    -> ([(CVarIName,CExpr)],[CFuncDecl])
pluralVarsOfPattern mname fpargs (n,pat) =
  pluralVarsOfPattern' mname fpargs (freshVar n) (n,pat)

pluralVarsOfPattern' :: String -> [Int] -> CVarIName -> (Int,CPattern)
                     -> ([(CVarIName,CExpr)],[CFuncDecl])
pluralVarsOfPattern' mname fpargs dfltpvar (n,pat) =
  if n `notElem` fpargs then ([],[]) else
  case pat of
    CPVar v -> ([(v,applyF tcplural [CVar v])], [])
    CPLit _ -> ([],[])
    CPComb _ pats ->
       (concatMap (projectPluralPatternVars mname dfltpvar ("project_"++show n))
                  (zip [1..] pats),
        stFunc (mname,"match_"++show n) 1 Private
               (baseType (pre "untyped")) --TODO???
               [CRule [pat] (CSimpleRhs preTrue [])] :
        concatMap (projectFunctions mname pat ("project_"++show n))
                  (zip [1..] pats))
    CPAs v apat -> let (renvars,mpfuns) =
                         pluralVarsOfPattern' mname fpargs v (n,apat)
                    in ([(v,applyF tcplural [CVar v])]++renvars,mpfuns)
    CPFuncComb _ _ -> funPatError
    CPLazy _       -> lazyPatError
    CPRecord _ _   -> recPatError

-- Generate the transformation of variables in a constructor pattern
-- into calls to projection functions
projectPluralPatternVars :: String -> CVarIName -> String -> (Int,CPattern)
                         -> [(CVarIName,CExpr)]
projectPluralPatternVars mname newpatvar projname (i,pat) = case pat of
  CPVar v -> [(v,applyF (mname,projname++"_"++show i)
                        [applyF tcplural [CVar newpatvar]])]
  CPLit _ -> []
  CPComb _ pats -> concatMap (projectPluralPatternVars mname newpatvar
                                             (projname++"_"++show i))
                             (zip [1..] pats)
  CPAs _ apat -> projectPluralPatternVars mname newpatvar
                                  (projname++"_"++show i) (1,apat)
  CPFuncComb _ _ -> funPatError
  CPLazy _       -> lazyPatError
  CPRecord _ _   -> recPatError

-- Generate definition of projection functions for a constructor pattern
projectFunctions :: String -> CPattern -> String -> (Int,CPattern)
                 -> [CFuncDecl]
projectFunctions mname cpattern projname (i,pat) = case pat of
  CPVar v -> [stFunc (mname,projname++"_"++show i) 1 Private
                     (baseType (pre "untyped")) --TODO???
                     [CRule [cpattern] (CSimpleRhs (CVar v) [])]]
  CPLit _ -> []
  CPComb _ pats -> concatMap (projectFunctions mname cpattern
                                             (projname++"_"++show i))
                             (zip [1..] pats)
  CPAs _ apat -> projectFunctions mname cpattern (projname++"_"++show i)
                                  (1,apat)
  CPFuncComb _ _ -> funPatError
  CPLazy _       -> lazyPatError
  CPRecord _ _   -> recPatError

-- Translate an expression possibly containing plural arguments.
tPluralExp :: [(QName,[Int])] -> [(CVarIName,CExpr)] -> CExpr -> CExpr
tPluralExp pargs plvars exp = case exp of
  CVar v              -> maybe exp id (lookup v plvars)
  CLit _              -> exp
  CSymbol _           -> exp
  CApply e1 e2        -> tPluralApply pargs plvars e1 e2
  CLambda pats e      -> CLambda pats (tPluralExp pargs plvars e)
  CLetDecl locals e   -> CLetDecl (map (tPluralLocalDecl pargs plvars) locals)
                                  (tPluralExp pargs plvars e)
  CDoExpr stats       -> CDoExpr (map (tPluralStat pargs plvars) stats)
  CListComp e stats   -> CListComp (tPluralExp pargs plvars e)
                                   (map (tPluralStat pargs plvars) stats)
  CCase ct e branches -> CCase ct (tPluralExp pargs plvars e)
                               (map (tPluralBranch pargs plvars) branches)
  CTyped e texp       -> CTyped (tPluralExp pargs plvars e) texp
  _ -> error "tPluralExp: records not yet supported"

tPluralBranch :: [(QName,[Int])] -> [(CVarIName,CExpr)] -> (CPattern,CRhs)
              -> (CPattern,CRhs)
tPluralBranch pargs plvars (pat,rhs) =
  (pat, tPluralRhs pargs plvars rhs)

tPluralRhs :: [(QName,[Int])] -> [(CVarIName,CExpr)] -> CRhs -> CRhs
tPluralRhs pargs plvars (CSimpleRhs exp locals) =
  (CSimpleRhs (tPluralExp pargs plvars exp)
              (map (tPluralLocalDecl pargs plvars) locals))
tPluralRhs pargs plvars (CGuardedRhs guardexps locals) =
  (CGuardedRhs (map tPluralGExp guardexps)
               (map (tPluralLocalDecl pargs plvars) locals))
 where
   tPluralGExp (guard,exp) =
     (tPluralExp pargs plvars guard, tPluralExp pargs plvars exp)

tPluralLocalDecl :: [(QName,[Int])] -> [(CVarIName,CExpr)] -> CLocalDecl
                 -> CLocalDecl
tPluralLocalDecl pargs _ (CLocalFunc fdecl) =
  CLocalFunc (tPluralFunc (error "tPluralLocalDecl")
                          pargs {- TODO: plvars ??? -} fdecl)
tPluralLocalDecl pargs plvars (CLocalPat pat rhs) =
  CLocalPat pat (tPluralRhs pargs plvars rhs)
tPluralLocalDecl _ _ (CLocalVars vs) = CLocalVars vs

tPluralStat :: [(QName,[Int])] -> [(CVarIName,CExpr)] -> CStatement
            -> CStatement
tPluralStat pargs plvars (CSExpr exp) =
  CSExpr (tPluralExp pargs plvars exp)
tPluralStat pargs plvars (CSPat pat exp) =
  CSPat pat (tPluralExp pargs plvars exp)
tPluralStat pargs plvars (CSLet locals) =
  CSLet (map (tPluralLocalDecl pargs plvars) locals)

-- Translate an application. If the operation to be called has plural
-- arguments, they are transformed into lambda abstractions.
tPluralApply :: [(QName,[Int])] -> [(CVarIName,CExpr)] -> CExpr -> CExpr
             -> CExpr
tPluralApply pargs plvars e1 e2 =
  maybe texp
        (\ (qn,args) -> let fpargs = maybe [] id (lookup qn pargs)
                         in if null fpargs
                            then texp
                            else applyF qn
                                   (map (tPluralArg fpargs) (zip [1..] args)))
        (apply2funcall (CApply e1 e2))
 where
  texp = CApply (tPluralExp pargs plvars e1) (tPluralExp pargs plvars e2)

  tPluralArg fpargs (n,arg) =
    if n `elem` fpargs
      then CApply (CSymbol tcPluralArg) (CLambda [CPVar (0,"_")] targ)
      else targ
   where
     targ = tPluralExp pargs plvars arg

------------------------------------------------------------------------
-- AbstractCurryGoodies:

-- try to transform an apply expression into a first-order function call:
apply2funcall :: CExpr -> Maybe (QName,[CExpr])
apply2funcall exp = case exp of
  CApply (CSymbol f) e -> Just (f,[e])
  CApply e1@(CApply _ _) e2 ->
    maybe Nothing (\ (qn,exps) -> Just (qn,exps++[e2])) (apply2funcall e1)
  _ -> Nothing

-- Call to "Prelude.True":
preTrue :: CExpr
preTrue = constF (pre "True")

-- Converts a list of AbstractCurry expressions into a conjunction.
list2conj :: [CExpr] -> CExpr
list2conj cs =
  if null cs then preTrue
             else foldr1 (\c1 c2 -> applyF (pre "&") [c1,c2]) cs
