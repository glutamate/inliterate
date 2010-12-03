module InOut where

import Language.Haskell.Exts
--import Language.Haskell.Exts.Pretty
import Data.Maybe
import System.Environment
import Text.PrettyPrint
--import System.IO.Unsafe


modDecls f (Module _ _ _ _ _ _ decls) = f decls

onDecls f (Module  v1 v2 v3 v4 v5 v6 decls) = Module v1 v2 v3 v4 v5 v6 $ f decls

setModuleName s (Module  v1 v2 v3 v4 v5 v6 decls) = Module v1 (ModuleName s) v3 v4 v5 v6 decls


addImport m (Module  v1 v2 v3 v4 v5 v6 decls) = Module v1 v2 v3 v4 v5 (v6++[m]) decls

impUnsafe = ImportDecl {importLoc = SrcLoc {srcFilename = "TestSinks.hs", srcLine = 7, srcColumn = 1}, importModule = ModuleName "System.IO.Unsafe", importQualified = True, importSrc = False, importPkg = Nothing, importAs = Just (ModuleName "SysIOUnface"), importSpecs = Nothing}

impAsk = ImportDecl {importLoc = SrcLoc {srcFilename = "TestSinks.hs", srcLine = 7, srcColumn = 1}, importModule = ModuleName "Ask", importQualified = True, importSrc = False, importPkg = Nothing, importAs = Just (ModuleName "Ask"), importSpecs = Nothing}

maySink (SpliceDecl sloc (InfixApp (var)
 (QVarOp (UnQual (Symbol "*>"))) sink)) = Just (sloc, var,sink)
maySink _ = Nothing

maySrc (SpliceDecl sloc (InfixApp (Var (UnQual (Ident vnm)))
 (QVarOp (UnQual (Symbol "<*"))) src)) = Just (sloc, vnm, src)
maySrc _ = Nothing

isSrcOrSink d = isJust (maySrc d) || isJust (maySink d)
isSink d = isJust (maySink d)

transSrc d = 
    case maySrc d of 
      Just (sloc, vnm, src) ->  PatBind (sloc) (PVar (Ident vnm)) Nothing (UnGuardedRhs (App (Var (Qual (ModuleName "SysIOUnface") (Ident "unsafePerformIO"))) src)) (BDecls [])
      Nothing -> d

addMain decls = 
    let sinks = catMaybes $ map maySink decls
        iosink (sloc, exp, sink) = Qualifier $ App sink exp
--Qualifier (App (App (Var (UnQual (Ident "writeSink"))) exp) (sink))
        themain = PatBind (SrcLoc "foo" 1 1) (PVar (Ident "main")) Nothing (UnGuardedRhs (Do (map iosink sinks))) (BDecls [])
    in decls++[themain]


chomp (' ':s) =  s
chomp s = s
 
withEq s = init (show $ chomp s)++"\\n  =>  \""


splitInlit :: [String] -> ([String],[String])
splitInlit = sI [] [] where
  sI tl mn [] = (reverse tl,
                 reverse  mn)
  sI tl mn (('>':'>':ln):lns) = sI (chomp ln:tl) mn lns
  sI tl mn (('>':ln):lns) = sI (chomp ln:tl) (("putStrLn "++show (chomp ln)):mn) lns
--  sI tl mn (('~':'>':ln):lns) = sI tl (("putStrLn $ "++withEq ln++"++show ("++chomp ln++")"):mn) lns
  sI tl mn (ln@('=':'>':_):lns) = 
      let (cmd',rest) = span isCmdOrComment lns
          cmd = map (drop 2) $ ln:(filter isCmd cmd')
          printIt = map (("putStrLn $ "++) . show . chomp) cmd
          askIt = "Ask.ask $ \n"++ unlines (map (ind 3) cmd)
      in sI tl (askIt:(reverse printIt++mn)) rest
  sI tl mn (ln:lns) = sI tl (("putStrLn "++show ln):mn) lns
  isComment ('-':'-':_) = True
  isComment s = False
  isCmd ('=':'>':_) = True
  isCmd s = False
  isCmdOrComment s = isCmd s || isComment s

inoutxform = filter (not . isSink) . addMain . map transSrc   

inxform = filter (not . isSink) . map transSrc   

ind n s = replicate n ' ' ++ s
