module Main where

import Language.Haskell.Exts
import Options.Applicative
import Text.PrettyPrint
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set

data Args = Args {
        argSigPath :: FilePath,
        argModPaths :: [FilePath]
    } deriving (Show)

argParser :: Parser Args
argParser = Args
    <$> argument str (metavar "SIG")
    <*> many (argument str (metavar "MOD"))

parseFileIO :: FilePath -> IO (Module SrcSpanInfo)
parseFileIO = fmap fromParseResult . parseFile

main :: IO ()
main = do
    args <- execParser (info argParser mempty)
    sig <- parseFileIO (argSigPath args)
    mods <- mapM parseFileIO (argModPaths args)
    let sig_exports = moduleExports sig
        max_len = maximum (map length sig_exports) + 1
    putStrLn . render $
        text " " <+> pipe <+> hsep (intersperse pipe (map renderModName mods)) $$
        hcat (intersperse pipe (replicate (length mods + 1) (text "---"))) $$
        vcat (map (renderRow max_len mods) sig_exports)

renderRow :: Int -> [Module l] -> String -> Doc
renderRow max_len mods sym =
    text sym $$ nest max_len (pipe <+> hsep (intersperse pipe (map (renderMod sym) mods)))

renderModName :: Module l -> Doc
renderModName (Module _ (Just (ModuleHead _ (ModuleName _ s) _ _)) _ _ _) = text s
renderModName _ = text " "

renderMod :: String -> Module l -> Doc
renderMod sym mod =
    if sym `Set.member` avail
        then text "✔"
        else text " "
  where
    -- Boo, should memo this!
    avail = Set.fromList (moduleExports mod)

pipe :: Doc
pipe = char '|'

moduleExports :: Module l -> [String]
moduleExports = from_mod
  where
    from_mod (Module _ (Just hd) _ _ _) = from_head hd
    from_mod (Module _ Nothing _ _ _)   = []
    from_head (ModuleHead _ _ _ (Just ex)) = from_exports ex
    from_head (ModuleHead _ _ _ Nothing) = []
    from_exports (ExportSpecList _ es) = concatMap from_export es
    from_export (EVar _ qn) = from_qn qn
    -- TODO: Probably shouldn't ignore namespace here
    from_export (EAbs _ _ qn) = from_qn qn
    -- NB: Purposely ignored constructors
    from_export (EThingWith _ _ qn _) = from_qn qn
    from_export (EModuleContents _ _) = []
    from_qn (UnQual _ n) = [from_n n]
    from_qn _ = []
    from_n (Ident _ s) = s
    from_n (Symbol _ s) = s
