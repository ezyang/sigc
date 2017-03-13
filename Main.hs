module Main where

import Language.Haskell.Exts
import Options.Applicative
import Text.PrettyPrint
import Data.Set (Set)
import Data.Char
import Data.List
import Control.Monad
import qualified Data.Set as Set

data Args = Args {
        argSigPath :: FilePath,
        argModPaths :: [String]
    } deriving (Show)

argParser :: Parser Args
argParser = Args
    <$> argument str (metavar "SIG")
    <*> many (argument str (metavar "MOD"))

parseFileIO :: FilePath -> IO (Module SrcSpanInfo, [Comment])
parseFileIO = fmap fromParseResult . parseFileWithComments defaultParseMode

main :: IO ()
main = do
    args <- execParser (info argParser mempty)
    (sig, sig_comments) <- parseFileIO (argSigPath args)
    let labeledModPaths = map splitModPath (argModPaths args)
    mods <- forM labeledModPaths $ \(k, v) -> do
        (mod, _) <- parseFileIO v
        return (k, mod)
    let sig_exports = moduleExports sig
        max_len = maximum (map (length . snd) sig_exports) + 1
        header =
            text (replicate max_len ' ') <> pipe <> hcat (intersperse pipe (map renderModName mods)) $$
            text (replicate max_len '-') <> pipe <> hcat (intersperse pipe (replicate (length mods) (text "---")))
    let rows = interleaveExports sig_exports sig_comments
    putStrLn . render $ vcat (map (renderRow header max_len (map snd mods)) rows)

splitModPath :: String -> (Maybe String, FilePath)
splitModPath s =
    case span (/= '=') s of
        (k, '=':v) -> (Just k, v)
        _ -> (Nothing, s)

data Row = RowHeader String
         | Row String

interleaveExports :: [(SrcSpanInfo, String)] -> [Comment] -> [Row]
interleaveExports [] _ = []
interleaveExports es [] = map (Row . snd) es
interleaveExports es'@((le,e):es) cs'@((Comment _ lc c):cs)
    | lc < srcInfoSpan le
    = case headerComment c of
        Just s -> RowHeader s : interleaveExports es' cs
        Nothing -> interleaveExports es' cs
    | otherwise
    = Row e : interleaveExports es cs'

headerComment :: String -> Maybe String
headerComment s =
    case span (== '*') (dropWhile isSpace s) of
        ("*", rs) -> Just (dropWhile isSpace rs)
        _ -> Nothing

renderRow :: Doc -> Int -> [Module l] -> Row -> Doc
renderRow header _ _ (RowHeader s) =
    text "" $$ text "**" <> text s <> text "**" $$ text "" $$ header
renderRow _header max_len mods (Row sym) =
    text sym $$ nest max_len (pipe <+> hsep (intersperse pipe (map (renderMod sym) mods)))

renderModName :: (Maybe String, Module l) -> Doc
renderModName (Just s, _)
    | length s == 1 = text (" " ++ s ++ " ")
    | length s == 2 = text (" " ++ s)
    | otherwise = text s
renderModName (Nothing, (Module _ (Just (ModuleHead _ (ModuleName _ s) _ _)) _ _ _)) = text s
renderModName _ = text " "

renderMod :: String -> Module l -> Doc
renderMod sym mod =
    if sym `Set.member` avail
        then text "✔"
        else text " "
  where
    -- Boo, should memo this!
    avail = Set.fromList (map snd (moduleExports mod))

pipe :: Doc
pipe = char '|'

moduleExports :: Module l -> [(l, String)]
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
    from_n (Ident l s) = (l, s)
    from_n (Symbol l s) = (l, s)
