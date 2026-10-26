-- |
-- Module      : Harmonic.Interface.Tidal.Recall
-- Description : Progressions as paste-able text (save \/ recall)
--
-- A generated 'PC.ProgressionContext' lives only as long as the GHCi
-- session that bound it. 'save' prints it as a chart — one bar per line,
-- the three layers side by side — shaped as a complete @name = recall …@
-- statement. Paste that into the live file and evaluating it binds the
-- identical context in any later session. The format is lossless for every
-- family: each state carries its notes (root first, in the stored
-- spelling), its stored chord name (the graph key), its approach movement
-- and, where the notes alone cannot pin it, its enharmonic spelling; strata
-- contexts carry per-bar provenance as a fourth column.
--
-- 'recall' is pure, so the pasted binding can be re-evaluated at will, and
-- lenient enough to hand-write: @"C E G (pedal)"@ names itself through the
-- corpus namer, and a bar with no movement reads as a pedal.
module Harmonic.Interface.Tidal.Recall
  ( save
  , recall
  , recallE
  , renderChart
  , renderState
  , parseState
  , familyWord
  ) where

import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (intercalate, transpose, dropWhileEnd, isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
import qualified Data.Sequence as Seq
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import Text.Read (readMaybe)

import Harmonic.Rules.Types.Pitch (NoteName, PitchClass(..), pitchClass, mkPitchClass)
import qualified Harmonic.Rules.Types.Harmony as H
import qualified Harmonic.Rules.Types.Progression as Prog
import qualified Harmonic.Rules.Types.ProgressionContext as PC
import qualified Harmonic.Rules.Types.Scale as Sc

-------------------------------------------------------------------------------
-- State  <->  "notes… name (movement) [flat|sharp]"
-------------------------------------------------------------------------------

-- |Render one state. Notes come root first; the remaining intervals are
-- spelled with the stored enharmonic function. The spelling word is appended
-- only when parsing the bare text would recover a different spelling (an
-- all-natural chord whose stored spelling is the non-default one), so the
-- common case stays a plain lead sheet.
renderState :: H.CadenceState -> String
renderState cs = unwords (base ++ mark)
  where
    root   = H.stateCadenceRoot cs
    rootPC = pitchClass root
    spell  = H.enharmonicFunc (H.stateSpelling cs)
    cad    = H.stateCadence cs
    ivs    = case H.cadenceIntervals cad of
               (P 0 : rest) -> rest
               other        -> other
    notes  = show root : [ show (spell (i + rootPC)) | i <- ivs ]
    name   = case H.cadenceFunctionality cad of
               "" -> "-"
               n  -> n
    base   = notes ++ [name, "(" ++ show (H.cadenceMovement cad) ++ ")"]
    mark   | parseState (unwords base) == Right cs = []
           | otherwise = [spellingWord (H.stateSpelling cs)]

spellingWord :: H.EnharmonicSpelling -> String
spellingWord H.FlatSpelling  = "flat"
spellingWord H.SharpSpelling = "sharp"

-- |Parse one state. Grammar: one or more note names (first = root), then in
-- any order an optional chord name (@-@ for an empty name; absent → the
-- corpus namer), an optional @(movement)@ in @show@ form or the integer
-- form of @lead'@ (absent → pedal), and an optional @flat@ \/ @sharp@.
parseState :: String -> Either String H.CadenceState
parseState s =
  case span isNoteTok (tokens s) of
    ([], _)          -> Left ("no notes in " ++ show s)
    (noteToks, rest) -> do
      let (rootTok : tailToks) = noteToks
          root   = noteFromTok rootTok
          rootPC = pitchClass root
          ivs    = P 0 : [ pitchClass (noteFromTok t) - rootPC | t <- tailToks ]
      (mName, mMove, mSpell) <- classify rest (Nothing, Nothing, Nothing)
      let name = case mName of
                   Nothing  -> H.corpusFunctionality ivs
                   Just "-" -> ""
                   Just n   -> n
          move = maybe H.Unison id mMove
          abs' = [ unPitchClass (i + rootPC) | i <- ivs ]
          lowered = map (map toLower) tailToks
          spelling = case mSpell of
            Just sp -> sp
            Nothing -> case H.noteNameImpliesSpelling root of
              Just sp -> sp
              Nothing
                | any (\t -> 'b' `elem` drop 1 t) lowered -> H.FlatSpelling
                | any ('#' `elem`) lowered                -> H.SharpSpelling
                | otherwise                                -> H.inferSpelling abs'
      pure (H.CadenceState (H.Cadence name move ivs) root spelling)
  where
    -- words, except that one parenthesised group ("(asc 5)") stays whole
    tokens str = case break (== '(') str of
      (before, '(' : rest) | (inner, ')' : after) <- break (== ')') rest
        -> words before ++ ["(" ++ inner ++ ")"] ++ words after
      _ -> words str

    classify [] acc = Right acc
    classify (t : ts) (n, m, sp)
      | Just sp' <- spellingFromWord t =
          if sp == Nothing then classify ts (n, m, Just sp') else Left ("two spellings in " ++ show s)
      | "(" `isPrefixOf` t && ")" `isSuffixOf` t = do
          mv <- parseMoveTok (init (drop 1 t))
          if m == Nothing then classify ts (n, Just mv, sp) else Left ("two movements in " ++ show s)
      | otherwise =
          if n == Nothing then classify ts (Just t, m, sp) else Left ("two chord names in " ++ show s)

    spellingFromWord t = case map toLower t of
      "flat"  -> Just H.FlatSpelling
      "sharp" -> Just H.SharpSpelling
      _       -> Nothing

    parseMoveTok inner
      | Just n <- readMaybe inner :: Maybe Int = Right (H.toMovement (P 0) (mkPitchClass n))
      | inner == "empty" = Right H.Empty
      | otherwise = case read inner :: H.Movement of
          H.Empty -> Left ("unknown movement " ++ show inner)
          mv      -> Right mv

-- The seventeen note names, matched case-insensitively on their @show@ form.
noteTable :: [(String, NoteName)]
noteTable = [ (map toLower (show n), n) | n <- [minBound .. maxBound] ]

isNoteTok :: String -> Bool
isNoteTok t = map toLower t `elem` map fst noteTable

noteFromTok :: String -> NoteName
noteFromTok t = maybe (error ("not a note name: " ++ t)) id (lookup (map toLower t) noteTable)

-------------------------------------------------------------------------------
-- Context  <->  chart lines
-------------------------------------------------------------------------------

-- |The family keyword 'save' prints and 'recall' expects.
familyWord :: PC.Family -> String
familyWord PC.FTriad    = "triad"
familyWord PC.FExtended = "extended"
familyWord PC.FStrata   = "strata"
familyWord PC.FJazz     = "jazz"
familyWord PC.FPoly     = "poly"

familyFromWord :: String -> Either String PC.Family
familyFromWord w =
  case [ f | f <- [minBound .. maxBound], familyWord f == map toLower w ] of
    (f : _) -> Right f
    []      -> Left ("unknown family " ++ show w ++ "; one of triad extended strata jazz poly")

-- |One line per bar: @T | S | M@, plus @| tristrata strata@ when the context
-- carries provenance. Columns are padded so the separators line up.
renderChart :: PC.ProgressionContext -> [String]
renderChart pc = map (dropWhileEnd (== ' ') . intercalate " | ") (align rows)
  where
    states = map (map renderState . toList . Prog.unProgression)
               [PC.triadLayer pc, PC.strataLayer pc, PC.modeLayer pc]
    prov   = case PC.pcProvenance pc of
               Nothing -> []
               Just sq -> [map renderProv (toList sq)]
    rows   = transpose (states ++ prov)
    align rs = let widths = map (maximum . (0 :) . map length) (transpose rs)
               in map (zipWith padTo widths) rs
    padTo n str = str ++ replicate (n - length str) ' '

renderProv :: (Sc.Tristrata, Sc.StrataLabel) -> String
renderProv (t, s) =
  intercalate "-" (map show [Sc.ts1 t, Sc.ts2 t, Sc.ts3 t]) ++ " " ++ show s

parseProv :: String -> Either String (Sc.Tristrata, Sc.StrataLabel)
parseProv str = case words str of
  [tri, lab] | [Just a, Just b, Just c] <- map readMaybe (splitOn "-" tri)
             , Just l <- readMaybe lab -> Right (Sc.Tristrata a b c, l)
  _ -> Left ("bad provenance " ++ show str ++ "; expected e.g. I-V-X V")

-- |Rebuild a context from its family keyword and chart lines; the total
-- form of 'recall' for callers that want the error rather than a crash.
recallE :: String -> [String] -> Either String PC.ProgressionContext
recallE famWord ls = do
  fam  <- familyFromWord famWord
  bars <- mapM parseBar (zip [1 :: Int ..] ls)
  let provs = [ p | (_, _, _, p) <- bars ]
  prov <- case (all isJust' provs, any isJust' provs) of
    (True, True) -> Right (Just (Seq.fromList [ p | Just p <- provs ]))
    (_, False)   -> Right Nothing
    _            -> Left "provenance must be on every bar or none"
  let lyr pick = Prog.fromCadenceStates [ pick b | b <- bars ]
  pure . PC.normalizeFamily $ PC.ProgressionContext
    { PC.triadLayer   = lyr (\(t, _, _, _) -> t)
    , PC.strataLayer  = lyr (\(_, s, _, _) -> s)
    , PC.modeLayer    = lyr (\(_, _, m, _) -> m)
    , PC.pcProvenance = prov
    , PC.pcFamily     = fam
    }
  where
    isJust' = maybe False (const True)
    at i msg = either (\e -> Left ("bar " ++ show i ++ ": " ++ msg ++ e)) Right
    parseBar (i, line) = case map trim (splitOn "|" line) of
      [t, s, m]    -> (,,,) <$> at i "T " (parseState t) <*> at i "S " (parseState s)
                            <*> at i "M " (parseState m) <*> pure Nothing
      [t, s, m, p] -> (,,,) <$> at i "T " (parseState t) <*> at i "S " (parseState s)
                            <*> at i "M " (parseState m) <*> (Just <$> at i "" (parseProv p))
      segs         -> Left ("bar " ++ show i ++ ": expected T | S | M [| provenance], got "
                              ++ show (length segs) ++ " segments")
    trim = dropWhileEnd (== ' ') . dropWhile (== ' ')

-- |Bind a saved chart back to a context. The first argument is the family
-- keyword 'save' printed; each string is one bar, @T | S | M@ states
-- separated by bars, with an optional fourth @tristrata strata@ column.
-- Malformed text raises an error naming the bar and column.
--
-- @
-- verse = recall "triad"
--   [ "D F A min (pedal)  | D F G A C minadd11 (pedal) | …"
--   , "A C E min (desc 5) | …"
--   ]
-- @
recall :: String -> [String] -> PC.ProgressionContext
recall famWord = either error id . recallE famWord

-- |Print a context as a complete, paste-ready @name = recall …@ statement:
--
-- @
-- save "verse" s
-- -- saved 2026-10-17 · 4 bars · triad
-- verse = recall "triad"
--   [ "D F A min (pedal)  | … | …"
--   , …
--   ]
-- @
--
-- Copy the block into the live file; evaluating it next session binds
-- @verse@ equal to @s@ (every layer, name, movement, spelling and strata
-- provenance). The text is parsed back and compared before printing, so a
-- block that would not round-trip is reported instead of emitted.
save :: String -> PC.ProgressionContext -> IO ()
save name pc = do
  day <- formatTime defaultTimeLocale "%Y-%m-%d" <$> getZonedTime
  let fam   = familyWord (PC.pcFamily pc)
      ls    = renderChart pc
      body  = case ls of
                [] -> ["  []"]
                (l : rest) -> ("  [ " ++ show l) : map (("  , " ++) . show) rest ++ ["  ]"]
      bars  = PC.pcLength pc
  case recallE fam ls of
    Right pc' | pc' == pc -> mapM_ putStrLn $
      ("-- saved " ++ day ++ " · " ++ show bars ++ (if bars == 1 then " bar · " else " bars · ") ++ fam)
      : (name ++ " = recall " ++ show fam)
      : body
    Right pc' -> putStrLn ("-- save: round-trip mismatch at bar " ++ show (firstDiff pc pc') ++ "; nothing emitted")
    Left err  -> putStrLn ("-- save: own output failed to parse (" ++ err ++ "); nothing emitted")
  where
    firstDiff a b =
      let bars' x = zip3 (states (PC.triadLayer x)) (states (PC.strataLayer x)) (states (PC.modeLayer x))
          states  = toList . Prog.unProgression
          diffs   = [ i | (i, x, y) <- zip3 [1 :: Int ..] (bars' a) (bars' b), x /= y ]
      in case diffs of { (i : _) -> i; [] -> 0 }
