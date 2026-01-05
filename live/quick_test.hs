-- Quick manual test of gen'' pipeline diagnostics
-- Run in GHCi: :l live/quick_test.hs

import qualified Harmonic.Lib as H

main :: IO ()
main = do
  putStrLn "\n=== Testing Complete Pipeline Diagnostics ==="
  
  let start = H.initCadenceState 0 "C" [0,4,7] H.FlatSpelling
      ctx = H.defaultContext
  
  (prog, diag) <- H.gen'' start 2 "*" 0.5 ctx
  
  putStrLn $ "\nStarting cadence: " ++ H.gdStartCadence diag
  putStrLn $ "Generated " ++ show (H.gdActualLen diag) ++ " chords"
  
  let step1 = head (H.gdSteps diag)
  
  putStrLn "\n═══ STEP 1 VERIFICATION ═══"
  putStrLn $ "Prior root: " ++ H.sdPriorRoot step1
  putStrLn $ "Selected DB intervals: " ++ H.sdSelectedDbIntervals step1
  putStrLn $ "Selected DB movement: " ++ H.sdSelectedDbMovement step1
  putStrLn $ "Posterior root: " ++ H.sdPosteriorRoot step1
  
  case H.sdTransformTrace step1 of
    Just tt -> do
      putStrLn "\n✓ Transform trace available:"
      putStrLn $ "  Raw DB intervals: " ++ H.ttRawDbIntervals tt
      putStrLn $ "  Raw DB movement: " ++ H.ttRawDbMovement tt
      putStrLn $ "  Final chord: " ++ H.ttFinalChord tt
    Nothing -> putStrLn "\n✗ No transform trace (verbosity too low)"
  
  putStrLn "\n✓ All diagnostic fields accessible"
  putStrLn "\nFinal progression:"
  print prog
