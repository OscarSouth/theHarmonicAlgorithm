-- verify_pipeline.hs
-- Quick verification of the new complete pipeline diagnostics
-- Run: stack ghci --ghci-options -XOverloadedStrings

:load boot/BootTidal.hs

putStrLn "\n=== Pipeline Diagnostic Verification ==="
putStrLn "Testing gen'' with new complete pipeline view...\n"

let start = initCadenceState 0 "C" [0,4,7] FlatSpelling
    ctx = defaultContext

gen'' start 3 "*" 0.5 ctx >>= \(prog, diag) ->
  let steps = gdSteps diag
  in do
    putStrLn $ "Starting: " ++ gdStartCadence diag ++ " @ " ++ gdStartRoot diag
    putStrLn $ "Generated " ++ show (gdActualLen diag) ++ " chords\n"
    
    forM_ (take 1 steps) $ \step -> do
      putStrLn "═══════════════════════════════════════════════════════════════════"
      putStrLn $ "═══ STEP " ++ show (sdStepNumber step) ++ " - COMPLETE PIPELINE ==="
      putStrLn "═══════════════════════════════════════════════════════════════════\n"
      
      putStrLn "[1] PRIOR STATE (before selection):"
      putStrLn $ "    Cadence: " ++ sdPriorCadence step
      putStrLn $ "    Root: " ++ sdPriorRoot step ++ " (PC=" ++ show (sdPriorRootPC step) ++ ")\n"
      
      putStrLn "[2] SELECTED FROM DB (raw database record):"
      putStrLn $ "    Intervals: " ++ sdSelectedDbIntervals step
      putStrLn $ "    Movement: " ++ sdSelectedDbMovement step
      putStrLn $ "    Functionality: " ++ sdSelectedDbFunctionality step
      putStrLn $ "    Selected from: " ++ sdSelectedFrom step ++ "\n"
      
      case sdAdvanceTrace step of
        Just at -> do
          putStrLn "[3] ADVANCE (apply movement):"
          putStrLn $ "    " ++ show (atCurrentRootPC at) ++ " + " ++ show (atMovementInterval at) 
                     ++ " = " ++ show (atNewRootPC at) ++ " → " ++ atNewRoot at ++ "\n"
        Nothing -> putStrLn "[3] ADVANCE: (verbosity too low)\n"
      
      putStrLn "[4] POSTERIOR STATE:"
      putStrLn $ "    Root: " ++ sdPosteriorRoot step ++ " (PC=" ++ show (sdPosteriorRootPC step) ++ ")\n"
      
      case sdTransformTrace step of
        Just tt -> do
          putStrLn "[5] RENDER (transpose & name):"
          putStrLn $ "    DB intervals: " ++ ttRawDbIntervals tt
          putStrLn $ "    Transpose by PC " ++ show (ttRootPC tt) ++ ": " ++ show (ttTransposedPitches tt)
          putStrLn $ "    Normalize: " ++ show (ttNormalizedPs tt)
          putStrLn $ "    Zero-form: " ++ show (ttZeroForm tt)
          putStrLn $ "    Name: " ++ ttFunctionality tt
          putStrLn $ "    Final: " ++ ttFinalChord tt ++ "\n"
        Nothing -> putStrLn "[5] RENDER: (verbosity too low)\n"
    
    putStrLn "═══════════════════════════════════════════════════════════════════"
    putStrLn "Final progression:"
    print prog
    putStrLn "\n✓ Pipeline diagnostic verification complete"

:quit
