module Analyze where

import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, proc, readProcess)

data Results = Results
  { output :: String,
    time :: Float,
    memory :: Float
    -- energy
    -- linesOfCode
  }

trim :: String -> String
trim = unwords . words

analyze :: FilePath -> [String] -> IO Results
analyze command args = do
  -- getPid :: ProcessHandle -> IO (Maybe Pid)
  -- waitForProcess :: ProcessHandle -> IO ExitCode
  -- (_, Just stdout, _, _) <- createProcess (proc command args) {std_out = CreatePipe}
  -- output <- stdout
  stdout <- readProcess command args ""
  let time = 1.0
  let memory = 1.0
  return (Results {output = trim stdout, time = time, memory = memory})
