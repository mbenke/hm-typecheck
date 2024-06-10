module Common.Debug where
import qualified Debug.Trace

-- trace = Debug.Trace.trace
trace = notrace
notrace s a = a
traces ws = trace (unwords ws)
