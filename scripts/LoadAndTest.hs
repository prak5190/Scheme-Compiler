import FrameworkHs.Testing
import FrameworkHs.Helpers (Option (Default, Option))
import System.Exit         (exitSuccess, exitFailure)

main = do 
  (vs,ivs) <- runDefault
  if (all isPass vs && all isFail ivs)
   then exitSuccess
   else exitFailure


isPass :: TestResult -> Bool
isPass (Pass s) = True
isPass (Fail e) = False

isFail :: TestResult -> Bool
isFail (Fail e) = True
isFail (Pass s) = False
