import FrameworkHs.Testing
import FrameworkHs.Helpers (Option (Default, Option))

type Valid = [TestResult]
type Invalid = [TestResult]

testDefault :: IO (Valid,Invalid)
testDefault = testAll Default Default
