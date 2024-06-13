{-# LANGUAGE OverloadedStrings #-}

module SPL.ArgParse where 

defaultOutputFilePath :: FilePath
defaultOutputFilePath = "output.ssm"

data Args = Args {
    getOutputFile :: FilePath,
    hideInfo :: Bool,
    hideWarnings :: Bool,
    skipOptimizer :: Bool
} deriving (Show)

getOutputFilePath :: [String] -> FilePath
getOutputFilePath [] = defaultOutputFilePath
getOutputFilePath ("--output":outputfile:_) = outputfile
getOutputFilePath ("--o":outputfile:_) = outputfile
getOutputFilePath (_:args) = getOutputFilePath args

parseArgs :: [String] -> Args
parseArgs args = Args {
                        getOutputFile = getOutputFilePath args,
                        hideInfo = "--hide-info" `elem` args,
                        hideWarnings = "--hide-warnings" `elem` args,
                        skipOptimizer = "--skip-optimizer" `elem` args
                    }