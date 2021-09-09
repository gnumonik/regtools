module Explore.CmdOpts where

import Options.Applicative 

data Options = Options {
    input :: FilePath
  , modeOptions :: ModeOptions
}

newtype Target = Target FilePath 

type PluginPath = FilePath 
type OutputPath = FilePath 
type HashFilePath = FilePath 


data ModeOptions = PLUGIN PluginPath OutputPath 
                 | SHELL 
                 | CHECKHASH HashFilePath 
           --      | ANALYZE 

getOpts = execParser $ info (parseOptions <**> helper) (
        fullDesc
    <>  progDesc ""
  )

parseOptions :: Parser Options 
parseOptions = Options <$> inPath <*> modeOpts

inPath :: Parser FilePath 
inPath = strOption $ 
  short 'i' <> long "input" <> metavar "INPUT-FILE" <> 
  help "Registry hive bin file to open"

pluginOpt :: Parser ModeOptions 
pluginOpt = PLUGIN 
   <$> argument str (metavar "PLUGIN-FILE")
   <*> argument str (metavar "PLUGIN-OUTPUT")

hashOpt :: Parser ModeOptions 
hashOpt = CHECKHASH 
  <$> argument str (metavar "HASH-INPUT")

shellOpt :: Parser ModeOptions 
shellOpt = pure SHELL 

{--
analyzeOpt :: Parser ModeOptions 
analyzeOpt = pure ANALYZE 
--}

modeOpts :: Parser ModeOptions 
modeOpts = subparser $ 
      command "plugin"  (pluginOpt `info` progDesc "Run a plugin and write the output to a file")
  <>  command "shell"   (shellOpt `info` progDesc "Open the regtools repl")
  <>  command "chkhash" (hashOpt `info` progDesc "Check a key hash file generated by regtools against the input hive bin")
 -- <>  command "analyze" (hashOpt `info` progDesc "Determine the amount of 'dead space' (i.e. areas where no nodes point to) in the hive bin")