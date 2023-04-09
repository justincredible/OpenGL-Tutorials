module ShaderManager (ShaderSource(..), ShaderInfo(..), loadShaders) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Graphics.Rendering.OpenGL

data ShaderSource =
    ByteStringSource B.ByteString
    | StringSource String
    | FileSource FilePath
    deriving (Eq, Ord, Show)

getSource (ByteStringSource bs) = return bs
getSource (StringSource str) = return $ packUtf8 str
getSource (FileSource path) = B.readFile path

data ShaderInfo = ShaderInfo ShaderType ShaderSource
    deriving (Eq, Ord, Show)

loadShaders infos =
    createProgram `bracketOnError` deleteObjectName $ \program -> do
        loadCompileAttach program infos
        linkAndCheck program
        return program

linkAndCheck = checked linkProgram linkStatus programInfoLog "link"

loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shType source : infos) =
    createShader shType `bracketOnError` deleteObjectName $ \shader -> do
        src <- getSource source
        shaderSourceBS shader $= src
        compileAndCheck shader
        attachShader program shader
        loadCompileAttach program infos

compileAndCheck = checked compileShader compileStatus shaderInfoLog "compile"

checked :: (t -> IO ()) -> (t -> GettableStateVar Bool) -> (t -> GettableStateVar String) -> String -> t -> IO ()
checked action getStatus getInfoLog message object = do
    action object
    ok <- get (getStatus object)
    unless ok $ do
        infoLog <- get (getInfoLog object)
        fail (message ++ " log: " ++ infoLog)
