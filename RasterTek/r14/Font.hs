module Font (Font,Font.initialize,buildVertexArray,buildIndexArray) where

import Data.Word

import Flow.Shutdown
import Texture

data CharFont = CharFont (Float,Float) Int
    deriving (Eq,Show)

data Font = Font {
    getList :: [CharFont], -- worse than 
    getTexture :: Maybe Texture }
    deriving (Eq,Show)

initialize fontdata texfile texunit = do
    fontlist <- fmap (map (parse.words) . lines) . readFile $ fontdata
    
    (success, texture) <- Texture.initialize texfile texunit False
    
    return (success, Just $ Font (map snd fontlist) texture)
    where
    parse [] = (' ',CharFont (0,0) 0)
    parse (x:xs)
        | read x == 32 = parse []
        | otherwise = let
            char = head . head $ xs
            left = read . head . tail $ xs
            right = read . head . tail . tail $ xs
            size = read . last $ xs
            in
            (char,CharFont (left,right) size)

instance Shutdown Font where
    shutdown (Font _ texture) = shutdown texture

buildVertexArray count _ dl [] _ _ = (count,concat . dl $ [])
buildVertexArray count font dl (c:sentence) drawx drawy
    | fromEnum c == 32 = buildVertexArray count font dl sentence (drawx + 3) drawy
    | otherwise = let
        CharFont (left,right) size = getList font !! (fromEnum c - 32)
        quad = [
            drawx, drawy, 0, left, 1,
            drawx, drawy-16, 0, left, 0,
            drawx + fromIntegral size, drawy, 0, right, 1,
            drawx + fromIntegral size, drawy-16, 0, right, 0 ]
        in
        buildVertexArray (count+1) font (dl.(quad:)) sentence (drawx + fromIntegral size + 1) drawy

buildIndexArray sentence = f (fromIntegral $ 4*length sentence) (0 :: Word8)
    where
    f m n
        | n >= m = []
        | otherwise = [n,n+1,n+2,n+2,n+1,n+3] ++ f m (n+4)
