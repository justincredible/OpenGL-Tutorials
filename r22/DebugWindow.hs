module DebugWindow (DebugWindow,initialize) where

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL

import Flow.Parameters
import Flow.Render
import Flow.Shutdown
import Flow.Update

data DebugWindow = DebugWindow {
    getVertexArray :: GLuint,
    getVertexBuffer :: GLuint,
    getIndexBuffer :: GLuint,
    getVertexCount :: GLsizei,
    getIndexCount :: GLsizei,
    getWindowSize :: (GLsizei, GLsizei),
    getDebugWindowSize :: (GLsizei, GLsizei),
    getPrevPos :: (GLint, GLint) }
    deriving (Eq, Show)

initialize wWidth wHeight bmWidth bmHeight = do
    vertexArray <- alloca $ (>>) . glGenVertexArrays 1 <*> peek
    
    glBindVertexArray vertexArray
    
    vertexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    
    let vertexSize = 5*sizeOf (0 :: GLfloat)
    glBufferData GL_ARRAY_BUFFER (fromIntegral $ 4*vertexSize) nullPtr GL_DYNAMIC_DRAW
    
    glEnableVertexAttribArray 0
    glEnableVertexAttribArray 1
    
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (fromIntegral vertexSize) nullPtr
    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (fromIntegral vertexSize) $ bufferOffset (3*sizeOf (0 :: GLfloat))
    
    indexBuffer <- alloca $ (>>) . glGenBuffers 1 <*> peek
    
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER indexBuffer
    withArray ([0,1,2,2,1,3] :: [GLubyte]) $ \ptr ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ 6*sizeOf (0 :: GLubyte)) ptr GL_STATIC_DRAW
    
    glBindVertexArray 0
    
    return (True, Just $ DebugWindow vertexArray vertexBuffer indexBuffer 4 6 (fromIntegral wWidth,fromIntegral wHeight) (bmWidth,bmHeight) (-1,-1))
    
    where
    bufferOffset = plusPtr nullPtr . fromIntegral

instance Render DebugWindow where
    render debugwindow@(DebugWindow vArray _ _ _ iCount _ _ _) = do
        glBindVertexArray vArray
        
        glDrawElements GL_TRIANGLES iCount GL_UNSIGNED_BYTE nullPtr
        
        glBindVertexArray 0
        
        return (True,debugwindow)

instance Update DebugWindow where
    update debugwindow (II posx posy)
        | (fromIntegral posx,fromIntegral posy) == getPrevPos debugwindow = return (True,debugwindow)
        | otherwise = do
            let (ww,wh) = getWindowSize debugwindow
                (bmw,bmh) = getDebugWindowSize debugwindow
                left = fromIntegral posx - fromIntegral ww/2
                right = left + fromIntegral bmw
                top = fromIntegral wh/2 - fromIntegral posy
                bottom = top - fromIntegral bmh
                vertices :: [GLfloat]
                vertices = [
                    left, top, 0, 0, 0,
                    left, bottom, 0, 0, 1,
                    right, top, 0, 1, 0,
                    right, bottom, 0, 1, 1 ]
                vertexsz = fromIntegral $ 5*sizeOf (0 :: GLfloat)
            glBindVertexArray (getVertexArray debugwindow)
    
            glBindBuffer GL_ARRAY_BUFFER (getVertexBuffer debugwindow)
            
            withArray vertices $
                flip (glBufferData GL_ARRAY_BUFFER (4*vertexsz)) GL_DYNAMIC_DRAW
            
            return (True,debugwindow { getPrevPos = (fromIntegral posx, fromIntegral posy) })

    update debugwindow _ = do
        putStrLn "Incorrect DebugWindow parameters."
        return (False,debugwindow)

instance Shutdown DebugWindow where
    shutdown (DebugWindow vArray vBuffer iBuffer _ _ _ _ _) = do
        glBindVertexArray vArray
        
        glDisableVertexAttribArray 0
        glDisableVertexAttribArray 1
        
        glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0
        with iBuffer $ glDeleteBuffers 1
        
        glBindBuffer GL_ARRAY_BUFFER 0
        with vBuffer $ glDeleteBuffers 1
        
        glBindVertexArray 0
        with vArray $ glDeleteVertexArrays 1
