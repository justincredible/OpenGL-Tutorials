module Model (Model,initialize,render) where

import Graphics.Rendering.OpenGL as GL
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr,plusPtr)
import Foreign.Storable (sizeOf)

data Model = Model VertexArrayObject NumArrayIndices

initialize = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    let vertices = [
            -1.0, -1.0, 0.0,    0.0, 1.0, 0.0, 
            0.0, 1.0, 0.0,      0.0, 1.0, 0.0, 
            1.0, -1.0, 0.0,     0.0, 1.0, 0.0 ]  :: [GLfloat]
        numVertices = length vertices
        indices = [0..2] :: [GLuint]
        numIndices = length indices

    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let vertexSize = sizeOf (head vertices)
    withArray vertices $ \ptr -> do
        let sizev = fromIntegral (numVertices * vertexSize)
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    indexBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just indexBuffer
    let indexSize = sizeOf (head indices)
    withArray indices $ \ptr -> do
        let sizei = fromIntegral (numIndices * indexSize)
        bufferData ElementArrayBuffer $= (sizei, ptr, StaticDraw)

    let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
        stride     = 6 * floatSize
        bufferOffset = plusPtr nullPtr . fromIntegral

    let position  = AttribLocation 0
        posOffset  = 0 * floatSize
    vertexAttribPointer position $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset posOffset))
    vertexAttribArray position $= Enabled

    let color  = AttribLocation 1
        clrOffset  = 3 * floatSize
    vertexAttribPointer color $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset clrOffset))
    vertexAttribArray color $= Enabled
    
    return $ Model triangles (fromIntegral numIndices)

render (Model vao idx) = do
    bindVertexArrayObject $= Just vao
    drawElements Triangles idx GL.UnsignedInt nullPtr
