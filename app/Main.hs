{-# LANGUAGE QuasiQuotes #-}

-- base

-- includes many sub-modules

-- GLFW-b

-- gl

-- raw-strings-qq

import ClipSubdiv (TrisData (TrisData), subdivide)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C.String (withCAStringLen)
import Graphics.GL.Core33
import Graphics.GL.Types
import Graphics.UI.GLFW (Key (Key'Space), KeyState (KeyState'Pressed))
import qualified Graphics.UI.GLFW as GFLW
import qualified Graphics.UI.GLFW as GLFW
import Text.RawString.QQ

winWidth = 800

winHeight = 600

winTitle = "Hello Triangle"

-- | Ensures that we only run GLFW code while it's initialized, and also that we
-- always terminate it when we're done. Also, this function should only be used
-- from the main thread.
bracketGLFW :: IO () -> IO ()
bracketGLFW act = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
  when initWorked act

-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
  -- print key
  when
    (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
    (GLFW.setWindowShouldClose window True)

vertexShaderSource :: String
vertexShaderSource =
  [r|#version 330 core
    layout (location = 0) in vec3 position;
    void main()
    {
        gl_Position = vec4(position.x, position.y, position.z, 1.0);
    }
    |]

fragmentShaderSource :: String
fragmentShaderSource =
  [r|#version 330 core
    out vec4 color;
    void main()
    {
        color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
    }
    |]

-- | Given a shader type and a shader source, it gives you (Right id) of the
-- successfully compiled shader, or (Left err) with the error message. In the
-- error case, the shader id is deleted before the function returns to avoid
-- accidentally leaking shader objects.
loadShader :: GLenum -> String -> IO (Either String GLuint)
loadShader shaderType source = do
  -- new shader object
  shaderID <- glCreateShader shaderType
  -- assign the source to the shader object
  withCAStringLen source $ \(strP, strLen) ->
    withArray [strP] $ \linesPtrsPtr ->
      withArray [fromIntegral strLen] $ \lengthsPtr ->
        glShaderSource shaderID 1 linesPtrsPtr lengthsPtr
  -- compile and check success
  glCompileShader shaderID
  success <- alloca $ \successP -> do
    glGetShaderiv shaderID GL_COMPILE_STATUS successP
    peek successP
  if success == GL_TRUE
    -- success: we're done
    then return (Right shaderID)
    -- failure: we get the log, delete the shader, and return the log.
    else do
      -- how many bytes the info log should be (including the '\0')
      logLen <- alloca $ \logLenP -> do
        glGetShaderiv shaderID GL_INFO_LOG_LENGTH logLenP
        peek logLenP
      -- space for the info log
      logBytes <- allocaBytes (fromIntegral logLen) $ \logP -> do
        -- space for the log reading result
        alloca $ \resultP -> do
          -- Try to obtain the log bytes
          glGetShaderInfoLog shaderID logLen resultP logP
          -- this is how many bytes we actually got
          result <- fromIntegral <$> peek resultP
          peekArray result logP
      -- delete the shader object and return the log
      glDeleteShader shaderID
      let prefix = case shaderType of
            GL_VERTEX_SHADER -> "Vertex"
            GL_GEOMETRY_SHADER -> "Geometry"
            GL_FRAGMENT_SHADER -> "Fragment"
            _ -> "Unknown Type"
      return $
        Left $
          prefix
            ++ " Shader Error:"
            ++ (map (toEnum . fromEnum) logBytes)

-- | Given a vertex shader object and a fragment shader object, this will link
-- them into a new program, giving you (Right id). If there's a linking error
-- the error log is retrieved, the program deleted, and (Left err) is returned.
linkProgram :: GLuint -> GLuint -> IO (Either String GLuint)
linkProgram vertexID fragmentID = do
  programID <- glCreateProgram
  glAttachShader programID vertexID
  glAttachShader programID fragmentID
  glLinkProgram programID
  success <- alloca $ \successP -> do
    glGetProgramiv programID GL_LINK_STATUS successP
    peek successP
  if success == GL_TRUE
    -- success: we're done
    then return (Right programID)
    -- failure: we get the log, delete the shader, and return the log.
    else do
      -- how many bytes the info log should be (including the '\0')
      logLen <- alloca $ \logLenP -> do
        glGetProgramiv programID GL_INFO_LOG_LENGTH logLenP
        peek logLenP
      -- space for the info log
      logBytes <- allocaBytes (fromIntegral logLen) $ \logP -> do
        -- space for the log reading result
        alloca $ \resultP -> do
          -- Try to obtain the log bytes
          glGetProgramInfoLog programID logLen resultP logP
          -- this is how many bytes we actually got
          result <- fromIntegral <$> peek resultP
          peekArray result logP
      -- delete the program object and return the log
      glDeleteProgram programID
      return $
        Left $
          "Program Link Error: "
            ++ (map (toEnum . fromEnum) logBytes)

-- | Given the source for the vertex shader and the fragment shader, compiles
-- both and links them into a single program. If all of that is successful, the
-- intermediate shaders are deleted before the final value is returned.
programFromSources :: String -> String -> IO (Either String GLuint)
programFromSources vertexSource fragmentSource = do
  eitherVertShader <- loadShader GL_VERTEX_SHADER vertexSource
  case eitherVertShader of
    Left e -> return $ Left e
    Right vertShader -> do
      eitherFragShader <- loadShader GL_FRAGMENT_SHADER fragmentSource
      case eitherFragShader of
        Left e -> do
          glDeleteShader vertShader
          return $ Left e
        Right fragShader -> do
          eitherProgram <- linkProgram vertShader fragShader
          glDeleteShader vertShader
          glDeleteShader fragShader
          return $ eitherProgram

main :: IO ()
main = bracketGLFW $ do
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
  case maybeWindow of
    Nothing -> putStrLn "Failed to create a GLFW window!"
    Just window -> do
      -- enable keys
      GLFW.setKeyCallback window (Just callback)

      -- calibrate the viewport
      GLFW.makeContextCurrent (Just window)
      (x, y) <- GLFW.getFramebufferSize window
      glViewport 0 0 (fromIntegral x) (fromIntegral y)

      -- ready our program
      eErrP <- programFromSources vertexShaderSource fragmentShaderSource
      shaderProgram <- case eErrP of
        Left e -> putStrLn e >> return 0
        Right p -> return p

      -- activate the program
      glUseProgram shaderProgram

      --
      -- -- setup our verticies
      -- let verticies = [
      --         0.5,  0.5, 0.0,  -- Top Right
      --         0.5, -0.5, 0.0,  -- Bottom Right
      --         -0.5, -0.5, 0.0, -- Bottom Left
      --         -0.5,  0.5, 0.0  -- Top Left
      --         ] :: [GLfloat]
      -- let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length verticies)
      -- verticesP <- newArray verticies
      --
      -- -- setup the indexes
      -- let indices = [  -- Note that we start from 0!
      --         0, 1, 3, -- First Triangle
      --         1, 2, 3  -- Second Triangle
      --         ] :: [GLuint]
      --
      -- let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * (length indices)
      -- indicesP <- newArray indices

      -- setup a vertex array object
      vaoP <- malloc
      glGenVertexArrays 1 vaoP
      vao <- peek vaoP
      glBindVertexArray vao

      -- setup a vertex buffer object and send it data
      vboP <- malloc
      glGenBuffers 1 vboP
      vbo <- peek vboP
      glBindBuffer GL_ARRAY_BUFFER vbo
      -- glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_STATIC_DRAW

      -- setup an element buffer object and send it data
      eboP <- malloc
      glGenBuffers 1 eboP
      ebo <- peek eboP
      glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
      -- glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize (castPtr indicesP) GL_STATIC_DRAW

      -- assign the attribute pointer information
      let threeFloats = fromIntegral $ sizeOf (0.0 :: GLfloat) * 3
      glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE threeFloats nullPtr
      glEnableVertexAttribArray 0

      -- unbind our vertex array object to prevent accidental changes in
      -- between our draw calls.
      glBindVertexArray 0

      -- Uncomment this line for "wireframe mode"
      glPolygonMode GL_FRONT_AND_BACK GL_LINE

      demo <- malloc :: IO (Ptr Int)
      poke demo 1

      coef <- malloc :: IO (Ptr Float)
      poke coef 0

      -- enter our main loop
      let loop = do
            shouldContinue <- not <$> GLFW.windowShouldClose window
            when shouldContinue $ do
              -- event poll
              GLFW.pollEvents

              time <- realToFrac . fromMaybe 0 <$> GLFW.getTime

              -- Demo helpers

              let avg (x1, y1, z1) (x2, y2, z2) = ((x1 + x2) / 2.0, (y1 + y2) / 2.0, (z1 + z2) / 2.0)
              c <- peek coef
              let lod (x, _, _) = toEnum $ min 7 $ round ((x + 1) / 2.0 * c) + 1
              let leftOfCenter (x, _, _) = if x < 0 then 5 else 2
              let trans fn (TrisData vs is) = TrisData (map fn vs) is

              let rotate = trans (\(x, y, z) -> (x * (cos time) - y * (sin time), x * (sin time) + y * (cos time), z))

              -- Demo chooser

              let onKey action key = do
                    k <- GLFW.getKey window key
                    case k of
                      KeyState'Pressed -> action
                      _ -> return ()

              let demoKey value = onKey (poke demo value)

              demoKey 1 GLFW.Key'1
              demoKey 2 GLFW.Key'2
              demoKey 0 GLFW.Key'0

              onKey (peek coef >>= \v -> poke coef (v + 0.1)) GLFW.Key'Up
              onKey (peek coef >>= \v -> poke coef (v - 0.1)) GLFW.Key'Down


              d <- peek demo :: IO Int

              let (TrisData vs is) =
                    case d of
                      1 ->
                        subdivide avg lod 0 (rotate start) [0, 1, 2]
                        where
                          start = TrisData [(-0.8, 0.0, 0.0), (0.4, 0.4, 0.0), (0.5, -0.6, 0.0)] []
                      2 ->
                        subdivide avg lod 0 (rotate penta) [0, 1, 2, 3, 4]
                        where
                          tau = 2 * pi
                          co = 0.7
                          vs2 = map ((\t -> (co * cos t, co * sin t, 0.0)) . (* tau) . (/ 5)) [0 .. 4]
                          penta = TrisData vs2 []
                      _ ->
                        TrisData [] []

              -- Rendering code

              let vertices = concatMap (\(x, y, z) -> [x, y, z]) vs
              let indices = concatMap (\(i, ii, iii) -> [i, ii, iii]) is

              let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length vertices)
              verticesP <- newArray vertices

              let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * (length indices)
              indicesP <- newArray indices

              glBindBuffer GL_ARRAY_BUFFER vbo
              glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_DYNAMIC_DRAW

              glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
              glBufferData GL_ELEMENT_ARRAY_BUFFER indicesSize (castPtr indicesP) GL_DYNAMIC_DRAW
              -- clear the screen
              glClearColor 0.2 0.3 0.3 1.0
              glClear GL_COLOR_BUFFER_BIT
              -- draw the triangle
              glBindVertexArray vao
              glDrawElements GL_TRIANGLES (toEnum $ length indices) GL_UNSIGNED_INT nullPtr
              glBindVertexArray 0
              -- swap buffers and go again
              GLFW.swapBuffers window
              loop
      loop

--
