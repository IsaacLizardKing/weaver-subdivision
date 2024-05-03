{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE QuasiQuotes #-}

import ClipSubdiv (TrisData (TrisData), subdivide)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C.String (withCAStringLen)
import Graphics.GL.Core33
import Graphics.GL.Types
import qualified Graphics.UI.GLFW as GLFW
import Text.RawString.QQ
import Control.Applicative ((<|>))

winWidth :: Int
winWidth = 800

winHeight :: Int
winHeight = 600

winTitle :: String
winTitle = "Hello Triangle"

-- | Ensures that we only run GLFW code while it's initialized, and also that we
-- always terminate it when we're done. Also, this function should only be used
-- from the main thread.
bracketGLFW :: IO () -> IO ()
bracketGLFW act = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
  when initWorked act

-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
callback :: GLFW.KeyCallback
callback window key _scanCode keyState _modKeys = when
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
            ++ map (toEnum . fromEnum) logBytes

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
            ++ map (toEnum . fromEnum) logBytes

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
          return eitherProgram

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
      GLFW.setKeyCallback window (Just callback)

      GLFW.makeContextCurrent (Just window)
      (winX, winY) <- GLFW.getFramebufferSize window
      glViewport 0 0 (fromIntegral winX) (fromIntegral winY)

      eErrP <- programFromSources vertexShaderSource fragmentShaderSource
      shaderProgram <- case eErrP of
        Left e -> putStrLn e >> return 0
        Right p -> return p

      glUseProgram shaderProgram

      -- setup a vertex array object
      vaoP <- malloc
      glGenVertexArrays 1 vaoP
      vao <- peek vaoP
      glBindVertexArray vao

      -- setup a VBO
      vboP <- malloc
      glGenBuffers 1 vboP
      vbo <- peek vboP
      glBindBuffer GL_ARRAY_BUFFER vbo

      -- setup an EBO
      eboP <- malloc
      glGenBuffers 1 eboP
      ebo <- peek eboP
      glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo

      -- assign the attribute pointer information
      let threeFloats = fromIntegral $ sizeOf (0.0 :: GLfloat) * 3
      glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE threeFloats nullPtr
      glEnableVertexAttribArray 0

      -- unbind our vertex array object to prevent accidental changes in
      -- between our draw calls.
      glBindVertexArray 0

      glPolygonMode GL_FRONT_AND_BACK GL_LINE

      -- main loop
      let loop demo coef controllable = do
            shouldContinue <- not <$> GLFW.windowShouldClose window
            when shouldContinue $ do
              -- event poll
              GLFW.pollEvents

              time <- realToFrac . fromMaybe 0 <$> GLFW.getTime

              -- Demo helpers

              let avg (x1, y1, z1) (x2, y2, z2) = ((x1 + x2) / 2.0, (y1 + y2) / 2.0, (z1 + z2) / 2.0)
              let lod (x, _, _) = toEnum $ min 7 $ round ((x + 1) / 2.0 * coef) + 1
              let _leftOfCenter (x, _, _) = if x < 0 then 5 else 2
              let trans fn (TrisData vs is) = TrisData (map fn vs) is
              let ripple (x, y, _) = toEnum $ max 2 $ min 6 $ round (controllable + 3 * cos (sqrt (x ^ (2 :: Int) + y ^ (2 :: Int)) * coef - time))
              let movable (x, y, _) = toEnum $ max 0 $ min 6 $ round (coef + (x * sin controllable + y * cos controllable))

              let vertRotate = trans (\(x, y, z) -> (x * cos time - y * sin time, x * sin time + y * cos time, z))

              -- Demo chooser

              let keyP key value = do
                    k <- GLFW.getKey window key
                    case k of
                      GLFW.KeyState'Pressed -> return $ Just value
                      _ -> return Nothing

              scenes <- sequence [keyP GLFW.Key'0 0, keyP GLFW.Key'1 1, keyP GLFW.Key'2 2, keyP GLFW.Key'3 3, keyP GLFW.Key'4 4, keyP GLFW.Key'5 5, keyP GLFW.Key'6 6, keyP GLFW.Key'7 7, keyP GLFW.Key'8 8, keyP GLFW.Key'9 9]
              let demo' = fromMaybe demo $ foldl1 (<|>) scenes

              change <- sequence [keyP GLFW.Key'Up (+0.1), keyP GLFW.Key'Down (+ (-0.1)), keyP GLFW.Key'0 (const 0)]
              let coef' = (fromMaybe id $ foldl1 (<|>) change) coef

              move <- sequence [keyP GLFW.Key'Left (+0.1), keyP GLFW.Key'Right (+ (-0.1))]
              let controllable' = (fromMaybe id $ foldl1 (<|>) move) controllable

              let (TrisData vs is) =
                    case demo' of
                      1 ->
                        subdivide avg lod 0 (vertRotate start) [0, 1, 2]
                        where
                          start = TrisData [(-0.8, 0.0, 0.0), (0.4, 0.4, 0.0), (0.5, -0.6, 0.0)] []
                      2 ->
                        subdivide avg lod 0 (vertRotate penta) [0, 1, 2, 3, 4]
                        where
                          tau = 2 * pi
                          co = 0.7
                          vs2 = map ((\t -> (co * cos t, co * sin t, 0.0)) . (* tau) . (/ 5)) [0 .. 4]
                          penta = TrisData vs2 []
                      3 ->
                        subdivide avg ripple 0 start [0, 1, 2]
                        where
                          start = TrisData [(0.0, 0.8, 0.0), (-0.8, -0.8, 0.0), (0.8, -0.8, 0.0)] []
                      4 ->
                        subdivide avg ripple 0 penta [0, 1, 2, 3, 4]
                        where
                          tau = 2 * pi
                          co = 0.7
                          vs2 = map ((\t -> (co * cos t, co * sin t, 0.0)) . (* tau) . (/ 5)) [0 .. 4]
                          penta = TrisData vs2 []
                      5 ->
                        subdivide avg movable 0 start [0, 1, 2]
                        where
                          start = TrisData [(0.0, 0.8, 0.0), (-0.8, -0.8, 0.0), (0.8, -0.8, 0.0)] []
                      6 ->
                        subdivide avg movable 1 penta [0, 1, 2, 3, 4]
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

              let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * length vertices
              verticesP <- newArray vertices

              let indicesSize = fromIntegral $ sizeOf (0 :: GLuint) * length indices
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

              loop demo' coef' controllable'
      loop (1 :: Int) (0.5 :: Float) (3.0 :: Float)

--
