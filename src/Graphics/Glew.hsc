{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Glew
-- Copyright   :  (c) Ivan Tomac 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some GLEW functions
----------------------------------------------------------------------

module Graphics.Glew where


#if defined ( __linux__ )
import Control.Monad (when)
#endif
import Control.Applicative

import Foreign
import Foreign.C


-- Used only for win32, and exported by Foreign
-- import System.IO.Unsafe


import Graphics.UI.GLUT


#include "GL/glew.h"

-- TODO: Add checks for other X11 systems, not just Linux.

#if defined( _WIN32 )
#   include "GL/wglew.h"
#elif defined( __linux__ )
#   include "GL/glxew.h"
#endif

#ifdef _WIN32

#{
let import name, sig =
   "foreign import stdcall \"gl" #name "\" gl" #name "\n"
   "  :: " #sig "\n"
}

#{
let importptr name, sig =
   "foreign import stdcall \"&__glew" #name "\" glew" #name "\n"
   "  :: Ptr (FunPtr (" #sig "))\n"
   "\n"
   "foreign import stdcall \"dynamic\" mk" #name "\n"
   "  :: FunPtr (" #sig ") -> " #sig "\n"
   "\n"
   "gl" #name " :: " #sig "\n"
   "gl" #name " = mk" #name " $ unsafePerformIO $ peek glew" #name "\n"
}

#{
let importwglptr name, sig =
   "foreign import stdcall \"&__wglew" #name "\" wglew" #name "\n"
   "  :: Ptr (FunPtr (" #sig "))\n"
   "\n"
   "foreign import stdcall \"dynamic\" mk" #name "\n"
   "  :: FunPtr (" #sig ") -> " #sig "\n"
   "\n"
   "wgl" #name " :: " #sig "\n"
   "wgl" #name " = mk" #name " $ unsafePerformIO $ peek wglew" #name "\n"
}

#else

#{
let import name, sig =
   "foreign import ccall \"gl" #name "\" gl" #name "\n"
   "  :: " #sig "\n"
}

#{
let importptr name, sig =
   "foreign import ccall \"&__glew" #name "\" glew" #name "\n"
   "  :: Ptr (FunPtr (" #sig "))\n"
   "\n"
   "foreign import ccall \"dynamic\" mk" #name "\n"
   "  :: FunPtr (" #sig ") -> " #sig "\n"
   "\n"
   "gl" #name " :: " #sig "\n"
   "gl" #name " = mk" #name " $ unsafePerformIO $ peek glew" #name "\n"
}

#{
let importglxptr name, sig =
   "foreign import ccall \"&__glewX" #name "\" glewX" #name "\n"
   "  :: Ptr (FunPtr (" #sig "))\n"
   "\n"
   "foreign import ccall \"dynamic\" mk" #name "\n"
   "  :: FunPtr (" #sig ") -> " #sig "\n"
   "\n"
   "glX" #name " :: " #sig "\n"
   "glX" #name " = mk" #name " $ unsafePerformIO $ peek glewX" #name "\n"
}

#endif

#{
importptr VertexAttribPointer
        , GLuint -> GLint       \
                 -> GLenum      \
                 -> GLboolean   \
                 -> GLsizei     \
                 -> Ptr a       \
                 -> IO ()
}

glVertexShader :: GLenum
glVertexShader = #const GL_VERTEX_SHADER

glFragmentShader :: GLenum
glFragmentShader = #const GL_FRAGMENT_SHADER

glInfoLogLength :: GLenum
glInfoLogLength = #const GL_INFO_LOG_LENGTH

glBool   :: GLenum
glBool   = #const GL_BOOL

glByte   :: GLenum
glByte   = #const GL_BYTE

glShort  :: GLenum
glShort  = #const GL_SHORT

glInt    :: GLenum
glInt    = #const GL_INT

glUByte  :: GLenum
glUByte  = #const GL_UNSIGNED_BYTE

glUShort :: GLenum
glUShort = #const GL_UNSIGNED_SHORT

glUInt   :: GLenum
glUInt   = #const GL_UNSIGNED_INT

glFloat  :: GLenum
glFloat  = #const GL_FLOAT

glDouble :: GLenum
glDouble = #const GL_DOUBLE

glFront :: GLenum
glFront = #const GL_FRONT

glDrawFramebufferEXT :: GLenum
glDrawFramebufferEXT = #const GL_DRAW_FRAMEBUFFER_EXT

glReadFramebufferEXT :: GLenum
glReadFramebufferEXT = #const GL_READ_FRAMEBUFFER_EXT

glColorAttachment0EXT :: GLenum
glColorAttachment0EXT = #const GL_COLOR_ATTACHMENT0_EXT

glFramebufferEXT :: GLenum
glFramebufferEXT = #const GL_FRAMEBUFFER_EXT

glRenderbufferEXT :: GLenum
glRenderbufferEXT = #const GL_RENDERBUFFER_EXT

glTexture1D,glTexture2D,glTexture3D :: GLenum
glTexture1D = #const GL_TEXTURE_1D
glTexture2D = #const GL_TEXTURE_2D
glTexture3D = #const GL_TEXTURE_3D

glRGBA :: GLenum
glRGBA = #const GL_RGBA

glTextureMinFilter :: GLenum
glTextureMinFilter = #const GL_TEXTURE_MIN_FILTER

glTextureMagFilter :: GLenum
glTextureMagFilter = #const GL_TEXTURE_MAG_FILTER

glTextureWrapS :: GLenum
glTextureWrapS = #const GL_TEXTURE_WRAP_S

glTextureWrapT :: GLenum
glTextureWrapT = #const GL_TEXTURE_WRAP_T

glLinear :: GLint
glLinear = #const GL_LINEAR

glNearest :: GLint
glNearest = #const GL_NEAREST

glLinearMipmapNearest :: GLint
glLinearMipmapNearest = #const GL_LINEAR_MIPMAP_NEAREST

glLinearMipmapLinear :: GLint
glLinearMipmapLinear = #const GL_LINEAR_MIPMAP_LINEAR

glNearestMipmapNearest :: GLint
glNearestMipmapNearest = #const GL_NEAREST_MIPMAP_NEAREST

glNearestMipmapLinear :: GLint
glNearestMipmapLinear = #const GL_NEAREST_MIPMAP_LINEAR

glClamp :: GLint
glClamp = #const GL_CLAMP

glClampToEdge :: GLint
glClampToEdge = #const GL_CLAMP_TO_EDGE

glRepeat :: GLint
glRepeat = #const GL_REPEAT

glTexture0 :: GLenum
glTexture0 = #const GL_TEXTURE0

#import GenTextures   , GLsizei -> Ptr GLuint -> IO ()
#import DeleteTextures, GLsizei -> Ptr GLuint -> IO ()
#import BindTexture   , GLenum -> GLuint -> IO ()
#import TexParameteri , GLenum -> GLenum -> GLint -> IO ()
#import TexParameterf , GLenum -> GLenum -> GLfloat -> IO ()

glGenTexture :: IO GLuint
glGenTexture = alloca ((>>) <$> glGenTextures 1 <*> peek)

glDeleteTexture :: GLuint -> IO ()
glDeleteTexture = flip with $ glDeleteTextures 1

#{
import TexImage2D
     , GLenum -> GLint      \
              -> GLint      \
              -> GLsizei    \
              -> GLsizei    \
              -> GLint      \
              -> GLenum     \
              -> GLenum     \
              -> Ptr a      \
              -> IO ()
}

#import DrawBuffer, GLenum -> IO ()

#importptr ActiveTexture, GLenum -> IO ()

#importptr GenFramebuffersEXT    , GLsizei -> Ptr GLuint -> IO ()
#importptr GenRenderbuffersEXT   , GLsizei -> Ptr GLuint -> IO ()
#importptr DeleteFramebuffersEXT , GLsizei -> Ptr GLuint -> IO ()
#importptr DeleteRenderbuffersEXT, GLsizei -> Ptr GLuint -> IO ()
#importptr BindFramebufferEXT    , GLenum -> GLuint -> IO ()
#importptr BindRenderbufferEXT   , GLenum -> GLuint -> IO ()
#importptr GenerateMipmapEXT     , GLenum -> IO ()

#{
importptr FramebufferRenderbufferEXT
        , GLenum -> GLenum -> GLenum -> GLuint -> IO ()
}

#{
importptr FramebufferTexture2DEXT
        , GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ()
}

#{
importptr RenderbufferStorageEXT
        , GLenum -> GLenum -> GLsizei -> GLsizei -> IO ()
}

glGenFramebufferEXT :: IO GLuint
glGenFramebufferEXT = alloca ((>>) <$> glGenFramebuffersEXT 1 <*> peek)

glGenRenderbufferEXT :: IO GLuint
glGenRenderbufferEXT = alloca ((>>) <$> glGenRenderbuffersEXT 1 <*> peek)

glDeleteFramebufferEXT :: GLuint -> IO ()
glDeleteFramebufferEXT = flip with $ glDeleteFramebuffersEXT 1

glDeleteRenderbufferEXT :: GLuint -> IO ()
glDeleteRenderbufferEXT = flip with $ glDeleteRenderbuffersEXT 1

#importptr EnableVertexAttribArray , GLuint -> IO ()
#importptr DisableVertexAttribArray, GLuint -> IO ()
#importptr GetAttribLocation       , GLuint -> CString -> IO GLint
#importptr GetUniformLocation      , GLuint -> CString -> IO GLint

#importptr CreateShader, GLenum -> IO GLuint
#importptr ShaderSource, GLuint -> GLsizei -> Ptr CString -> Ptr GLint -> IO ()

#importptr CompileShader, GLuint -> IO ()
#importptr CreateProgram, IO GLuint
#importptr AttachShader , GLuint -> GLuint -> IO ()
#importptr LinkProgram  , GLuint -> IO ()
#importptr UseProgram   , GLuint -> IO ()

#importptr GetProgramiv, GLuint -> GLenum -> Ptr GLint -> IO ()
#importptr GetShaderiv , GLuint -> GLenum -> Ptr GLint -> IO ()

#{
importptr GetProgramInfoLog
        , GLuint -> GLsizei -> Ptr GLsizei -> CString -> IO ()
}

#{
importptr GetShaderInfoLog
        , GLuint -> GLsizei -> Ptr GLsizei -> CString -> IO ()
}

glGetProgramLog :: GLuint -> IO String
glGetProgramLog = glGetLog glGetProgramiv glGetProgramInfoLog

-- glGetProgramLog p =
--   do len <- alloca ((>>) <$> glGetProgramiv p glInfoLogLength <*> peek)
--      allocaArray
--          (fromIntegral len)
--          ((>>) <$> glGetProgramInfoLog p (fromIntegral len) nullPtr <*> peekCString)

glGetShaderLog :: GLuint -> IO String
glGetShaderLog = glGetLog glGetShaderiv glGetShaderInfoLog

-- glGetShaderLog p =
--   do len <- alloca ((>>) <$> glGetShaderiv p glInfoLogLength <*> peek)
--      allocaArray
--          (fromIntegral len)
--          ((>>) <$> glGetShaderInfoLog p (fromIntegral len) nullPtr <*> peekCString)

-- abstract commonality:

-- glGetLog :: GLenum
--             -> (GLuint -> GLsizei -> Ptr a -> Ptr CChar -> IO b)
--             -> GLuint
--             -> IO String
glGetLog :: (Storable c, Integral c, Num b) =>
            (a -> GLenum -> Ptr c -> IO d)
            -> (a -> b -> Ptr q -> Ptr CChar -> IO d)
            -> a
            -> IO String

glGetLog getFrom getLog p =
  do len <- alloca ((>>) <$> getFrom p glInfoLogLength <*> peek)
     allocaArray
         (fromIntegral len)
         ((>>) <$> getLog p (fromIntegral len) nullPtr <*> peekCString)


#importptr Uniform1f, GLint -> GLfloat -> IO ()
#importptr Uniform2f, GLint -> GLfloat -> GLfloat -> IO ()
#importptr Uniform3f, GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()
#importptr Uniform4f, GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
#importptr Uniform1i, GLint -> GLint -> IO ()
#importptr Uniform2i, GLint -> GLint -> GLint -> IO ()
#importptr Uniform3i, GLint -> GLint -> GLint -> GLint -> IO ()
#importptr Uniform4i, GLint -> GLint -> GLint -> GLint -> GLint -> IO ()

#importptr Uniform1fv, GLint -> GLsizei -> Ptr GLfloat -> IO ()
#importptr Uniform2fv, GLint -> GLsizei -> Ptr GLfloat -> IO ()
#importptr Uniform3fv, GLint -> GLsizei -> Ptr GLfloat -> IO ()
#importptr Uniform4fv, GLint -> GLsizei -> Ptr GLfloat -> IO ()
#importptr Uniform1iv, GLint -> GLsizei -> Ptr GLint -> IO ()
#importptr Uniform2iv, GLint -> GLsizei -> Ptr GLint -> IO ()
#importptr Uniform3iv, GLint -> GLsizei -> Ptr GLint -> IO ()
#importptr Uniform4iv, GLint -> GLsizei -> Ptr GLint -> IO ()


glGetAttribLoc :: GLuint -> String -> IO GLint
glGetAttribLoc o s = withCString s $ glGetAttribLocation o

glGetUniformLoc :: GLuint -> String -> IO GLint
glGetUniformLoc o s = withCString s $ glGetUniformLocation o

glLoadShader :: GLuint -> String -> IO ()
glLoadShader o s = withCString s $ (`with` glLoadShader')
 where
   glLoadShader' = glShaderSource o 1 `flip` nullPtr

glEnableVSync :: Bool -> IO ()
glWaitVSync   :: IO ()

#if defined( _WIN32 )

#importwglptr SwapIntervalEXT, CInt -> IO CInt

glEnableVSync = fmap (const ()) . wglSwapIntervalEXT . fromIntegral . fromEnum
glWaitVSync   = return ()

#elif defined ( __linux__ )

#importglxptr GetVideoSyncSGI , Ptr CUInt -> IO CInt
#importglxptr WaitVideoSyncSGI, CInt -> CInt -> Ptr CUInt -> IO CInt

#importglxptr SwapIntervalSGI, CInt -> IO CInt

foreign import ccall "&__GLXEW_SGI_video_sync" glxew_SGI_video_sync
  :: Ptr GLboolean

glxewSGIVideoSync :: Bool
glxewSGIVideoSync = unsafePerformIO $ (toEnum.fromIntegral) <$> peek glxew_SGI_video_sync

glWaitVSync =
  when glxewSGIVideoSync $ do
    count <- alloca ((>>) <$> glXGetVideoSyncSGI <*> peek)
    _     <- alloca $ glXWaitVideoSyncSGI 2 (fromIntegral ((count + 1) `mod` 2))
    return ()

foreign import ccall "&__GLXEW_SGI_swap_control" glxew_SGI_swap_control
  :: Ptr GLboolean

glxewSGISwapControl :: Bool
glxewSGISwapControl =
  unsafePerformIO $ (toEnum.fromIntegral) <$> peek glxew_SGI_swap_control

glEnableVSync =
  when glxewSGISwapControl .
    fmap (const ()) . glXSwapIntervalSGI . fromIntegral . fromEnum

#else

glEnableVSync = const $ return ()
glWaitVSync   = return ()

#endif

foreign import ccall "glewInit" glewInit :: IO Int


---- Queries

glAlloc1 :: Storable a => (Ptr a  -> IO ()) -> IO a
glAlloc1 f = alloca ((>>) <$> f <*> peek)

-- TODO: use glAlloc1 above.


#import GetDoublev  , GLenum -> Ptr GLdouble  -> IO ()
#import GetBooleanv , GLenum -> Ptr GLboolean -> IO ()
#import GetFloatv   , GLenum -> Ptr GLfloat   -> IO ()
#import GetIntegerv , GLenum -> Ptr GLint     -> IO ()


glGetDouble  :: GLenum -> IO GLdouble
glGetBoolean :: GLenum -> IO GLboolean
glGetFloat   :: GLenum -> IO GLfloat
glGetInteger :: GLenum -> IO GLint

glGetDouble  = glAlloc1 . glGetDoublev
glGetBoolean = glAlloc1 . glGetBooleanv
glGetFloat   = glAlloc1 . glGetFloatv
glGetInteger = glAlloc1 . glGetIntegerv


-- There are a *lot* of enums.  We could import all of them from glew.h,
-- with some simple code generation.  Start by grepping for #define in glew.h.
-- 
-- Or #include glew.h and use the constants directly.

glMaxCombinedTextureImageUnits :: GLenum
glMaxCombinedTextureImageUnits = #const GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS


-- GLAPI void GLAPIENTRY glEnable (GLenum cap);

#import Enable , GLenum -> IO ()
