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

import Control.Applicative

import Foreign
import Foreign.C

import System.IO.Unsafe

import Graphics.UI.GLUT


#include "GL/glew.h"

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

glTexture2D :: GLenum
glTexture2D = #const GL_TEXTURE_2D

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
glGetProgramLog p = do
    len <- alloca ((>>) <$> glGetProgramiv p glInfoLogLength <*> peek)
    allocaArray
        (fromIntegral len)
        ((>>) <$> glGetProgramInfoLog p len nullPtr <*> peekCString)

glGetShaderLog :: GLuint -> IO String
glGetShaderLog p = do
    len <- alloca ((>>) <$> glGetShaderiv p glInfoLogLength <*> peek)
    allocaArray
        (fromIntegral len)
        ((>>) <$> glGetShaderInfoLog p len nullPtr <*> peekCString)


#importptr Uniform1f, GLint -> GLfloat -> IO ()
#importptr Uniform2f, GLint -> GLfloat -> GLfloat -> IO ()
#importptr Uniform3f, GLint -> GLfloat -> GLfloat -> GLfloat -> IO ()
#importptr Uniform4f, GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
#importptr Uniform1i, GLint -> GLint -> IO ()
#importptr Uniform2i, GLint -> GLint -> GLint -> IO ()
#importptr Uniform3i, GLint -> GLint -> GLint -> GLint -> IO ()
#importptr Uniform4i, GLint -> GLint -> GLint -> GLint -> GLint -> IO ()

-- What to do with the 'const GLfloat *' etc?

-- #importptr Uniform1fv, GLint -> GLsizei -> const GLfloat * -> IO ()
-- #importptr Uniform2fv, GLint -> GLsizei -> const GLfloat * -> IO ()
-- #importptr Uniform3fv, GLint -> GLsizei -> const GLfloat * -> IO ()
-- #importptr Uniform4fv, GLint -> GLsizei -> const GLfloat * -> IO ()
-- #importptr Uniform1iv, GLint -> GLsizei -> const GLint * -> IO ()
-- #importptr Uniform2iv, GLint -> GLsizei -> const GLint * -> IO ()
-- #importptr Uniform3iv, GLint -> GLsizei -> const GLint * -> IO ()
-- #importptr Uniform4iv, GLint -> GLsizei -> const GLint * -> IO ()


glGetAttribLoc :: GLuint -> String -> IO GLint
glGetAttribLoc o s = withCString s $ glGetAttribLocation o

glGetUniformLoc :: GLuint -> String -> IO GLint
glGetUniformLoc o s = withCString s $ glGetUniformLocation o

glLoadShader :: GLuint -> String -> IO ()
glLoadShader o s = withCString s $ (`with` glLoadShader')
    where
        glLoadShader' = glShaderSource o 1 `flip` nullPtr

foreign import ccall "glewInit" glewInit :: IO Int
