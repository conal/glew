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


import Foreign
import Foreign.C

import System.IO.Unsafe

import Graphics.UI.GLUT


#include "GL/glew.h"

#ifdef _WIN32

#{
let import name, sig =
   "foreign import stdcall \"gl" #name \"\" gl" #name "\n"
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


#importptr EnableVertexAttribArray, GLuint -> IO ()
#importptr GetAttribLocation      , GLuint -> CString -> IO GLint
#importptr GetUniformLocation     , GLuint -> CString -> IO GLint

#importptr CreateShader, GLenum -> IO GLuint
#importptr ShaderSource, GLuint -> GLsizei -> Ptr CString -> Ptr GLint -> IO ()

#importptr CompileShader, GLuint -> IO ()
#importptr CreateProgram, IO GLuint
#importptr AttachShader , GLuint -> GLuint -> IO ()
#importptr LinkProgram  , GLuint -> IO ()
#importptr UseProgram   , GLuint -> IO ()

#{
importptr Uniform4f
        , GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
}

glGetAttribLoc :: GLuint -> String -> IO GLint
glGetAttribLoc o s = withCString s $ glGetAttribLocation o

glGetUniformLoc :: GLuint -> String -> IO GLint
glGetUniformLoc o s = withCString s $ glGetUniformLocation o

glLoadShader :: GLuint -> String -> IO ()
glLoadShader o s = withCString s $ (`with` glLoadShader')
    where
        glLoadShader' = glShaderSource o 1 `flip` nullPtr

foreign import ccall "glewInit" glewInit :: IO Int
