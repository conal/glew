{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Graphics.Glew
-- Copyright   :  (c) Ivan Tomac 2009
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some GLEW functions
----------------------------------------------------------------------

module Graphics.Glew
  ( VertexAttribPointerType, glVertexAttribPointer
  , EnableVertexAttribType , glEnableVertexAttrib
  , glewInit
  ) where


import Data.Int

import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

import Graphics.UI.GLUT


{-# INCLUDE "GL/glew.h" #-}

type VertexAttribPointerType a
    = GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr a -> IO ()

foreign import ccall "&__glewVertexAttribPointer" glewVertexAttribPointer
  :: Ptr (FunPtr (VertexAttribPointerType a))

foreign import ccall "dynamic" mkVertexAttribPointer
  :: FunPtr (VertexAttribPointerType a) -> VertexAttribPointerType a

glVertexAttribPointer :: VertexAttribPointerType a
glVertexAttribPointer =
    mkVertexAttribPointer $ unsafePerformIO $ peek glewVertexAttribPointer

type EnableVertexAttribType
    = GLuint -> IO ()

foreign import ccall "&__glewEnableVertexAttribArray" glewEnableVertexAttrib
  :: Ptr (FunPtr EnableVertexAttribType)

foreign import ccall "dynamic" mkEnableVertexAttrib
  :: FunPtr EnableVertexAttribType -> EnableVertexAttribType

glEnableVertexAttrib :: EnableVertexAttribType
glEnableVertexAttrib =
    mkEnableVertexAttrib $ unsafePerformIO $ peek glewEnableVertexAttrib

foreign import ccall "glewInit" glewInit :: IO Int
