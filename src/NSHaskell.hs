{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
module NSHaskell where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Bindings.Objc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Data.List (sort, intercalate)
import Data.Map.Strict (Map, fromList, lookup, keys)
import qualified Data.Map.Strict as Map

class Nil a where
    nil   :: a
    isNil :: Eq a => a -> Bool
    isNil = (nil ==)

class HasName a where
    getName :: a -> IO String

class HasClass a where
    getClass      :: a -> IO Class
    getSuperClass :: a -> IO Class

type Sel = CSEL
type Id = CId
type Method = CMethod
type Class = CClass

instance Nil (Ptr a) where
    nil = c'nil

instance HasName String where
    getName = return . id

instance HasName Class where
    getName = (peekCString =<<) . c'class_getName

instance HasName Id where
    getName = (peekCString =<<) . c'object_getClassName

instance HasName Sel where
    getName = (peekCString =<<) . c'sel_getName

instance HasName Method where
    getName = (getName =<<) . c'method_getName

instance HasClass Class where
    getClass = return . id
    getSuperClass = c'class_getSuperclass

instance HasClass Id where
    getClass = (getClass =<<) . c'object_getClass
    getSuperClass = (getSuperClass =<<) . getClass

instance HasClass String where
    getClass s = withCString s c'objc_getClass
    getSuperClass = (getSuperClass =<<) . getClass

getMetaClass :: Class -> IO Class
getMetaClass c = do
    isMeta <- c'class_isMetaClass c
    if (c'YES == isMeta)
    then return c
    else liftIO $ c'object_getClass $ castPtr c

withSelector :: String -> (Sel -> IO a) -> IO a
withSelector s f = withCString s $ \p -> c'sel_getUid p >>= f

getMethods :: Class -> IO [Method]
getMethods c = do
    def <- getClass c
    alloca $ \ptr -> do
        ptrMs <- c'class_copyMethodList def ptr
        n     <- peek ptr
        peekArray (fromIntegral n) ptrMs

getClassMethods :: HasClass a => a -> IO [Method]
getClassMethods a = getClass a >>= getMetaClass >>= getMethods

getSelectors :: HasClass a => a -> IO [Sel]
getSelectors a = getClass a >>= getMethods >>= mapM c'method_getName

getClassSelectors :: HasClass a => a -> IO [Sel]
getClassSelectors a = getClassMethods a >>= mapM c'method_getName

printClass :: Class -> IO ()
printClass c = do
    name      <- getName c
    iSels     <- getSelectors c
    iSelNames <- mapM getName iSels
    cSels     <- getClassSelectors c
    cSelNames <- mapM getName cSels

    putStrLn name
    putStrLn "  Class\n  "
    putStrLn $ intercalate "\n    " $ sort cSelNames
    putStrLn "\n  Instance\n  "
    putStrLn $ intercalate "\n    " $ sort iSelNames

printAvailableClasses :: IO ()
printAvailableClasses = do
    num <- c'objc_getClassList nullPtr 0
    let num' = fromIntegral num
    classes <- allocaArray num' $ \ptr -> do
        _ <- c'objc_getClassList ptr num
        peekArray num' ptr
    names <- mapM getName classes
    putStrLn $ intercalate "\n  " $ "Available classes:": sort names


foreign import ccall safe "objc/objc.h objc_msgSend" c'objc_msgSend_CInt_CString :: CId -> CSEL -> CInt -> IO CString

description :: Id -> IO String
description c = do
    desc <- withSelector "description" $ c'objc_msgSend c
    cstr <- withSelector "cStringUsingEncoding:" $ \s -> c'objc_msgSend_CInt_CString desc s 4
    peekCString cstr

