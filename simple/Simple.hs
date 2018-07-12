{-# LANGUAGE ForeignFunctionInterface, TypeApplications, TypeFamilies #-}
module Simple where

import Foreign
import Data.Coerce
import Data.IORef
import Data.Text as T
import Data.Word
import System.IO.Unsafe

import Godot.Api
import qualified Godot.Methods as Godot
import Godot.Gdnative.Internal
import Godot.Nativescript
import Godot.Internal.Dispatch
import Godot.Gdnative.Types

import Foreign.C

import Data.IORef
import Data.Vector

-- We encapsulate SIMPLE's member variables in the UserData type.
data UserData = UserData
                GodotObject    -- We must encode the actual instance of SIMPLE (analogous to C++'s `this`).
                (IORef String) -- We use IORef to emulate OO state mutation.

-- Then we emulate OO class inheritance: UserData inherits from GodotReference.
instance HasBaseClass UserData where
  type BaseClass UserData = GodotReference
  super (UserData obj _) = GodotReference obj

godot_gdnative_init :: GodotGdnativeInitOptionsPtr -> IO ()
godot_gdnative_init optPtr = do
  putStrLn "gdnative init"
  opt <- peek optPtr
  initApiStructs opt -- We hide details involving `godot_gdnative_core_api_struct` and `godot_gdnative_ext_nativescript_api_struct`.

godot_gdnative_terminate :: GodotGdnativeTerminateOptionsPtr -> IO ()
godot_gdnative_terminate optPtr = putStrLn "gdnative terminate"

godot_nativescript_init :: GdnativeHandle -> IO ()
godot_nativescript_init desc = do
  putStrLn "nativescript init"
  -- Below we use simplified wrapper functions for `godot_nativescript_register_class`
  -- and `godot_nativescript_register_method`.
  registerClass desc "SIMPLE" "Reference" simple_constructor simple_destructor
  registerMethod desc "SIMPLE" "get_data" GodotMethodRpcModeDisabled simple_get_data

simple_constructor :: GodotObject -> IO UserData
simple_constructor obj = do
   putStrLn "SIMPLE._init()"
   stringRef <- newIORef "World from GDNative!"
   return (UserData obj stringRef)

simple_destructor :: GodotObject -> UserData -> IO ()
simple_destructor obj userData = do
  putStrLn "SIMPLE._byebye()"
  return ()

simple_get_data :: GodotObject -> UserData -> Vector GodotVariant -> IO GodotVariant
simple_get_data _ t@(UserData obj ioRefStr) _ = do
  userDataStr <- readIORef ioRefStr
  userDataGodotStr <- toLowLevel $ T.pack userDataStr                 -- Convert our String to a GodotString via toLowLevel.
  userDataGodotVariant <- toLowLevel (VariantString userDataGodotStr) -- Convert our GodotString into a GodotVariant via toLowLevel.
  godot_string_destroy userDataGodotStr                               -- We must still destroy the GodotString, just as we would in C.
  return userDataGodotVariant                                         -- The GodoVariant we return is still destroyed automatically by Godot.

-- We finally export the core godot_* functions, so that NativeScript can call
-- them automatically when instances of our SIMPLE script are loaded.
foreign export ccall godot_gdnative_init :: GodotGdnativeInitOptionsPtr -> IO ()
foreign export ccall godot_gdnative_terminate :: GodotGdnativeTerminateOptionsPtr -> IO ()
foreign export ccall godot_nativescript_init :: GdnativeHandle -> IO ()