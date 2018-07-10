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



godot_gdnative_init :: GodotGdnativeInitOptionsPtr -> IO ()
godot_gdnative_init optPtr = do
  putStrLn "gdnative init"
  opt <- peek optPtr
  initApiStructs opt

foreign export ccall godot_gdnative_init :: GodotGdnativeInitOptionsPtr -> IO ()

godot_gdnative_terminate :: GodotGdnativeTerminateOptionsPtr -> IO ()
godot_gdnative_terminate optPtr = putStrLn "gdnative terminate"

foreign export ccall godot_gdnative_terminate :: GodotGdnativeTerminateOptionsPtr -> IO ()

godot_nativescript_init :: GdnativeHandle -> IO ()
godot_nativescript_init desc = do
  putStrLn "nativescript init"
  registerClass desc "TestClass" "Node" (\obj -> return (TestClass obj (show obj))) (\_ _ -> return ())
  registerMethod desc "TestClass" "do_a_thing" GodotMethodRpcModeDisabled $
    \_ t@(TestClass obj str) _ -> do
      putStrLn str
      
      str <- Godot.get_class t >>= fromLowLevel
      putStr "Godot.get_class is "
      putStrLn $ T.unpack str

      outStr <- toLowLevel (T.pack "i did a thing")
      toLowLevel (VariantString outStr)


foreign export ccall godot_nativescript_init :: GdnativeHandle -> IO ()

data TestClass = TestClass GodotObject String
  deriving (Show, Eq)

instance HasBaseClass TestClass where
  type BaseClass TestClass = GodotReference
  super (TestClass obj _) = GodotReference obj


{- Dummy Data -}

data UserData
-- typedef struct user_data_struct {
-- 	char data[256];
-- } user_data_struct;

data MethodData

simple_constructor :: GodotObject -> MethodData -> UserData -> Int -> GodotVariant -> IO ()
simple_constructor inst methodData = undefined

simple_destructor :: GodotObject -> MethodData -> UserData -> IO()
simple_destructor inst methodData userData = undefined

simple_get_data :: GodotObject -> MethodData -> UserData -> Int -> [GodotVariant] -> GodotVariant
simple_get_data inst methodData userData numArgs listOfArgs = undefined