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
  outStr <- (toLowLevel (T.pack "i did a thing")) :: IO GodotString
  lol <- (toLowLevel (VariantString outStr) :: IO GodotVariant)
  return ()
  -- toLowLevel (VariantString outStr)
  -- Replace with SIMPLE class
  -- registerClass desc "TestClass" "Node" (\obj -> return (TestClass obj (show obj))) (\_ _ -> return ())
  -- registerMethod desc "TestClass" "do_a_thing" GodotMethodRpcModeDisabled $
  --   \_ t@(TestClass obj str) _ -> do
  --     putStrLn str
      
  --     str <- Godot.get_class t >>= fromLowLevel
  --     putStr "Godot.get_class is "
  --     putStrLn $ T.unpack str

  --     outStr <- toLowLevel (T.pack "i did a thing")
  --     (toLowLevel (VariantString outStr) :: IO GodotVariant)


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

-- type InstanceMethodFun = Ptr GodotVariant -> GodotObject -> Ptr () -> Ptr () -> CInt -> Ptr (Ptr GodotVariant) -> IO (Ptr GodotVariant)
-- simple_get_data :: InstanceMethodFun
{-
 godot_variant simple_get_data(godot_object *p_instance, void *p_method_data, void *p_user_data, int p_num_args, godot_variant **p_args) {
	godot_string data;
	godot_variant ret;
	user_data_struct * user_data = (user_data_struct *) p_user_data;

	api->godot_string_new(&data);
	api->godot_string_parse_utf8(&data, user_data->data);
	api->godot_variant_new_string(&ret, &data);
	api->godot_string_destroy(&data);

	return ret;
} 
-}
simple_get_data :: Ptr GodotVariant -> GodotObject -> Ptr () -> Ptr () -> CInt -> Ptr (Ptr GodotVariant) -> IO (Ptr GodotVariant) 
simple_get_data returnPtr instObj ptrMethodData ptrUserData numArgs listOfArgs = do
   userData <- peek ptrUserData

   -- Q: How does one cast a String (into a GodotString) into a GodotVariant (with memory cleanup, as below)?
   -- godot_string data;
   -- godot_variant ret;
   -- user_data_struct * user_data = (user_data_struct *) p_user_data;
   -- api->godot_string_new(&data);
   -- api->godot_string_parse_utf8(&data, user_data->data);
   -- api->godot_variant_new_string(&ret, &data);
   -- api->godot_string_destroy(&data);

   outStr <- (toLowLevel (T.pack "i did a thing")) :: IO GodotString
   toLowLevel (VariantString outStr)

   return returnPtr