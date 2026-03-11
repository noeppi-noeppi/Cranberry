module Cranberry.StoragePool (connectPooledStorage, setupPooledStorage) where

import Cranberry.Types
import Data.Pool

instance Disposable (Pool a) where
  dispose = destroyAllResources

connectPooledStorage :: StorageAdapter a => IO a -> IO (Pool a)
connectPooledStorage resource = newPool $ defaultPoolConfig resource dispose 300.0 5

setupPooledStorage :: StorageAdapter a => (a -> IO ()) -> Pool a -> IO ()
setupPooledStorage setup pool = withResource pool setup

instance StorageAdapter a => StorageAdapter (Pool a) where
  getShortLink pool id = withResource pool $ \con -> getShortLink con id
  putShortLink pool id dest = withResource pool $ \con -> putShortLink con id dest
  putNewShortLink pool id dest = withResource pool $ \con -> putNewShortLink con id dest
  putRandomShortLink pool url = withResource pool $ \con -> putRandomShortLink con url
  deleteShortLink pool id = withResource pool $ \con -> deleteShortLink con id
  listShortLinks pool = withResource pool $ \con -> listShortLinks con
  getAccessTokenDetails pool token = withResource pool $ \con -> getAccessTokenDetails con token
  createAccessToken pool permissionLevel token = withResource pool $ \con -> createAccessToken con permissionLevel token
  revokeAccessToken pool token = withResource pool $ \con -> revokeAccessToken con token
