{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Security
  ( Authenticated(..)
  , authenticate
  , User(..)
  , userPath
  , PermitLevel(..)
  , PermitAct(..)
  , Permit
  , demotePermsWriteToRead
  , authListReadPerm
  , authListWritePerm
  , listParentReadPerm
  , authDirReadPerm
  , authDirWritePerm
  ) where

import Config (Config(..))
import GHC.Records (HasField(..))
import System.FilePath ((</>),(<.>),takeDirectory)
import Web.Scotty (ActionM) -- TODO remove dependency on scotty


------------ Base ------------

data Authenticated = NoAuth | Authd

authenticate :: User 'NoAuth -> ActionM (Maybe (User 'Authd))
authenticate Me = pure (Just Me) -- TODO

data User (auth :: Authenticated) = Me -- TODO multi-user

deauthenticate :: User lvl -> User 'NoAuth
deauthenticate Me = Me -- TODO can probably use coerce here

userPath :: User auth -> FilePath
userPath Me = "marbou"

data PermitLevel = Read | Write
data PermitAct
  = ForList
  | ForDir
data Permit (lvl :: PermitLevel) (act :: PermitAct) where
  ListPermit :: Config
             -> User 'Authd -- ^ requesting user
             -> User owner -- ^ list owner
             -> FilePath -- ^ url path to list requested
             -> Permit lvl 'ForList
  DirPermit :: Config
            -> User 'Authd -- ^ requesting user
            -> User owner -- ^ list owner
            -> FilePath -- ^ url path to list requested
            -> Permit lvl 'ForDir

demotePermsWriteToRead :: Permit 'Write act -> Permit 'Read act
demotePermsWriteToRead (ListPermit a b c d) = ListPermit a b c d -- TODO I think coerce would work
demotePermsWriteToRead (DirPermit a b c d) = DirPermit a b c d

------------ Lists ------------

authListReadPerm :: Config -> User 'Authd -> (User owner, FilePath) -> Maybe (Permit 'Read 'ForList)
authListReadPerm config requester (owner, urlpath) = -- TODO
  Just $ ListPermit config requester owner urlpath

authListWritePerm :: Config -> User 'Authd -> (User owner, FilePath) -> Maybe (Permit 'Write 'ForList)
authListWritePerm config requester (owner, urlpath) = -- TODO
  Just $ ListPermit config requester owner urlpath

instance HasField "requester" (Permit any 'ForList) (User 'Authd) where
  getField (ListPermit _ requester _ _) = requester
instance HasField "owner" (Permit any 'ForList) (User 'NoAuth) where
  getField (ListPermit _ _ owner _) = deauthenticate owner
instance HasField "urlpath" (Permit any 'ForList) FilePath where
  getField (ListPermit _ _ _ urlpath) = urlpath
instance HasField "filepath" (Permit any 'ForList) FilePath where
  getField (ListPermit config _ owner urlpath)
    = config.usersRoot </> userPath owner </> "lists" </> urlpath <.> "ld"

listParentReadPerm :: Permit any 'ForList -> Maybe (Permit 'Read 'ForDir)
listParentReadPerm (ListPermit config requester owner urlpath) = do
  parentPath <- case takeDirectory urlpath of
    ".." -> Nothing
    "." -> Just ""
    dirPath -> Just dirPath
  authDirReadPerm config requester (owner, parentPath)

------------ Directories ------------

authDirReadPerm :: Config -> User 'Authd -> (User owner, FilePath) -> Maybe (Permit 'Read 'ForDir)
authDirReadPerm config requester (owner, urlpath) = -- TODO
  Just $ DirPermit config requester owner urlpath

authDirWritePerm :: Config -> User 'Authd -> (User owner, FilePath) -> Maybe (Permit 'Write 'ForDir)
authDirWritePerm config requester (owner, urlpath) = -- TODO
  Just $ DirPermit config requester owner urlpath


instance HasField "requester" (Permit any 'ForDir) (User 'Authd) where
  getField (DirPermit _ requester _ _) = requester
instance HasField "owner" (Permit any 'ForDir) (User 'NoAuth) where
  getField (DirPermit _ _ owner _) = deauthenticate owner
instance HasField "urlpath" (Permit any 'ForDir) FilePath where
  getField (DirPermit _ _ _ urlpath) = urlpath
instance HasField "filepath" (Permit any 'ForDir) FilePath where
  getField (DirPermit config _ owner urlpath)
    = config.usersRoot </> userPath owner </> "lists" </> urlpath