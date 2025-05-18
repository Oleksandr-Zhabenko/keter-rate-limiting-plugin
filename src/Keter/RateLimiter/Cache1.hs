-- | Instance for storing Double values (for leaky bucket)
instance CacheStore (InMemoryStore "leaky_bucket") Double IO where
  readStore (InMemoryStore ref) _prefix key = do
    cache <- readIORef ref
    mval <- C.lookup cache key
    case mval of
      Nothing -> return Nothing
      Just txt -> case decodeStrict (encodeUtf8 txt) of
        Nothing -> return Nothing
        Just val -> return (Just val)

  writeStore (InMemoryStore ref) _prefix key val expiresIn = do
    cache <- readIORef ref
    let bs = encode val
        strictBs = LBS.toStrict bs
        txt = decodeUtf8' strictBs
    case txt of
      Left _ -> return ()
      Right vtxt -> do
        let expiration = Just (TimeSpec 0 (fromIntegral expiresIn * 1000000000))
        C.insert' cache expiration key vtxt

  deleteStore (InMemoryStore ref) _prefix key = do
    cache <- readIORef ref
    C.delete cache key
