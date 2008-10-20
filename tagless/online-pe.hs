{-# LANGUAGE Rank2Types #-}
-- [ccshan 2008-10-20]

type Repr r sv dv = (Maybe sv, r dv)

appPE :: (forall a b. r (a->b) -> r a -> r b) ->
         Repr r (Repr r sa da -> Repr r sb db) (da->db) ->
         Repr r sa da ->
         Repr r sb db
appPE app m n = case fst m of
    Nothing -> (Nothing, app (snd m) (snd n))
    Just g -> g n

lamPE :: (forall a b. (r a -> r b) -> r (a->b)) ->
         (Repr r sa da -> Repr r sb db) ->
         Repr r (Repr r sa da -> Repr r sb db) (da->db)
lamPE lam g = (Just g, lam (\x -> snd (g (Nothing, x))))
