module Utils.Functions where
import Data.Time (NominalDiffTime)

whenNothing :: Monad m => Maybe a -> m () -> m ()
whenNothing Nothing f = f
whenNothing (Just _) _ = return ()

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()


-- | Given a @NominalDiffTime@, returns a multiple of the @NominalDiffTime@
times :: NominalDiffTime -> Int -> NominalDiffTime
times dt n = iterate (+ dt) dt !! n

