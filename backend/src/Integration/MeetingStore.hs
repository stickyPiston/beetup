{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

module Integration.MeetingStore where

import Utils.Datatypes (Meeting, MeetingId, Availability, UserId)
import Database.Persist.Sqlite (insert, updateWhere, selectFirst, fromSqlKey, toSqlKey)
import Database.Persist ((==.), (=.), Entity(..))
import Utils.Functions (meetingToEntity, entityToMeeting, availabilityToEntity)
import Utils.DbInit (EntityField(MeetingEntityMeetingId, MeetingEntityAvailabilities, MeetingEntityUserIds), MeetingEntity(..), AvailabilityEntity (..))
import Utils.Endpoint (SqlQuery)
import Control.Monad (void)

-- | Store a meeting inside the database
storeMeeting :: Meeting -- ^ Meeting object to store
             -> SqlQuery ()
storeMeeting = void . insert . meetingToEntity

-- | Queries the database for a meeting with the given ID
findMeetingById :: MeetingId -- ^ Id of the meeting that is to be queried 
                -> SqlQuery (Maybe Meeting)
findMeetingById id = fmap entityToMeeting <$> selectFirst [MeetingEntityMeetingId ==. id] []

-- | Updates the availabilities  of the given user for a specific meeting
updateAvailabilities :: MeetingId -- ^ Id of the meeting that needs to be updated 
                     -> UserId -- ^ Id of the user who's availabilities are updated
                     -> [Availability] -- ^ List of new availabilities that replace the old ones
                     -> SqlQuery ()
updateAvailabilities mId uId as =
  selectFirst [MeetingEntityMeetingId ==. mId] [] >>= \case
    Just Entity { entityVal = meeting } ->
      let notOwn (AvailabilityEntity _ _ id) = fromSqlKey id /= fromIntegral uId
          unaffectedAvails = filter notOwn meeting.meetingEntityAvailabilities
          newAs = unaffectedAvails ++ map availabilityToEntity as
       in updateWhere [MeetingEntityMeetingId ==. mId] [MeetingEntityAvailabilities =. newAs]
    Nothing -> return ()

-- | Connects a user to a meeting
updateUsers :: MeetingId -- ^ Id of the meeting that needs to be updated
            -> UserId -- ^ Id of the user that is added to the meeting
            -> SqlQuery ()
updateUsers mId uId =
  selectFirst [MeetingEntityMeetingId ==. mId] [] >>= \case
    Just Entity { entityVal = meeting } ->
      let users  = meeting.meetingEntityUserIds
          newUid = toSqlKey $ fromIntegral uId
       in if newUid `elem` users
        then return ()
        else updateWhere [MeetingEntityMeetingId ==. mId] [MeetingEntityUserIds =. newUid : users]
    Nothing -> return ()
