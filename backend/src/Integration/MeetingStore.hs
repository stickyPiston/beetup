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

storeMeeting :: Meeting -> SqlQuery ()
storeMeeting = void . insert . meetingToEntity

findMeetingById :: MeetingId -> SqlQuery (Maybe Meeting)
findMeetingById id = fmap entityToMeeting <$> selectFirst [MeetingEntityMeetingId ==. id] []

updateAvailabilities :: MeetingId -> UserId -> [Availability] -> SqlQuery ()
updateAvailabilities mId uId as =
  selectFirst [MeetingEntityMeetingId ==. mId] [] >>= \case
    Just Entity { entityVal = meeting } ->
      let notOwn (AvailabilityEntity _ _ id) = fromSqlKey id /= fromIntegral uId
          unaffectedAvails = filter notOwn meeting.meetingEntityAvailabilities
          newAs = unaffectedAvails ++ map availabilityToEntity as
       in updateWhere [MeetingEntityMeetingId ==. mId] [MeetingEntityAvailabilities =. newAs]
    Nothing -> return ()

updateUsers :: MeetingId -> UserId -> SqlQuery ()
updateUsers mId uId =
  selectFirst [MeetingEntityMeetingId ==. mId] [] >>= \case
    Just Entity { entityVal = meeting } ->
      let users  = meeting.meetingEntityUserIds
          newUid = toSqlKey $ fromIntegral uId
       in if newUid `elem` users
        then return ()
        else updateWhere [MeetingEntityMeetingId ==. mId] [MeetingEntityUserIds =. newUid : users]
    Nothing -> return ()
