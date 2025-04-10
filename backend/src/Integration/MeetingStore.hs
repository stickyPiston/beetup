{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

module Integration.MeetingStore where

import Utils.Datatypes (Meeting, MeetingId, Availability, UserId)
import Database.Persist.Sqlite (insert, updateWhere, selectFirst, fromSqlKey)
import Database.Persist ((==.), (=.), Entity(..))
import Utils.Functions (meetingToEntity, entityToMeeting, availabilityToEntity)
import Integration.Init (EntityField(MeetingEntityMeetingId, MeetingEntityAvailabilities), MeetingEntity(..), AvailabilityEntity (..))
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