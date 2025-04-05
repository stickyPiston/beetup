module Integration.MeetingStore where

import Utils.Datatypes (Meeting, MeetingId, Availability)
import Database.Persist.Sqlite (insert, update, selectFirst, Entity (entityKey))
import Database.Persist ((==.), (=.))
import Utils.Functions (meetingToEntity, entityToMeeting, availabilityToEntity)
import Integration.Init (EntityField(MeetingEntityMeetingId, MeetingEntityAvailabilities))
import Utils.Endpoint (SqlQuery)
import Control.Monad (void)

storeMeeting :: Meeting -> SqlQuery ()
storeMeeting = void . insert . meetingToEntity

findMeetingById :: MeetingId -> SqlQuery (Maybe Meeting)
findMeetingById id = fmap entityToMeeting <$> selectFirst [MeetingEntityMeetingId ==. id] []

updateAvailabilities :: MeetingId -> [Availability] -> SqlQuery (Maybe Meeting)
updateAvailabilities mId as = selectFirst [MeetingEntityMeetingId ==. mId] [] >>= maybe (return Nothing) updateMeeting
  where
    updateMeeting meeting = do
      update (entityKey meeting) [MeetingEntityAvailabilities =. map availabilityToEntity as]
      findMeetingById mId