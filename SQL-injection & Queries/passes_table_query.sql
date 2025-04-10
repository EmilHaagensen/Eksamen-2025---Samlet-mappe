-- 	Oprettelse af tabel med passes, lignende den med shots


CREATE TABLE `PASSES`AS 
SELECT 
	* 
FROM _matchevents_passes_sl AS passes
LEFT JOIN _matchevents_common_sl AS common
USING (EVENT_WYID, MATCH_WYID, COMPETITION_WYID, PRIMARYTYPE)
LEFT JOIN _matchevents_secondarytype_sl AS secondarytype
USING (EVENT_WYID, MATCH_WYID, COMPETITION_WYID, PRIMARYTYPE)
LEFT JOIN _matchevents_possessiontypes_sl AS posstype
USING (EVENT_WYID, MATCH_WYID, COMPETITION_WYID, PRIMARYTYPE);


CREATE TABLE `TEMP_PASS`
SELECT
	PASSES.*,
    teams.Home_team,
    opp_teams.Away_team
FROM PASSES
LEFT JOIN (SELECT DISTINCT TEAM_WYID, TEAMNAME AS Home_team FROM _teams_sl) AS teams
USING (TEAM_WYID)
LEFT JOIN (SELECT DISTINCT TEAM_WYID, TEAMNAME AS Away_team FROM _teams_sl) AS opp_teams
ON opp_teams.TEAM_WYID = PASSES.OPPONENTTEAM_WYID;

DROP TABLE `PASSES`;
RENAME TABLE `TEMP_PASS` TO `PASSES`;

-- Rykker rækkerne med teamnames til korrekt lokation i datasæt

ALTER TABLE `PASSES`
MODIFY COLUMN `Home_team` varchar(12) AFTER TEAM_WYID;

ALTER TABLE `PASSES`
MODIFY COLUMN `Away_team` varchar(12) AFTER OPPONENTTEAM_WYID;
