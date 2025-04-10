-- undersøgelse, skulle ikke bruges til noget tror jeg?
SELECT TABLE_NAME, COLUMN_NAME, DATA_TYPE, IS_NULLABLE, COLUMN_KEY, COLUMN_DEFAULT
FROM information_schema.columns
WHERE table_schema = 'Fodbolddata'
ORDER BY TABLE_NAME, ORDINAL_POSITION;



-- Sætter timeout threshhold op grundet query loadtime
SET GLOBAL net_read_timeout = 600;
SET GLOBAL net_write_timeout = 600;
SET GLOBAL wait_timeout = 28800;
SET GLOBAL interactive_timeout = 28800;



-- Laver dataframe over skud med elementerne som vi kender fra MongoDB
CREATE TABLE `SHOTS`AS 
SELECT
*
FROM _matchevents_shots_sl AS shots
LEFT JOIN _matchevents_common_sl AS common
USING (EVENT_WYID, MATCH_WYID, COMPETITION_WYID, PRIMARYTYPE)
LEFT JOIN _matchevents_secondarytype_sl AS secondarytype
USING (EVENT_WYID, MATCH_WYID, COMPETITION_WYID, PRIMARYTYPE)
LEFT JOIN _matchevents_possessiontypes_sl AS posstype
USING (EVENT_WYID, MATCH_WYID, COMPETITION_WYID, PRIMARYTYPE)
LEFT JOIN _players_sl as players
USING (COMPETITION_WYID, SEASON_WYID, PLAYER_WYID);



-- Joiner teamnavne ind i ovenstående tabel
CREATE TABLE `TEST` AS 
SELECT 
    SHOTS.*, 
    teams.Home_team,  
    opp_teams.Away_team  
FROM SHOTS
LEFT JOIN (SELECT DISTINCT TEAM_WYID, TEAMNAME AS Home_team FROM _teams_sl) AS teams
USING (TEAM_WYID)
LEFT JOIN (SELECT DISTINCT TEAM_WYID, TEAMNAME AS Away_team FROM _teams_sl) AS opp_teams
ON opp_teams.TEAM_WYID = SHOTS.OPPONENTTEAM_WYID;

-- dropper shots tabel, da "test" er den nye shots-tabel, hvorefter jeg ALTER tabelnavn til shots.

DROP TABLE `SHOTS`;
RENAME TABLE `TEST` TO `SHOTS`;

-- Rykker rækkerne med teamnames til korrekt lokation i datasæt

ALTER TABLE `SHOTS`
MODIFY COLUMN `Home_team` varchar(12) AFTER TEAM_WYID;

ALTER TABLE `SHOTS`
MODIFY COLUMN `Away_team` varchar(12) AFTER OPPONENTTEAM_WYID;

-- Der kan eventuelt tilføjes flere elementer, men det bliver kringlet grundet duplikanter af IDs
-- Hvilket SQL ikke er helt tilfreds med


-- Samme procedure som før med at lave ny test tabel 



CREATE TABLE test2 AS
SELECT 
    s.*,
    d.`DATE`
FROM SHOTS AS s
LEFT JOIN (
    SELECT DISTINCT SEASON_WYID, MATCH_WYID, `DATE` 
    FROM _teammatches_sl
) AS d
USING (SEASON_WYID, MATCH_WYID);


DROP TABLE SHOTS;

RENAME TABLE test2 TO `SHOTS`;



