## score and active team

see rows 4594-4596: active team is the last team with an offensive possesion, not necessarily the scoring team. 

see rows 96466-96468: when the inactive team scores, nothing is listed for td, xp.result, or two.pt.result

see rows 50488:50491. Active team stays on TCU, despite Ohio State getting a fumble for a td. Since Ohio State never
had offensive possesion the active team never switches

playtypes that include this are ( )

## td ambiguity
play type 17 and 37 both seem to be punt block td, not sure of the difference. 17 playstring explicitly says td, 37 does not, but is implied based on "kick"
play type 29 is ambiguous if a td was scored

## punt distance yds 
not given in play desc for punt return tds, can be manually calculated. see row 4595

## incorrect team id
id 2440 is used for both Nevada and Arksansas St (1 x) 

### two pt tries and extra points need to be cleaned up
there are instances where these are not documented explicitly (i.e. only in the description), 
it seems to happen on tds scored from non offenseive plays, turnovers, blocks, fumbles etc. 


## there is no date or week listed in the data