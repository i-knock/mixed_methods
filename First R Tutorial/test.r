

setwd("/Users/antoniosanchezmartin/OneDrive/Masters/Q3 Mixed Methods/tutorial_code")

?readtext
coronanet <- readtext::readtext("policy_responses.csv", text_field = "description")
coronanet