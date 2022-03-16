# %%script false --no-raise-error
# https://github.com/twintproject/twint/wiki/Configuration
import twint

values_to_search = [
    "#ABPSouthampton",
    "#DP_World",
    "#GaPorts",
    "#PortofHamburg",
    "#HutchisonPPC",
    "#LondonPortAuth",
    "#PANYNJ",
    "#portdebarcelona",
    "#PortMTL",
    "#Port_Houston",
    "#PortofAntwerp",
    "#felixstowe_port",
    "#portoflongbeach",
    "#PortofLA",
    "#PortofOakland",
    "#PortOfRotterdam",
    "#PortofSeattle",
    "#PortVancouver",
    "#Port_Zeebrugge",
    "#portodigenova",
    "#PuertoAlgeciras",
    "#PuertodeCtg",
    "#SCPorts",
    "#PortofVirginia",
    "#AutPortValencia"
]

values_to_search = ['"port" AND "expansion"'] # both words should be included

values_to_search = ['port'] # only port

values_to_search = ['"harbour" OR "harbor"']

c = twint.Config()
for value in values_to_search:
    # c.Limit = 1000
    file_name = f"./twitter_data_{value}.csv"
    c.Output = file_name
    c.Store_csv = True
    c.Search = value
    c.Filter_retweets = True
    c.Lang = "en"
    twint.run.Search(c)