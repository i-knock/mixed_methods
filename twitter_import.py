import twint

# https://github.com/twintproject/twint/wiki/Configuration

hashtags = [
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

c = twint.Config()
c.Output = f"all_hashtags.csv"
for hashtag in hashtags:
    print("test")
    c.Limit = 1000
    c.Store_csv = True
    # c.Search = '"port" and "expansion"'
    # c.Search = '"port"'
    c.Search = hashtag
    c.Filter_retweets = True
    c.Lang = "en"
    twint.run.Search(c)