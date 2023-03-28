# USCIS Immigration Form Processing Times

**This project is not affiliated with any government agency and should not be considered official by any means.**

The repository contains a study in programmatically obtaining [processing time data](https://egov.uscis.gov/processing-times/) for various immigration forms from the [U.S. Citizenship and Immigration Services (USCIS)](https://www.uscis.gov/) using the [R programming language](https://www.r-project.org/).

Information is obtained by making the same standard HTTPS requests the [processing time form](https://egov.uscis.gov/processing-times/)
makes to communicate with the USCIS Processing Time API found at:

```
https://egov.uscis.gov/processing-times/api/
```

You can see an example of the workflow for obtaining the I-485 processing time data for Chicago, IL in the following GIF:

![Web API Responses from USCIS Processing Times](https://i.imgur.com/FeRwvu4.gif)



## Acknowledgements

There has been noted work on retrieving processing time information from the USCIS; however, the efforts have largely avoided using _R_.

- [Ariel Rodriguez Romero's exploration of API endpoints](https://github.com/arielsvn/arielsvn.github.io/issues/3)
- [Data for Democracy's `immigration-connect` (Python)](https://github.com/Data4Democracy/immigration-connect/tree/master/uscis-processing-time)
- [Hemanth Makkapati's `uscis-tracker`](https://github.com/hmakkapati/uscis-tracker)
- [Gregor Martynus's `uscis-service-center-processing-times` (Node)](https://github.com/gr2m/uscis-service-center-processing-times)
- [Andrew Duberstein's `uscis-wait-times` (Python)](https://github.com/ajduberstein/uscis-wait-times)
- [J. Zebedee's `uscis` (SQL)](https://github.com/jzebedee/uscis)

