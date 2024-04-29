# MIAMI-AD (Methylation in Aging and Methylation in AD): an integrative knowledgebase that facilitates explorations of DNA methylation across sex, aging, and Alzheimer’s disease
David Lukacsovich, Deirdre O’Shea, Hanchen Huang, Wei Zhang, Juan Young, X. Steven Chen, Sven-Thorsten Dietrich, Brian Kunkle, Eden Martin, Lily Wang

### Citation 

Lukacsovich D et al. (2023) MIAMI-AD (Methylation in Aging and Methylation in AD): an integrative atlas of DNA methylation across sex, aging, and Alzheimer's disease. medRxiv https://www.medrxiv.org/content/10.1101/2023.12.04.23299412v1

Please also cite the original study papers in which the results were obtained.

### Description 

We created MIAMI-AD, a comprehensive knowledge base containing manually curated summary statistics from 97 published tables in 37 studies, all of which included at least 100 participants. MIAMI-AD enables easy browsing, querying, and downloading DNAm associations at multiple levels – at individual CpG, gene, genomic regions, or genome-wide, in one or multiple studies. Moreover, it also offers tools to perform integrative analyses, such as comparing DNAm associations across different phenotypes or tissues, as well as interactive visualizations. This open-access resource is freely available to the research community, and all the underlying data can be downloaded. MIAMI-AD ([https://miami-ad.org](https://miami-ad.org/)) facilitates integrative explorations to better understand the interplay between DNAm across aging, sex, and AD.

### Installation

If you wish to run MIAMI-AD on your own computer, the code can be downloaded and installed using

```
library(devtools)
install_github("TransBioInfoLab/MIAMI.AD")
```

This repository is missing the SQLite database file (3.2 GB) that contains the detabase information. It's hosted on [Box](https://miami.box.com/s/qye8oj8m8n127oc5qamgdvwmalsamun0). To run MIAMI-AD on your own server, it needs to be downloaded and placed in the `inst/shiny/Data/SQL_DBs` directory.

Once installed, it can be run via
```
MIAMI.AD::run_app()
```

Or running the `app.r` file in the main directory.

### Navigation

For instructions on navigating or using the server, click on the **Tutorial** tab. Alternatively, in each tab there is a **Start Tutorial** button at the top left, that will give a brief overview of the relevant tab.