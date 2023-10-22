# R-FALLOW: a model of rural landscape dynamics and economic-ecological trade-off of land use scenario

## A User’s Manual



### World Agroforestry (ICRAF)

### 2023



**Disclaimer and Copyright**

Although efforts have been made to incorporate relevant process knowledge on a range of interactions, a computer model is not more (and not less) than research and supporting tool. Model’s prospective outputs may help in developing specific hypotheses for research, in exploring potential future options on development strategies, but they should not be used as authoritative statements per se.

Copy right, but do not copy wrong. The R-FALLOW model was developed on the basis of publicly funded research at the World Agroforestry (ICRAF) and may be used for non-commercial research purposes in the interest of research or governmental institution as well as farmers of the world. 

\-------------------------------------------------------------------------------- 

For further information, consultation, and technical support please contact: 

Rachmat Mulia (<r.mulia@cifor-icraf.org>)

Betha Lusiana (<b.lusiana@cifor-icraf.org>) 

**Suggested citation:**

Mulia R, Asmara DH, Lusiana B, Salvan GAR, and van Noordwijk M. 2023. R-FALLOW: a model of rural landscape dynamics and economic-ecological trade-off of land use scenario. A User’s Manual. World Agroforestry (ICRAF), Bogor, Indonesia and Los Banos, the Philippines. 

**Acknowledgment**

The R-FALLOW and this user’s manual are produced mainly thanks to the financial support from the Sustainable Farming in Tropical Asian Landscapes (SFITAL) Project funded by the International Fund for Agricultural Development (IFAD) and MARS Incorporation.
## **Table of Contents**

[1.	What is R-FALLOW?](#_toc148435188)

[2.	Why is it important?](#_toc148435189)

[3.	Who can use R-FALLOW?](#_toc148435190)

[4.	Brief description of FALLOW modelling principles](#_toc148435191)

[5.	Preparing inputs for model simulation](#_toc148435192)

[5.1 Spatial inputs](#_toc148435193)

[5.1.1 How to prepare the landcover map](#_toc148435194)

[5.1.2 How to prepare the soil map](#_toc148435195)

[5.2 Non-spatial inputs](#_toc148435196)

[5.2.1 Socio-economic inputs](#_toc148435197)

[5.2.2 Biophysical and demographic inputs](#_toc148435198)

[6.	How to open and parameterize R-FALLOW](#_toc148435199)

[6.1 How to open the model](#_toc148435200)

[6.2 How to parameterize the model](#_toc148435201)

[6.2.1 Module “Initial Input”](#_toc148435202)

[6.2.2 Option 1 of parameterizing R-FALLOW](#_toc148435203)

[6.2.3 Option 2 of parameterizing R-FALLOW](#_toc148435204)

[6.2.4 Option 3 of parameterizing R-FALLOW](#_toc148435205)

[7. How to run the model](#_toc148435206)

[8. Outputs of the model](#_toc148435207)

[9. Useful references](#_toc148435208)

[Annex 1 Complete list of input maps and parameters required by R-FALLOW](#_toc148435209)

## <a name="_toc148435188"></a>**What is R-FALLOW?**

R-FALLOW (**F**orest, **A**groforest, **L**ow-value **L**and **O**r **W**aste?) is a model of rural landscape dynamics and related economic-ecological trade-off developed using the R language. It has unique characteristics as compared to other rural landscape models mainly because it simulates landcover change at landscape (e.g., district or province) scale *driven by smallholder farmers' decision on labour, financial and land allocation to land use options*. Therefore, it considers smallholder farmers as the main agent of land use dynamics, along with possible land use change over State’s or concession lands or conversion of forest lands. Existing models of rural landscape dynamics usually create projection of future land uses using e.g., land use transition matrix. 

The initial version of the FALLOW model was developed by van Noordwijk (2002) using the Stella programming platform, which was then translated into the PC Raster version (e.g., see Suyamto et al. 2009) to better handle the spatial input information. More recently, to better facilitate model’s parametrisation and a possibility for the model to ‘communicate’ with other models developed using the contemporary R or Java language, the FALLOW model is translated into the R programming language. The FALLOW model has been used as a supporting tool to assess projected impacts and analyse economic-ecological trade-off among land use scenarios in different countries especially in Southeast Asia such as Indonesia, the Philippines, and Vietnam (van Noordwijk et al. 2008, Lusiana et al. 2012, Mulia et al. 2013, Tata et al. 2013, Mulia et al. 2018).

## <a name="_toc148435189"></a>**Why is it important?**

There are four possible directions where an implementation of land use strategies can lead to (Figure 1). For example, a strategy which prioritizes an economic benefit might lead to a better livelihood of rural population relative to the baseline condition but brings about a decline in an ecological indicator (namely, the ‘Red Development’ strategy in Figure 1). An ideal land use strategy should bring about improvement both in socio-economic and ecological goals (the ‘Green Development’ in Figure 1), relative to the baseline.   

<a name="_ref147934195"></a><a name="_toc148435228"></a>Figure 1 Prospective diagram depicting the impact of land use strategies on economic (x axes) and ecological indicator (y axes) relative to the baseline condition (central point of the diagram).

R-FALLOW is important because it can be used to assess possible impacts of land use scenarios which accommodate local stakeholders’ preference and decisions on labour, financial and land allocation for future land uses, and possible land use change over State’s or concession lands or conversion of forest lands. The key economic indicator projected by the model is income per capita of smallholder farmers[^1], while the key ecological indicator is the total aboveground carbon storage at landscape level. Thanks to the indicators, the model can be used to analyse a trade-off among possible land use scenarios, relative to the baseline, as inputs to develop sensible land use strategies. 

## <a name="_toc148435190"></a>**Who can use R-FALLOW?**
The model can be used by research and development actors, academics or students, or local authorities who are interested in assessing the impacts of land use scenarios and local stakeholders’ decision in <a name="_hlk147416673"></a>labour, financial and land allocation on income per capita of smallholder farmers and aboveground carbon storage of rural landscape. In a more detailed, the model is suitable for those who are interested in the following possible research questions:

- Which capitals (labours, money, and land) are constraining the adoption or expansion of certain land use options by smallholder farmers over the simulated landscape?
- How far the government subsidy or market incentive could influence the expansion and total production of certain crop products?  
- Will more effective agricultural extension services enhance farmers’ preference to certain land uses and allocate more investment to those land use options? 
- Does the community have a certain ‘cultural preference’ in cultivating crops and what is possible impact on the expansion of different land use options over the landscape?
- Is there any forest protection program implemented to certain areas over the landscape, and what will be the consequence on aboveground carbon storage at landscape level?
- Have the simulate community been involved in the collection of non-timber forest products and if the income from the collection could substantially increase household’s income?
- In a possible presence of natural disaster that might affect partial or the whole area over the landscape, what will be the consequence on the working force and investment to agricultural land uses?

## <a name="_toc148435191"></a>**Brief description of FALLOW modelling principles**
As highlighted in <a name="_hlk148012627"></a>Lusiana et al. (2012), land use change model to understand the trade-offs between economic and ecological purposes might not be necessarily complex, in contrast to models which simulate detailed biophysical interactions between crops, soils, and climate. However, it should consider important drivers of land use change to include household economics and its influence on the household’s decision making and agricultural productivity. A desirable land use change model should be generic and flexible enough for possible uses at various sites and site-specific conditions.

R-FALLOW treats land use and land cover simultaneously, assuming that land use dynamics are a major determinant of land-cover change over the simulated landscape. As summarized by Lusiana et al. (2012), the interactions between different factors or modules in FALLOW start with the changes in soil fertility at farm level wherein soil fertility depletes during cropping periods and recovers during fallow periods. The actual fertility determines agricultural yield and total production from agricultural farms, and along with non-timber forest products, contribute to food sufficiency and household’s economic capital. The population dynamics affects labour force as well as demand for food. Meanwhile, local stakeholders’ strategic decision on labour, financial capital, and land allocation determines land use changes and land cover mosaic over the simulated landscape.

In the model, we can simulate smallholder farmers which prioritize more profitable land use options or those strongly influenced by cultural preference. For both types of farmers, the economic expectation is influenced by a certain initial knowledge and can possibly change through learning from experience or new information acquired from external sources (e.g., from extension services or neighbours). Following their decision on labour, financial capital, and land allocation, farmers can select lands which are biophysically suitable for new crop cultivations. Their perception of plot attractiveness is assumed to depend on e.g., relative soil fertility and market accessibility[^2]. Their decision determines location of the new plots. land use change. The overall landscape dynamics will influence livelihood of the community and ecology of the simulated landscape. A more detailed description of the FALLOW modelling principles is given by e.g., van Noordwijk (2002) and Suyamto et al. (2009).


<a name="_toc148435229"></a>Figure 2 Schematic diagram of FALLOW modelling principles and its modules. 

(Source: Lusiana et al. (2012). The livestock module is, however, not available in the current R-FALLOW version) 

R-FALLOW has a default spatial resolution of 1 ha and temporal resolution of 1 year. Therefore, the model simulates annual decision of smallholder farmers in labour, financial capital, and land allocation. It is suitable to simulate land use change at meso-scale, for example district or province level.

## <a name="_toc148435192"></a>**Preparing inputs for model simulation**

R-FALLOW requires two types of inputs: spatial inputs in the form of maps and non-spatial inputs or parameter values. 

### <a name="_toc148435193"></a>**5.1 Spatial inputs**

The recommended format of input maps for R-FALLOW is tif, which can be produced using any geographical information system (GIS) software. The minimum input maps[^3] that should be prepared for running the model are listed in Table 1. **Please note that all input maps for R-FALLOW should have a consistent geographic projection**. Annex 1 provides a complete list of input maps for R-FALLOW and explains that several input maps should be produced for each crop product or cultivation practice. For example, the land suitability map should be produced for each cultivation practice simulated using the model. Another example, the map of distance to processing industries should be produced for the main product of the cultivation practices. 

<a name="_ref147935487"></a><a name="_toc148435372"></a>Table 1 Minimum input maps for R-FALLOW

|**No**|**Map**|**Name in R-FALLOW**|**Values or unit**|**Description**|
| :- | :- | :- | :- | :- |
|1|Boundary of simulated area|Area.tif|1 or N/A|1= simulated area, N/A=outside simulated area|
|2|Initial landcover types	|Initlc.tif|Landcover ID 0 - 40|Landcover map of the simulated area with ID from 0-40 as a default setting. **Please note that it is possible for R-FALLOW to simulate more than 41 landcover types**. Please see the description below on how to prepare the landcover map.|
|3|Soil fertility|Initsoil.tif, maxsoil.tif|1 – 5 |Initial (or actual) and maximum soil fertility map. Please see the description below on how to prepare the soil map.|
|4|Boundary of forest protection areas|Reserve.tif|0 or 1|Protection forest area (1= protected, 0=not protected)|
|5|Slope|Slope.tif|degree|Can be derived from the DEM map |
|6|Distance to road|Droad.tif|m|Distance of each pixel to the closest main road for transporting crop products|
|7|Distance to river|Driver.tif|m|Distance of each pixel to the closest main river for transporting crop products|
|8|Distance to market|Dmart.tif|m|Distance of each pixel to the closest main market for selling crop products|
|9|Distance to settlement|Dset.tif|m|Distance of each pixel to the closest settlement for e.g., plot’s maintenance|
|10|Distance to processing industry|Dind.tif|m|Distance of each pixel to the closest processing industry of products from simulated cultivation practices.|
|11|Land suitability |Stfood1-4.tif, staf1-8.tif|0 or 1|0=not suitable, 1=suitable, should be produced for each cultivation practice. |

#### <a name="_toc148435194"></a>5.1.1 How to prepare the landcover map

The current version of R-FALLOW can simulate, by default, maximum 41 landcover types. **However, depending on the complexities of landcover types in the simulated site, it is possible for R-FALLOW to simulate more than 41 landcover types**. Table 2 lists the 41 landcover types and they consist of 4 categories of natural forest based on its intactness or regeneration stage namely pioneer, young secondary, old secondary, or primary stage; 4 different annual crops; and 8 different perennial shrub or tree-based practices. Perennial shrubs include coffee or tea monoculture plantation. Each perennial shrub or tree-based practice has four production stages namely pioneer, early production, peak production, and postproduction stage. Users can determine the length (years) of each stage. 

<a name="_ref147937539"></a><a name="_toc148435373"></a>Table 2 The default 41 landcover types simulated by R-FALLOW

<table><tr><th colspan="1" valign="top"><b>Landcover</b></th><th colspan="1" valign="top"><b>ID</b></th><th colspan="1" valign="top"><b>Description</b></th></tr>
<tr><td colspan="1" valign="top">Settlement</td><td colspan="1" valign="top">0</td><td colspan="1" valign="top"></td></tr>
<tr><td colspan="1" valign="top">Natural forest</td><td colspan="1" valign="top">1 – 4</td><td colspan="1" valign="top">Natural forest of <a name="_hlk147938199"></a>pioneer, young secondary, old secondary, or primary stage, respectively. Forest plantation can be considered as tree-based practice simulated using different IDs (9 – 40). </td></tr>
<tr><td colspan="1" valign="top">Annual crops</td><td colspan="1" valign="top">5 – 8 </td><td colspan="1" valign="top">Four different annual crops</td></tr>
<tr><td colspan="3" valign="top"><i>Perennial shrubs or tree-based practices (plantation or agroforestry)</i></td></tr>
<tr><td colspan="1" valign="top">Practice 1</td><td colspan="1" valign="top">9 – 12  </td><td colspan="1" rowspan="8">Pioneer, early production, peak production, and postproduction stage, respectively.</td></tr>
<tr><td colspan="1" valign="top">Practice 2</td><td colspan="1" valign="top">13 – 16 </td></tr>
<tr><td colspan="1" valign="top">Practice 3</td><td colspan="1" valign="top">17 – 20 </td></tr>
<tr><td colspan="1" valign="top">Practice 4</td><td colspan="1" valign="top">21 – 24 </td></tr>
<tr><td colspan="1" valign="top">Practice 5</td><td colspan="1" valign="top">25 – 28 </td></tr>
<tr><td colspan="1" valign="top">Practice 6</td><td colspan="1" valign="top">29 – 32 </td></tr>
<tr><td colspan="1" valign="top">Practice 7</td><td colspan="1" valign="top">33 – 36 </td></tr>
<tr><td colspan="1" valign="top">Practice 8</td><td colspan="1" valign="top">37 – 40 </td></tr>
</table>


Users might obtain input landcover map from e.g., the authorities or local stakeholders of the simulated district or province, and they might need to do some re-classification of land cover types to match the ID setting of R-FALLOW.  

#### <a name="_toc148435195"></a>5.1.2 How to prepare the soil map

The level of soil fertility in R-FALLOW ranges from 1 (least) to 5 (most fertile). Maximum soil fertility is the maximum level of soil fertility that a soil type can achieve and should be equal or higher than the initial or actual soil fertility. To prepare the initial and maximum soil fertility map:

- Please obtain a soil map which covers the entire simulated area and contains sufficient information for a soil expert to assign the relative value between 1 and 5 to the different soil types. For example, a soil map which follows e.g., the FAO standard classification (<https://www.fao.org/soils-portal/data-hub/soil-classification/en/>) or contains information of the level of soil organic contents over the simulated area[^4].
- Please ask a soil expert to approximate the initial and maximum level of soil fertility of each soil type or the relative level of soil fertility of different areas over the simulated landscape. 
- If no soil information is available for the simulated site, a derivation from the global or regional dataset (for example, <https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/en/>)  might be useful as the first approximation. 

### <a name="_toc148435196"></a>**5.2 Non-spatial inputs**

The non-spatial inputs consist of economic, biophysical, and demographic inputs. Below is a description of minimum non-spatial inputs required by R-FALLOW, while a more detailed information is given in Annex 1.

#### <a name="_toc148435197"></a>5.2.1 Socio-economic inputs 

There are at minimum 9 socio-economic inputs[^5] required by R-FALLOW (Table 3). Those inputs need to be estimated for each cultivation (annual crop or tree-based) practice[^6]. For the inputs which relate to unit currency, it is recommendable to use the values over the past e.g., 5 years, and take the average to incorporate the historical trend. User can assign a coefficient of variation of each input parameter for R-FALLOW to generate values around the average using the variation[^7]. 

<a name="_ref148014027"></a><a name="_toc148435374"></a>Table 3 Minimum socio-economic inputs required by R-FALLOW

|**No**|**Name**|**Unit**|**Description**|
| :- | :- | :- | :- |
|1|Establishment cost|Unit currency per ha|Cost to establish each cultivation practice. |
|2|Establishment labour|Person day per ha|Labour required to establish each cultivation practice|
|3|Initial financial capital|Unit currency|Total financial capital of all farm households in the simulated landscape at the start of the simulation. It can be approximated for example by multiplying the average annual net income per household by the total number of farm households in the simulated area.|
|4|Secondary consumption fraction|0-1|Fraction of income for secondary consumption|
|5|Households’ consumption|Ton per capita|Collected crop yield used for household’s consumption (while the rest can be e.g., for sale). Input for each cultivation practice. |
|6|Return to labour|Unit currency per person day|Net income per unit labour required to establish, maintain the farm, and collect the harvest. For cultivation practices with perennial crops, can be estimated for example for the first 10 years after farm establishment.  |
|7|Return to land|Unit currency per ha|Net income per unit land size (by default hectare). As return to labour, for cultivation practices with perennial crops, can be estimated for example for the first 10 years after farm establishment.|
|8|Product price|Unit currency per ton|Price of crop product from the cultivation practices. If a practice integrates various main crops, need to take the average price of the different main crops. Otherwise, can be represented by the price of the main crop.|
|9|Non-labour cost|Unit currency per ha|Non-labour cost for maintaining the farm and harvesting. Need to estimate for each cultivation practice.|

#### <a name="_toc148435198"></a>5.2.2 Biophysical and demographic inputs

There are at minimum 8 biophysical and demographic inputs required by R-FALLOW (Table 4). Related to the biophysical inputs, for the case of perennial crop- or tree-based practices, estimation for each growth or production stage (pioneer, early-, peak-, and post-production) is necessary. 

<a name="_ref148016609"></a><a name="_toc148435375"></a>Table 4 Minimum biophysical and demographic inputs required by R-FALLOW

|**No**|**Name**|**Unit**|**Description**|
| :- | :- | :- | :- |
|1|Aboveground biomass|Ton per ha|Average aboveground biomass of each land cover type (please remind that there are, by default, maximum 40 land cover types apart from settlement). The model assumes the biomass contains 46% carbon. |
|2|Land cover time bound|Year|Need to specify the minimum and maximum age of each production stage. The range of e.g., peak production stage of a certain tree-based practice might not necessarily be the same as that of other simulated tree-based practices.  |
|3|Yield|Ton per ha|Yield of annual crop or each stage of perennial crop- or tree-based practices.|
|4|Harvesting productivity|Ton per person day|Harvesting productivity of each cultivation practice.|
|5|Initial human population|People|At the start of the simulation|
|6|Annual population growth rate|%||
|7|Fraction of productive farm labours |0-1|Fraction of productive farm labours, including household’s labours, relative to the total population of the simulated site|
|8|Annual working days|Person day per year|Average annual working days|

## <a name="_toc148435199"></a>**How to open and parameterize R-FALLOW**

### <a name="_toc148435200"></a>**6.1 How to open the model**

There are two possibilities: using or without internet connection. 

#### 6.1.1 Using internet connection

There are two ways to open the model online:

**Option 1: Using an internet browser** (e.g., Google Chrome) and this web address <https://degi.shinyapps.io/rfallow/>. However, it is currently still a temporary address with limited runtime access (because it is hosted on a free server). The more permanent host and web address will available soon. Figure 3 shows the main interface of R-FALLOW opened using the web address[^8]. The main advantage of opening the model using the web address is that no software or plugin is necessary to install on our computer.

<a name="_ref148361725"></a><a name="_toc148435230"></a>Figure 3 The main user interface of R-FALLOW 

**Option2:** Because the first option currently has a limitation, **we can open the model using RStudio software like described below but it still needs an internet connection**. Please follow these steps:
- Please install R and RStudio in our desktop[^9], and please just follow the instructions provided on the website to install the software (note: R and RStudio are available for any computer’s operating system).
- Please open RStudio and run the script below:

*library(shiny)*

*runGitHub("rfallow", "degi")*

To run the script, copy one line of the script to “Console” window in RStudio, then press “Enter”, and please do the same with the next line (Figure 4). 

- Please note that RStudio may ask you to install or update some missing R libraries. Please approve all the requested updates which may appear on the notifications generated by RStudio, or we can also install or update the libraries by selecting the menu “Tools” and then “Install packages”. A dialogue box like shown in Figure 5 will appear and please put the name of the missing libraries (namely, missing libraries informed by RStudio) into the “Packages (separate multiple with space or comma)”, and then click “Install”. Once all required libraries are installed, the script will generate a user interface like that shown earlier in Figure 3. 

The second option is more complicated than the first one because we need to install R, RStudio, and required libraries. However, it is not necessary for us to copy the model’s source code like the option without internet which will be described below.  

<a name="_ref148541619"></a>Figure 4 Running the script in RStudio to open R-FALLOW without copying the source code

<a name="_ref148541916"></a>Figure 5 Install missing R libraries using RStudio

#### 6.1.2 Without internet connection
Please do the following steps:

* Please copy the R source code available on GitHub (<https://github.com/degi/rfallow>, Figure 6) to one folder on your computer’s hard drive.

<a name="_ref148362991"></a><a name="_toc148435231"></a>Figure 6 The source code for running R-FALLOW without internet connection 

* Using RStudio, please open the file “run\_RFallow\_shiny\_GUI.R” which contains the following script (Figure 7):

*library(shiny)*

*d <- dirname(rstudioapi::getActiveDocumentContext()$path)*

*setwd(paste0(d, "/.."))*

*runApp("rfallow")*

* Please run the script by putting the cursor on the first line and click the green arrow (or press CTRL + Enter) and do similarly for the second until the fourth line. A user interface like shown earlier in Figure 3 will appear. 

Please note that, to be able to open R-FALLOW using the script and without internet connection, your computer should have all required R libraries. However, the libraries will not be automatically updated if you keep using R-FALLOW without internet connection for long term, unless it is connected to internet.  

<a name="_ref148363361"></a><a name="_toc148435232"></a>Figure 7 R scripts to open R-FALLOW without internet connection

### <a name="_toc148435201"></a>**6.2 How to parameterize the model**

#### <a name="_toc148435202"></a>6.2.1 Module “Initial Input”

There are three different ways of parameterizing the model through the module “Initial Input” (Figure 8):
* Upload initial landcover map and manually input parameter values using the model’s interface (option 1). In this option, we can upload each input map one by one, and assign parameter values manually into the tables provided by the model.
* Load a compressed file (\*.zip) containing all input maps (\*.tif) and input parameter values in csv files (option 2). This option can be selected if you have prepared all input maps in tif and input parameters values in csv files. The csv files are available in the zip file of example of R-FALLOW application in Buol district, Sulawesi Island, Indonesia that can be obtained from the model’s developers. Users can simply replace the inputs maps and parameter values in the csv files to suit their simulation site.  
* Load a compressed file (\*.zip) containing all input maps (\*.tif) and input parameter values available in the PC Raster format (namely, the previous version of FALLOW) (option 3). This option facilitates users who have input data in such format to run with R-FALLOW.  

<a name="_ref148025535"></a><a name="_toc148435233"></a>Figure 8 Three different ways of parameterizing R-FALLOW

#### <a name="_toc148435203"></a>6.2.2 Option 1 of parameterizing R-FALLOW
If you selected the Option 1, then a user interface like shown in Figure 9 will appear. 

<a name="_ref148263919"></a><a name="_toc148435234"></a>Figure 9 Option 1 of parameterizing R-FALLOW

Please follow these steps:

* Upload your initial land cover map. Again, a preferable format of input maps for R-FALLOW is tif. The uploaded land cover map will be automatically displayed by R-FALLOW like shown, as an example, in Figure 10, for the case of the Buol district.
* Please set the preferred colour, land use and land cover name, related growth or production stage (please refer again the section 5.1.1 on such stages), and land cover ‘short name’.
* Once you have set the land cover table, please save the setting using the menu on the top right of the table (Figure 11). The land cover setting that you saved, will be automatically displayed by R-FALLOW in other tables of input parameters that you will need to set up. This will ensure that you use consistent land use or land cover setting during the model’s parameterization.

<a name="_ref148270689"></a><a name="_toc148435235"></a>Figure 10 An example land cover map opened using the Option 1

<a name="_ref148271112"></a><a name="_toc148435236"></a>Figure 11 Section for preferable setting of land use and land cover information 

* Please upload other required input maps and all tables of input parameters by checking available modules: “Spatial Data”, “Biophysics by Land Cover”, etc.

The module “Spatial Data” facilitates the upload of other spatial information required by the model (Figure 12). The interface will automatically display the uploaded maps, and it helps users to re-check the accuracy and relevance of the input maps. 

<a name="_ref148365962"></a><a name="_toc148435237"></a>Figure 12 Module of "Spatial Data Input”

The module “Biophysics by Land Cover” requires information of the maximum age of each stage of perennial crop or tree-based practices (Land cover time bound), initial land cover age which determines the age of each landcover type at the start of the simulation, soil depletion rate, floor biomass, aboveground biomass, and probability of fire use and spread. 

<a name="_toc148435238"></a>Figure 13 Module of "Biophysics by Land Cover”

The module "Biophysics by Livelihood” requires biophysical information by livelihood option. The latter is household’s income source which consists of 15 different options namely, off-farm, non-timber forest products (NTFP), timber, 4 annual crop practices, and 8 perennial crop- or tree-based practices. In this module, users can assign parameter values which relate to harvesting productivity, demand per capita, probability to sell, fraction of loss when transporting agricultural products, and main factors which determine the plot attractiveness of crop expansion.   

<a name="_toc148435239"></a>Figure 14 Module of "Biophysics by Livelihood”

The module of “Economic by Land Cover” requires information of non-labour costs and yield by land cover type (Figure 15).

<a name="_ref148367334"></a><a name="_toc148435240"></a>Figure 15 Module of “Economic by Land Cover”

The module "Economics by Livelihood” requires information of product price, possible subsidy, labour requirement for farm establishment, return to land and labour (in the sub-module “Initial knowledge”), cost for establishing new farm, and external labour needed to maintain the farms.

<a name="_toc148435241"></a>Figure 16 Module of "Economics by Livelihood”

The module "Socio Cultural” facilitates users to assign values for cultural influence namely the influence of cultural preference in choosing land use options; extension property which informs availability of agricultural extension, credibility of the extension staffs as perceived by smallholder farmers, and openness of smallholder farmers to information conveyed through the extension; return to land and labour informed by external sources which can be different as compared to the current return to land and labour assumed or experienced by smallholder farmers[^10]; and local habit of burning farm lands before new crop cultivations.    

<a name="_toc148435242"></a>Figure 17 Module of "Socio Cultural”

The module “Others” is for inputs which relate to demographic condition (initial population, annual growth rate, fraction of productive farm labours, annual working days, initial financial capital, and secondary consumption fraction), types of farmers and their learning process (population fraction, alpha and beta factor, and ‘profit orientation’), time and impact of natural disaster (on human population, financial capital, working days), and some conversion factors (timber volume to biomass, biomass to carbon, local currency to USD, and fee of external labour).  

<a name="_toc148435243"></a>Figure 18 Module of "Others”

* Save the input parameter setting using the module “Checklist Summary” (Figure 19). R-FALLOW will compile input maps and parameter values in a zip file. You can later open the compressed file using Option 2.    

<a name="_ref148369997"></a><a name="_toc148435244"></a>Figure 19 Module of "Checklist Summary” 

#### <a name="_toc148435204"></a>6.2.3 Option 2 of parameterizing R-FALLOW
The suggested steps are as follow:

* Please extract the example file “Inputs\_FALLOW\_Buol.zip” into any folder in your computer. As mentioned earlier, the zip file contains input maps and several csv files which integrate non-spatial inputs required by R-FALLOW (Figure 20). Please remind that, the R-FALLOW will also produce a similar zip file if you selected the Option 1 and saved the parameter setting using the module “Checklist Summary”.

<a name="_ref148263356"></a><a name="_toc148435245"></a>Figure 20 Spatial and non-spatial inputs required by R-FALLOW using the Option 2

* The zip file “Inputs\_FALLOW\_Buol.zip” contains example of inputs for the case of R-FALLOW simulation in the Buol district. Please replace each map with your input maps and keep the same file names. Please also refer again to Table 1 and Annex 1 for an explanation of each map.
* Please open each csv file using e.g., MS Excel to assign parameter values. Please note that each input table on the R-FALLOW’s interface is represented by a csv file. For examples: 
- The agentprop.csv (Figure 21) corresponds to the table “Farmer learning” in the module “Others” which contains information of population, impact of self-reflection and suggestions from others in decision making (i.e., alpha and beta learning respectively), and degree of profit orientation.  

<a name="_ref148385697"></a><a name="_toc148435246"></a>Figure 21 The agentprop csv file

- The biophysic\_lc.csv (Figure 22) corresponds to all tables in the module “Biophysics by Land Cover” which contains information of land cover time bound, soil depletion rate, etc. **Please note that**, while in Option 1 you can set the land cover names etc. using the model’s interface, in Option 2 you will need to do that inside the csv files. For example, here in the biophysic\_lc.csv, you need to replace the crop’s names from patchouli, rice etc. (namely, the crops in the Buol district) with the names of your simulated crops. There are other csv files, apart from the biophysic\_lc.csv, which also require you to replace the sample crop names. Please use consistent crop names across the different csv files.  

<a name="_ref148386134"></a><a name="_toc148435247"></a>Figure 22 The biophysic\_lc csv file

- Please assign parameter values in other csv files and refer to Annex 1 which describes the link between R-FALLOW modules and corresponding csv files.   

* Once you have replaced the input maps and set suitable parameter values through the csv files, please re-zip those files, and open with R-FALLOW using the Option 2. The model will automatically display the input maps and parameter values in corresponding modules and tables. **Please note that**, using the Option 2, you can still modify parameter values in any table on the R-FALLOW’s interface and re-save the parameter setting using the module “Checklist Summary”. **However**, in Option 2, you can only modify land use or land cover name using the csv files namely outside R-FALLOW, not through the model’s interface. Modifying the land use or land cover name through the model’s interface will make the assigned parameter values in other tables become 0 because of inconsistent land use or land cover names between the land cover setting and those referred in the other tables.  

#### <a name="_toc148435205"></a>6.2.4 Option 3 of parameterizing R-FALLOW
The users of the PC Raster version of FALLOW can compress the input maps (namely, maps in PC Raster format) and parameter files (\*.par) into a zip file. Please upload the zip file using the Option 3 and as in Option 2, the R-FALLOW will display all maps and input parameter values in corresponding modules and tables. The users can save the parameter setting using the module “Checklist Summary”.   

## <a name="_toc148435206"></a>**7. How to run the model**
Once all spatial and non-spatial inputs were ready, we can run the model using the module “Run Simulation’ (Figure 23). Please determine the length of the simulation year and then click “Run”. 

<a name="_ref148278850"></a><a name="_toc148435248"></a>Figure 23 Module of “Run Simulation”

The “iteration (year)” indicates current simulation year while “Progress Detail” indicates steps executed within each simulation year. Once the simulation was completed, R-FALLOW will generate a notification like shown in Figure 24.

<a name="_ref148280623"></a><a name="_toc148435249"></a>Figure 24 Notification of end of simulation 

## <a name="_toc148435207"></a>**8. Outputs of the model**
There are several outputs that can be displayed using R-FALLOW and saved into images or tables. Please select the dropdown list to know possible outputs to display (Figure 25). 

<a name="_ref148280822"></a><a name="_toc148435250"></a>Figure 25 List of possible outputs to display using R-FALLOW

The model can display the outputs either in chart, table, or map. For example, Figure 26 shows the projected final land cover (at simulation year 30) in a map and land cover area across 30 simulation years in a chart. Please select “Table” to get similar outputs in a table format. For the chart option, you can choose between 50% or 100% width as the chart size and click “Display”. R-FALLOW can save output charts or maps in an image format while output tables in csv format, by clicking “Download”.

<a name="_ref148280951"></a><a name="_toc148435251"></a>Figure 26 Examples of outputs of R-FALLOW

## <a name="_toc148435208"></a>**9. Useful references** 
Ordered by publication year:

- van Noordwijk M. 2002. Scaling trade-offs between crop productivity, carbon stocks and biodiversity in shifting cultivation landscape mosaics: the FALLOW model. Ecol. Model. 149, 113–126. Available at: <https://www.sciencedirect.com/science/article/abs/pii/S030438000100518X>  
- van Noordwijk, M., Suyamto, D.A., Lusiana, B., Ekadinata, A. and Hairiah, K. 2008. Facilitating agroforestation of landscapes for sustainable benefits: tradeoffs between carbon stocks and local development benefits in Indonesia according to the FALLOW model. Agriculture Ecosystems and Environment Special Issue on Climate Change Research CGIAR. Available at: <https://www.sciencedirect.com/science/article/abs/pii/S0167880908000224>    
- Suyamto DA, Mulia R, van Noordwijk M, Lusiana B. 2009. Fallow 2.0: Manual and Software. Available at: <https://www.researchgate.net/publication/250927208_FALLOW_20_Manual_and_Software> 
- Lusiana, B., van Noordwijk, M., Cadisch, G. 2012. Land sparing or sharing? Exploring livestock fodder options in combination with land use zoning and consequences for livelihoods and net carbon stocks using the FALLOW model. Agriculture, Ecosystems and Environment 159: 145– 160. Available at: <https://www.sciencedirect.com/science/article/abs/pii/S0167880912002666>   
- Mulia, R., Widayati, A., Suyanto, Agung, P., Zulkarnain, M.T. 2013. Low carbon emission development strategies for Jambi, Indonesia: simulation and trade-off analysis using the FALLOW model. Mitig Adapt Strateg Glob Change. doi: 10.1007/s11027-013-9485-8. Available at: <https://link.springer.com/article/10.1007/s11027-013-9485-8>     
- Tata, H.L., van Noordwijk, M., Ruysschaert, D., Mulia, R., Rahayu, S., Mulyoutami, E., Widayati, A., Ekadinata, A., Zen, R., Darsoyo, A., Oktaviani, R., and Dewi, S. 2013. Will REDD+ funding stop peat swamp conversion to oil palm in orangutan habitat in Tripa (Aceh, Sumatra, Indonesia)? Mitig Adapt Strateg Glob Change. doi: 10.1007/s11027-013-9524-5. Available at: <https://link.springer.com/article/10.1007/s11027-013-9524-5>   
- Mulia R, Nguyen MP, Do HT. 2018. Forest and crop-land intensification in the four agro-ecological regions of Vietnam: Impact assessment with the FALLOW model. Towards Low-Emission Landscapes in Vietnam. Mulia R, Simelton E (Eds). Pp 89-108. Available at: <https://apps.worldagroforestry.org/region/sea/publications/detail?pubID=4437>   
# <a name="_toc148435209"></a>**Annex 1 Complete list of input maps and parameters required by R-FALLOW**

**Spatial inputs (maps)**

|**No**|**Input map**|**Name in R-FALLOW**|**Unit or values** |**Notes**|
| :- | :- | :- | :- | :- |
|1|Land cover	|Initlc.tif|Land cover ID 0-41|In R-FALLOW, the default maximum number of land cover types is 41. However, there is a possibility to simulate more land cover types.|
|2|Initial (current) and maximum soil fertility |Initsoil.tif and maxsoil.tif|1 – 5 |1= least, 5=most fertile|
|3|Area boundary|Area.tif|1=simulated area, N/A=non-simulated area||
|4|Forest protection area|Reserve.tif|1=protection area, 0=not protected||
|5|Slope|Slope.tif|degree |Can be derived from DEM map|
|6|Distance to road|Droada.tif|m||
|7|Distance to river|Drivera.tif|m||
|8|Distance to market|Dmarta.tif|m||
|9|Distance to settlement|Dseta.tif|m||
|10|Distance to processing industries of product from annual crop practice i|<p>Dindfd1a.tif, dindfd2a.tif, dindfd3a.tif, dindfd4a.tif</p><p></p>|m||
|11|Distance to processing industries of main product from tree-based practice i|<p>Dindaf1a.tif, dindaf2a.tif, …, dindaf8a.tif</p><p></p>|m||
|12|Distance to processing industries of non-timber forest product |<p>Dindnta.tif</p><p></p>|m||
|13|Distance to timber processing industries|Dindloga.tif|m|Timber from logging areas|
|14|Initial logging area|Initlog.tif|1=logging area, 0=not logging area|Area of logging for timber|
|15|Land suitability of annual crop practice|Stfood1.tif, stfood2.tif, stfood3.tif, stfood4.tif|1=suitable, 0=not suitable||
|16|Land suitability of tree-based practice|Staf1.tif, …, staf8.tif|1=suitable, 0=not suitable||
|17|Areas affected by disaster|Disaster.tif|1=affected by disaster, 0=not affected by disaster||

**Non-spatial inputs by module on R-FALLOW interface**

\1) Module “Biophysics by Land Cover” (corresponding csv file: biophysics\_lc\.csv)

|**No**|**Parameter name**|**Unit**|**Description**|
| :- | :- | :- | :- |
|1|Land cover time bound|Year|Need to specify the minimum and maximum age of each production stage. The range of e.g., peak production stage of a certain tree-based practice might not necessarily be the same as that of other simulated tree-based practices.  |
|2|Depletion rate|% per ton|Soil fertility depletion rate to produce one ton of crop products|
|3|Half time for recovery|Year|Period needed to achieve half of maximum soil fertility|
|4|Aboveground biomass|Ton per ha|Average aboveground biomass by land cover type|
|5|Floor biomass|%|Fraction from aboveground biomass|
|6|Initial land cover age|Year|Initial age of each land cover type (i.e., age at the start of the simulation)|
|7|Probability of fire spread|0-1|A probability that fire spreads from adjacent plots which implement burning practice|

\2) Module “Biophysics by Livelihood” (biophysics\_II\.csv)

|**No**|**Parameter name**|**Unit**|**Description**|
| :- | :- | :- | :- |
|1|Harvesting productivity|Ton per person day|Harvesting productivity for each livelihood option|
|2|Demand per capita|Ton per capita per year|Collected yield used for consumption|
|3|Probability to sell|0-1|Probability that the unconsumed yield will be sold to the market|
|4|Loss fraction|0-1|Fraction of collected yield will be lost during e.g., transporting the products|
|5|Expansion determinant|0-1|Weighting factor reflecting the importance of various factors (soil fertility, land productivity, land suitability, transport access, plot maintenance, slope, floor biomass) for land expansion, to calculate plot attractiveness|

\3) Module “Economics by Land Cover” (economics\_lc\.csv)

|**No**|**Parameter name**|**Unit**|**Description**|
| :- | :- | :- | :- |
|1|Non-labour cost |Unit currency per ha|For maintaining and harvesting, for each land cover type|
|2|Yield|Ton per ha|Yield of each land cover type that can be represented by the yield of main product or in case of a mixed practice (e.g., agroforestry) with several main products: the total of yield from the various main products|

\4) Module “Economics by Livelihood” (economics\_II\.csv)

|**No**|**Parameter name**|**Unit**|**Description**|
| :- | :- | :- | :- |
|1|Product price|Unit currency per ton|Price of crop product from the cultivation practices. If a practice integrates various main crops, need to take the average price of the different main crops. Otherwise, can be represented by the price of the main crop.|
|2|Return to labour|Unit currency per person day|Net income per unit labour required to establish, maintain the farm, and collect the harvest. For cultivation practices with perennial crops, can be estimated for example for the first 10 years after farm establishment.  |
|3|Return to land|Unit currency per ha|Net income per unit land size (by default hectare). As return to labour, for cultivation practices with perennial crops, can be estimated for example for the first 10 years after farm establishment.|
|4|Subsidy for farm establishment|Unit currency||
|5|Subsidy for farm maintenance|Unit currency||
|6|Establishment labour|Person day per ha|Labour required to establish each cultivation practice|
|7|Establishment cost|Unit currency per ha|Cost to establish each cultivation practice |
|8|External labour |Person day per ha|External labour involved in plot maintenance or harvesting|

\5) Module “Socio Cultural” (socio\_II\.csv)

|**No**|**Parameter name**|**Unit**|**Description**|
| :- | :- | :- | :- |
|1|Cultural influence|0-1|Non-economic consideration for choosing livelihood options|
|2|Payoff to labour|Unit currency per person day|Return to labour according to external source of information |
|3|Payoff to land|Unit currency per ha|Return to land according to external source of information|
|4|Extension availability|0 or 1|0=no agricultural extension available, 1= available|
|5|Extension credibility|0-1|Credibility of the extension staffs according to farmer’s perspective|
|6|Exposure fraction|0-1|Farmer’s openness to agricultural extension|
|7|Probability of using fire for land clearing|0-1||

\6) Module “Others” 

|**No**|**Parameter name**|**Unit**|**Description**|
| :- | :- | :- | :- |
|*Demography (demographics.csv)*||||
|1|Initial human population|People|At the start of the simulation|
|2|Annual population growth rate|%||
|3|Fraction of productive farm labours |0-1|Fraction of productive farm labours, including household’s labours, relative to the total population of the simulated site|
|4|Annual working days|Person day per year|Average annual working days|
|5|Initial financial capital|Unit currency|Total financial capital of all farm households in the simulated landscape at the start of the simulation. It can be approximated for example by multiplying the average annual net income per household by the total number of farm households in the simulated area.|
|<p>6</p><p></p>|Secondary consumption fraction|0-1|Fraction of income for secondary consumption|
|<p></p><p>*Disaster (disaster.csv)*</p>||||
|1|Impact on human population |0-1|Decrease in human population due to disaster|
|2|Impact on financial capital |0-1|Decrease in financial capital due to disaster|
|3|Impact on working day |0-1|Decrease in working day due to disaster|
|4|Time of disaster event|[]|In which simulation year, a disaster took place or will likely take place|
|*Farmer learning (agentprop.csv)*||||
|1|Population fraction|0-1|Population fraction of two possible types of farmers (agent 1 and 2). For example, agent 1 represents conservative farmers, agent 2 represents ‘profit-oriented farmers. If only one type of farmers is simulated, please put 0 for all parameters related to the type 2|
|2|Alpha factor|0-1|0 = farmers ignore current year experience, 1= farmers fully consider current year experience in land use decision making |
|3|Beta factor|0-1|0 = farmers ignore suggestions from others, 1= farmers fully consider suggestions from others in land use decision making|
|4|Land use priority|>0|0=available resources (labour, land, financial capital) will be allocated uniformly among livelihood options, >0 = available resource will be allocated mainly to more profitable livelihood options |
|*Converter (converter.csv)*||||
|1|Timber volume to biomass|Ton per m<sup>3</sup> |Default is 1.25|
|2|Biomass to carbon|Ton carbon per ton biomass|Default is 0.46|
|3|Local currency per USD|USD per unit local currency|If users need economic outputs in USD |
|4|Fee of external labour|Unit currency per person day||


26


[^1]: The model does not consider income of e.g., concession companies or households who engage only in non-farm activities. 
[^2]: Factors considered in determining plot attractiveness are soil fertility, land suitability e.g., based on climate and topography, capacity of the plot or land for production, access (to river, main road, market, and processing industry), maintenance e.g., distance to settlement or lands with the same crop species, slope or elevation, and floor biomass i.e., lands with high floor biomass will incur higher land preparation or establishment cost.    
[^3]: ‘Minimum' here means most important spatial information that the users should gather to run R-FALLOW. 
[^4]: For example, for the case of the Philippines, information of soil organic carbon over the country can be obtained from this document (page 38): <https://www.unccd.int/sites/default/files/ldn_targets/Philippines%20LDN%20TSP%20Country%20Report.pdf> 
[^5]: Like the case of spatial inputs, ‘minimum' here means most important non-spatial information that the users should gather to run R-FALLOW and get reasonable results.
[^6]: The practices should be consistent with those set in the land cover map.
[^7]: This will make the results of two or several simulations using R-FALLOW are not exactly the same.
[^8]: And as described later below, a similar user interface will appear if the model runs offline.
[^9]: R and RStudio software are free software which can be downloaded from e.g., 

    <https://posit.co/download/rstudio-desktop/> 
[^10]: In their decision making, smallholder farmers can consider if they will follow what they assume or experience or adjust the return to land and labour by, to some extent, incorporating the information from external sources. The adjustment or non-adjustment reflects their ‘learning’ process.
