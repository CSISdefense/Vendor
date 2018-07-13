CREATE TABLE [DIIG].[dbo].[FPDSNE](
	[unique_transaction_id] [varchar](36) NULL,
	[obligatedamount] [decimal](19, 4) NULL,
	[signeddate] [date] NULL,
	[effectivedate] [date] NULL,
	[vendorname] [nvarchar](150) NULL,
	[vendoralternatename] [varchar](150) NULL,
	[vendorlegalorganizationname] [nvarchar](150) NULL,
	[vendordoingasbusinessname] [nvarchar](150) NULL,
	[vendorcountrycode] [varchar](50) NULL,
	[dunsnumber] [varchar](13) NULL,
	[parentdunsnumber] [varchar](13) NULL,
	[registrationdate] [date] NULL,
	[renewaldate] [date] NULL,
	[mod_parent] [varchar](150) NULL,
	[locationcode] [varchar](9) NULL,
	[placeofperformancecountrycode] [varchar](3) NULL,
	[psc_cat] [varchar](2) NULL,
	[productorservicecode] [varchar](4) NULL,
	[principalnaicscode] [varchar](6) NULL,
	[countryoforigin] [varchar](3) NULL,
	[fiscal_year] [smallint] NULL,
	[extentcompeted] [varchar](3) NULL,
	[numberofoffersreceived] [bigint] NULL,
	[typeofsetaside] [varchar](10) NULL,
	[organizationaltype] [varchar](30) NULL,
	[womenownedflag] [bit] NULL,
	[veteranownedflag] [bit] NULL,
	[srdvobflag] [bit] NULL,
	[localgovernmentflag] [bit] NULL,
	[minorityinstitutionflag] [bit] NULL,
	[aiobflag] [bit] NULL,
	[stategovernmentflag] [bit] NULL,
	[federalgovernmentflag] [bit] NULL,
	[minorityownedbusinessflag] [bit] NULL,
	[apaobflag] [bit] NULL,
	[tribalgovernmentflag] [bit] NULL,
	[baobflag] [bit] NULL,
	[naobflag] [bit] NULL,
	[saaobflag] [bit] NULL,
	[haobflag] [bit] NULL,
	[ishispanicservicinginstitution] [bit] NULL,
	[contractingofficerbusinesssizedetermination] [varchar](1) NULL,
	[receivescontracts] [bit] NULL,
	[receivesgrants] [bit] NULL,
	[receivescontractsandgrants] [bit] NULL,
	[isforeignownedandlocated] [bit] NULL,
	[iscitylocalgovernment] [bit] NULL,
	[iscountylocalgovernment] [bit] NULL,
	[isfederalgovernmentagency] [bit] NULL,
	[isforeigngovernment] [bit] NULL,
	[isindiantribe] [bit] NULL,
	[isinternationalorganization] [bit] NULL,
	[islocalgovernmentowned] [bit] NULL,
	[ismunicipalitylocalgovernment] [bit] NULL,
	[isnativehawaiianownedorganizationorfirm] [bit] NULL,
	[isotherminorityowned] [bit] NULL,
	[istriballyownedfirm] [bit] NULL,
	[istribalcollege] [bit] NULL,
	[isalaskannativeownedcorporationorfirm] [bit] NULL,
	[iswomenownedsmallbusiness] [bit] NULL,
	[isecondisadvwomenownedsmallbusiness] [bit] NULL,
	[isjointventurewomenownedsmallbusiness] [bit] NULL,
	[isjointventureecondisadvwomenownedsmallbusiness] [bit] NULL,
	[numberofemployees] [bigint] NULL,
	[annualrevenue] [decimal](19, 4) NULL,
	[CSISUniqueIndexID] [uniqueidentifier] NULL,
	[TypeOfBusiness] [varchar](1) NULL,
	[ContractNumber] [varchar](15) NULL,
	[primary_place_of_performance_county_name] [varchar](22) NULL,
	[indian_tribe_federally_recognized] [bit] NULL,
	[other_minority_owned_business] [bit] NULL,
	[local_government_owned] [bit] NULL,
	[international_organization] [bit] NULL,
	[us_government_entity] [bit] NULL,
	[alaskan_native_servicing_institution] [bit] NULL,
	[native_hawaiian_servicing_institution] [bit] NULL,
 )

 INSERT INTO [dbo].[FPDSNE] ( 
	   [unique_transaction_id]
      ,[obligatedamount]
      ,[signeddate]
      ,[effectivedate]
      ,[vendorname]
      ,[vendoralternatename]
      ,[vendorlegalorganizationname]
      ,[vendordoingasbusinessname]
      ,[vendorcountrycode]
      ,[dunsnumber]
      ,[parentdunsnumber]
      ,[registrationdate]
      ,[mod_parent]
      ,[locationcode]
      ,[placeofperformancecountrycode]
      ,[psc_cat]
      ,[productorservicecode]
      ,[principalnaicscode]
      ,[countryoforigin]
      ,[fiscal_year]
      ,[extentcompeted]
      ,[numberofoffersreceived]
      ,[typeofsetaside]
      ,[organizationaltype]
      ,[womenownedflag]
      ,[veteranownedflag]
      ,[srdvobflag]
      ,[localgovernmentflag]
      ,[minorityinstitutionflag]
      ,[aiobflag]
      ,[stategovernmentflag]
      ,[federalgovernmentflag]
      ,[minorityownedbusinessflag]
      ,[apaobflag]
      ,[tribalgovernmentflag]
      ,[baobflag]
      ,[naobflag]
      ,[saaobflag]
      ,[haobflag]
      ,[ishispanicservicinginstitution]
      ,[contractingofficerbusinesssizedetermination]
      ,[receivescontracts]
      ,[receivesgrants]
      ,[receivescontractsandgrants]
      ,[isforeignownedandlocated]
      ,[isfederalgovernmentagency]
      ,[isforeigngovernment]
      ,[isindiantribe]
      ,[isinternationalorganization]
      ,[islocalgovernmentowned]
      ,[isnativehawaiianownedorganizationorfirm]
      ,[isotherminorityowned]
      ,[istriballyownedfirm]
      ,[isalaskannativeownedcorporationorfirm]
      ,[iswomenownedsmallbusiness]
      ,[isecondisadvwomenownedsmallbusiness]
      ,[isjointventurewomenownedsmallbusiness]
      ,[isjointventureecondisadvwomenownedsmallbusiness]
      ,[numberofemployees]
      ,[annualrevenue]
      ,[CSISUniqueIndexID]
      ,[TypeOfBusiness]
      ,[ContractNumber]
      ,[primary_place_of_performance_county_name]
      ,[indian_tribe_federally_recognized]
      ,[other_minority_owned_business]
      ,[local_government_owned]
      ,[international_organization]
      ,[us_government_entity]
      ,[alaskan_native_servicing_institution]
      ,[native_hawaiian_servicing_institution])
 SELECT [unique_transaction_id]
      ,[obligatedamount]
      ,[signeddate]
      ,[effectivedate]
      ,[vendorname]
      ,[vendoralternatename]
      ,[vendorlegalorganizationname]
      ,[vendordoingasbusinessname]
      ,[vendorcountrycode]
      ,[dunsnumber]
      ,[parentdunsnumber]
      ,[registrationdate]
      ,[mod_parent]
      ,[locationcode]
      ,[placeofperformancecountrycode]
      ,[psc_cat]
      ,[productorservicecode]
      ,[principalnaicscode]
      ,[countryoforigin]
      ,[fiscal_year]
      ,[extentcompeted]
      ,[numberofoffersreceived]
      ,[typeofsetaside]
      ,[organizationaltype]
      ,[womenownedflag]
      ,[veteranownedflag]
      ,[srdvobflag]
      ,[localgovernmentflag]
      ,[minorityinstitutionflag]
      ,[aiobflag]
      ,[stategovernmentflag]
      ,[federalgovernmentflag]
      ,[minorityownedbusinessflag]
      ,[apaobflag]
      ,[tribalgovernmentflag]
      ,[baobflag]
      ,[naobflag]
      ,[saaobflag]
      ,[haobflag]
      ,[ishispanicservicinginstitution]
      ,[contractingofficerbusinesssizedetermination]
      ,[receivescontracts]
      ,[receivesgrants]
      ,[receivescontractsandgrants]
      ,[isforeignownedandlocated]
      ,[isfederalgovernmentagency]
      ,[isforeigngovernment]
      ,[isindiantribe]
      ,[isinternationalorganization]
      ,[islocalgovernmentowned]
      ,[isnativehawaiianownedorganizationorfirm]
      ,[isotherminorityowned]
      ,[istriballyownedfirm]
      ,[isalaskannativeownedcorporationorfirm]
      ,[iswomenownedsmallbusiness]
      ,[isecondisadvwomenownedsmallbusiness]
      ,[isjointventurewomenownedsmallbusiness]
      ,[isjointventureecondisadvwomenownedsmallbusiness]
      ,[numberofemployees]
      ,[annualrevenue]
      ,[CSISUniqueIndexID]
      ,[TypeOfBusiness]
      ,[ContractNumber]
      ,[primary_place_of_performance_county_name]
      ,[indian_tribe_federally_recognized]
      ,[other_minority_owned_business]
      ,[local_government_owned]
      ,[international_organization]
      ,[us_government_entity]
      ,[alaskan_native_servicing_institution]
      ,[native_hawaiian_servicing_institution]
 FROM [Contract].[FPDS]

 er

 SELECT *
	FROM DIIG.dbo.allSAMData s
	LEFT OUTER JOIN DIIG.Contract.FPDS f
	on s.duns = f.dunsnumber
	--LEFT OUTER JOIN DIIG.Contractor.DunsnumberToParentContractorHistory dtpch
	--on s.duns = dtpch.dunsnumber
	--and (year(s.registrationDate) + iif(month(s.registrationDate)>=10,1,0))=
	--	dtpch.FiscalYear