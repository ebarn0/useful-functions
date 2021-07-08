
#This script is abit messy - sorry, but ive just added it in so you can see how to set up an odbc connection and run 
#SQL queries in R. It's actually not too slow!
#You need to make sure you're connected to the VPN, and that you've set up an ODBC connection already. 

library("RODBC") 

#Getting data thru SQL connection------------

connection = odbcConnect("odbcsql")
data = sqlQuery(connection,"SELECT
      
     [ONSAgeBand]
      ,[AdultOrChild]
     
      ,[OPCost]
      ,[OPMFFCost]
      ,[OPPreMFFCost]
      ,[OPLocalAdjustmentCost]
      ,[OPBestPracticeTariffAdjustment]
      ,[OPUnbundledHRGCost]
      ,[CostingMethodDescription]
      ,[Costed]
      ,[IsCosted]
      ,[IsExcluded]
      ,[AppointmentFullDate]

      ,[ProviderCode]
      ,[ProviderName]
      ,[ProviderSiteCode]
      ,[ProviderSiteName]

      ,[OPPointOfDelivery]

      ,[HRG4Code]
      ,[HRG4Description]
      ,[HRG4DescriptionWithCode]
      ,[HRG4SubChapterCode]
      ,[HRG4SubChapterDescription]
      ,[HRG4ChapterCode]
      ,[HRG4ChapterDescription]
      ,[TreatmentSpecialtyCode]
      ,[TreatmentSpecialty]

      ,[SUSDerivedHRG4Code]
      ,[SUSDerivedHRG4Description]

      ,[AttendanceStatusCode]
      ,[AttendanceStatus]
      ,[DidOrDidNotAttend]
      ,[FirstAttendanceCode]
      ,[FirstAttendance]
      ,[AttendanceOutcomeCode]
      ,[AttendanceOutcome]
      ,[AttendanceOutcomeWithCode]
      ,[OperationStatusCode]
      ,[OperationStatus]
	   
      ,[PrimaryDiagnosisICD10Code]
      ,[PrimaryDiagnosisICD10Description]
	  ,PrimaryDiagnosisICD10Chapter
	  ,PrimaryProcedureOPCS4Chapter
      ,[PrimaryProcedureOPCS4Code]
      ,[PrimaryProcedureOPCS4]

  FROM [AnalystGlobal].[SUS].[Outpatient]

  where ProviderCode in ('RXN', 'RXL', 'RXR', 'RTX')
  and TreatmentSpecialtyCode = '130'
  and AppointmentFinancialYearName ='2019-20'")




################################################################
#Summaries-------------
library(tidyverse)
summary = data%>%
  group_by(FirstAttendance)%>%
  summarise(Cost = sum(OPCost), N = n(), costper = Cost/N)

####################################################################
#Inpatient ----

library("RODBC") 

#Getting data thru SQL connection------------

connection = odbcConnect("odbcsql")
data = sqlQuery(connection,"SELECT
      
     [ONSAgeBand]
      ,[AdultOrChild]
     
      ,[OPCost]
      ,[OPMFFCost]
      ,[OPPreMFFCost]
      ,[OPLocalAdjustmentCost]
      ,[OPBestPracticeTariffAdjustment]
      ,[OPUnbundledHRGCost]
      ,[CostingMethodDescription]
      ,[Costed]
      ,[IsCosted]
      ,[IsExcluded]
      ,[AppointmentFullDate]

      ,[ProviderCode]
      ,[ProviderName]
      ,[ProviderSiteCode]
      ,[ProviderSiteName]

      ,[OPPointOfDelivery]

      ,[HRG4Code]
      ,[HRG4Description]
      ,[HRG4DescriptionWithCode]
      ,[HRG4SubChapterCode]
      ,[HRG4SubChapterDescription]
      ,[HRG4ChapterCode]
      ,[HRG4ChapterDescription]
      ,[TreatmentSpecialtyCode]
      ,[TreatmentSpecialty]

      ,[SUSDerivedHRG4Code]
      ,[SUSDerivedHRG4Description]

      ,[AttendanceStatusCode]
      ,[AttendanceStatus]
      ,[DidOrDidNotAttend]
      ,[FirstAttendanceCode]
      ,[FirstAttendance]
      ,[AttendanceOutcomeCode]
      ,[AttendanceOutcome]
      ,[AttendanceOutcomeWithCode]
      ,[OperationStatusCode]
      ,[OperationStatus]
	   
      ,[PrimaryDiagnosisICD10Code]
      ,[PrimaryDiagnosisICD10Description]
	  ,PrimaryDiagnosisICD10Chapter
	  ,PrimaryProcedureOPCS4Chapter
      ,[PrimaryProcedureOPCS4Code]
      ,[PrimaryProcedureOPCS4]

  FROM [AnalystGlobal].[SUS].[Outpatient]

  where ProviderCode in ('RXN', 'RXL', 'RXR', 'RTX')
  and TreatmentSpecialtyCode = '130'
  and AppointmentFinancialYearName ='2019-20'")




################################################################
#Summaries-------------
data = sqlQuery(connection,"SELECT
      
     [ONSAgeBand]
      ,[AdultOrChild]
     
      ,[IPCost]

      ,[ProviderCode]
      ,[ProviderName]
      ,[ProviderSiteCode]
      ,[ProviderSiteName]

	   ,[SUSDerivedSpellHRG4Code]
      ,[SUSDerivedSpellHRG4Description]
      ,[SUSDerivedSpellHRG4DescriptionWithCode]
      ,[SUSDerivedSpellHRG4SubChapterCode]
      ,[SUSDerivedSpellHRG4SubChapterDescription]
      ,[SUSDerivedSpellHRG4ChapterCode]
      ,[SUSDerivedSpellHRG4ChapterDescription]

      ,[TreatmentSpecialtyCode]
      ,[TreatmentSpecialty]

      ,[LengthOfStay]

	  ,IPPointOfDelivery
	   
      ,[PrimaryDiagnosisICD10Code]
      ,[PrimaryDiagnosisICD10Description]
      ,[PrimaryProcedureOPCS4Code]
      ,[PrimaryProcedureOPCS4]
	  ,IsLastInSpell

  FROM [AnalystGlobal].[SUS].[InpatientSpell]

  where ProviderCode in ('RXN', 'RXL', 'RXR', 'RTX')
  and TreatmentSpecialtyCode = '130'
  and AdmissionFinancialYearName ='2018-19'")
library(tidyverse)
summary = data%>%
  filter(IsLastInSpell == 1)%>%
  filter(IPPointOfDelivery %in% c('PbR Elective', 'Non PbR Elective'))%>%
  group_by(SUSDerivedSpellHRG4Description )%>%
  summarise(Cost = sum(IPCost), N = n(), Beddays = sum(LengthOfStay))
write.csv(summary, '~/Project work/LSC/LSC PCB/201819 IP LSC PCB oph hrg nosite elective IP spells.csv', row.names = F)
