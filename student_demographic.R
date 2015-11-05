
#AALC demographic dataset assembly, Ann Arbor Data Dive 2015
#script written by Josh Gardner, UMSI

library(plyr)
library(tidyr)
library(stringr)
library(xlsx)

#===================================================================================
#studentlist

#read student lists and create "recordyear" field to indicate school year this record represents
studentlist_13_14 = read.csv("alldata/June 30 2014 EOY Student List Certified.csv")
studentlist_14_15 = read.csv("alldata/June 30 2015 EOY Student List Certified.csv")
studentlist_15_16 = read.csv("alldata/Fall 2015 Student List Uncertified.csv")

studentlist_13_14$RecordYear = "2013-2014"
studentlist_14_15$RecordYear = "2014-2015"
studentlist_15_16$RecordYear = "2015-2016"

#for 13_14 and 15_16: subset columns for name, address, city, ZIP, UIC. rename the columns.

studentlist_13_14 = studentlist_13_14[,c(10, 11, 12, 13, 25, 28, 30, 31, 32, 144)]
studentlist_13_14 = rename(studentlist_13_14, c("PersonalDemographicsCity" = "City", "ZipCode" = "ZIP"))

studentlist_15_16 = studentlist_15_16[,c(10, 11, 12, 13, 26, 29, 31, 32, 33, 145)]
studentlist_15_16 = rename(studentlist_15_16, c("PersonalDemographicsCity" = "City", "ZipCode" = "ZIP"))

#for 14_15 , subset columns for name, address, city, ZIP, UIC. rename the columns.
studentlist_14_15 = studentlist_14_15[,c(4, 5, 6, 7, 8)]
studentlist_14_15 = separate(studentlist_14_15, col = "txtName", into = c("LastName", "FirstName"), sep = ",")
studentlist_14_15 = rename(studentlist_14_15, c("txtAddress" = "StreetAddress" , "txtCity" = "City", "txtZip" = "ZIP", "textbox9" = "UIC"))

#TODO: combine using rbind (NOTE: SOME FIELDS MISSING FRMO 14-15 DATA)
# 
# studentlist = rbind(studentlist_13_14, studentlist_14_15, studentlist_15_16)
# studentlist = rename(studentlist, "PersonalDemographicsCity" = "City")

#===================================================================================
#enrollment data

enrollment_data = read.csv("alldata/2015-16 Enrollment Form Responses 07 15 2015.csv")

#subset only needed data
enrollment_data = enrollment_data[,c(2, 3, 4, 7, 11, 14, 15, 18, 22:24)]

#TODO: reformat to match destination data in merge
enrollment_data = rename(enrollment_data, c("Legal.last.name" = "LastName", "Legal.first.name" = "FirstName", "Middle.name" = "MiddleName", "Street.Address" = "StreetAddress", "Zip.code" = "ZipCode", "What.language.s..does.the.student.speak."= "Languages", "Does.the.student.receive.special.education.services.listed.on.an.IEP." = "SpecialEducation", "X2015.16.Grade.level" = "Grade in 2015-16"))
enrollment_data = mutate(enrollment_data, MiddleName = substr(MiddleName, 1, 1))

#===================================================================================
#attendance - NOTE: 2015-2016 data not available: only have data through October, but this data could be summarized & used to make forecasts if desired.

attendance_2013_2014 = read.csv("alldata/2013-2014 Daily Attendance by Student.csv", strip.white=TRUE)[1:336,]
attendance_2014_2015 = read.csv("alldata/2014-2015 Daily Attendance by Student.csv", strip.white=TRUE)[1:261,]

attendance_2013_2014$RecordYear = "2013-2014"
attendance_2014_2015$RecordYear = "2014-2015"

attendance_2013_2014 = rename(attendance_2013_2014, c("X" = "Student", "tardies" = "Tardies", "exc.absence" = "ExcusedAbsences", "absent" = "UnexcusedAbsences", "suspended" = "SuspensionAbsences", "total.EA....A...S" = "TotalAbsences"))
attendance_2014_2015 = rename(attendance_2014_2015, c("X" = "Student", "tardies" = "Tardies", "exc.absence" = "ExcusedAbsences", "absent" = "UnexcusedAbsences", "suspended" = "SuspensionAbsences", "total.EA....A...S" = "TotalAbsences"))

attendance_2013_2014$Dropped = FALSE; attendance_2013_2014[321:336, "Dropped"] = TRUE
attendance_2014_2015$Dropped = FALSE; attendance_2014_2015[244:261, "Dropped"] = TRUE

#subset to only include valid student names, reformat names, and split into columns to match destination data

attendance_2013_2014 = attendance_2013_2014[grepl(',', attendance_2013_2014$Student),]
attendance_2014_2015 =  attendance_2014_2015[grepl(',', attendance_2014_2015$Student),]

attendance_2013_2014$Student = gsub(" \\(.\\)", "", attendance_2013_2014$Student); attendance_2013_2014$Student = str_trim(attendance_2013_2014$Student)
attendance_2014_2015$Student = gsub(" \\(.\\)", "", attendance_2014_2015$Student); attendance_2014_2015$Student = str_trim(attendance_2014_2015$Student)


attendance_2013_2014 = separate(attendance_2013_2014, col = "Student", into = c("LastName", "FirstName"), sep = ",")
attendance_2014_2015 = separate(attendance_2014_2015, col = "Student", into = c("LastName", "FirstName"), sep = ",")


#subset
attendance_2013_2014 = attendance_2013_2014[,c("LastName", "FirstName", "Tardies", "ExcusedAbsences", "UnexcusedAbsences", "SuspensionAbsences", "TotalAbsences", "RecordYear", "Dropped")]
attendance_2014_2015 = attendance_2014_2015[,c("LastName", "FirstName", "Tardies", "ExcusedAbsences", "UnexcusedAbsences", "SuspensionAbsences", "TotalAbsences", "RecordYear", "Dropped")]

attendance_master = rbind(attendance_2013_2014, attendance_2014_2015)

#===================================================================================
#directly certified students

DC1 = read.xlsx("alldata/Directly Certified Students Report 2014-15.xlsx", sheetIndex=1, startRow=3)
DC2 = read.xlsx("alldata/Directly Certified Students Report 2014-15.xlsx", sheetIndex=2, startRow=3)
DC3 = read.xlsx("alldata/Directly Certified Students Report 2014-15.xlsx", sheetIndex=3, startRow=3)

names(DC1) = c("UIC", "EligCat", "LastName", "FirstName", "MiddleName", "DOB", "Gender", "StudentNumber", "ExitDate", "MatchDate")
names(DC2) = c("UIC", "EligCat", "LastName", "FirstName", "MiddleName", "DOB", "Gender", "StudentNumber", "ExitDate", "MatchDate")
names(DC3) = c("UIC", "EligCat", "LastName", "FirstName", "MiddleName", "DOB", "Gender", "StudentNumber", "ExitDate", "MatchDate")


directly_certified = rbind(DC1, DC2, DC3)

directly_certified = directly_certified[,c(1:5)]


#===================================================================================
#merge with student list
#read in other datasets

