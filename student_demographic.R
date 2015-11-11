
#AALC demographic dataset assembly, Ann Arbor Data Dive 2015
#script written by Josh Gardner, UMSI

#load packages
library(plyr)
library(tidyr)
library(stringr)
library(xlsx)

# load FillIn function; this is a modified script from:
# http://www.r-bloggers.com/fillin-a-function-for-filling-in-missing-data-in-one-data-frame-with-info-from-another/
source("FillIn.R")

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

studentlist_13_14 = studentlist_13_14[,c(10, 11, 12, 13, 17, 28, 30, 32, 144)]
studentlist_13_14 = rename(studentlist_13_14, c("PersonalDemographicsCity" = "City", "ZipCode" = "ZIP"))

studentlist_15_16 = studentlist_15_16[,c(10, 11, 12, 13, 17, 29, 31, 33, 145)]
studentlist_15_16 = rename(studentlist_15_16, c("PersonalDemographicsCity" = "City", "ZipCode" = "ZIP"))

#for 14_15 , subset columns for name, address, city, ZIP, UIC. rename the columns.
studentlist_14_15 = studentlist_14_15[,c(4, 5, 6, 7, 8, 41)]
studentlist_14_15 = separate(studentlist_14_15, col = "txtName", into = c("LastName", "FirstName"), sep = ",")
studentlist_14_15 = rename(studentlist_14_15, c("txtAddress" = "StreetAddress" , "txtCity" = "City", "txtZip" = "ZIP", "textbox9" = "UIC"))
studentlist_14_15$MiddleName = NA
studentlist_14_15$Gender = NA

#combine using rbind (NOTE: SOME FIELDS MISSING FROM 14-15 DATA)

studentlist = rbind(studentlist_13_14, studentlist_14_15, studentlist_15_16)

#trim whitespace, coerce first and last names to uppercase, and remove non-alphanumeric characters for matching in merges
studentlist$FirstName = gsub("[^[:alnum:] ]", "", str_trim(toupper(studentlist$FirstName)))
studentlist$LastName = gsub("[^[:alnum:] ]", "", str_trim(toupper(studentlist$LastName)))


#===================================================================================
#enrollment data

enrollment_data = read.csv("alldata/2015-16 Enrollment Form Responses 07 15 2015.csv")

#subset only needed data
enrollment_data = enrollment_data[,c(2, 3, 7, 11, 14, 15, 18)]

#reformat to match destination data in merge
enrollment_data = rename(enrollment_data, c("Legal.last.name" = "LastName", "Legal.first.name" = "FirstName", "What.language.s..does.the.student.speak."= "Languages", "Does.the.student.receive.special.education.services.listed.on.an.IEP." = "SpecialEducation", "X2015.16.Grade.level" = "GradeLevel_2015_2016"))
revalue(enrollment_data$Gender, c("Male" = "M", "Female" = "F")) -> enrollment_data$Gender

#coerce first and last names to uppercase and remove non-alphanumeric data for matching in merges
enrollment_data$FirstName = gsub("[^[:alnum:] ]", "", toupper(enrollment_data$FirstName))
enrollment_data$LastName = gsub("[^[:alnum:] ]", "", toupper(enrollment_data$LastName))

#pull gender values to fill in student_list
studentlist = FillIn(D1 = studentlist, D2 = enrollment_data, Var1 = "Gender", Var2 = "Gender", KeyVar = c("FirstName", "LastName"))
#studentlist = FillIn(D1 = studentlist, D2 = enrollment_data, Var1 = "Gender", Var2 = "Gender", KeyVar = c("FirstName", "LastName"))

#remove gender variable after filling in

enrollment_data = enrollment_data[,-4]

studentlist = merge(studentlist, enrollment_data, by=c("LastName", "FirstName"), all.x=TRUE)

#remove columns duplicated in merge; rename

studentlist = studentlist[,-c(14:17)]

studentlist = rename(studentlist, c("Ethnicity.x" = "Ethnicity", "Languages.x" = "Languages", "SpecialEducation.x" = "SpecialEducation", "GradeLevel_2015_2016.x" = "GradeLevel_2015_2016"))

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


#subset columns to keep only relevant/needed data
attendance_2013_2014 = attendance_2013_2014[,c("LastName", "FirstName", "Tardies", "ExcusedAbsences", "UnexcusedAbsences", "SuspensionAbsences", "TotalAbsences", "RecordYear", "Dropped")]
attendance_2014_2015 = attendance_2014_2015[,c("LastName", "FirstName", "Tardies", "ExcusedAbsences", "UnexcusedAbsences", "SuspensionAbsences", "TotalAbsences", "RecordYear", "Dropped")]

#combine to master dataset
attendance_master = rbind(attendance_2013_2014, attendance_2014_2015)

#clean names to transform for best matching by converting to caps, removing non-alphanumeric characters, and stripping whitespace

attendance_master$FirstName = gsub("[^[:alnum:] ]", "", str_trim(toupper(attendance_master$FirstName)))
attendance_master$LastName = gsub("[^[:alnum:] ]", "", str_trim(toupper(attendance_master$LastName)))

#merge to studentlist
studentlist = merge(studentlist, attendance_master, all.x=TRUE)
#===================================================================================
#directly certified students

DC1 = read.xlsx("alldata/Directly Certified Students Report 2014-15.xlsx", sheetIndex=1, startRow=3)
DC2 = read.xlsx("alldata/Directly Certified Students Report 2014-15.xlsx", sheetIndex=2, startRow=3)
DC3 = read.xlsx("alldata/Directly Certified Students Report 2014-15.xlsx", sheetIndex=3, startRow=3)

names(DC1) = c("UIC", "EligCat", "LastName", "FirstName", "MiddleName", "DOB", "Gender", "StudentNumber", "ExitDate", "MatchDate")
names(DC2) = c("UIC", "EligCat", "LastName", "FirstName", "MiddleName", "DOB", "Gender", "StudentNumber", "ExitDate", "MatchDate")
names(DC3) = c("UIC", "EligCat", "LastName", "FirstName", "MiddleName", "DOB", "Gender", "StudentNumber", "ExitDate", "MatchDate")


directly_certified = rbind(DC1, DC2, DC3)

directly_certified = directly_certified[,c(1, 2)]

directly_certified = rename(directly_certified, c("EligCat" = "Direct_Cert_Type"))

#create new TRUE field to indicate directly certified students
directly_certified$DirectlyCertified=TRUE
#add more descriptive names -- S = "SNAP" (supplemental nutrition assistance program), F = "FOSTER" (foster child)
directly_certified$Direct_Cert_Type = revalue(directly_certified$Direct_Cert_Type, c("S" = "SNAP", "F" = "FOSTER"))


#merge list
studentlist = merge(studentlist, directly_certified, all.x=TRUE)

#fill in "DirectlyCertified" field for students not directly certified
studentlist$DirectlyCertified[is.na(studentlist$DirectlyCertified)] <- "FALSE"

#===================================================================================
write.csv(unique(studentlist), file="studentlist_complete.csv", row.names=FALSE)
write.csv(unique(studentlist[,-c(2,3)]), file="studentlist_public.csv", row.names=FALSE)
write.csv(unique(studentlist[,c(1:3)]), file="UIC_KEY.csv", row.names=FALSE)
#read in other datasets?

