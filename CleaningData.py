# import pandas
import csv
import glob

def ListOfPersistentInstitutes():
	nums = {}
	fileList = glob.glob("Data/FallEnrollmentsDiversity/*")
	for thisFile in fileList:
		year = thisFile.split("_")[1].split(".")[0]
		with open(thisFile) as csvFile:
			csvReader = csv.reader(csvFile,delimiter=",")
			for row in csvReader:
				if row[0] == "InstrLevel": continue
				if row[0] == "Grand Total": continue
				if row[0] == "Sector Total": continue
				# if row[0] == "Total Enrollment": continue
				# if row[0] == " Total Enrollment All Sectors Combined": break
				# if row[0] == " Total Enrollment at Out-of-State Institutions": break
				# if row[0] == " Total Enrollment at Independent NFP Institutions:": continue
				# if row[0] == " Total Enrollment at Independent For-Profit Institutions:": continue
				# if row[0] == " Independent For-Profit Institutions::": continue
				# if row[0] == " Total Enrollment at Public Universities:": continue
				if row[0][0] == " ": continue
				if row[0] == " Out-of-State Institutions::": continue
				if row[0] == " Public Universities::": continue
				if "total enrollment" in row[0].lower(): continue
				if row[0].lower() == "undergraduate": continue
				if row[0].lower() == "graduate": continue
				if row[0].lower() == "total":	continue
				if row[0] not in nums.keys():
					nums[row[0]] = 1
				else:
					nums[row[0]]+=1
	# for i in nums:
	# 	if nums[i] == 21:
	# 		print "%s: \n\t%s" %(i,nums[i])
	# 	else: continue
	# print len(nums)
	return nums

def CleanDiversity():
	InstNames = ListOfPersistentInstitutes()
	cleanedData = []
	thisCollege = []
	fileList = glob.glob("Data/FallEnrollmentsDiversity/*")
	for thisFile in fileList:
		year = thisFile.split("_")[1].split(".")[0]
		with open(thisFile) as csvFile:
			csvReader = csv.reader(csvFile, delimiter=",")
			for row in csvReader:
				if row[0] == "InstrLevel": continue
				if row[0] == "Grand Total": continue
				if row[0] == "Sector Total": continue
				if row[0] == "Total Enrollment": continue
				if row[0] == " Total Enrollment All Sectors Combined": break
				if row[0] == " Total Enrollment at Out-of-State Institutions": break
				if row[0] in InstNames:
					# if ("university" in row[0].lower() or "college" in row[0].lower() or "school" in row[0].lower() or "institute" in row[0].lower() or "seminary" in row[0].lower() or "inst." in row[0].lower()) and ("enrollment" not in row[0].lower() or ":" not in row[0] or "total" not in row[0].lower()):
					if len(thisCollege) == 4:
						cleanedData.append([thisCollege[0]]+thisCollege[1])
						cleanedData.append([thisCollege[0]]+thisCollege[2])
						cleanedData.append([thisCollege[0]]+thisCollege[3])
					thisCollege = [row[0]]
					continue
				if (len(row) == 25): 
					row.append(year)
					if row[0].lower() == "undergraduate":
						thisCollege.append(row)
					elif row[0].lower() == "graduate":
						thisCollege.append(row)
					elif row[0].lower() == "total":
						thisCollege.append(row)
					else:
						continue

	with open('CleanedData.csv', 'wb') as myfile:
		wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
		wr.writerow(["Institute","InstrLevel","AfAmF","AfAmM","AIANF","AIANM","AsianF","AsianM","HispF","HispM","NHOPIF","NHOPIM","WhF","WhM","MultF","MultM","NRAF","NRAM","UnkF","UnkM","AllF","AllM","GenderNoneTotal","AllTotal","SortNum","empty","Year"])
		for i in cleanedData:
			wr.writerow(i)
	return


if __name__ == "__main__":
	CleanDiversity()
	# ListOfPersistentInstitutes()