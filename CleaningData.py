# import pandas
import csv
import glob

def toCsv():
	return

def CleanDiversity():
	cleanedData = []
	thisCollege = []
	fileList = glob.glob("Data/FallEnrollmentsDiversity/*")
	for thisFile in fileList:
		with open(thisFile) as csvFile:
			csvReader = csv.reader(csvFile, delimiter=",")
			for row in csvReader:
				if row[0] == "InstrLevel": continue
				if row[0] == "Grand Total": continue
				if row[0] == "Sector Total": continue
				if row[0] == "Total Enrollment": continue
				if row[0] == " Total Enrollment All Sectors Combined": break
				if row[0] == " Total Enrollment at Out-of-State Institutions": break

				if ("university" in row[0].lower() or "college" in row[0].lower() or "school" in row[0].lower() or "institute" in row[0].lower() or "seminary" in row[0].lower() or "inst." in row[0].lower()) and ("enrollment" not in row[0].lower() or ":" not in row[0] or "total" not in row[0].lower()):
					if len(thisCollege) == 4:
						cleanedData.append([thisCollege[0]]+thisCollege[1])
						cleanedData.append([thisCollege[0]]+thisCollege[2])
						cleanedData.append([thisCollege[0]]+thisCollege[3])
					thisCollege = [row[0]]
					continue
				if (len(row) == 25): 
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
		wr.writerow(["Insititue","InstrLevel","AfAmF","AfAmM","AIANF","AIANM","AsianF","AsianM","HispF","HispM","NHOPIF","NHOPIM","WhF","WhM","MultF","MultM","NRAF","NRAM","UnkF","UnkM","AllF","AllM","GenderNoneTotal","AllTotal","SortNum"])
		for i in cleanedData:
			wr.writerow(i)

	return


if __name__ == "__main__":
	CleanDiversity()