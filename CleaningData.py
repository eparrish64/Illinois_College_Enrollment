# import pandas
import csv
import glob

def toCsv():
	return

def CleanDiversity():

	cleanedData = []
	
	thisCollege = []
	fileList = glob.glob("Data/FallEnrollmentsDiversity/*")
	for thisFile in [fileList[0]]:
		# print thisFile
		with open(thisFile) as csvFile:
			csvReader = csv.reader(csvFile, delimiter=",")


			#  or "institutions" in row[0].lower() or "total enrollment all sectors combined" in row[0].lower() or "grand total" in row[0].lower() or "sector" in row[0].lower()
			for row in csvReader:
				# print(len(row))
				if row[0] == "InstrLevel": continue
				if row[0] == "Grand Total": continue
				if row[0] == "Sector Total": continue
				if row[0] == "Total Enrollment": continue
				if row[0] == " Total Enrollment All Sectors Combined": break
				if row[0] == " Total Enrollment at Out-of-State Institutions": break

				if ("university" in row[0].lower() or "college" in row[0].lower() or "school" in row[0].lower() or "institute" in row[0].lower() or "seminary" in row[0].lower() or "inst." in row[0].lower()) and ("enrollment" not in row[0].lower() or ":" not in row[0] or "total" not in row[0].lower()):
					# print thisCollege
					if len(thisCollege) == 4:
						cleanedData.append([thisCollege[0]]+thisCollege[1])
						cleanedData.append([thisCollege[0]]+thisCollege[2])
						cleanedData.append([thisCollege[0]]+thisCollege[3])
						# print [thisCollege[0]]+thisCollege[1]
					thisCollege = [row[0]]
					# print thisCollege
					continue
				# elif 
				if (len(row) == 25): 
					# if "sector" not in row[0].lower() or "grand" not in row[0].lower():
					# if "grand" not in row[0].lower(): 
					# 	if "sector" not in row[0].lower(): 
							# print(row)
							# enrollInfo = {"Type": 	row[0],
							# 	"InstrLevel" : 		row[1],
							# 	"AfAmF" : 			row[2],
							# 	"AfAmM" : 			row[3],
							# 	"AIANF" : 			row[4],
							# 	"AIANM" : 			row[5],
							# 	"AsianF" : 			row[6],
							# 	"AsianM" : 			row[7],
							# 	"HispF" : 			row[8],
							# 	"HispM" : 			row[9],
							# 	"NHOPIF" : 			row[10],
							# 	"NHOPIM" : 			row[11],
							# 	"WhF" : 			row[12],
							# 	"WhM" : 			row[13],
							# 	"MultF" : 			row[14],
							# 	"MultM" : 			row[15],
							# 	"NRAF" : 			row[16],
							# 	"NRAM" : 			row[17],
							# 	"UnkF" : 			row[18],
							# 	"UnkM" : 			row[19],
							# 	"AllF" : 			row[20],
							# 	"AllM" : 			row[21],
							# 	"GenderNoneTotal" : row[22],
							# 	"AllTotal" : 		row[23],
							# 	"SortNum" : 		row[24],
							# }
							# print enrollInfo
					if row[0].lower() == "undergraduate":
						thisCollege.append(row)
						# print (thisCollege)
					elif row[0].lower() == "graduate":
						thisCollege.append(row)
						# print (thisCollege)
					elif row[0].lower() == "total":
						thisCollege.append(row)
					else:
						continue
							# print(thisCollege)
				# if len(row) == 18: 
				# # 	print(row)
				# print(thisCollege)
				# else:
					# print len(row)
					# enrollInfo = {row[0]: row}
					# enrollInfo = {"Type": 	row[0],
					# 	"InstrLevel" : 		row[1],
					# 	"AfAmF" : 			row[2],
					# 	"AfAmM" : 			row[3],
					# 	"AIANF" : 			row[4],
					# 	"AIANM" : 			row[5],
					# 	"AsianF" : 			row[6],
					# 	"AsianM" : 			row[7],
					# 	"HispF" : 			row[8],
					# 	"HispM" : 			row[9],
					# 	"NHOPIF" : 			row[10],
					# 	"NHOPIM" : 			row[11],
					# 	"WhF" : 			row[12],
					# 	"WhM" : 			row[13],
					# 	"MultF" : 			row[14],
					# 	"MultM" : 			row[15],
					# 	"NRAF" : 			row[16],
					# 	"NRAM" : 			row[17],
					# 	"UnkF" : 			row[18],
					# 	"UnkM" : 			row[19],
					# 	"AllF" : 			row[20],
					# 	"AllM" : 			row[21],
					# 	"GenderNoneTotal" : row[22],
					# 	"AllTotal" : 		row[23],
					# 	"SortNum" : 		row[24],
					# }
					# print enrollInfo
					# continue

	# for i in cleanedData: print i
	with open('CleanedData.csv', 'wb') as myfile:
		wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
		for i in cleanedData:
			wr.writerow(i)

	return


if __name__ == "__main__":
	CleanDiversity()














					# thisCollege = {	row[0] : {	
# "InstrLevel" : [],
# "AfAmF" : [],
# "AfAmM" : [],
# "AIANF" : [],
# "AIANM" : [],
# "AsianF" : [],
# "AsianM" : [],
# "HispF" : [],
# "HispM" : [],
# "NHOPIF" : [],
# "NHOPIM" : [],
# "WhF" : [],
# "WhM" : [],
# "MultF" : [],
# "MultM" : [],
# "NRAF" : [],
# "NRAM" : [],
# "UnkF" : [],
# "UnkM" : [],
# "AllF" : [],
# "AllM" : [],
# "GenderNoneTotal" : [],
# "AllTotal" : [],
# "SortNum" : [],
							# }
					# }
					# thisCollege = {row[0]}