###USAGE: python select_tax.py input output
#!/usr/bin/python
import sys
import re
#Creating input and output files
input_file= open (sys.argv[1],'r')
#input_file= open (sys.argv[2],'r')
output_file= open (sys.argv[2], 'w')
#Flag just for printing the header and some variables 
count_line= 1
global tax_to_compare
tax_to_compare="none"
prev_tax_levels="none"
current_tax_levels="none"
#ID = "SampleID"
#output_file.write ('%s' '\t' % (ID))
with open(sys.argv[1]) as input_file:
	for i, line in enumerate(input_file):
		#Print the header as it is
		#output_file.write ('\n')
		if count_line==1:
			count_line=2
			header = line.split()
			for hd in header:
				output_file.write ('%s' '\t'% (hd))
			output_file.write ('\n')
		# Iteration for the other lines/rows
		else: 
			#Split the row per each column, the first one is where the tax info is, the variable other_values 
			#stores the rest of the information/columns
			#output_file.write ('\n')
			row= line.split("\t")
			first_col= row [0]
			other_values = row [1:]
			#Split the first column per taxa level and get hte numer of taxa levels with the function len 
			tax_levels= first_col.split("|")
			number_tax_levels= len(tax_levels) 

			#First condition, when there is only one tax level, then we print the line at it is, this is 
			# useful because the first line contains other metadata with no taxonomical relation.
			#We also create the variable pre_tax_levels that stores the lower taxonomical level from the previous row
			if number_tax_levels ==1:
				output_file.write ('%s'% (first_col))
				for r in other_values:
					output_file.write ('\t' '%s'% (r))
				#output_file.write ('\n')
				prev_tax_levels= tax_levels[0]
			#For the rest of the rows. As python start counting the columns at "0", then we use 
			#the "correct" variables to point correctly to taxonomical levels, "-1" for the lower one in the row 
			#and "-2" for the upper.
			else: 
				correct= number_tax_levels -1
				correct2=number_tax_levels -2
				current_higher_tax_levels=tax_levels[correct2] 
				#Condition: if the upper taxonomical level of the current row is equal to the previous lower level
				#print current_higher_tax_levels, prev_tax_levels
				if current_higher_tax_levels == prev_tax_levels:
					prev_tax_levels=tax_levels[correct]
					current_tax_levels=tax_levels[correct]
					#print current_tax_levels
				#Re-assing the tow variables to compare, previous tax level and the current. 
					#Condition: is the lower taxonomical level is numeric we skip this line if not, then we print it
					if current_tax_levels.isdigit() :
						#print "digit"
						prev_tax_levels=tax_levels[correct2]
						continue
					if not current_tax_levels.isdigit():
						#print "not digit"
						first_tax=tax_levels[0]
						more_tax=tax_levels[1:]
						output_file.write ('%s' % (first_tax))
						for i in more_tax:
							output_file.write ('|' '%s' % (i))
						for r in other_values:
							output_file.write ('\t' '%s' % (r))
						#output_file.write ('\n')
				else:
					prev_tax_levels=tax_levels[correct]
					#Condition: if the levels are diferent, then we want to keep the row, and if the lower tax level is numeric we remove the number (last field)
					if prev_tax_levels.isdigit() :
						tax_levels=tax_levels[:-1]
						first_tax=tax_levels[0]
						more_tax=tax_levels[1:]
						output_file.write ('%s' % (first_tax))
						for i in more_tax:
							output_file.write ('|' '%s' % (i))
						for r in other_values:
							output_file.write ('\t' '%s' % (r))
					else:
						first_tax=tax_levels[0]
						output_file.write ('%s' % (first_tax))
						more_tax=tax_levels[1:]
						for i in more_tax:
							output_file.write ('|' '%s' % (i))
						for r in other_values:
							output_file.write ('\t' '%s' % (r))
					#output_file.write ('\n')