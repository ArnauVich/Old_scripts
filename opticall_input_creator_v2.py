
#!/bin/python

#####Create Opticall input from GenomeStudio report ##################
#####GS report with following columns: Chr, Name, Position,GTtype,TopAlleles,X, Y
import sys
#Creating input and output files
input_file= open (sys.argv[1],'r')
output_file= open (sys.argv[2], 'w')
#Flag just for printing the header. 
count_line= 1

#Opening and reading file line per line 
with open(sys.argv[1]) as input_file:
    for i, line in enumerate(input_file):
		#Print header. 
		if count_line==1:
			#Change the flag value for the rest of the lines
			count_line=2
			header= line.split("\t")
			header_al="Alleles"
			inten= header[8:]
			snp_id=header[0]
			A=header[1]
			B=header[2]
			allele1=header[3]
			allele2=header[4]
			chro=header[5]
			second_id=header[6]
			pos=header[7]
			output_file.write ('%s''\t''%s' '\t' '%s' '\t' '%s' %(chro,snp_id, pos, header_al))
			end=len(header)
			#For each sample in the header we print 3 times (each column will have a header)
			x=8
			y=12
			#Iterate over the row, subseting each 4 values, changing the 2nd intensity for the 3th and removing the 4th one. 
			while y<=end:
				intensities= header[x:y]
				GT= intensities[0]
				TOP= intensities[1]
				X= intensities[2]
				Y=intensities[3]
				#NN= intensities[3]
				output_file.write ('\t''%s' '\t' '%s' % (X, Y))
				x= y 
				y= x + 4

		else:
			#print count_line
			#output_file.write ('\n')
			header= line.split("\t")
			inten= header[8:]
			snp_id=header[0]
			AB=header[1]
			Alleles=header[2]
			split_al=Alleles.split()
			allele1=split_al[0]
			allele2=split_al[1]
			chro=header[3]
			second_id=header[4]
			pos=header[5]
			output_file.write ('%s' '\t' '%s' '\t' '%s' '\t' '%s''%s' %(chro, snp_id, pos, allele1, allele2))
			end=len(header)
			#For each sample in the header we print 3 times (each column will have a header)
			x=6
			y=10
			#Iterate over the row, subseting each 4 values, changing the 2nd intensity for the 3th and removing the 4th one. 
			while y<=end:
				intensities= header[x:y]
				GT= intensities[0]
				TOP= intensities[1]
				X= intensities[2]
				Y=intensities[3]
				#NN= intensities[3]
				output_file.write ('\t''%s' '\t' '%s' % (X, Y))
				x= y 
				y= x + 4
input_file.close()
output_file.close()