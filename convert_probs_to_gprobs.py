#!/bin/python

#########################################################################
# Creating BeagleCall input from Opticall output                        #
#  Basically modifies the probabilities file                            #
#  From *.probs --> *.gprobs                                            #
#########################################################################

#INPUT: Opticall output, *.probs files. This file contains a header with these levels: SNP, Coor, Alleles, and Sample ID. 
#Each SNP is represented in a row, and for each sample there are four probabilites: P(AA), P(BB), P(AB), P(NN)
#BEAGLECALL format for *.gprobs files: SNP per row --> marker alleleA alleleB 3 probabilities: P(AA), P(AB), P(BB)
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
                        #print count_line
                        header= line.split()
                        samples= header[3:]
                        output_file.write ('marker' '\t' 'alleleA' '\t' 'alleleB' )
                        #For each sample in the header we print 3 times (each column will have a header)
                        for s in samples:
                                #s=s[:s.rfind('_')]
                                s=s[:-1]
                                output_file.write ('\t' '%s'  % (s))
                else :
                        #Print a new line
                        output_file.write ('\n')
                        # Split row
                        marker_info= line.split()
                        #First element is the SNP name
                        snp= marker_info[0]
                        #Get the number of columns in the row
                        end= len(marker_info) 
                        #Third element is the allele (AB), split them and print in 2 columns 
                        alleles= marker_info[2]
                        alleleA= alleles[0]
                        alleleB= alleles[1]
                        output_file.write ('%s' '\t' '%s' '\t' '%s' '\t' % (snp, alleleA, alleleB))
                        #Creating start point and end point for the first and the last intensity of the first sample (4 values). 
                        #The first one is in th 4th column (3th in the list) 
                        x=3
                        y=5
                        #Iterate over the row, subseting each 4 values, changing the 2nd intensity for the 3th and removing the 4th one. 
                        while y<=end:
                                intensities= marker_info[x:y]
                                AA= intensities[0]
                                BB= intensities[1]
                                output_file.write ('\t' '%s' '\t' '%s' '\t' % (AA, BB))
                                x= y 
                                y= x + 2
                #break
input_file.close()
output_file.close()