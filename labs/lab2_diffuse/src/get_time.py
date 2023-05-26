###################################################################
# 
#  Used to aggraegate all the *.time file outputs for easy
#  LaTeX copy and paste.
#
#    07/15/2021
#    Miko Stulajter
#
##################################################################

import os
import math
f_out = open("timing_results.txt", "w")
files=[]
for filename in os.listdir(os.getcwd()+'/results'):
    files.append(filename)
files.sort()
for filename in files:
    for filename2 in os.listdir(os.getcwd()+"/results/"+filename):
        if filename2.endswith(".time"):
            file1 = open('results/'+filename+"/"+filename2,'r')
            Lines = file1.readlines()
            f_out.write("Results for "+filename.replace("test_","")+'\n')
            f_out.write("\n    real        user        sys \n")
            for line in Lines:
                if line.find('m')>=0:
                    time=line.replace("real", "").replace("sys", "").replace("user", "").replace("m"," ").replace("s"," ").strip()
                    time=time.split()
                    time=int(time[0])*60+float(time[1])
                    f_out.write("   {:.2f}  ".format(time))
    f_out.write("\n \n")
    f_out.write("--------------------------------------------------------\n")


f_out.close()


