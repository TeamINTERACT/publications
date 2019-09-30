#!/bin/bash

make -f Makefile clean
make -f Makefile

# ./Debug/lzEntropy testInput/entropyIn_P2_T600_W1.csv > test.out
# CONFIGURE
dataset="victoria"
dataMode="withDutyCycle"
dataType="gps"
dataDir=/Volumes/Seagate_Rui/dimensionality/data/
#dataDir=/Users/ruizhang/Dropbox/dimensionality_activity_space_0326/src/lz_entropy_rate-master/data
resultDir=./result/
outputFile=$resultDir/$dataset/$dataMode/entropy.csv
fileCount=0
# CONFIGURE

# Need to figure out how to
echo $dataset
for user_file in $( ls "$dataDir/$dataset/$dataType/$dataMode" ); do
	user_file_path=$dataDir/$dataset/$dataType/$dataMode/$user_file
	python preprocess_gps_file.py $user_file_path
	# echo $user_file_path
    entropy=$(./Debug/lzEntropy $user_file_path)
    let "fileCount=fileCount+1"
    # echo $entropy
    echo "$user_file,$entropy"
done
echo $fileCount



