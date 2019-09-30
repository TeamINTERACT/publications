%---------------------
% Script with article: 
% (Title here)
% Corresponding author Ruben Brondeel (ruben.brondeel@gmail.com)

% This script calculates the Matlab version of activity counts, 
% by executing the Matlab function agfilt (see script 'agfilt.m')
% The agfilt function was developed in : 
% Brond JC, Andersen LB, Arvidsson D. Generating ActiGraph Counts from 
% Raw Acceleration Recorded by an Alternative Monitor. Med Sci Sports Exerc. 2017.

% The data files were .csv format. There was one file per participant. 

% Matlab version 2017b
%---------------------

addpath('your_directory\matlab_functions')

% folder from which to read in files (raw accelerometer files)
folderInn = strcat(path, 'raw_accelerometer_data/');  

% folder where second level count data will be saved
folderOut = strcat(path, 'count_sec_matlab/');

% list of files
csvfiles = char(ls([folderInn, '\*.csv']));

% sample frequency (typically between 30hz and 100hz)
filesf = 50;

% load the coefficients for the agfilt function
load('agcoefficients.mat');

% for loop to run agfilt function for each file (typically 1 file per participant)
for i = 1:size(csvfiles,1)
    fileInn = fullfile(folderInn, csvfiles(i,:));
    acc = csvread(fileInn, 1, 0);
    
    count_sec = agfilt(acc, filesf, B, A); 
    
    fileOut = fullfile(folderOut, csvfiles(i,:));
    csvwrite(fileOut, count_sec)
end




