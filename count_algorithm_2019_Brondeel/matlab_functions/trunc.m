function [ outd ] = trunc( data, min_value )

% Truncate a vector such that any value lower than min_value is set to 0.
% :param data: a vector of any dimension containing numerical data
% :param min_value: a float value the elements of data should not fall below
% :return: the truncated vector

outd = data;
    
I = find(data(:,1)<min_value);
        
outd(I,1) = 0;


    
    