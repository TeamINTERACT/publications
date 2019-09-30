

function [ outd ] = pptrunc( data, max_value)

% Saturate a vector such that no element's absolute value exceeds max_value.
%   :param data: a vector of any dimension containing numerical data
%   :param max_value: a float value of the absolute value to not exceed
%   :return: the saturated vector

outd = data;

N = length(data(1,:));

for n=1:N
    I = find(data(:,n) > max_value);
    outd(I,n) = max_value;
    I = find(data(:,n) < -max_value);
    outd(I,n) = -max_value;
end

