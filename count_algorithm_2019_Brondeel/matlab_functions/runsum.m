function rs = runsum(data,len,threshold)
% Compute the running sum of values in a vector exceeding some threshold within a range of indices.
% Divides the data into len(data)/length chunks and sums the values in excess of the threshold for each chunk.
% :param data: a 1D numerical vector to calculate the sum of
% :param len: the length of each chunk to compute a sum along, as a positive integer
% :param threshold: a numerical value used to find values exceeding some threshold
% :return: a vector of length len(data)/length containing the excess value sum for each chunk of data

N = size(data);
cnt = ceil (N(1) / len);

rs = zeros(cnt,N(2));

for k=1:N(2)
    for n=1:cnt
        rs(n,k) = 0;
        for p=1+len*(n-1):len*n

            if (p<=N(1) && data(p,k)>=threshold)
                rs(n,k) = rs(n,k) + data(p,k) - threshold;
            end; 
        end
    end
end