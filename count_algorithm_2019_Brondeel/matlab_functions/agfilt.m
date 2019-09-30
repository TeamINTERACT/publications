
function counts = agfilt(data,filesf,B,A)

% Get activity counts for a set of accelerometer observations.
% First resamples the data frequency to 30Hz, then applies a Butterworth filter to the signal, then filters by the
% coefficient matrices, saturates and truncates the result, and applies a running sum to get the final counts.
% :param data: the vertical axis of accelerometer readings, as a vector
% :param filesf: the number of observations per second in the file
% :param a: coefficient matrix for filtering the signal
% :param b: coefficient matrix for filtering the signal
% :return: a vector containing the final counts

deadband = 0.068;
sf = 30;
peakThreshold = 2.13;
adcResolution = 0.0164;
integN = 10;
gain = 0.965;

if (filesf>sf)
    dataf = resample(data,sf,filesf);

[B2,A2] = butter(4,[0.01 7]./(sf/2));
dataf = filtfilt(B2,A2,dataf);

S = size(dataf);

B = B * gain;

for n=1:S(2)
    
    fx8up = filter(B,A,dataf(:,n));
    
    fx8 = pptrunc(downsample(fx8up,3),peakThreshold);
    
    counts(:,n) = runsum(floor(trunc(abs(fx8),deadband)./adcResolution),integN,0);
    
end
    