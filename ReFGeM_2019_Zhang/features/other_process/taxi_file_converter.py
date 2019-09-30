import datetime
from functools import reduce
import pandas as pd
from dateutil.parser import parse



def str2float(s):
    if s[0] == '-':
        pos_s = s[1:]
        return -reduce(lambda x, y: x + int2dec(y), map(str2int, pos_s.split('.')))
    else:
        return reduce(lambda x, y: x + int2dec(y), map(str2int, s.split('.')))


def char2num(s):
    return {'0':0,'1':1,'2':2,'3':3,'4':4,'5':5,'6':6,'7':7,'8':8,'9':9}[s]


def str2int(s):
    return reduce(lambda x,y:x*10+y,map(char2num,s))


def intLen(i):
  return len('%d'%i)


def int2dec(i):
  return i/(10**intLen(i))


def convert_taxi_txt_to_csv(input_file, output_file):
    """
    Convert the original taxi gps file to the common format used in this project
    :param input_file:
    :param output_file:
    :return:
    """
    taxi_gps_list = []
    with open(input_file) as in_file:
        for line in in_file.readlines():
            parts = line.split(sep=';')
            user_id = int(parts[0])
            # 2014-02-01 00:00:00.739166+01
            valid_record_time = parts[1][:-3]
            print(parts[1])
            # record_time = datetime.datetime.strptime(valid_record_time, "%Y-%m-%d %H:%M:%S")
            record_time = parse(valid_record_time)
            latlongstr = parts[2]
            # print(latlongstr)
            latlong = latlongstr[6:-2].split(sep=' ')
            lat = str2float(latlong[0])
            long = str2float(latlong[1])
            taxi_gps_list.append([user_id, record_time, lat, long])
            # break

    taxi_gps_df = pd.DataFrame(taxi_gps_list, columns=['user_id','record_time','lat','lon'])
    taxi_gps_df.to_csv(output_file, index=False)


if __name__ == '__main__':
    input_file = "../../data/taxi/taxi_gps.txt"
    output_file = "../../data/taxi/taxi_gps_1.csv"
    convert_taxi_txt_to_csv(input_file, output_file)





