#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2015 by Araik Tamazian

import argparse
import pandas as pd
import numpy as np
import progressbar as pbar
import timeit
import datetime




def parse_args():
    """
    Parse command-line arguments.
    """
    parser = argparse.ArgumentParser('Extract user sessions from log.')
    parser.add_argument('input', help='an input file')
    parser.add_argument('output', help='an output file')
    parser.add_argument('-p', '--protocol', default='6', type=str,
                        help='extract sessions with a specified protocol')
    parser.add_argument('-t', '--maxtime', default=0.1, type=float,
                        help='largest session time allowed')
    parser.add_argument('-n', '--ipnum', default=0, type=int,
                       help='number of destination IPs for which sessions will be extracted')
    return parser.parse_args()

def extract_session(input_file, output_file, protocol, maxtime, ipnum):
    """
    Given handles of input and output files, read the connection
    statistics from the input file and output the connections grouped
    by their destination to the output handle.

    :param input_file: an input handle where connection statistics
        are read from
    :param output_file: an output handle where grouped statistics
        are written to
    :param protocol: a protocol which connections are to be considered
    :type protocol: str
    :return: the tuple of two numbers: the number of connection
        records processed and the number of grouped records written
        to the specified output file
    :rtype: tuple
    """

    # Read data from log
    print('Reading data from file ...')
    data = pd.read_csv(input_file, sep='\t', header=None,
                names=['ptime', 'ipdst', 'ipsrc', 'proto', 'psize'],
                dtype={'ptime': float, 'ipdst': str, 'ipsrc': str, 'proto': str, 'psize': int},
                compression='gzip')


    data = data[data.proto == protocol]
    data = data[np.isfinite(data['psize'])]
    print('Number of read rows in data: %d'%len(data))
    print('Ranking source by destination IPs ...')
    all_ipsrc= pd.Series.value_counts(data.ipsrc)
    #all_ipsrc= all_ipsrc[all_ipsrc > 1]

    if ipnum==0:
        ipnum=len(all_ipsrc)
    if ipnum > len(all_ipsrc):
        ipnum = len(all_ipsrc)
    all_ipsrc = all_ipsrc.index[0:ipnum]



    print('Extracting user sessions:')
    bar = pbar.ProgressBar()
    for i in bar(range(ipnum)):
        data1 = data[data.ipsrc == all_ipsrc[i]]
        data1 = data1.set_index(np.arange(len(data1.ptime)))
        time = np.asarray(data1.ptime)

        dtime = np.diff(time)
        dtime = np.insert(dtime,0,0.0)

        stime = time[dtime > maxtime]
        stime = np.insert(stime,0,time[0])

        ncon2 = np.ones(len(time), dtype=np.int64)
        size = np.array(data1.psize)

        ind = np.asarray(np.where(dtime > maxtime)).flatten()

        if len(ind)>0:
            ssize1 = np.array_split(size, ind)
            ssize = [np.sum(a) for a in ssize1]

            ncon1 = np.array_split(ncon2,ind)
            ncon = [np.sum(a) for a in ncon1]

            time1 = np.array_split(time, ind)
            time_duration = [np.subtract(x[len(x) - 1], x[0]) for x in time1]

        else:
            ssize = size.sum()
            ncon = ncon2.sum()
            time_duration = np.subtract(time[len(time)-1],time[0])

        stime = pd.Series(stime, name='stime', dtype=np.float64)
        time_duration = pd.Series(time_duration, name="time_duration", dtype=np.float64)
        sip1 = np.repeat(all_ipsrc[i], len(stime))
        sip = pd.Series(sip1, name='ipsrc', dtype=str)
        ncons = pd.Series(ncon, name='ncon', dtype=np.int64)
        ssize = pd.Series(ssize, name='ssize', dtype=np.int64)

        if (i == 0):
            out = pd.concat([stime, time_duration, sip, ncons, ssize], axis=1)
        else:
            out1 = pd.concat([stime, time_duration, sip, ncons, ssize], axis=1)
            out = out.append(out1)



    # Sort sessions by arrival time
    out = out.sort_values(by='stime')

    # Write sessions to file
    print('Writing results to file ...')
    out[['stime']] = out[['stime']].astype(np.float64)
    out[['stime']] = pd.Series(["{0:.6f}".format(val) for val in out['stime']], index=out.index)
    out[['time_duration']] = out[['time_duration']].astype(np.float64)
    out[['time_duration']] = pd.Series(["{0:.6f}".format(val) for val in out['time_duration']], index=out.index)
    out[['ipsrc']] = pd.Series(["{:15}".format(val) for val in out['ipsrc']], index=out.index)
    out[['ncon']] = pd.Series(["{:6.0f}".format(val) for val in out['ncon']], index=out.index)
    out[['ssize']] = pd.Series(["{:9.0f}".format(val) for val in out['ssize']], index=out.index)
    out.to_csv(output_file, header=False, index=False, sep='\t')

    
if __name__ == '__main__':
    start = timeit.default_timer()
    args = parse_args()
    with open(args.input) as input_file:
        with open(args.output, 'w') as output_file:
            extract_session(input_file, output_file, args.protocol, args.maxtime, args.ipnum)
    stop = timeit.default_timer()
    print('run time: %0.3f' % float(stop - start))
