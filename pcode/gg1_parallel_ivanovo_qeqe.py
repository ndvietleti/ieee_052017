import numpy as np
import pandas as pd
import glob
from gg1_function import single_server, estimate_mm1, rand_qexp, qexp_rate
import matplotlib as mplt
from tqdm import trange
from joblib import Parallel, delayed
import os

def kingman_estimate(tau, tserv):
    avtau = np.mean(tau)
    avtserv = np.mean(tserv)
    lam = 1/avtau
    mu = 1/avtserv
    ca = np.std(tau)/avtau
    cs = np.std(tserv)/avtserv
    p = lam/mu
    y = (p/(1-p))*0.5*(ca**2 + cs**2)*avtserv
    return y


def processInput(i, n, time, ssize, c):
    return single_server(n, time, ssize/c[i])


if __name__ == '__main__':    
    path = "../data_ses/src_ses/D"
    data = []
    timep = []
    sizep = []
    sur_size2 = []
    fdate = ["20170131", "20170201", "20170202", "20170203", "20170204",
             "20170205", "20170206", "20170207", "20170208", "20170209",
             "20170210", "20170211", "20170212", "20170213", "20170214"]

    qvec = [1.1, 1.09, 1.11, 1.09, 1.13,
            1.12, 1.09, 1.13, 1.1, 1.1,
            1.09, 1.11, 1.1,1.11, 1.11]
    l = [1.18, 1.17, 1.2, 1.16,1.24,
         1.19, 1.16, 1.19, 1.12, 1.17,
         1.18, 1.2,1.17, 1.17, 1.16]
    for j in trange(len(fdate), desc="Progress"):
        allfile = sorted(glob.glob(path + fdate[j]+"/*.txt"))
        df = []
        timep = []
        sizep = []
        sur_size2 = []
        for i in range(len(allfile)):
            df = pd.read_csv(allfile[i], header=None, index_col=None, delim_whitespace=True, na_filter=True, usecols=[0,4])
            df = df.dropna()
            df.columns = ['t', 's']
            timep1 = np.asarray(df['t'])
            timep1 = np.insert(np.diff(timep1), 0, 0.0)
            sizep1 = np.asarray(df['s'])

            sur_size12 = np.random.permutation(sizep1)
            timep = np.append(timep, timep1)
            sizep = np.append(sizep, sizep1)
            sur_size2 = np.append(sur_size2, sur_size12)

        n = 1000
        print n

        timep = timep / np.mean(timep)
        sizep = sizep / np.mean(sizep)
        sur_size2 = sur_size2 / np.mean(sur_size2)

        t_rate = qexp_rate(qvec[j], 1)
        sur_time = rand_qexp(n, qvec[j], t_rate)
        sur_time = np.asarray(sur_time).flatten()
        print sur_time

        sur_s = rand_qexp(n, 1.51, 9.69)
        sur_s = np.asarray(sur_s).flatten()
        ind = np.asarray(np.where(sur_s < 0)).flatten()
        print max(sur_s)
        print "v1"

        c = np.logspace(np.log10(1.1), np.log10(10), 30)  # Values of throughput
        inputs = range(len(c))
        scores = 6

        res_real = Parallel(n_jobs=scores)(delayed(processInput)(i, n, timep, sizep, c) for i in inputs)
        res_real = pd.DataFrame(data=res_real, index=c, columns=['Ur', 'Wr', 'Lr'])
        util_real = res_real['Ur']
        print "v2"
        res_mm1 = []
        for i in range(len(c)):  # Queueing system MM1
            res_mm1.append(estimate_mm1(1 / np.mean(timep), c[i] / np.mean(sizep)))
        res_mm1 = pd.DataFrame(data=res_mm1, index=c, columns=['Um', 'Wm', 'Lm'])
        util_mm1 = res_mm1['Um']

        print "v3"

        res_sur = Parallel(n_jobs=scores)(delayed(processInput)(i, n, sur_time, sur_s, c) for i in inputs)
        res_sur = pd.DataFrame(data=res_sur, index=c, columns=['Us', 'Ws', 'Ls'])
        util_sur = res_sur['Us']
        print "v4"
        res_k = []
        for i in range(len(c)):
            res_k.append(kingman_estimate(timep, sizep / c[i]) + np.mean(sizep) / c[i])
        res_k = pd.DataFrame(data=res_k, index=c, columns=['Ws'])
        util_k = np.mean(sizep) / (np.mean(timep) * c)

        out = {'col1': util_real, 'col2': res_real['Wr'], 'col3': util_sur, 'col4': res_sur['Ws'],
               'col5': util_mm1, 'col6': res_mm1['Wm'], 'col7': util_k, 'col8': res_k['Ws']}
        outd = pd.DataFrame(data=out)
        outpath = "../ws/"
        os.chdir(outpath)
        outfile = "wsq_" + fdate[j] + ".txt"
        outd.to_csv(outfile, sep='\t', header=False, index=False)

        #   mplt.use('Qt5Agg')
        import matplotlib.pyplot as plt

        plt.figure()
        plt.plot(util_real, res_real['Wr'], 'ro', markersize=15, fillstyle="none", label="Empirical")
        plt.plot(util_sur, res_sur['Ws'], 'bs', markersize=15, fillstyle="none", label="QE/QE/1")
        plt.plot(util_mm1, res_mm1['Wm'], 'k-', linewidth=3, fillstyle="none", label="M/M/1")
        plt.plot(util_k, res_k['Ws'], 'm--', linewidth=3, label="Kingman")

        cur_date = fdate[j]
        plt.title(cur_date[6:8] + "/" + cur_date[4:6] + "/" + cur_date[:4], fontsize=30)
        plt.xlabel('U', size=30, fontstyle='italic')
        plt.ylabel('$\overline{W}$', size=30, fontstyle='italic')
        plt.axis([0.08, 1.2, 0.08, 120000])
        plt.yscale('log')
        plt.xscale('log')
        ax = plt.gca()
        ax.set_xticks([0.1, 0.3, 0.5, 0.7, 0.9])
        ax.get_xaxis().set_major_formatter(mplt.ticker.ScalarFormatter())
        ax.legend().set_visible(False)
        plt.xticks(color='black', size=30)
        plt.yticks(color='black', size=30)
        plt.minorticks_off()
        plt.legend(loc="upper left")
        fig = mplt.pyplot.gcf()
        fig.set_size_inches(12, 12)
        fig_file = "fig6q_" + str(j + 1) + ".eps"
        fig.savefig(fig_file)











