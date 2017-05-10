#Simulate queuing system G/G/1
#@Author: Nguyen Duc Viet
import numpy as np
import simpy


data_wt = []       # waiting time
data_nc = []

def source(env, n, t_int, t_srv, counter):
    """Source generates clients randomly"""
    for i in range(n):
        c = client(env, counter, t_srv[i])
        env.process(c)
        yield env.timeout(t_int[i])


def client(env, counter, t_srv):
    """Client arrives, is served and leaves"""
    t_arr = env.now
    with counter.request() as req:
        yield req
        data_nc.append(1)
        t_wait = env.now - t_arr
        data_wt.append(t_wait)
        yield env.timeout(t_srv)


def single_server(n, t_int, t_srv):
    """Simulates a G/G/1 queue"""
    env = simpy.Environment()
    counter = simpy.Resource(env, capacity=1)
    t_start = env.now               # time of simulation start
    p = env.process(source(env, n, t_int, t_srv, counter))
    env.run(p)
    t_sim = env.now - t_start       # total simulation time
    t_wait = np.array(data_wt)
    del data_wt[:]              # reset list variable containing waiting time
    t_busy = np.sum(t_srv[:n])       # calculate time during server was busy
    t_soj = np.sum(t_wait) + t_busy   # total time spent to be served (sojourn time)

    nc = np.asarray(data_nc)

    # Calculate utilization and average sojourn time
    x = np.sum(nc)/t_sim
    util = t_busy/t_sim
    w = np.mean(t_wait) + np.mean(t_srv)
    l = w*x
    #w = np.mean(t_srv) + np.mean(t_wait)
    return util, w, l


#function for simulating M/M/1
def estimate_mm1(int_rate, srv_rate):
    util = int_rate/srv_rate
    if util > 1:
        util = 1
    w = 1/(srv_rate-int_rate)
    l = int_rate*w
    return util, w, l


#Function for simulating QE/QE/1
def qexp_rate(q, ave):
    rate = 1/(ave*(3-2*q))
    return rate


def ts_qlog(x,q):
    if q==1:
        y=np.log(x)
    else:
        y = (x**(1-q)-1)/(1-q)
    return y


def rand_qexp(N,q,rate):
    q1 = 1/(2-q)
    u = np.random.uniform(0,1,size=(1,N))
    y = -q1*ts_qlog(u,q1)/rate
    return y
