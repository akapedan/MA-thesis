#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
# This code was written by Arbian Halilaj for the Master's Thesis at the
# University of St. Gallen.
# Supervisor: Prof. Jana Mareckova
# For questions contact arbian.halilaj@student.unisg.ch
"""

# MONTE-CARLO SIMULATION

import random
import numpy as np
import pandas as pd
import math
import numpy.random as nrand
from econml.dml import CausalForestDML
import matplotlib.pyplot as plt

random.seed(20212022)

# Set parameters
n_sim = 1000 # Number of simulations
dim_x = 10 # Number of Confounders
n = 1000 # Number of Observations
m = 100 # Number of Issuers
z = 1 # Dependency Factor (Higher = More Dependent)

#CIR Process parameters
all_time = 1000 # Amount of time to simulate
all_delta = 1/252 # Delta, the rate of time e.g. 1/252 = daily, 1/12 = monthly
all_sigma = 0.3 # Volatility
gbm_mu = 0.2 # Annual drift factor for geometric brownian motion
cir_a = 0.5 # Rate of mean reversion
cir_mu = 2 # Long run average interest rate
all_r0 = 1 # Starting interest rate value

def brownian_motion_log_returns(all_time, all_delta, all_sigma):
    
    sqrt_delta_sigma = math.sqrt(all_delta) * all_sigma
    return nrand.normal(loc=0, scale=sqrt_delta_sigma, size=all_time)


def geometric_brownian_motion_log_returns(all_time, all_delta, 
                                          all_sigma, gbm_mu):
    
    wiener_process = np.array(brownian_motion_log_returns(all_time, all_delta, 
                                                          all_sigma))
    sigma_pow_mu_delta = (gbm_mu - 0.5 * math.pow(all_sigma, 2.0)) * all_delta
    return wiener_process + sigma_pow_mu_delta

def cox_ingersoll_ross_levels(all_time, all_delta, all_sigma, cir_a=0.0, 
                              cir_mu=0.0, all_r0=0.0):
    
    brownian_motion = brownian_motion_log_returns(all_time, all_delta, 
                                                  all_sigma)
    
    # Setup the parameters for interest rates
    a, mu, zero = cir_a, cir_mu, all_r0

    # Assumes output is in levels
    levels = [zero]
    for i in range(1, all_time):
        drift = a * (mu - levels[i-1]) * all_delta
        randomness = math.sqrt(levels[i - 1]) * brownian_motion[i - 1]
        levels.append(levels[i - 1] + drift + randomness)
    return np.array(levels)

def issuer_effect(n,m,z):
    
    issuer_list = list(range(1, m + 1))
    issuer = np.random.choice(issuer_list, size=n, replace=True)
    issuer = pd.DataFrame(issuer)
    df = issuer.sort_values(by=0)
    
    dfs=pd.DataFrame()
    for i in range(1, m+1):
        df_i = issuer.loc[issuer.iloc[:,0]==i,:]
        len_df_i = len(df_i)
        bonus = z * np.random.uniform(0, 1) / 100
        bonus_list = [bonus] * len_df_i
        dfs = dfs.append(bonus_list, ignore_index = True)
        
    df.reset_index(inplace=True)
    df[1] = dfs[0]
    df = df.drop('index', axis=1)
    
    return df

def dgp1(dim_x, n, m, z, all_time, all_delta, all_sigma, cir_a=0.0, 
         cir_mu=0.0, all_r0=0.0):
    
    x = np.random.normal(0,1, size=(n,dim_x)) / 100
    betas = np.array(list(range(1, dim_x + 1))) / (dim_x)
    g = (x @ betas) 
    d = np.random.binomial(1, 0.2, n)

    date = list(range(0,801))
    df2 = pd.DataFrame(date)
    interest_rate = cox_ingersoll_ross_levels(all_time = all_time, 
                                              all_delta = all_delta, 
                                              all_sigma = all_sigma, 
                                              cir_a = cir_a, cir_mu = cir_mu, 
                                              all_r0 = all_r0)
    interest_rates = pd.DataFrame(interest_rate/100)
    df2[1] = interest_rates[0]
    random_date = np.random.choice(date, size=n, replace=True)
    random_date = pd.DataFrame(random_date)
    df3 = pd.merge(df2, random_date, on=0)
    effect = issuer_effect(n,m,z)
    df4 = effect.sample(frac=1).reset_index(drop=True)
    df4[2] = df3[1]
    #df4[3] = df3[0]
    df4.columns = ['Issuer', 'Issuer Effect', 'Interest Rate']
    df5 = df4.set_index('Issuer')
    df_final = np.sum(df5, axis=1).tolist()
    df_final = pd.DataFrame(df_final)    
    
    y0 = np.random.choice(df_final[0], size=n, replace=False)
    y1 = 0.1 + g + np.random.choice(df_final[0], size=n, replace=False)
    y = d * y1 + (1-d) * y0

    return (x, d, y)

def dgp2(dim_x, n, all_time, all_delta, all_sigma, cir_a=0.0, 
         cir_mu=0.0, all_r0=0.0):
    
    x = np.random.normal(0,1, size=(n,dim_x)) / 100
    betas = np.array(list(range(1, dim_x + 1))) / (dim_x)
    g = (x @ betas) 
    d = np.random.binomial(1, 0.2, n)

    interest_rate = cox_ingersoll_ross_levels(all_time = all_time, 
                                              all_delta = all_delta, 
                                              all_sigma = all_sigma, 
                                              cir_a = cir_a, cir_mu = cir_mu, 
                                              all_r0 = all_r0)
    interest_rates = pd.DataFrame(interest_rate/100)
    
    y0 = np.random.choice(interest_rates[0], size=n, replace=False)
    y1 = 0.1 + g + np.random.choice(interest_rates[0], size=n, replace=False)
    y = d * y1 + (1-d) * y0

    return (x, d, y)

def cf(x,y,d):

    est = CausalForestDML(discrete_treatment=True)
    est.fit(Y=y, T=d, X=x, W=None)
    effect = est.effect(x)
    ate = np.mean(effect)
    return(ate)

def simulation(n_sim, n, dim_x, m, z, all_time, all_delta, all_sigma, 
               cir_a=0.0, cir_mu=0.0, all_r0=0.0):
    
    all_results = np.empty( (n_sim, 1, 2) )  # initialize for results

    # Loop through many simulations
    for i in range(n_sim):
        # Run DGP1
        x, d, y = dgp1(dim_x, n, m, z, all_time, all_delta, all_sigma, 
                       cir_a=0.0, cir_mu=0.0, all_r0=0.0)
        all_results[i,0,0] = cf(x,y,d)

        # Run DGP2
        x, d, y = dgp2(dim_x, n, all_time, all_delta, all_sigma, 
                       cir_a=0.0, cir_mu=0.0, all_r0=0.0)
        all_results[i,0,1] = cf(x,y,d)
        
    return all_results

results = simulation(n_sim, n, dim_x, m, z, all_time, all_delta, 
                     all_sigma, cir_a=0.0, cir_mu=0.0, all_r0=0.0)

def plot_results(results,truth):

    plt.figure()
    plt.hist(x=results[:,0,0], bins='auto', 
             color='red',alpha=0.5,label="CF (no i.i.d )")
    plt.hist(x=results[:,0,1], bins='auto', 
             color='blue',alpha=0.5,label="CF (i.i.d")
    plt.axvline(x=truth,label="truth")
    plt.legend(loc='upper right')
    #plt.title('DGP' + str(dgp))
    plt.show()
    
plot_results(results,0.1) 