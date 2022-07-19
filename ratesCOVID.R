ratesCOVID <- function(stN, allParameters){
  
  #Load all different parameters here (remember numbers from runCOVID)
  GenP = allParameters[[1]];
  IntP = allParameters[[2]];
  EpiP = allParameters[[3]];
  
  # Number of age groups.
  nAG = length(EpiP$fhn_t)
  
  # Naming states and vectors management.
  Nhn = stN[seq(1,nAG,1)];            Nh  = stN[seq(1*nAG+1, 2*nAG, 1)];
  Nps = stN[seq(2*nAG+1, 3*nAG, 1)];  Ns  = stN[seq(3*nAG+1, 4*nAG, 1)];
  Nsh	= stN[seq(4*nAG+1, 5*nAG, 1)];  Nsc = stN[seq(5*nAG+1, 6*nAG, 1)];
  Nd	= stN[seq(6*nAG+1, 7*nAG, 1)];  Nr  = stN[seq(7*nAG+1, 8*nAG, 1)];
  Nsc_nc = rep(0, nAG);               Nsc_ic = rep(0, nAG);
  
  # Totals per stage independent of age group.
  NhnT = sum(Nhn);  NhT  = sum(Nh);
  NpsT = sum(Nps);  NsT  = sum(Ns);
  NshT = sum(Nsh);  NscT = sum(Nsc);
  NdT  = sum(Nd);   NrT  = sum(Nr);
  
  # Total population.

    Nt = sum(sum(stN));
  
  
  # ALGEBRAICS required for rates computations.
  # Fraction of interactions with AS and S among the total interactions
  f_ips = IntP[["rfi_ps"]]* NpsT / sum(NhnT + NhT + sum(IntP$rfi_ps * NpsT) + sum(IntP$rfi_s * NsT) + NrT)
  f_is  = IntP[["rfi_s"]] * NsT  / sum(NhnT + NhT + sum(IntP$rfi_ps * NpsT) + sum(IntP$rfi_s * NsT) + NrT)
  
  # Weighted averages of lpa over age groups.
  lpa_ps_av = sum(Nps * IntP[["lpa_ps"]]) / NpsT
  if (NpsT == 0) {
    lpa_ps_av = 0;
  }
  
  lpa_s_av  = sum(Ns  * IntP[["lpa_s"]]) / NsT
  if (NsT == 0)  {
    lpa_s_av = 0
  }
  
  # Probability of infection per interaction is function of personal protection and awarenes.
  pi_ps = (1 - IntP[["lpa_h"]]) * (1 - lpa_ps_av)
  pi_s  = (1 - IntP[["lpa_h"]]) * (1 - lpa_s_av)
  
  
  # # TEMP STOPPING CONFINEMENT
  # if t>IntP.tIstop,
  #     IntP.ni_h(:) = 15;
  # end
  
  # INFECTION AND TRANSITION RATES
  # Infection rates (all vectorial per age group).
  ri_ps = pi_ps * f_ips * IntP$ni_h * Nh  # Rate of infection of H by AS for each age range.
  ri_s  = pi_s  * f_is  * IntP$ni_h * Nh  # Rate of infection of H by S  for each age range.
  
  # Transition rates (all vectorial per age group).
  rs_ps  = EpiP$fs_ps  * Nps / EpiP$ts_ps    # Rate of transition from H  to AS for each age range.
  rsh_s  = EpiP$fsh_s  * Ns  / EpiP$tsh_s    # Rate of transition from AS to S  for each age range.
  rsc_sh = EpiP$fsc_sh * Nsh / EpiP$tsc_sh   # Rate of transition from SH to SC for each age range.
  
  # Function for critical care units allocation.
  capICt = GenP$capICpM * 1e-6 * Nt        # Total number of critical care units available.
  
  # Total shortage of IC units to take away from Nsc_ic.
  Nsc_ncT= max(0, NscT - capICt);
  
  # Counter of shortage of IC units pending to take away from Nsc_ic.
  left = Nsc_ncT
  
  # Loop of transfer of Nsc_ic to Nsc_nc starting from older to younger.
  for (i in 1:nAG) {
    if (left > 0) {
      Nsc_nc[nAG + 1 - i] = min(Nsc[nAG + 1 - i], left)
      left = max(left - Nsc[nAG + 1 - i], 0)
    }
  }
  
  # Number of individuals in IC per age group is the Nsc minus those with no IC available.
  Nsc_ic = Nsc - Nsc_nc
  
  # Death rate is that of critical in care plus that of critical with no care.
  rd_scic = EpiP$fd_sc * Nsc_ic / EpiP$td_sc;
  rd_scnc = Nsc_nc / EpiP$td_nc;
  rd_sc   = rd_scic + rd_scnc;
  
  # Recovery rates (Vectorial per age group)
  # Rate of recovery from AS for each age range.
  rr_ps = EpiP$fr_ps * Nps    / EpiP$tr_ps
  # Rate of recovery from S  for each age range.
  rr_s  = EpiP$fr_s  * Ns     / EpiP$tr_s
  # Rate of recovery from SH for each age range.
  rr_sh = EpiP$fr_sh * Nsh    / EpiP$tr_sh
  # Rate of recovery from SC for each age range.
  rr_sc = EpiP$fr_sc * Nsc_ic / EpiP$tr_sc
  
  # Age group weighted average rates of ingection by PS and S
  ri_psT = sum(ri_ps * Nps) / NpsT;
  ri_sT  = sum(ri_s  * Ns)  / NsT;
  
  # Computation of the reproductive number (R0)
  R0 = sum( (ri_psT/ NpsT)*(EpiP$tr_ps * EpiP$fr_ps             +  EpiP$ts_ps * EpiP$fs_ps            ) + 
            (ri_sT / NsT) *(EpiP$tr_s  * EpiP$fs_ps * EpiP$fr_s +  EpiP$tsh_s * EpiP$fs_ps * EpiP$fsh_s) );
  
  r = list(ri_ps = ri_ps,  ri_s = ri_s, rs_ps = rs_ps, rsh_s = rsh_s, rsc_sh = rsc_sh, 
           rd_sc = rd_sc, rr_ps = rr_ps, rr_s = rr_s,  rr_sh = rr_sh, rr_sc  = rr_sc, R0 = R0)
  return(r) 
  }