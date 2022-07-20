# SEE DEFINITION AND NOMENCLATURE AT THE BOTTOM
# function dNi_dt = modelCOVID(t, stN, options, IntP, EpiP)

newModelCOVID <- function(t, stN, allParameters){
  with(as.list(c(stN, allParameters)), {
    
  GenP <- allParameters[[1]]
  IntP <- allParameters[[2]] 
  EpiP <- allParameters[[3]] 
  
  # Calls rate calculator function.
  r = ratesCOVID(stN, allParameters)
          
  # Population balance equations (Vectorial per age group)
  dNhn_dt = rep(0, times = length(EpiP$fhn_t)); 
  dNh_dt  =-(r$ri_ps + r$ri_s);
  dNps_dt = (r$ri_ps + r$ri_s) - r$rs_ps  - r$rr_ps;
  dNs_dt  = r$rs_ps  - r$rsh_s  - r$rr_s;
  dNsh_dt = r$rsh_s  - r$rsc_sh - r$rr_sh;
  dNsc_dt = r$rsc_sh - r$rd_sc  - r$rr_sc;
  dNd_dt  = r$rd_sc;
  dNr_dt  = r$rr_ps + r$rr_s + r$rr_sh + r$rr_sc;
  
  dNi_dt <- c(dNhn_dt, dNh_dt, dNps_dt, dNs_dt, dNsh_dt,  dNsc_dt, dNd_dt,  dNr_dt)     
  # Function returning states derivatives to the solver.
  return(list(dNi_dt))    #Check the need to transpose

  })
}



#  NOMENCLATURE AND DEFINITIONS
# Name	Units	Definitions
# tFinal days	Length of simulation
# Nh	#	Number of healthy
# Nps	#	Number of pre-symptomatic
# Ns	#	Number of symptomatic
# Nsh	#	Number of symptomatic hospitalised
# Nsc	#	Number of symptomatic critical
# Nd	#	Number of dead
# Nr	#	Number of recovered & immune

# pi_ps	# inf / # int-AS	Prob of infection per int with AS
# pi_s	# inf / # int-S	Prob of infection per int with S
# ni	# int /# H.d	Number of interindividual interactions per day
#
# IFR	#D/#AS	Infectio fatality ratio
# fs_ps	#S/#AS	Fraction of AS that will become S
# fsh_s	#SH/#S	Fraction of S that will become SH
# fsc_sh	#SC/#SH	Fraction of SH that will become SC
# fd_sc	#D/#SC	Fraction of SC that will die D
#
# fr_ps	#R/#AS	Fraction of AS that will recover R
# fr_s	#R/#S	Fraction of S that will recover R
# fr_sh	#R/#SH	Fraction of SH that will recover R
# fr_sc	#R/#SC	Fraction of SC that will recover R
#
# ts_ps	days	Time to develop symptoms
# tsh_s	days	Time to become hospitalised
# tsc_sh days	Time to become critical
# td_sc	days	Time to die from critical
# tr_ps	days	Time to recover without symptoms
# tr_s	days	Time to recover from mild symptoms
# tr_sh	days	Time to recover from hospitalisation
# tr_sc	days	Time to recover critical
