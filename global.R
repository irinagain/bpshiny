library(bp)
library(dplyr)
library(ggplot2)
bp_ghana <- bp::bp_ghana
bp_hypnos <- bp::bp_hypnos
bp_jhs <- bp::bp_jhs
bp_preg <- bp::bp_preg
bp_children <- bp::bp_children


hypnos_proc <- process_data(bp_hypnos,
                            bp_type = 'abpm',
                            sbp = "syst",
                            dbp = "DIAST",
                            date_time = "date.time",
                            id = "id",
                            wake = "wake",
                            visit = "visit",
                            hr = "hr",
                            map = "map",
                            rpp = "rpp",
                            pp = "pp",
                            ToD_int = c(5, 13, 18, 23))

jhs_proc <- process_data(bp_jhs,
                         sbp = "Sys.mmHg.",
                         dbp = "Dias.mmHg.",
                         date_time = "DateTime",
                         hr = "pulse.bpm.")

children_proc <- process_data(bp_children, 
                              sbp = 'sbp', dbp = 'dbp',
                              id = 'id', visit = 'visit')

bppreg_proc <- process_data(bp_preg, sbp = 'SBP', dbp = 'DBP',
                            id = 'ID')
bpghana_proc <- process_data(bp_ghana, sbp = 'SBP', dbp = 'DBP', id = 'ID')
