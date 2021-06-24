bp_ghana <- bp::bp_ghana
bp_hypnos <- bp::bp_hypnos
bp_jhs <- bp::bp_jhs

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