library(flopr)
setwd('/home/ada/ymartinez/Plate_Reader/growth_study/20230302_HB101_pGLO_OD600_GFP_induction_10h_C1')

flopr::process_plate(data_csv = "20230302_pGLO_OD600_GFP_10h_C1_parsed.csv",
                     blank_well = c("H1", "H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12"),
                     neg_well = c("G1", "G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12"),
                     od_name = "OD600",
                     flu_names = "GFP",
                     af_model = "spline",
                     to_MEFL = TRUE,
                     flu_gains = 100,
                     conversion_factors_csv = "/home/ada/ymartinez/Plate_Reader/calibration/20230301/definitiva/calibration_membrane_parsed_cfs.csv")

