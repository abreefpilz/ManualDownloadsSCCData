## script is called by gh action to update manually downloaded data
# saves multiple data files 

#sourceYeah

# Write out the functions with arguments for all the manual download files

# BVR
manual_file_collate(raw_files = "./BVRPlatform", year="2023", just_CCR_EXO = F, outfile= "/current_files/BVRplatform_L1")

# FCR
manual_file_collate(raw_files = "./CR6_Files/Catwalk", year="2023", just_CCR_EXO = F, outfile= "/current_files/FCRCatwalk_L1")

# FCR Met
manual_file_collate(raw_files = "./MetStation", year="2023", just_CCR_EXO = F, outfile= "/current_files/FCRMet_L1")

# Weir
manual_file_collate(raw_files = "./WeirData", year="2023", just_CCR_EXO = F, outfile= "/current_files/WeirData_L1")

# CCR
manual_file_collate(raw_files = "./CCR_manual_downloads/CCR_dam_downloads/Waterquality", year="2023", just_CCR_EXO = F, outfile= "/current_files/CCRWaterquality_L1")
manual_file_collate(raw_files = "./CCR_manual_downloads/CCR_dam_downloads/Metstation", year="2023", just_CCR_EXO = F, outfile= "/current_files/CCRMetstation_L1")
manual_file_collate(raw_files = "./CCR_manual_downloads/CCR_1_5_EXO_downloads", year="2023", just_CCR_EXO = T, outfile= "/current_files/CCR_1_5_EXO_L1")

