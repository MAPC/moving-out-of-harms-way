
source("libraries.R")

permit_path<- "C:/Users/zlacovino/OneDrive - Metropolitan Area Planning Council/Documents - Moving People Out of Harm's Way/GIS data/2019-2024 Permits_Winchester/"

months<-c("January", "February", "March", "April", "May",
       "June", "July", "August", "September", "October",
       "November", "December")


years<-c("2019", "2020", "2021", "2022", "2023")






year_compilation<-NULL

all_years<-NULL





for(x in 1:5){ #years
  
  #months
  for(y in 1:12){
    year_compilation[[y]]<- read.xlsx(paste0(permit_path, years[x], " Permits/", months[y],".xlsx"))%>%
      janitor::clean_names()%>%
      mutate(source = paste(months[y], years[x]))%>%
      mutate(addressT = ifelse(
        grepl("Winchester", address), address, paste(address, "Winchester, MA 01890")))%>%
      geocode(address = addressT, method = "arcgis")
    
    print(paste0(permit_path, years[x], " Permits/", months[y],".xlsx"))
  }
  
  all_years[[x]]<-rbindlist(year_compilation)
}

full_data<-rbindlist(all_years)%>%
  filter(!grepl("add", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Air", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("A/C", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Attic", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Basement", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Beam", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Backyard", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("bath", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("chimney", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("closet", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Door", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("dormer", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Deck", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Fence", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Finish", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Finsh", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Fit", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Flooring", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Kitch", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Install", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Insula", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Inter", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Porch", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Pool", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Remod", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Remove", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("reno", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Repair", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("replac", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("roof", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Tent", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Sign", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Siding", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Solar", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Step", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Skylight", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Subfloor", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("wall", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Water", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("weather", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Work", description_of_work, ignore.case = TRUE))%>%
  filter(!grepl("Window", description_of_work, ignore.case = TRUE))
  
  
  
write.csv(full_data, "winchester_bp_clean.csv")  

unique(full_data$source) 

full_data<-read_csv("winchester_bp_clean.csv")

full_data_sf<-full_data%>%
  filter(!is.na(long)| !is.na(lat))%>%
  st_as_sf(., coords= c("long", "lat"), crs = lat_lon_CRS)%>%
  st_transform(mass_mainland)

ggplot(full_data_sf)+
  geom_sf()


arc.write("K:/DataServices/Projects/Current_Projects/Environment/Moving_Out_of_Harms_Way/Project_Files/Default.gdb/buildingpermits", full_data_sf)
