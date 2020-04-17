DomoR::init('covid-ce-org','aed97fd2e1df84ad84c6957b7ecd632e9b28ea627f8e86f0')
DomoR::list_ds()

df <- DomoR::fetch('a213b44d-c176-467b-83dd-062fdae8bce7')

head(df)


mobility_median_age_domo<-mobility_median_age%>% select("municipality", "county" = "county.x" , "geoAddress","date", "median_distance_traveled_km", "NAME", "variable", "estimate", "lat" = "X", "lon"="Y" , "age_bin")

mobility_median_age_domo<-mobility_median_age_domo %>%  st_set_geometry(NULL) 

#create
DomoR::create(mobility_median_age_domo, name=" descartes_muni_and_census_block_median_age", description="This data set joins descrate municipality data based on census block centroids from the 2017 ACS with median age")

#overwrite
DomoR::replace_ds('ab84f24a-73d4-0188-d8aa-8fe78103a721', df)


