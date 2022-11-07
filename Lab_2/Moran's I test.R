#reading in Hamilton boundary geometry in order to check for auto-correlation of residuals 
hamilton_polygons <- rgdal::readOGR("https://raw.githubusercontent.com/gisUTM/GGR376/master/Lab_1/houseValues.geojson") 

#combining model residuals and model input data into new data frame
model_1_dataframe <- cbind(value_trans, residuals = model_1$residuals)
model_1_dataframe

#joining dataframe to spatial data
hamilton_spdf <- merge(hamilton_polygons, model_1_dataframe, by.x = "CTUID", all.x = FALSE)
spplot(hamilton_spdf, "residuals")

#creating a list of ploygons that share borders
hamilton_nb <- poly2nb(hamilton_spdf).

#Convert Neigbours List to Spatial Weights
hamilton_listw <- nb2listw(hamilton_nb)

#conduct Moran I calculation (Monte-Carlo)

moran.mc(hamilton_spdf$residuals, hamilton_listw, 999)

moran.test(hamilton_spdf$residuals, hamilton_listw)

#spatial lag model 
lag_model = lagsarlm(logValue ~ logIncome,
                     data = hamilton_spdf,
                     listw = hamilton_listw)
summary(lag_model)

#adding residuals from lag model to SpatialPolygonDataFrame
hamilton_spdf$lagResids <- residuals(lag_model)
moran.mc(hamilton_spdf$lagResids, hamilton_listw, 999)

#spatial error model
error_model = errorsarlm(logValue ~ logIncome,
                         data = hamilton_spdf,
                         listw = hamilton_listw)
summary(error_model)

#adding residuals from error model to SpatialPolygonDataFrame
hamilton_spdf$errorResids <- residuals(error_model)
moran.mc(hamilton_spdf$errorResids, hamilton_listw, 999)

#Lagrange Multiplier diagnostics to select either error or lag model 
summary(lm.LMtests(model_1, hamilton_listw, test="all"))

#adding another variable
model_2 <- lm(logValue ~ logIncome + logRoom, data = value_trans)