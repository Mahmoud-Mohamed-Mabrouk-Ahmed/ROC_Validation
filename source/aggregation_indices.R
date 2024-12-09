library(vegan)

#x=matrix(rnorm(10), nrow = 5)
??rnorm

xcor=runif(10,1,100)

ycor=runif(10,1,100)

data=cbind(xcor,ycor)


stomata_arrange<- function(x,image_area){
  
  stomata_num=dim(x)[1]
  PD=spantree(dist(x))$dist/sum(spantree(dist(x))$dist)
  constant=1/( dim(x)[1]-1)
  stomata_evenness=(sum(PD[PD<constant])+(dim(x)[1]-1-length(PD[PD<constant]))*constant-constant)/(1-constant)
  ######
  distance_to_gravity=c()
  for (i in 1:dim(x)[1]){
    distance_to_gravity[i] =dist(rbind(x[i,],colMeans(x)))
  }
  mean_distance=mean(distance_to_gravity)
  
  sum_deviance=sum(distance_to_gravity-mean_distance)
  
  sum_abs_deviance=sum(abs(distance_to_gravity-mean_distance))
  
  stomata_divergence=(sum_deviance+mean_distance)/(sum_abs_deviance+mean_distance)
  
  stomatal_density=dim(x)[1]/image_area
  
  theoretical_distance=1/(2*(stomatal_density^0.5))
  
  nearest_neighbor_distance=c()
  
  for ( ai in 1: dim(x)[1]){
    
    nearest_neighbor_distance[ai]=sort(as.matrix(dist(x))[,ai])[2]
  }
  obseverd_distance=sum(nearest_neighbor_distance)/dim(x)[1]
  stomata_aggregation=obseverd_distance/ theoretical_distance
  
  
  return(cbind(stomata_num,stomatal_density,stomata_evenness,stomata_divergence,stomata_aggregation))
}

# --------------------------------------------------------------------------------------------------
# Image size calculations

pic_width_in_pixels = 2592
pic_height_in_pixels = 1944
pic_size_in_pixels = pic_width_in_pixels * pic_height_in_pixels #5038848

pixel_size_micrometers = 0.4 #(in the pipeline based on the attached photo that used in calibration to convert pixel to micro)

# 1 micrometer = 0.001 millimeter

pic_width_in_mm = (pic_width_in_pixels * pixel_size_micrometers) / 1000
pic_height_in_mm = (pic_height_in_pixels * pixel_size_micrometers) / 1000

pixel_size_millimeters = pixel_size_micrometers / 1000
pixel_size_millimeters  = 4e-04

Picture_Size_mm2 = pic_width_in_mm * pic_height_in_mm

Picture_Size_mm2 

# ---------------------------------------------------------------------------------------------------
# running function trial

# Apply the stomata_arrange function to the data and store the result
results <- as.data.frame(stomata_arrange(data, Picture_Size_mm2))

# View the results
print(results)



