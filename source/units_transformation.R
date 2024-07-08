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
