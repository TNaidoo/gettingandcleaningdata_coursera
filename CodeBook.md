# Codebook for Tidy Data

The features selected for this dataset come from the accelerometer and gyroscope 3-axial (x, y, and z) raw signals.

  - The time domain signals were captured at a constant rate of 50 Hz. 
  - These were then filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. 
  - The acceleration signal was separated into body and gravity acceleration signals using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
  - The body linear acceleration and angular velocity were derived in time to obtain Jerk signals. 
  - The magnitude of these three-dimensional signals were calculated using the Euclidean norm. 
  - Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing frequency domain signals. 

  Variable                        | Variable Description  
  ----------                      |--------------------------------------------
  Subject                         | Reference to each of the volunteers  
  Activity                        | Activity being performed by the volunteer  
  time_body_acc_mean_x            | The mean of the body x-axis time domain signal from the accelerometer  
  time_body_acc_mean_y            | The mean of the body y-axis time domain signal from the accelerometer  
  time_body_acc_mean_z            | The mean of the body z-axis time domain signal from the accelerometer  
  time_body_acc_std_x             | The standard deviation of the body x-axis time domain signal from the accelerometer  
  time_body_acc_std_y             | The standard deviation of the body y-axis time domain signal from the accelerometer  
  time_body_acc_std_z             | The standard deviation of the body z-axis time domain signal from the accelerometer  
  time_gravity_acc_mean_x         | The mean of the gravity x-axis time domain signal from the accelerometer  
  time_gravity_acc_mean_y         | The mean of the gravity y-axis time domain signal from the accelerometer  
  time_gravity_acc_mean_z         | The mean of the gravity z-axis time domain signal from the accelerometer  
  time_gravity_acc_std_x          | The standard deviation of the gravity x-axis time domain signal from the accelerometer  
  time_gravity_acc_std_y          | The standard deviation of the gravity y-axis time domain signal from the accelerometer  
  time_gravity_acc_std_z          | The standard deviation of the gravity z-axis time domain signal from the accelerometer  
  time_body_acc_jerk_mean_x       | The mean of the body x-axis time domain jerk signal from the accelerometer  
  time_body_acc_jerk_mean_y       | The mean of the body y-axis time domain jerk signal from the accelerometer  
  time_body_acc_jerk_mean_z       | The mean of the body z-axis time domain jerk signal from the accelerometer  
  time_body_acc_jerk_std_x        | The standard deviation of the body x-axis time domain jerk signal from the accelerometer  
  time_body_acc_jerk_std_y        | The standard deviation of the body y-axis time domain jerk signal from the accelerometer  
  time_body_acc_jerk_std_z        | The standard deviation of the body z-axis time domain jerk signal from the accelerometer  
  time_body_gyro_mean_x           | The mean of the body x-axis time domain signal from the gyroscope  
  time_body_gyro_mean_y           | The mean of the body y-axis time domain signal from the gyroscope  
  time_body_gyro_mean_z           | The mean of the body z-axis time domain signal from the gyroscope  
  time_body_gyro_std_x            | The standard deviation of the body x-axis time domain signal from the gyroscope  
  time_body_gyro_std_y            | The standard deviation of the body y-axis time domain signal from the gyroscope  
  time_body_gyro_std_z            | The standard deviation of the body z-axis time domain signal from the gyroscope  
  time_body_gyro_jerk_mean_x      | The mean of the body x-axis time domain jerk signal from the gyroscope  
  time_body_gyro_jerk_mean_y      | The mean of the body y-axis time domain jerk signal from the gyroscope  
  time_body_gyro_jerk_mean_z      | The mean of the body z-axis time domain jerk signal from the gyroscope  
  time_body_gyro_jerk_std_x       | The standard deviation of the body x-axis time domain jerk signal from the gyroscope  
  time_body_gyro_jerk_std_y       | The standard deviation of the body y-axis time domain jerk signal from the gyroscope  
  time_body_gyro_jerk_std_z       | The standard deviation of the body z-axis time domain jerk signal from the gyroscope  
  time_body_acc_mag_mean          | The mean of the body magnitude time domain signal from the accelerometer  
  time_body_acc_mag_std           | The standard deviation of the body magnitude time domain signal from the accelerometer  
  time_gravity_acc_mag_mean       | The mean of the gravity magnitude time domain signal from the accelerometer  
  time_gravity_acc_mag_std        | The standard deviation of the gravity magnitude time domain signal from the accelerometer  
  time_body_acc_jerk_mag_mean     | The mean of the body magnitude time domain jerk signal from the accelerometer  
  time_body_acc_jerk_mag_std      | The standard deviation of the body magnitude time domain jerk signal from the accelerometer  
  time_body_gyro_mag_mean         | The mean of the body magnitude time domain signal from the gyroscope  
  time_body_gyro_mag_std          | The standard deviation of the body magnitude time domain signal from the gyroscope  
  time_body_gyro_jerk_mag_mean    | The mean of the body magnitude time domain jerk signal from the gyroscope  
  time_body_gyro_jerk_mag_std     | The standard deviation of the body magnitude time domain jerk signal from the gyroscope  
  freq_body_acc_mean_x            | The mean of the body x-axis frequency domain signal from the accelerometer  
  freq_body_acc_mean_y            | The mean of the body y-axis frequency domain signal from the accelerometer  
  freq_body_acc_mean_z            | The mean of the body z-axis frequency domain signal from the accelerometer  
  freq_body_acc_std_x             | The standard deviation of the body x-axis frequency domain signal from the accelerometer  
  freq_body_acc_std_y             | The standard deviation of the body y-axis frequency domain signal from the accelerometer  
  freq_body_acc_std_z             | The standard deviation of the body z-axis frequency domain signal from the accelerometer  
  freq_body_acc_jerk_mean_x       | The mean of the body x-axis frequency domain jerk signal from the accelerometer  
  freq_body_acc_jerk_mean_y       | The mean of the body y-axis frequency domain jerk signal from the accelerometer  
  freq_body_acc_jerk_mean_z       | The mean of the body z-axis frequency domain jerk signal from the accelerometer  
  freq_body_acc_jerk_std_x        | The standard deviation of the body x-axis frequency domain jerk signal from the accelerometer  
  freq_body_acc_jerk_std_y        | The standard deviation of the body y-axis frequency domain jerk signal from the accelerometer  
  freq_body_acc_jerk_std_z        | The standard deviation of the body z-axis frequency domain jerk signal from the accelerometer  
  freq_body_gyro_mean_x           | The mean of the body x-axis frequency domain signal from the gyroscope  
  freq_body_gyro_mean_y           | The mean of the body y-axis frequency domain signal from the gyroscope  
  freq_body_gyro_mean_z           | The mean of the body z-axis frequency domain signal from the gyroscope  
  freq_body_gyro_std_x            | The standard deviation of the body x-axis frequency domain signal from the gyroscope  
  freq_body_gyro_std_y            | The standard deviation of the body y-axis frequency domain signal from the gyroscope  
  freq_body_gyro_std_z            | The standard deviation of the body z-axis frequency domain signal from the gyroscope  
  freq_body_acc_mag_mean          | The mean of the body magnitude frequency domain signal from the accelerometer  
  freq_body_acc_mag_std           | The standard deviation of the body magnitude frequency domain signal from the accelerometer  
  freq_bodybody_acc_jerk_mag_mean | The mean of the body magnitude frequency domain jerk signal from the accelerometer  
  freq_bodybody_acc_jerk_mag_std  | The standard deviation of the body magnitude frequency domain jerk signal from the accelerometer  
  freq_bodybody_gyro_mag_mean     | The mean of the body magnitude frequency domain signal from the gyroscope  
  freq_bodybody_gyro_mag_std      | The standard deviation of the body magnitude frequency domain signal from the gyroscope  
  freq_bodybody_gyro_jerk_mag_mean| The mean of the body magnitude frequency domain jerk signal from the gyroscope  
  freq_bodybody_gyro_jerk_mag_std | The standard deviation of the body magnitude frequency domain jerk signal from the gyroscope  
