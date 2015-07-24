
# Creating a tidy dataset

+ ## Introduction  
	The run_analysis function takes a set of raw data based on human activity recognition using smartphones. The experiment was carried out with 30 volunteers between the ages of 19 and 48 years old. Each volunteer performed six activities whilst wearing a smartphone: walking, walking upstairs, walking downstairs, sitting, standing, and laying). The data has been supplied courtesy of:  
	Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.  
	Smartlab - Non Linear Complex Systems Laboratory  
	DITEN - Università degli Studi di Genova.  
	Via Opera Pia 11A, I-16145, Genoa, Italy.  
	activityrecognition@smartlab.ws  
	www.smartlab.ws
	
+ ## Requirements  
	The following libraries are required in order to run the function provided:
	- dplyr
	- reshape2: This is required for the melt and dcast functions

	The following data files should be located in the working directory:
	- X.test.txt
	- x_train.txt
	- y_test.txt
	- y_train.txt
	- subject_test.txt
	- subject_train.txt
	- features.txt
	- activity_labels.txt

+ ## References  
	[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
	
+ ## Procedure followed in the function  
	- The raw data is read in
	- All relevant data is merged into a single dataset
	- Labels are allocated to the columns; the latter of which was provided in the features file.
	- The mean and standard deviation for each variable was extracted into a new dataset. Note: In this case only the mean() and std() results were extracted according to the description of the variables provided in the features_info file.
	- Column labels were formatted to be more meaningful
	- A set of tidy data was produced containing the average of each variable (as specified in the Codebook) for each subject and each activity.
	- The tidy dataset is finally extracted and stored in the working directory.
