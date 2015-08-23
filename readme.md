# Getting and cleaning data (data science 3). Course project
This is a course project for the Coursera class [Getting and Cleaning Data] (https://class.coursera.org/getdata-031) which is a part of the [Data Science Specialization] (https://www.coursera.org/specialization/jhudatascience/1?utm_medium=courseDescripTop). This is the third course and its goal is to teach how to obtain data from various sources, clean and manipulate date and create tiny datasets.  
This repository contains file ```CodeBook.md``` where the information about the dataset is given, file ```tidy_dataset``` contains dataset ofbatined from the original results of measurement. The R-script ```project.R``` does the following

- Creates the directory for the project. In created directory the original results of measurement are downloaded and unzipped. All names are checked, to avoid potential problems if file or deirectory already exists.
- Loads the information about activities and features. 
- Loads test and training datasets with features related only to mean of standard deviation.
- Merged both datasets.
- Creates a tidy dataset containing mean value of each variable each subject and activity pair. 

Obtained tiny dataset is saved in file ```tidy_dataset.txt```
