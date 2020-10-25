This source is divided into 4 folders plus other related files. Each dataset with corresponding analysis related files has been kept in a folder. Additionally "Appendix and All R notebooks" folder contains overall meta data files including Appendix and r notebooks nb.html files.

The above has been drilled down below:
1) Folder Kc_house - King County data set:
	-	Contains data set
	-	Contains r notebook folder which includes files such as .Rmd, .nb.html, .docx files.
	-	Contains rscript files and should be check in the following sequence to analyse the content.
			>	Kc_Import_Explore_Clean.R
			>	kc_Normal.R
			>	kc_Transform_log.R
			>	kc_log_bestFit.R
			>	kc_log_manual.R
		Additionally, nb.html file in from above folder can also be used to analyse efficently.
	-	Contains Results_pictures folder which includes few pictures of results of the models.

2) Folder L5_house - Computer generated half million house price observations:
	-	Contains data set
	-	Contains r notebook folder which includes files such as .Rmd, .nb.html, .docx files
	-	Contains only 1 rscript file.
		Additionally, nb.html file in from above folder can also be used to analyse efficently.
	-	Contains Results_pictures folder which includes few pictures of results of the models .

3) Folder Bank:
	-	Contains data set
	-	Contains bank_import_primaryExplore.R script, which is common and used by every model for reading , pre processing and cleaning the data set.
	-	Contains r notebook folder which includes files such as .Rmd, .nb.html, .docx files. One type of file for each model.
	-	Contains folders containf rscript files for each model.
		Additionally, nb.html file in from above folder can also be used to analyse efficently.
	-	Contains Compare_underSampling.R file, which is used to compare the best model of each type and then created and fitted the undersampled training data set.

4) Folder Appendix and All R notebooks:
	-	Contains Appendix
	-	Contains all the .nb.html files of r notebook.

5) Contains presentation sdal19148496-report-dmml1.pptx file used for video presentation.
