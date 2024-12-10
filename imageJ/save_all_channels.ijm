inputDir = getDirectory("Input Directory");
outputDir = getDirectory("Output Directory");

list = getFileList(inputDir);


//Loop over each file in the directory

for (i = 0; i < list.length; i++) {
    filename = list[i];
    open(inputDir + filename);
    fileNoExtension = File.nameWithoutExtension;
    
    // Split channels
	run("Split Channels");
	
	// Get the titles of all open images
    titles = getList("image.titles");
    // Assuming the split channels are the last three images opened
    c1name = titles[titles.length - 3];
    c2name = titles[titles.length - 2];
    c3name = titles[titles.length - 1];

    // Merge the channels back together
    run("Merge Channels...", "c1=[" + c1name + "] c2=[" + c2name + "] c3=[" + c3name + "] create");
	
	
	// Rename file
    rename(fileNoExtension);

    // Split the channels again
    run("Split Channels");

	for (j=0;j<nImages;j++) {
	        selectImage(j+1);
	        title = getTitle;
	        print(title);
	
	        //Save as "png" or "tif"
	        saveAs("png", outputDir+title);
	}
	run("Close All");
} 

