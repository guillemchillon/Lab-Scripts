dir = getDirectory("Choose a Directory");
//ids=newArray(nImages);
for (i=0;i<nImages;i++) {
        selectImage(i+1);
        title = getTitle;
        print(title);

        //Save as "png" or "tif"
        saveAs("png", dir+title);
} 
run("Close All")
