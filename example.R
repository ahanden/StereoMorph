library(StereoMorph)
library(jpeg)

# Before running this script, use the findCorners.py program
# to extract frames and detect checkerboard corners.

local_source_files = "c:\\Users\\Chris\\Documents\\StereoMorph\\R"

for(src.file in list.files(local_source_files, full.names=TRUE, pattern='[.]R$')) {
    print(paste("Loading", src.file))
    source(src.file)
}

calibrateCameras(img.dir='calibration_videos', cal.file='calibration.txt',
corner.dir='Corners', verify.dir='Verify', sq.size='19.75 mm', nx=8,
ny=6, error.dir="Errors", undistort=TRUE, print.progress=T, run.parallel=TRUE)
