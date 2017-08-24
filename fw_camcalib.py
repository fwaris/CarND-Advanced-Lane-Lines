from cv2 import *
import numpy as np
# import cv2
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import glob

folder = 'D:/Repos/CarND-Advanced-Lane-Lines/'
#img = mpimg.imread(folder + 'camera_cal/calibration2.jpg')
imgs = glob.glob(folder + 'camera_cal/calibration*.jpg' )

objp = np.zeros((9*6,3), np.float32)
objp[:,:2] = np.mgrid[0:9,0:6].T.reshape(-1,2)

objpoints = []
imgpoints = []
pattern = (9,6)

for fname in imgs:
    fname = imgs[0]
    img = cv2.imread(fname)
    plt.imshow(img)
    gray = cv2.cvtColor(img,cv2.COLOR_BGR2GRAY)
    plt.imshow(gray)
    ret, crnrs = cv2.findChessboardCorners(gray,pattern,None)
    if ret == True:
        imgpoints.append(crnrs)
        objpoints.append(objp)
    else:
        raise Exception('unable to read calibration image')
    

img.shape
g = cv2.cvtColor(img,cv2.COLOR_BGR2GRAY)
plt.imshow(g)
ret, crnrs = cv2.findChessboardCorners(g,(9,6),None)
if ret == True:
        imgpoints.append(crnrs)
        objpoints.append(objp)
        img = cv2.drawChessboardCorners(img,(9,6),crnrs,ret)
        img = img[...,::-1] #brg to rgn
        plt.imshow(img)

