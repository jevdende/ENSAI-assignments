import numpy as np
from pathlib import Path
import sys
from PIL import Image
import multiprocessing
import os

def work(red, green, blue, result, start, end):
    result[start:end] = 0.299 * red + 0.587 * green + 0.114 * blue


if __name__=='__main__':

    # file to use
    if len(sys.argv) < 2:
        print("Usage: python greyscale_seq.py <filename>")
        exit(0)

    filename = sys.argv[1]

    # Loading image
    img = np.array(Image.open(filename))
    
    # Conversion from one component into 1D
    red = img[:, :, 0]
    green = img[:, :, 1]
    blue = img[:, :, 2]
    
    red_1d = red.flatten()
    green_1d = green.flatten()
    blue_1d = blue.flatten()

    # Set process count
    no_processes = 12

    # Initialize the splits for the different processes
    no_pixels = red.size
    pixel_split = np.linspace(0, no_pixels, no_processes + 1, dtype=int)
    
    # Initialize the processes
    processes = []
    result = multiprocessing.Array('f', int(no_pixels))
    for i in range(no_processes):
        start = pixel_split[i]
        end = pixel_split[i+1]
        
        processes.append(multiprocessing.Process(target = work, args=(red_1d[start:end], green_1d[start:end], 
                                                                      blue_1d[start:end], result, start, end)))

    # Start the processes    
    for p in processes:
        p.start()

    for p in processes:
        p.join()
    
    # Reshape the end result
    luma_2d = np.reshape(result, (img.shape[0], img.shape[1]))
    luma_2d = np.uint8(luma_2d)

    # Generating image
    basename = Path(filename).stem

    # Save image
    Image.fromarray(luma_2d).save(basename + "_shared.jpg")
