import numpy as np
from pathlib import Path
import sys
from PIL import Image
import multiprocessing
import os

def work(red, green, blue):
    # Draw random numbers and normalise to sum to 1
    w = np.random.rand(3)
    w = w / sum(w)

    # Construct the image
    image = w[0] * red + w[1] * green + w[2] * blue
    image = np.uint8(image)

    # Save the file
    process_id = os.getpid()
    Image.fromarray(image).save(f"cat_random{process_id}.jpg")


if __name__=='__main__':

    # file to use
    if len(sys.argv) < 2:
        print("Usage: python greyscale_seq.py <filename>")
        exit(0)

    filename = sys.argv[1]

    # Loading image
    img = np.array(Image.open(filename))

    # Getting each component level
    red = img[:, :, 0]
    green = img[:, :, 1]
    blue = img[:, :, 2]

    # Set process count
    no_processes = 10

    # Initialize the processes
    processes = []
    for i in range(no_processes):
        processes.append(multiprocessing.Process(target = work, args=(red, green, blue)))
    
    for p in processes:
        p.start()

    for p in processes:
        p.join()

    print("Finished with the parallel image generation!")
