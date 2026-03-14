import random
import multiprocessing
import os
from multiprocessing import Pool
import time

# Calculate the number of points in the unit circle
# out of n points picked randomly in the unit square
def monte_carlo_dice_part(n):
    
    # Construct a count vector for each of the 11 possibilities
    count = [0] * 11

    for i in range(n):
        dice1=random.randint(1,6)
        dice2=random.randint(1,6)
        total_value = dice1 + dice2

        # Add the counts to the corresponding number
        count[total_value - 2] = count[total_value - 2] + 1
        
    #return
    return count


if __name__=='__main__':
    
    np = multiprocessing.cpu_count()
    print('You have {0:1d} CPUs'.format(np))
    
    # Nummber of points to use for the probability estimation
    n = 1000000
    
    # Iterable with a list of points to generate in each worker
    # each worker process gets n/np number of points to calculate the probabilities from
    part_count=[int(n/np) for i in range(np)]

    # Create the worker pool
    pool = Pool(processes=np)   

    # Parallel map
    count=pool.map(monte_carlo_dice_part, part_count)
    
    # Add the counts for each of the outcomes together
    probabilities = [sum(column) / n for column in zip(*count)]

    # Compare to the true results
    true_probabilities = [1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 5/36, 4/36, 3/36, 2/36, 1/36]

    # Print the results and compare to the true values
    print("Dice  Estimated   True")
    for s, est, tru in zip(range(2, 13), probabilities, true_probabilities):
        print(f"{s:3}  {est:8.4f}  {tru:8.4f}")