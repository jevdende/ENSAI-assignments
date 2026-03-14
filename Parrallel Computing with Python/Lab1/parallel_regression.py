import numpy as np
from multiprocessing import Pool, cpu_count

# generate synthetic data
def generate_data(n_samples=10000):
    # Generate n_samples of observations from a Uniform(0, 10) distribution
    X = np.random.rand(n_samples,1) * 10

    # Compute the dependent variable y as a function of X, a constant and 
    # noise that is distributed according to a Normal(0,1) distribution.
    y = 3.5 * X.squeeze() + 7 + np.random.randn(n_samples)  # y = 3.5x + 7 + noise

    return X, y

# calculate gradient on a data chunk
def compute_gradient(data_chunk):
    # Extract the X, y and current weights from the data chunk
    X_chunk, y_chunk, weights = data_chunk

    # Calculate the amount of observations in our data chunk
    n = len(X_chunk)

    # Construct the predictions based on the current weights for y.
    # This will be used for the calculation of the gradient.
    predictions = X_chunk @ weights

    # Calculate the errors of our prediction.
    errors = predictions - y_chunk.reshape(-1, 1)

    # Calculate the gradient.
    gradient = (2 / n) * X_chunk.T @ errors

    return gradient

# training model with multiprocessing
def train_linear_regression(X, y, epochs=100, lr=0.01):
    # Add a constant to the covariates (the X matrix). 
    X_b = np.hstack([X, np.ones((X.shape[0], 1))])
    # Initialise the weights, i.e the parameters we want to estimate, to 0.
    weights = np.zeros((X_b.shape[1], 1))
    # See the amount of available cores on the computer
    n_cores = cpu_count()
    # Initialise the Pool object to have n_cores different processes that all will run parallel
    pool = Pool(processes=n_cores)

    # Loop over the epochs, i.e. the amount of times we are going to let the model train on our data
    for epoch in range(epochs):
        # Prepare data chunks for each separate process
        chunks = np.array_split(range(len(X_b)), n_cores)
        data_chunks = [(X_b[i], y[i], weights) for i in chunks]
        
        # Use the previously defined pool function to calculate the gradients in parallel. 
        # Here for each data chunk X we map it to a list that contains the corresponding gradient.
        gradients = pool.map(compute_gradient, data_chunks)

        # Average the gradients over the different data chunks to obtain the final gradient.
        mean_gradient = sum(gradients) / len(gradients)

        # Update the weights by subtracting the learning rate times the gradient, which is part of the
        # procedure of gradient descent.
        weights -= lr * mean_gradient

        # Every 10 epochs, print the current epoch and the loss values
        if epoch % 10 == 0 or epoch == epochs - 1:
            loss = np.mean((X_b @ weights - y.reshape(-1, 1))**2)
            print(f"Epoch {epoch}: Loss = {loss:.4f}")

    # Close the pool, meaning we can no longer submit tasks to it.
    pool.close()
    # Wait untill all tasks are completed before finishing the code
    pool.join()
    return weights


if __name__ == '__main__':
    # Initialise our data
    X, y = generate_data()
    # Train the weights according to gradient descent
    weights = train_linear_regression(X, y, epochs=1000, lr=0.01)
    print("\nLearned weights :", weights.ravel())
