import torch
import sys
from pathlib import Path

# Add project root to path
sys.path.append(str(Path(__file__).parent.parent))

from engine.active_learner import BayesianActiveLearner
from botorch.test_functions import Branin

def run_synthetic_test():
    print("Testing BayesianActiveLearner with Branin function...")
    
    # Branin function domain: [-5, 10] x [0, 15]
    branin = Branin()
    bounds = torch.tensor([[-5.0, 0.0], [10.0, 15.0]])

    # Generate initial random training data (5 points)
    train_X = bounds[0] + (bounds[1] - bounds[0]) * torch.rand(5, 2)
    # We want to maximize the negative Branin function
    train_Y = -branin(train_X).unsqueeze(-1)

    learner = BayesianActiveLearner()
    print("Fitting initial model...")
    learner.fit(train_X, train_Y)

    best_f = train_Y.max().item()
    print(f"Initial best value: {best_f:.4f}")

    print("Optimizing acquisition function for next candidate...")
    candidates = learner.optimize_acquisition(bounds, best_f, q=1)
    
    print(f"Suggested next candidate experiment: {candidates.numpy()}")
    
    new_Y = -branin(candidates).unsqueeze(-1)
    print(f"Observation at new candidate: {new_Y.item():.4f}")

if __name__ == "__main__":
    run_synthetic_test()
