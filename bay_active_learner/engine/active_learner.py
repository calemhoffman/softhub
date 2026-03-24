import torch
from botorch.models import SingleTaskGP
from botorch.fit import fit_gpytorch_mll
from gpytorch.mlls import ExactMarginalLogLikelihood
from botorch.acquisition import ExpectedImprovement
from botorch.optim import optimize_acqf

class BayesianActiveLearner:
    def __init__(self):
        self.model = None
        self.mll = None

    def fit(self, train_X: torch.Tensor, train_Y: torch.Tensor):
        """Fits the Gaussian Process surrogate model."""
        self.model = SingleTaskGP(train_X, train_Y)
        self.mll = ExactMarginalLogLikelihood(self.model.likelihood, self.model)
        fit_gpytorch_mll(self.mll)
        return self.model

    def get_acquisition_function(self, best_f: float):
        """Returns the Expected Improvement acquisition function."""
        if self.model is None:
            raise ValueError("Model must be fitted before getting the acquisition function.")
        return ExpectedImprovement(self.model, best_f=best_f)

    def optimize_acquisition(self, bounds: torch.Tensor, best_f: float, q: int = 1):
        """Optimizes the acquisition function to find the next query point."""
        acq_func = self.get_acquisition_function(best_f)
        candidates, _ = optimize_acqf(
            acq_function=acq_func,
            bounds=bounds,
            q=q,
            num_restarts=5,
            raw_samples=20,
        )
        return candidates
