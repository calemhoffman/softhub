import torch
from .active_learner import BayesianActiveLearner
import pandas as pd

class ExperimentPrioritizer:
    def __init__(self, learner: BayesianActiveLearner):
        self.learner = learner

    def prioritize(self, candidate_X: torch.Tensor, candidate_metadata: pd.DataFrame, best_f: float):
        """
        Given a set of candidate experiments and their metadata, computes the acquisition value
        for each and returns them sorted by priority.
        """
        acq_func = self.learner.get_acquisition_function(best_f)
        
        # Ensure correct shape for botorch (b x q x d)
        if candidate_X.dim() == 1:
            candidate_X = candidate_X.unsqueeze(1).unsqueeze(1)
        elif candidate_X.dim() == 2:
            candidate_X = candidate_X.unsqueeze(1)
            
        with torch.no_grad():
            acq_values = acq_func(candidate_X)
        
        results = candidate_metadata.copy()
        results['priority_score'] = acq_values.numpy()
        return results.sort_values(by='priority_score', ascending=False)
