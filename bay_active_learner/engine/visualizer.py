import matplotlib.pyplot as plt
import numpy as np

class ModelVisualizer:
    def plot_zn_space(self, train_X, train_Y, candidate_X, prioritizer_scores, filename="zn_acquisition_space.png"):
        plt.figure(figsize=(10, 6))
        
        # Plot candidates colored by their Priority Score (Expected Improvement)
        # Assuming training features are [Z, N] 
        # Z is index 0, N is index 1
        sc = plt.scatter(candidate_X[:, 1].numpy(), candidate_X[:, 0].numpy(), 
                         c=prioritizer_scores, cmap='viridis', s=60, alpha=0.8,
                         label='Candidate Experiments')
        plt.colorbar(sc, label='Expected Improvement (Priority Score)')
        
        # Plot existing measured training data
        plt.scatter(train_X[:, 1].numpy(), train_X[:, 0].numpy(), 
                    c='red', marker='X', s=120, linewidth=2, label='Current Observations')
                    
        plt.xlabel('Neutron Number (N)')
        plt.ylabel('Proton Number (Z)')
        plt.title('Active Learning Acquisition Space (Z vs N)')
        plt.legend(loc='lower right')
        plt.grid(True, alpha=0.3)
        
        plt.savefig(filename, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"Saved Z-N space visualization to {filename}")
