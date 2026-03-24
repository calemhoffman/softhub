import sys
from pathlib import Path

# Add project root to path
sys.path.append(str(Path(__file__).parent.parent))

from engine.simulator import PyNucAstroWrapper
import numpy as np

def run_test():
    print("Initializing PyNucAstro Simulator...")
    # Instantiate with max_A = 10 to keep it very fast just to verify functionality
    wrapper = PyNucAstroWrapper(reaclib_path="data/results03241818", max_A=10)
    
    print("Evaluating thermodynamic trajectory...")
    time_arr, enuc_arr = wrapper.evaluate(T9=1.5, rho=1.e6, t_max=10.0)
    
    print(f"Time Output array Length: {len(time_arr)}")
    print(f"Energy Gen Output array Length: {len(enuc_arr)}")
    
    # Mock observation curve shifted by 0.5s
    observed = np.exp(-(time_arr - 2.5)**2)*1e16 
    fitness = wrapper.calculate_fitness(time_arr, enuc_arr, observed)
    print(f"Fitness Score comparing simulated light curve vs observation: {fitness:.4e}")

if __name__ == "__main__":
    run_test()
