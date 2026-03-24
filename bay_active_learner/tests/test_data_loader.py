import sys
from pathlib import Path

# Add project root to path
sys.path.append(str(Path(__file__).parent.parent))

from data.data_loader import ReaclibDataLoader

def test_parser():
    loader = ReaclibDataLoader("data/results03241818")
    print("Loading data...")
    df = loader.load_data()
    print(f"Loaded {len(df)} records.")
    if not df.empty:
        print("Sample Data:")
        print(df.head())

if __name__ == "__main__":
    test_parser()
