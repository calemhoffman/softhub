import pandas as pd
import numpy as np
from pathlib import Path

class ReaclibDataLoader:
    def __init__(self, db_path: str = "data/results03241818"):
        self.db_path = Path(db_path)
    
    def load_data(self) -> pd.DataFrame:
        """
        Parses the JINA REACLIB formatted flat file.
        """
        if not self.db_path.exists():
            print(f"Warning: {self.db_path} not found. Returning empty DataFrame.")
            return pd.DataFrame()
        
        records = []
        with open(self.db_path, "r") as f:
            lines = f.readlines()
            
        i = 0
        while i < len(lines):
            line = lines[i].rstrip()
            if len(line) <= 2 and line.strip().isdigit():
                # Chapter line
                chapter = int(line.strip())
                i += 1
                if i >= len(lines): break
                
                header_line = lines[i]
                # Nuclei are 5 chars each (up to 6)
                n1 = header_line[5:10].strip()
                n2 = header_line[10:15].strip()
                n3 = header_line[15:20].strip()
                n4 = header_line[20:25].strip()
                n5 = header_line[25:30].strip()
                n6 = header_line[30:35].strip()
                label = header_line[43:47].strip()
                q_value_str = header_line[52:64].strip()
                q_value = float(q_value_str) if q_value_str else 0.0
                
                i += 1
                p1_line = lines[i]
                a0, a1, a2, a3 = [float(p1_line[j:j+13]) for j in range(0, 52, 13)]
                
                i += 1
                p2_line = lines[i]
                a4, a5, a6 = [float(p2_line[j:j+13]) for j in range(0, 39, 13)]
                
                records.append({
                    "chapter": chapter,
                    "n1": n1, "n2": n2, "n3": n3, "n4": n4, "n5": n5, "n6": n6,
                    "label": label, "q_value": q_value,
                    "a0": a0, "a1": a1, "a2": a2, "a3": a3, "a4": a4, "a5": a5, "a6": a6
                })
            i += 1
            
        df = pd.DataFrame(records)
        return df

    def preprocess(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Preprocesses the nuclear reaction data for the engine.
        Extracts physical features Z (protons), A (mass), N (neutrons) from the n1 nucleus.
        """
        # A simple periodic table index mapping for fast Z lookups
        ptable = [
            "dummy", "h", "he", "li", "be", "b", "c", "n", "o", "f", "ne", "na", "mg", "al", "si", "p", "s", "cl", "ar", "k", "ca",
            "sc", "ti", "v", "cr", "mn", "fe", "co", "ni", "cu", "zn", "ga", "ge", "as", "se", "br", "kr", "rb", "sr", "y", "zr",
            "nb", "mo", "tc", "ru", "rh", "pd", "ag", "cd", "in", "sn", "sb", "te", "i", "xe", "cs", "ba", "la", "ce", "pr", "nd",
            "pm", "sm", "eu", "gd", "tb", "dy", "ho", "er", "tm", "yb", "lu", "hf", "ta", "w", "re", "os", "ir", "pt", "au", "hg",
            "tl", "pb", "bi", "po", "at", "rn", "fr", "ra", "ac", "th", "pa", "u", "np", "pu", "am", "cm", "bk", "cf", "es", "fm"
        ]
        
        def parse_isotope(nuc_str: str):
            nuc = nuc_str.strip().lower()
            if not nuc:
                return 0, 0, 0
            
            # Special particles
            if nuc == 'n': return 0, 1, 1
            if nuc == 'p': return 1, 1, 0
            if nuc == 'd': return 1, 2, 1
            if nuc == 't': return 1, 3, 2
            
            # Extract letters and numbers
            import re
            match = re.match(r"([a-z]+)(\d+)?(.*)", nuc)
            if not match:
                return 0, 0, 0
                
            symbol = match.group(1)
            mass_str = match.group(2)
            
            # Handle JINA specific things like "al-6" or "al*6" for isomers
            if symbol == 'al' and not mass_str:
                return 13, 26, 13
                
            try:
                z = ptable.index(symbol)
            except ValueError:
                return 0, 0, 0
                
            a = int(mass_str) if mass_str else 0
            n = a - z if a > 0 else 0
            return z, a, n

        # Apply parsing to target nucleus (n1)
        # Note: we can parse n2, n3 as well, but for simplicity we feature off the primary reactant.
        parsed = df['n1'].apply(parse_isotope)
        df['z1'] = [p[0] for p in parsed]
        df['a1'] = [p[1] for p in parsed]
        df['n_neutrons1'] = [p[2] for p in parsed]
        
        return df
