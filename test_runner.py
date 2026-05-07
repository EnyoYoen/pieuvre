#!/usr/bin/env python3

import subprocess
import sys
from pathlib import Path

def run_tests():
    test_path = Path("test")
    
    # Find all .8pus files
    proof_files = sorted(test_path.glob("*.8pus"))
    
    print(f"Running {len(proof_files)} proof tests...\n")

    passed = 0
    failed = 0
    
    for proof_file in proof_files:
        try:
            result = subprocess.run(
                ["dune", "exec", "bin/pieuvre.exe", "--", str(proof_file)],
                capture_output=True,
                text=True,
                timeout=5
            )
            
            if result.returncode == 0:
                print(f"✓ {proof_file.name}")
                passed += 1
            else:
                print(f"✗ {proof_file.name}")
                if result.stderr:
                    print(f"  Error: {result.stderr.strip()}")
                failed += 1
        except subprocess.TimeoutExpired:
            print(f"✗ {proof_file.name} (timeout)")
            failed += 1
        except Exception as e:
            print(f"✗ {proof_file.name} ({e})")
            failed += 1
    
    print(f"\n{passed}/{passed + failed} tests passed")
    return failed == 0

if __name__ == "__main__":
    success = run_tests()
    sys.exit(0 if success else 1)
