#!/usr/bin/env python3

# Baud rate calculator for 16550 UART

import sys

BASE_FREQ = 12_000_000 if len(sys.argv) < 2 else float(sys.argv[1])
#BASE_FREQ = 12_587_500
PERCENT_ERROR_THRESHOLD = 3.0

STANDARD_RATES = [
        230400,
        115200,
        57600,
        38400,
        28800,
        19200,
        14400,
        9600,
        7200,
        4800,
        3600,
        2400,
        1800,
        1200
]

def percent_error(actual, expected):
    return abs((actual-expected)/expected)*100

print(f'Baud rate divisors for base frequency of {BASE_FREQ/1_000_000:.6f} MHz:')
for rate in STANDARD_RATES:
    divisor = BASE_FREQ/(16*rate)
    divisor_rounded = round(divisor)
    actual = BASE_FREQ/divisor_rounded/16
    pct_err = percent_error(actual, rate)
    label = '✅' if pct_err <= PERCENT_ERROR_THRESHOLD else '❌'
    print(f'{label} {rate:6} baud: divisor={divisor_rounded:<5} (actual={actual:.3f} baud, error={pct_err:.2f}%)')


