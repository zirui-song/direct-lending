#!/usr/bin/env python3
"""
5d_MatchedPanelLoans.py

Create matched pairs of nonbank and bank loans using:
- Exact matching on Fama-French 12 industry (derived from SIC)
- Exact matching on facility_type
- Propensity score matching (without replacement) based on:
  * clean_interest_spread (bps)
  * firm size: log(at)
  * leverage: (dltt + dlc)/at, or lt/at as fallback
  * ebitda

Inputs:
- ../Data/Intermediate/5c_PanelAllLoans.csv (built in 5c)

Output:
- ../Data/Intermediate/5d_MatchedPanelLoans.csv (one row per matched pair)

Notes:
- Drops observations missing required matching variables
- Estimates propensity score model using logistic regression
- Propensity score matching without replacement within caliper
"""

import pandas as pd
import numpy as np
from pathlib import Path
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler


def get_fama_french_12_industry(sic_code):
    """Convert SIC code to Fama-French 12 industry classification (short codes)."""
    if pd.isna(sic_code) or sic_code == '' or sic_code == 0:
        return 'Other'
    try:
        sic = int(sic_code)
    except Exception:
        return 'Other'

    # 1 NoDur - Consumer Nondurables
    if (100 <= sic <= 999 or 2000 <= sic <= 2399 or 2700 <= sic <= 2749 or 
        2770 <= sic <= 2799 or 3100 <= sic <= 3199 or 3940 <= sic <= 3989):
        return 'NoDur'
    # 2 Durbl - Consumer Durables
    if (2500 <= sic <= 2519 or 2590 <= sic <= 2599 or 3630 <= sic <= 3659 or 
        3710 <= sic <= 3711 or 3714 <= sic <= 3714 or 3716 <= sic <= 3716 or 
        3750 <= sic <= 3751 or 3792 <= sic <= 3792 or 3900 <= sic <= 3939 or 
        3990 <= sic <= 3999):
        return 'Durbl'
    # 3 Manuf - Manufacturing
    if (2520 <= sic <= 2589 or 2600 <= sic <= 2699 or 2750 <= sic <= 2769 or 
        3000 <= sic <= 3099 or 3200 <= sic <= 3569 or 3580 <= sic <= 3629 or 
        3700 <= sic <= 3709 or 3712 <= sic <= 3713 or 3715 <= sic <= 3715 or 
        3717 <= sic <= 3749 or 3752 <= sic <= 3791 or 3793 <= sic <= 3799 or 
        3830 <= sic <= 3839 or 3860 <= sic <= 3899):
        return 'Manuf'
    # 4 Enrgy - Oil, Gas, and Coal
    if (1200 <= sic <= 1399 or 2900 <= sic <= 2999):
        return 'Enrgy'
    # 5 Chems - Chemicals
    if (2800 <= sic <= 2829 or 2840 <= sic <= 2899):
        return 'Chems'
    # 6 BusEq - Business Equipment
    if (3570 <= sic <= 3579 or 3660 <= sic <= 3692 or 3694 <= sic <= 3699 or 
        3810 <= sic <= 3829 or 7370 <= sic <= 7379):
        return 'BusEq'
    # 7 Telcm - Telecom
    if (4800 <= sic <= 4899):
        return 'Telcm'
    # 8 Utils - Utilities
    if (4900 <= sic <= 4949):
        return 'Utils'
    # 9 Shops - Wholesale/Retail/Services
    if (5000 <= sic <= 5999 or 7200 <= sic <= 7299 or 7600 <= sic <= 7699):
        return 'Shops'
    # 10 Hlth - Healthcare
    if (2830 <= sic <= 2839 or 3693 <= sic <= 3693 or 3840 <= sic <= 3859 or 
        8000 <= sic <= 8099):
        return 'Hlth'
    # 11 Money - Finance
    if (6000 <= sic <= 6999):
        return 'Money'
    # 12 Other
    return 'Other'


def compute_features(df: pd.DataFrame) -> pd.DataFrame:
    """Compute matching features: ff12, term_loan indicator, clean spread, lagged log assets, lagged leverage, lagged ebitda."""
    out = df.copy()

    # Industry
    if 'sic' in out.columns:
        out['ff12'] = out['sic'].apply(get_fama_french_12_industry)
    else:
        out['ff12'] = 'Other'

    # Term loan indicator (treat all term loan variants as the same)
    if 'facility_type' in out.columns:
        out['term_loan'] = out['facility_type'].str.contains('term loan', case=False, na=False).astype(int)
    else:
        out['term_loan'] = 0

    # Clean spread (current period)
    if 'clean_interest_spread' in out.columns:
        out['spread'] = pd.to_numeric(out['clean_interest_spread'], errors='coerce')
    elif 'interest_spread' in out.columns:
        out['spread'] = pd.to_numeric(out['interest_spread'], errors='coerce')
    else:
        out['spread'] = np.nan

    # Use lagged log assets if available, otherwise compute from current at
    if 'log_at_lag1' in out.columns:
        out['log_at'] = pd.to_numeric(out['log_at_lag1'], errors='coerce')
    else:
        at = pd.to_numeric(out.get('at', np.nan), errors='coerce')
        out['log_at'] = np.log(at.replace({0: np.nan}))

    # Use lagged leverage if available, otherwise compute from current values
    if 'leverage_lag1' in out.columns:
        out['leverage'] = pd.to_numeric(out['leverage_lag1'], errors='coerce')
    else:
        at = pd.to_numeric(out.get('at', np.nan), errors='coerce')
        dltt = pd.to_numeric(out.get('dltt', np.nan), errors='coerce')
        dlc = pd.to_numeric(out.get('dlc', np.nan), errors='coerce')
        lt = pd.to_numeric(out.get('lt', np.nan), errors='coerce')
        
        lev1 = (dltt.fillna(0) + dlc.fillna(0)) / at
        lev2 = lt / at
        out['leverage'] = lev1
        out.loc[out['leverage'].isna(), 'leverage'] = lev2

    # Use lagged EBITDA if available, otherwise use current value
    if 'ebitda_lag1' in out.columns:
        out['ebitda_val'] = pd.to_numeric(out['ebitda_lag1'], errors='coerce')
    else:
        if 'ebitda' in out.columns:
            out['ebitda_val'] = pd.to_numeric(out['ebitda'], errors='coerce')
        else:
            out['ebitda_val'] = np.nan

    return out


def estimate_propensity_score(df: pd.DataFrame, match_cols: list[str]) -> pd.DataFrame:
    """
    Estimate propensity scores using logistic regression.
    
    Args:
        df: DataFrame with matching variables and nonbank_lender indicator
        match_cols: List of column names to use in propensity score model
    
    Returns:
        DataFrame with added propensity_score column
    """
    print("Estimating propensity scores using logistic regression...")
    
    # Prepare data for propensity score estimation
    df_ps = df.copy()
    
    # Create dummy variables for categorical variables
    if 'ff12' in df_ps.columns:
        ff12_dummies = pd.get_dummies(df_ps['ff12'], prefix='ff12')
        df_ps = pd.concat([df_ps, ff12_dummies], axis=1)
    
    # Define features for propensity score model (only numeric features)
    # Note: term_loan is already a binary indicator, so no dummies needed
    ps_features = match_cols.copy()
    
    # Filter to available numeric features only
    available_features = [col for col in ps_features if col in df_ps.columns]
    
    # Prepare X and y
    X = df_ps[available_features].fillna(0)  # Fill missing values with 0
    y = df_ps['nonbank_lender']
    
    # Standardize features
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X)
    
    # Fit logistic regression
    ps_model = LogisticRegression(random_state=42, max_iter=1000)
    ps_model.fit(X_scaled, y)
    
    # Predict propensity scores
    propensity_scores = ps_model.predict_proba(X_scaled)[:, 1]
    df_ps['propensity_score'] = propensity_scores
    
    print(f"Propensity score model fitted with {len(available_features)} features")
    print(f"Propensity score range: {propensity_scores.min():.4f} to {propensity_scores.max():.4f}")
    
    return df_ps


def propensity_score_matching(df_nonbank: pd.DataFrame, df_bank: pd.DataFrame, 
                            caliper_multiplier: float = 0.2) -> list[dict]:
    """
    Perform propensity score matching without replacement.
    
    Args:
        df_nonbank: DataFrame of nonbank loans with propensity scores
        df_bank: DataFrame of bank loans with propensity scores
        caliper_multiplier: Multiplier for standard deviation of logit propensity score (default 0.2)
    
    Returns:
        List of matched pairs
    """
    # Calculate caliper as 0.2 × SD(logit propensity score)
    all_scores = pd.concat([df_nonbank['propensity_score'], df_bank['propensity_score']])
    # Avoid log(0) and log(1) by clipping
    logit_scores = np.log(np.clip(all_scores, 1e-10, 1-1e-10) / (1 - np.clip(all_scores, 1e-10, 1-1e-10)))
    caliper = caliper_multiplier * np.std(logit_scores)
    
    print(f"Performing propensity score matching with caliper = {caliper:.4f} (0.2 × SD(logit PS))")
    
    matched_pairs = []
    used_bank_indices = set()
    
    # Sort nonbank loans by propensity score for deterministic matching
    df_nonbank_sorted = df_nonbank.sort_values('propensity_score').reset_index(drop=True)
    
    for _, nb_row in df_nonbank_sorted.iterrows():
        nb_ps = nb_row['propensity_score']
        
        # Find available bank loans within caliper
        available_banks = df_bank[~df_bank.index.isin(used_bank_indices)].copy()
        
        if available_banks.empty:
            continue
        
        # Calculate propensity score differences
        available_banks['ps_diff'] = abs(available_banks['propensity_score'] - nb_ps)
        
        # Filter by caliper
        within_caliper = available_banks[available_banks['ps_diff'] <= caliper]
        
        if within_caliper.empty:
            continue
        
        # Find closest match within caliper
        best_match_idx = within_caliper['ps_diff'].idxmin()
        best_match = df_bank.loc[best_match_idx]
        
        # Record the match (using lag1 variables for matching covariates)
        matched_pairs.append({
            'ff12': nb_row.get('ff12'),
            'term_loan': nb_row.get('term_loan'),
            'ps_distance': float(within_caliper.loc[best_match_idx, 'ps_diff']),
            'loan_id_nonbank': nb_row.get('loan_id'),
            'loan_id_bank': best_match.get('loan_id'),
            'accession_nonbank': nb_row.get('accession'),
            'accession_bank': best_match.get('accession'),
            'gvkey_nonbank': nb_row.get('gvkey'),
            'gvkey_bank': best_match.get('gvkey'),
            'year_nonbank': nb_row.get('year') if pd.notna(nb_row.get('year')) else np.nan,
            'year_bank': best_match.get('year') if pd.notna(best_match.get('year')) else np.nan,
            'propensity_score_nonbank': float(nb_ps),
            'propensity_score_bank': float(best_match['propensity_score']),
            'spread_nonbank': float(nb_row.get('spread')) if pd.notna(nb_row.get('spread')) else np.nan,
            'spread_bank': float(best_match.get('spread')) if pd.notna(best_match.get('spread')) else np.nan,
            # Loan characteristics
            'maturity_months_nonbank': float(nb_row.get('maturity_months')) if pd.notna(nb_row.get('maturity_months')) else np.nan,
            'maturity_months_bank': float(best_match.get('maturity_months')) if pd.notna(best_match.get('maturity_months')) else np.nan,
            'facility_amount_nonbank': float(nb_row.get('facility_amount')) if pd.notna(nb_row.get('facility_amount')) else np.nan,
            'facility_amount_bank': float(best_match.get('facility_amount')) if pd.notna(best_match.get('facility_amount')) else np.nan,
            # Lag1 variables (used for matching)
            'log_at_lag1_nonbank': float(nb_row.get('log_at')) if pd.notna(nb_row.get('log_at')) else np.nan,
            'log_at_lag1_bank': float(best_match.get('log_at')) if pd.notna(best_match.get('log_at')) else np.nan,
            'leverage_lag1_nonbank': float(nb_row.get('leverage')) if pd.notna(nb_row.get('leverage')) else np.nan,
            'leverage_lag1_bank': float(best_match.get('leverage')) if pd.notna(best_match.get('leverage')) else np.nan,
            'ebitda_lag1_nonbank': float(nb_row.get('ebitda_val')) if pd.notna(nb_row.get('ebitda_val')) else np.nan,
            'ebitda_lag1_bank': float(best_match.get('ebitda_val')) if pd.notna(best_match.get('ebitda_val')) else np.nan,
        })
        
        used_bank_indices.add(best_match_idx)
    
    print(f"Matched {len(matched_pairs)} pairs using propensity score matching")
    return matched_pairs


def main():
    print("=" * 80)
    print("5d_MatchedPanelLoans.py - Match Nonbank Loans to Bank Loans")
    print("=" * 80)

    script_dir = Path(__file__).parent
    panel_csv = script_dir / ".." / "Data" / "Intermediate" / "5c_PanelAllLoans.csv"
    output_csv = script_dir / ".." / "Data" / "Intermediate" / "5d_MatchedPanelLoans.csv"

    if not panel_csv.exists():
        print(f"Error: Input panel not found: {panel_csv}")
        return

    print("Loading panel...")
    df = pd.read_csv(panel_csv)
    print(f"Loaded {len(df)} rows, {len(df.columns)} columns")

    # Compute features
    df = compute_features(df)

    # Required columns
    req = ['nonbank_lender', 'ff12', 'term_loan', 'spread', 'log_at', 'leverage', 'ebitda_val', 'accession', 'loan_id']
    for r in req:
        if r not in df.columns:
            print(f"Error: required column missing: {r}")
            return

    # Keep only rows with all matching vars present
    df_match = df.dropna(subset=['ff12', 'term_loan', 'spread', 'log_at', 'leverage', 'ebitda_val', 'nonbank_lender'])
    print(f"Rows with complete matching vars: {len(df_match)}")

    # Split nonbank vs bank
    df_nonbank = df_match[df_match['nonbank_lender'] == 1].copy()
    df_bank = df_match[df_match['nonbank_lender'] == 0].copy()
    print(f"Nonbank candidates: {len(df_nonbank)}; Bank candidates: {len(df_bank)}")

    # Define matching variables for propensity score model
    match_cols = ['spread', 'log_at', 'leverage', 'ebitda_val']
    
    # Estimate propensity scores for all observations
    df_with_ps = estimate_propensity_score(df_match, match_cols)
    
    # Split back into nonbank and bank with propensity scores
    df_nonbank_ps = df_with_ps[df_with_ps['nonbank_lender'] == 1].copy()
    df_bank_ps = df_with_ps[df_with_ps['nonbank_lender'] == 0].copy()
    
    # Per-industry and facility-type propensity score matching
    matched_rows = []
    
    industries = sorted(df_nonbank_ps['ff12'].dropna().unique())
    for ind in industries:
        nb_ind = df_nonbank_ps[df_nonbank_ps['ff12'] == ind].copy()
        bank_ind = df_bank_ps[df_bank_ps['ff12'] == ind].copy()

        if bank_ind.empty:
            print(f"Warning: No bank candidates in industry {ind}; skipping {len(nb_ind)} nonbank rows")
            continue

        # Further split by term_loan within industry
        term_loan_values = sorted(nb_ind['term_loan'].dropna().unique())
        for term_val in term_loan_values:
            nb_sub = nb_ind[nb_ind['term_loan'] == term_val].copy()
            bank_sub = bank_ind[bank_ind['term_loan'] == term_val].copy()

            if bank_sub.empty:
                term_type = "Term Loan" if term_val == 1 else "Non-Term Loan"
                print(f"Warning: No bank candidates in industry {ind}, {term_type}; skipping {len(nb_sub)} nonbank rows")
                continue

            # Perform propensity score matching within this industry-term_loan combination
            matches = propensity_score_matching(nb_sub, bank_sub, caliper_multiplier=0.2)
            matched_rows.extend(matches)

    if not matched_rows:
        print("No matches were formed.")
        return

    df_matched = pd.DataFrame(matched_rows)
    df_matched = df_matched.sort_values(['ff12', 'term_loan', 'ps_distance']).reset_index(drop=True)

    print(f"Matched pairs formed: {len(df_matched)}")
    print(f"Saving to: {output_csv}")
    df_matched.to_csv(output_csv, index=False)
    print("Saved.")


if __name__ == "__main__":
    main()


