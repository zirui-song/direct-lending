#!/usr/bin/env python3
"""
5e_DescribeMatchedPanelLoans.py

Describe matched nonbank vs bank loans using borrower covariates and loan terms.

Inputs:
- ../Data/Intermediate/5d_MatchedPanelLoans.csv (matched accession pairs)
- ../Data/Intermediate/5c_PanelAllLoans.csv (full panel with covariates)

Outputs (figures):
- ../Results/Figures/5e_Hist_BorrowerCovariates_Nonbank_vs_Bank.png
- ../Results/Figures/5e_Hist_LoanTerms_Nonbank_vs_Bank.png
- ../Results/Figures/5e_Hist_<var>_Nonbank_vs_Bank.png (per-variable)

Notes:
- Borrower covariates: log(at) (size), leverage, ebitda, market_to_book (if present)
- Loan terms: clean_interest_spread, maturity_months, facility_amount
"""

import pandas as pd
import numpy as np
from pathlib import Path
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import plotly.io as pio
import re


def ensure_fig_dir() -> Path:
    fig_dir = Path("/Users/zrsong/MIT Dropbox/Zirui Song/Research Projects/PSW_Nonbank Direct Lending/Results/Figures")
    fig_dir.mkdir(parents=True, exist_ok=True)
    return fig_dir


def extract_year_from_date(date_col, df):
    """Extract year from various date formats."""
    if date_col not in df.columns:
        return None
    
    # Try different date parsing methods
    date_series = df[date_col].copy()
    
    # Convert to string and extract year
    date_series = date_series.astype(str)
    
    # Extract year from various formats
    def extract_year(date_str):
        if pd.isna(date_str) or date_str == 'nan' or date_str == '':
            return None
        
        # Try to extract 4-digit year
        year_match = re.search(r'(19|20)\d{2}', str(date_str))
        if year_match:
            return int(year_match.group())
        return None
    
    return date_series.apply(extract_year)


def create_time_series_plot(df: pd.DataFrame, fig_dir: Path) -> None:
    """Create time series plot showing number of loans by bank and nonbank over years."""
    
    # Filter to observations with valid years
    df_with_year = df[df['year'].notna() & (df['year'] >= 2010) & (df['year'] <= 2023)].copy()
    
    if len(df_with_year) == 0:
        print("Warning: No valid years found for time series plot")
        return
    
    # Count loans by year and group
    yearly_counts = df_with_year.groupby(['year', 'group']).size().unstack(fill_value=0)
    
    # Create the plot
    fig = go.Figure()
    
    # Add traces for each group
    colors = {"Nonbank": "#1f77b4", "Bank": "#ff7f0e"}
    
    for group in ["Nonbank", "Bank"]:
        if group in yearly_counts.columns:
            fig.add_trace(go.Scatter(
                x=yearly_counts.index,
                y=yearly_counts[group],
                mode='lines+markers',
                name=group,
                line=dict(color=colors[group], width=3),
                marker=dict(size=8)
            ))
    
    # Update layout
    fig.update_layout(
        title='Number of Matched Loans by Lender Type Over Time',
        xaxis_title='Year',
        yaxis_title='Number of Loans',
        title_x=0.5,
        height=500,
        width=1000,
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=1.02,
            xanchor="right",
            x=1
        )
    )
    
    # Add grid
    fig.update_xaxes(showgrid=True, gridwidth=1, gridcolor='lightgray')
    fig.update_yaxes(showgrid=True, gridwidth=1, gridcolor='lightgray')
    
    # Save the plot
    pio.write_image(fig, str(fig_dir / "5e_TimeSeries_MatchedLoans_ByLenderType.png"), 
                   width=1000, height=500, scale=2)
    
    print(f"Time series plot created: {len(df_with_year)} observations from {df_with_year['year'].min()} to {df_with_year['year'].max()}")


def load_data(base_dir: Path) -> tuple[pd.DataFrame, pd.DataFrame]:
    matched = pd.read_csv(base_dir / ".." / "Data" / "Intermediate" / "5d_MatchedPanelLoans.csv")
    panel = pd.read_csv(base_dir / ".." / "Data" / "Intermediate" / "5c_PanelAllLoans.csv")
    return matched, panel


def build_matched_frame(matched: pd.DataFrame, panel: pd.DataFrame) -> pd.DataFrame:
    # Select covariates of interest from panel (including lagged variables)
    want_cols = [
        "loan_id", "accession", "clean_interest_spread", "maturity_months", "facility_amount",
        "at", "dltt", "dlc", "lt", "ebitda", "market_to_book", "gvkey", "year",
        "log_at_lag1", "leverage_lag1", "ebitda_lag1"  # Add lagged variables
    ]
    have_cols = [c for c in want_cols if c in panel.columns]
    df_cov = panel[have_cols].copy()

    # Compute current period size and leverage
    df_cov["log_at"] = np.log(pd.to_numeric(df_cov.get("at"), errors="coerce").replace({0: np.nan}))
    dltt = pd.to_numeric(df_cov.get("dltt"), errors="coerce") if "dltt" in df_cov.columns else np.nan
    dlc = pd.to_numeric(df_cov.get("dlc"), errors="coerce") if "dlc" in df_cov.columns else np.nan
    lt = pd.to_numeric(df_cov.get("lt"), errors="coerce") if "lt" in df_cov.columns else np.nan
    at = pd.to_numeric(df_cov.get("at"), errors="coerce") if "at" in df_cov.columns else np.nan

    lev1 = (pd.Series(dltt).fillna(0) + pd.Series(dlc).fillna(0)) / pd.Series(at)
    lev2 = pd.Series(lt) / pd.Series(at)
    df_cov["leverage"] = lev1
    df_cov.loc[df_cov["leverage"].isna(), "leverage"] = lev2

    # Prepare nonbank and bank frames using loan_id for matching
    nonbank = matched[["loan_id_nonbank", "gvkey_nonbank", "term_loan", "ff12"]].merge(
        df_cov, left_on="loan_id_nonbank", right_on="loan_id", how="left"
    )
    nonbank["group"] = "Nonbank"
    nonbank["gvkey"] = nonbank["gvkey_nonbank"]  # Use the gvkey from matched data
    
    bank = matched[["loan_id_bank", "gvkey_bank", "term_loan", "ff12"]].merge(
        df_cov, left_on="loan_id_bank", right_on="loan_id", how="left"
    )
    bank["group"] = "Bank"
    bank["gvkey"] = bank["gvkey_bank"]  # Use the gvkey from matched data

    # Harmonize columns (include lagged variables)
    keep = ["group", "loan_id", "accession", "gvkey", "term_loan", "ff12", "clean_interest_spread", 
            "maturity_months", "facility_amount", "log_at", "leverage", "ebitda", "market_to_book", "year",
            "log_at_lag1", "leverage_lag1", "ebitda_lag1"]
    have_keep = [c for c in keep if c in nonbank.columns]
    df_nb = nonbank[have_keep].rename(columns={"loan_id": "loan_id_used"})
    have_keep = [c for c in keep if c in bank.columns]
    df_bk = bank[have_keep].rename(columns={"loan_id": "loan_id_used"})

    combined = pd.concat([df_nb, df_bk], ignore_index=True)
    return combined


def create_histograms(df: pd.DataFrame, fig_dir: Path) -> None:
    # Variable groups (current period)
    covariates = [c for c in ["log_at", "leverage", "ebitda", "market_to_book"] if c in df.columns]
    loan_terms = [c for c in ["clean_interest_spread", "maturity_months", "facility_amount"] if c in df.columns]
    # Lagged variables (used for matching)
    lagged_covariates = [c for c in ["log_at_lag1", "leverage_lag1", "ebitda_lag1"] if c in df.columns]

    # Helper to draw grouped hist
    def plot_grouped(vars_list, title, out_name, log_flags=None):
        rows = 1
        cols = len(vars_list)
        fig = make_subplots(rows=rows, cols=cols, subplot_titles=vars_list)
        colors = {"Nonbank": "#1f77b4", "Bank": "#ff7f0e"}
        for i, var in enumerate(vars_list, start=1):
            sub = df[["group", var]].dropna()
            # Basic clipping for facility_amount
            if var == "facility_amount":
                q99 = sub[var].quantile(0.99)
                sub = sub[sub[var] <= q99]
            # Reasonable clipping for spreads
            if var == "clean_interest_spread":
                sub = sub[(sub[var] >= 0) & (sub[var] <= 2000)]
            for g in ["Nonbank", "Bank"]:
                vals = sub[sub["group"] == g][var]
                fig.add_trace(go.Histogram(x=vals, name=g, opacity=0.6, marker_color=colors[g], nbinsx=30, showlegend=(i==1)), row=1, col=i)
        fig.update_layout(title=title, title_x=0.5, barmode="overlay", height=450)
        pio.write_image(fig, str(fig_dir / out_name), width=1500, height=450, scale=2)

    # Helper to draw histograms by term loan type
    def plot_by_term_loan_type(vars_list, title_prefix, out_prefix):
        if "term_loan" not in df.columns:
            return
        
        term_loan_types = sorted(df["term_loan"].dropna().unique())
        term_labels = {0: "Non-Term Loan", 1: "Term Loan"}
        
        for term_val in term_loan_types:
            df_term = df[df["term_loan"] == term_val]
            if len(df_term) < 10:  # Skip if too few observations
                continue
                
            rows = 1
            cols = len(vars_list)
            fig = make_subplots(rows=rows, cols=cols, subplot_titles=vars_list)
            colors = {"Nonbank": "#1f77b4", "Bank": "#ff7f0e"}
            
            for i, var in enumerate(vars_list, start=1):
                sub = df_term[["group", var]].dropna()
                # Basic clipping for facility_amount
                if var == "facility_amount":
                    q99 = sub[var].quantile(0.99)
                    sub = sub[sub[var] <= q99]
                # Reasonable clipping for spreads
                if var == "clean_interest_spread":
                    sub = sub[(sub[var] >= 0) & (sub[var] <= 2000)]
                    
                for g in ["Nonbank", "Bank"]:
                    vals = sub[sub["group"] == g][var]
                    fig.add_trace(go.Histogram(x=vals, name=g, opacity=0.6, marker_color=colors[g], nbinsx=30, showlegend=(i==1)), row=1, col=i)
            
            term_label = term_labels.get(term_val, f"Term Loan {term_val}")
            fig.update_layout(title=f"{title_prefix} - {term_label}", title_x=0.5, barmode="overlay", height=450)
            safe_term_label = term_label.replace(" ", "_").replace("/", "_").replace("-", "_")
            pio.write_image(fig, str(fig_dir / f"{out_prefix}_{safe_term_label}.png"), width=1500, height=450, scale=2)

    # Overall histograms
    if covariates:
        plot_grouped(covariates, "Borrower Covariates (Current): Nonbank vs Bank", "5e_Hist_BorrowerCovariates_Nonbank_vs_Bank.png")
        for v in covariates:
            plot_grouped([v], f"{v} (Current): Nonbank vs Bank", f"5e_Hist_{v}_Nonbank_vs_Bank.png")

    if lagged_covariates:
        plot_grouped(lagged_covariates, "Borrower Covariates (Lagged, Used for Matching): Nonbank vs Bank", "5e_Hist_BorrowerCovariates_Lagged_Nonbank_vs_Bank.png")
        for v in lagged_covariates:
            plot_grouped([v], f"{v} (Used for Matching): Nonbank vs Bank", f"5e_Hist_{v}_Nonbank_vs_Bank.png")

    if loan_terms:
        plot_grouped(loan_terms, "Loan Terms: Nonbank vs Bank", "5e_Hist_LoanTerms_Nonbank_vs_Bank.png")
        for v in loan_terms:
            plot_grouped([v], f"{v}: Nonbank vs Bank", f"5e_Hist_{v}_Nonbank_vs_Bank.png")

    # Histograms by term loan type
    if covariates:
        plot_by_term_loan_type(covariates, "Borrower Covariates (Current)", "5e_Hist_BorrowerCovariates_ByTermLoanType")
    if lagged_covariates:
        plot_by_term_loan_type(lagged_covariates, "Borrower Covariates (Lagged)", "5e_Hist_BorrowerCovariates_Lagged_ByTermLoanType")
    if loan_terms:
        plot_by_term_loan_type(loan_terms, "Loan Terms", "5e_Hist_LoanTerms_ByTermLoanType")


def print_matching_summary(df: pd.DataFrame) -> None:
    """Print summary statistics about the matched panel."""
    print("\n" + "="*60)
    print("MATCHING SUMMARY")
    print("="*60)
    
    # Basic counts
    nonbank_count = len(df[df["group"] == "Nonbank"])
    bank_count = len(df[df["group"] == "Bank"])
    print(f"Matched pairs: {nonbank_count} (should be equal)")
    print(f"Nonbank observations: {nonbank_count}")
    print(f"Bank observations: {bank_count}")
    
    # By term loan type
    if "term_loan" in df.columns:
        print(f"\nBy term loan type:")
        term_summary = df.groupby(["group", "term_loan"]).size().unstack(fill_value=0)
        term_summary.columns = ["Non-Term Loan", "Term Loan"]
        print(term_summary)
    
    # By industry
    if "ff12" in df.columns:
        print(f"\nBy Fama-French 12 industry:")
        ind_summary = df.groupby(["group", "ff12"]).size().unstack(fill_value=0)
        print(ind_summary)
    
    # Matching quality - compare means for lagged variables (used in matching)
    print(f"\nMatching Quality - Lagged Variables (used for matching):")
    lagged_vars = ["log_at_lag1", "leverage_lag1", "ebitda_lag1"]
    available_lagged = [v for v in lagged_vars if v in df.columns]
    
    for var in available_lagged:
        nonbank_mean = df[df["group"] == "Nonbank"][var].mean()
        bank_mean = df[df["group"] == "Bank"][var].mean()
        diff = nonbank_mean - bank_mean
        pct_diff = (diff / bank_mean * 100) if bank_mean != 0 else 0
        print(f"{var:20s}: Nonbank={nonbank_mean:8.2f}, Bank={bank_mean:8.2f}, Diff={diff:8.2f} ({pct_diff:+.1f}%)")
    
    # Current period variables
    print(f"\nCurrent Period Variables:")
    numeric_vars = ["clean_interest_spread", "log_at", "leverage", "ebitda", "maturity_months", "facility_amount"]
    available_vars = [v for v in numeric_vars if v in df.columns]
    
    for var in available_vars:
        nonbank_mean = df[df["group"] == "Nonbank"][var].mean()
        bank_mean = df[df["group"] == "Bank"][var].mean()
        diff = nonbank_mean - bank_mean
        pct_diff = (diff / bank_mean * 100) if bank_mean != 0 else 0
        print(f"{var:20s}: Nonbank={nonbank_mean:8.2f}, Bank={bank_mean:8.2f}, Diff={diff:8.2f} ({pct_diff:+.1f}%)")


def main():
    print("=" * 80)
    print("5e_DescribeMatchedPanelLoans.py - Describe Matched Panel")
    print("=" * 80)

    base_dir = Path(__file__).parent
    fig_dir = ensure_fig_dir()

    try:
        matched, panel = load_data(base_dir)
    except Exception as e:
        print(f"Error loading inputs: {e}")
        return

    print(f"Matched pairs: {len(matched)}")
    print(f"Panel rows: {len(panel)}")

    df = build_matched_frame(matched, panel)
    print(f"Combined analysis rows: {len(df)} (nonbank + bank)")
    print("Available columns:", [c for c in df.columns if c not in ["group", "loan_id_used"]])

    # Print matching summary
    print_matching_summary(df)

    # Create time series plot
    create_time_series_plot(df, fig_dir)

    # Create histograms
    create_histograms(df, fig_dir)
    print(f"\nFigures written to: {fig_dir}")


if __name__ == "__main__":
    main()


