#!/usr/bin/env python3
"""
5e_DescribeMatchedPanelLoans_Direct.py

Describe matched direct vs non-direct loans using borrower covariates and loan terms.

Inputs:
- ../Data/Intermediate/5d_MatchedPanelLoans_Direct.csv (matched accession pairs)
- ../Data/Intermediate/5c_PanelAllLoans.csv (full panel with covariates)

Outputs (figures):
- ../Results/Figures/5e_Hist_BorrowerCovariates_Direct_vs_NonDirect.png
- ../Results/Figures/5e_Hist_LoanTerms_Direct_vs_NonDirect.png
- ../Results/Figures/5e_Hist_<var>_Direct_vs_NonDirect.png (per-variable)

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
    """Create time series plot showing number of loans by direct and non-direct over years."""
    
    # Filter to observations with valid years (from beginning of sample to 2023)
    df_with_year = df[df['year'].notna() & (df['year'] <= 2023)].copy()
    
    if len(df_with_year) == 0:
        print("Warning: No valid years found for time series plot")
        return
    
    # Count loans by year and group
    yearly_counts = df_with_year.groupby(['year', 'group']).size().unstack(fill_value=0)
    
    # Create the plot
    fig = go.Figure()
    
    # Add traces for each group
    colors = {"Direct": "#2ca02c", "Non-Direct": "#d62728"}
    
    for group in ["Direct", "Non-Direct"]:
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
        title='Number of Matched Loans by Direct vs Non-Direct Over Time',
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
    pio.write_image(fig, str(fig_dir / "5e_TimeSeries_MatchedLoans_Direct_vs_NonDirect.png"), 
                   width=1000, height=500, scale=2)
    
    print(f"Time series plot created: {len(df_with_year)} observations from {df_with_year['year'].min()} to {df_with_year['year'].max()}")


def create_loan_count_plots(df: pd.DataFrame, fig_dir: Path) -> None:
    """Create bar charts showing total loan counts by direct vs non-direct, and direct bank vs direct nonbank."""
    
    # 1. Direct vs Non-Direct total counts
    direct_count = len(df[df["group"] == "Direct"])
    nondirect_count = len(df[df["group"] == "Non-Direct"])
    
    fig = go.Figure()
    fig.add_trace(go.Bar(
        x=["Direct", "Non-Direct"],
        y=[direct_count, nondirect_count],
        marker_color=["#2ca02c", "#d62728"],
        text=[direct_count, nondirect_count],
        textposition='auto',
    ))
    fig.update_layout(
        title='Total Number of Loans: Direct vs Non-Direct',
        xaxis_title='Loan Type',
        yaxis_title='Number of Loans',
        title_x=0.5,
        height=500,
        width=800
    )
    fig.update_xaxes(showgrid=True, gridwidth=1, gridcolor='lightgray')
    fig.update_yaxes(showgrid=True, gridwidth=1, gridcolor='lightgray')
    pio.write_image(fig, str(fig_dir / "5e_LoanCounts_Direct_vs_NonDirect.png"), 
                   width=800, height=500, scale=2)
    print(f"Loan count plot (Direct vs Non-Direct) created: Direct={direct_count}, Non-Direct={nondirect_count}")
    
    # 2. Direct Bank vs Direct Nonbank (only for direct loans)
    df_direct_only = df[df["group"] == "Direct"].copy()
    
    if "nonbank_lender" in df_direct_only.columns:
        direct_bank = len(df_direct_only[df_direct_only["nonbank_lender"] == 0])
        direct_nonbank = len(df_direct_only[df_direct_only["nonbank_lender"] == 1])
        
        fig2 = go.Figure()
        fig2.add_trace(go.Bar(
            x=["Direct Bank", "Direct Nonbank"],
            y=[direct_bank, direct_nonbank],
            marker_color=["#1f77b4", "#ff7f0e"],
            text=[direct_bank, direct_nonbank],
            textposition='auto',
        ))
        fig2.update_layout(
            title='Total Number of Direct Loans: Bank vs Nonbank',
            xaxis_title='Lender Type',
            yaxis_title='Number of Loans',
            title_x=0.5,
            height=500,
            width=800
        )
        fig2.update_xaxes(showgrid=True, gridwidth=1, gridcolor='lightgray')
        fig2.update_yaxes(showgrid=True, gridwidth=1, gridcolor='lightgray')
        pio.write_image(fig2, str(fig_dir / "5e_LoanCounts_DirectBank_vs_DirectNonbank.png"), 
                       width=800, height=500, scale=2)
        print(f"Loan count plot (Direct Bank vs Direct Nonbank) created: Direct Bank={direct_bank}, Direct Nonbank={direct_nonbank}")
    else:
        print("Warning: nonbank_lender column not found. Skipping direct bank vs direct nonbank plot.")


def create_direct_bank_vs_nonbank_timeseries(df: pd.DataFrame, fig_dir: Path) -> None:
    """Create time series plot showing number of direct bank vs direct nonbank loans over years."""
    
    # Filter to direct loans only
    df_direct_only = df[df["group"] == "Direct"].copy()
    
    if "nonbank_lender" not in df_direct_only.columns:
        print("Warning: nonbank_lender column not found. Skipping direct bank vs nonbank time series.")
        return
    
    # Filter to observations with valid years (from beginning of sample to 2023)
    df_with_year = df_direct_only[df_direct_only['year'].notna() & (df_direct_only['year'] <= 2023)].copy()
    
    if len(df_with_year) == 0:
        print("Warning: No valid years found for direct bank vs nonbank time series plot")
        return
    
    # Create lender type label
    df_with_year['lender_type_label'] = df_with_year['nonbank_lender'].map({0: 'Direct Bank', 1: 'Direct Nonbank'})
    
    # Count loans by year and lender type
    yearly_counts = df_with_year.groupby(['year', 'lender_type_label']).size().unstack(fill_value=0)
    
    # Create the plot
    fig = go.Figure()
    
    # Add traces for each lender type
    colors = {"Direct Bank": "#1f77b4", "Direct Nonbank": "#ff7f0e"}
    
    for lender_type in ["Direct Bank", "Direct Nonbank"]:
        if lender_type in yearly_counts.columns:
            fig.add_trace(go.Scatter(
                x=yearly_counts.index,
                y=yearly_counts[lender_type],
                mode='lines+markers',
                name=lender_type,
                line=dict(color=colors[lender_type], width=3),
                marker=dict(size=8)
            ))
    
    # Update layout
    fig.update_layout(
        title='Number of Direct Loans by Lender Type Over Time',
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
    pio.write_image(fig, str(fig_dir / "5e_TimeSeries_DirectBank_vs_DirectNonbank.png"), 
                   width=1000, height=500, scale=2)
    
    print(f"Time series plot (Direct Bank vs Direct Nonbank) created: {len(df_with_year)} observations from {df_with_year['year'].min()} to {df_with_year['year'].max()}")


def load_data(base_dir: Path) -> tuple[pd.DataFrame, pd.DataFrame]:
    matched = pd.read_csv(base_dir / ".." / "Data" / "Intermediate" / "5d_MatchedPanelLoans_Direct.csv")
    panel = pd.read_csv(base_dir / ".." / "Data" / "Intermediate" / "5c_PanelAllLoans.csv")
    return matched, panel


def build_matched_frame(matched: pd.DataFrame, panel: pd.DataFrame) -> pd.DataFrame:
    # Select covariates of interest from panel (including lagged variables and nonbank_lender)
    want_cols = [
        "loan_id", "accession", "clean_interest_spread", "maturity_months", "facility_amount",
        "at", "dltt", "dlc", "lt", "ebitda", "market_to_book", "gvkey", "year",
        "log_at_lag1", "leverage_lag1", "ebitda_lag1", "nonbank_lender"  # Add nonbank_lender
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

    # Prepare direct and non-direct frames using loan_id for matching
    direct = matched[["loan_id_direct", "gvkey_direct", "term_loan", "ff12"]].merge(
        df_cov, left_on="loan_id_direct", right_on="loan_id", how="left"
    )
    direct["group"] = "Direct"
    direct["gvkey"] = direct["gvkey_direct"]  # Use the gvkey from matched data
    
    nondirect = matched[["loan_id_nondirect", "gvkey_nondirect", "term_loan", "ff12"]].merge(
        df_cov, left_on="loan_id_nondirect", right_on="loan_id", how="left"
    )
    nondirect["group"] = "Non-Direct"
    nondirect["gvkey"] = nondirect["gvkey_nondirect"]  # Use the gvkey from matched data

    # Harmonize columns (include lagged variables and nonbank_lender)
    keep = ["group", "loan_id", "accession", "gvkey", "term_loan", "ff12", "clean_interest_spread", 
            "maturity_months", "facility_amount", "log_at", "leverage", "ebitda", "market_to_book", "year",
            "log_at_lag1", "leverage_lag1", "ebitda_lag1", "nonbank_lender"]
    have_keep = [c for c in keep if c in direct.columns]
    df_direct = direct[have_keep].rename(columns={"loan_id": "loan_id_used"})
    have_keep = [c for c in keep if c in nondirect.columns]
    df_nondirect = nondirect[have_keep].rename(columns={"loan_id": "loan_id_used"})

    combined = pd.concat([df_direct, df_nondirect], ignore_index=True)
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
        colors = {"Direct": "#2ca02c", "Non-Direct": "#d62728"}
        for i, var in enumerate(vars_list, start=1):
            sub = df[["group", var]].dropna()
            # Basic clipping for facility_amount
            if var == "facility_amount":
                q99 = sub[var].quantile(0.99)
                sub = sub[sub[var] <= q99]
            # Reasonable clipping for spreads
            if var == "clean_interest_spread":
                sub = sub[(sub[var] >= 0) & (sub[var] <= 2000)]
            for g in ["Direct", "Non-Direct"]:
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
            colors = {"Direct": "#2ca02c", "Non-Direct": "#d62728"}
            
            for i, var in enumerate(vars_list, start=1):
                sub = df_term[["group", var]].dropna()
                # Basic clipping for facility_amount
                if var == "facility_amount":
                    q99 = sub[var].quantile(0.99)
                    sub = sub[sub[var] <= q99]
                # Reasonable clipping for spreads
                if var == "clean_interest_spread":
                    sub = sub[(sub[var] >= 0) & (sub[var] <= 2000)]
                    
                for g in ["Direct", "Non-Direct"]:
                    vals = sub[sub["group"] == g][var]
                    fig.add_trace(go.Histogram(x=vals, name=g, opacity=0.6, marker_color=colors[g], nbinsx=30, showlegend=(i==1)), row=1, col=i)
            
            term_label = term_labels.get(term_val, f"Term Loan {term_val}")
            fig.update_layout(title=f"{title_prefix} - {term_label}", title_x=0.5, barmode="overlay", height=450)
            safe_term_label = term_label.replace(" ", "_").replace("/", "_").replace("-", "_")
            pio.write_image(fig, str(fig_dir / f"{out_prefix}_{safe_term_label}.png"), width=1500, height=450, scale=2)

    # Overall histograms
    if covariates:
        plot_grouped(covariates, "Borrower Covariates (Current): Direct vs Non-Direct", "5e_Hist_BorrowerCovariates_Direct_vs_NonDirect.png")
        for v in covariates:
            plot_grouped([v], f"{v} (Current): Direct vs Non-Direct", f"5e_Hist_{v}_Direct_vs_NonDirect.png")

    if lagged_covariates:
        plot_grouped(lagged_covariates, "Borrower Covariates (Lagged, Used for Matching): Direct vs Non-Direct", "5e_Hist_BorrowerCovariates_Lagged_Direct_vs_NonDirect.png")
        for v in lagged_covariates:
            plot_grouped([v], f"{v} (Used for Matching): Direct vs Non-Direct", f"5e_Hist_{v}_Direct_vs_NonDirect.png")

    if loan_terms:
        plot_grouped(loan_terms, "Loan Terms: Direct vs Non-Direct", "5e_Hist_LoanTerms_Direct_vs_NonDirect.png")
        for v in loan_terms:
            plot_grouped([v], f"{v}: Direct vs Non-Direct", f"5e_Hist_{v}_Direct_vs_NonDirect.png")

    # Histograms by term loan type
    if covariates:
        plot_by_term_loan_type(covariates, "Borrower Covariates (Current)", "5e_Hist_BorrowerCovariates_Direct_ByTermLoanType")
    if lagged_covariates:
        plot_by_term_loan_type(lagged_covariates, "Borrower Covariates (Lagged)", "5e_Hist_BorrowerCovariates_Lagged_Direct_ByTermLoanType")
    if loan_terms:
        plot_by_term_loan_type(loan_terms, "Loan Terms", "5e_Hist_LoanTerms_Direct_ByTermLoanType")


def print_matching_summary(df: pd.DataFrame) -> None:
    """Print summary statistics about the matched panel."""
    print("\n" + "="*60)
    print("MATCHING SUMMARY")
    print("="*60)
    
    # Basic counts
    direct_count = len(df[df["group"] == "Direct"])
    nondirect_count = len(df[df["group"] == "Non-Direct"])
    print(f"Matched pairs: {direct_count} (should be equal)")
    print(f"Direct observations: {direct_count}")
    print(f"Non-Direct observations: {nondirect_count}")
    
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
        direct_mean = df[df["group"] == "Direct"][var].mean()
        nondirect_mean = df[df["group"] == "Non-Direct"][var].mean()
        diff = direct_mean - nondirect_mean
        pct_diff = (diff / nondirect_mean * 100) if nondirect_mean != 0 else 0
        print(f"{var:20s}: Direct={direct_mean:8.2f}, Non-Direct={nondirect_mean:8.2f}, Diff={diff:8.2f} ({pct_diff:+.1f}%)")
    
    # Current period variables
    print(f"\nCurrent Period Variables:")
    numeric_vars = ["clean_interest_spread", "log_at", "leverage", "ebitda", "maturity_months", "facility_amount"]
    available_vars = [v for v in numeric_vars if v in df.columns]
    
    for var in available_vars:
        direct_mean = df[df["group"] == "Direct"][var].mean()
        nondirect_mean = df[df["group"] == "Non-Direct"][var].mean()
        diff = direct_mean - nondirect_mean
        pct_diff = (diff / nondirect_mean * 100) if nondirect_mean != 0 else 0
        print(f"{var:20s}: Direct={direct_mean:8.2f}, Non-Direct={nondirect_mean:8.2f}, Diff={diff:8.2f} ({pct_diff:+.1f}%)")


def main():
    print("=" * 80)
    print("5e_DescribeMatchedPanelLoans_Direct.py - Describe Matched Direct vs Non-Direct Panel")
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
    print(f"Combined analysis rows: {len(df)} (direct + non-direct)")
    print("Available columns:", [c for c in df.columns if c not in ["group", "loan_id_used"]])

    # Print matching summary
    print_matching_summary(df)

    # Create loan count plots
    create_loan_count_plots(df, fig_dir)

    # Create time series plot
    create_time_series_plot(df, fig_dir)

    # Create direct bank vs nonbank time series
    create_direct_bank_vs_nonbank_timeseries(df, fig_dir)

    # Create histograms
    create_histograms(df, fig_dir)
    print(f"\nFigures written to: {fig_dir}")


if __name__ == "__main__":
    main()

